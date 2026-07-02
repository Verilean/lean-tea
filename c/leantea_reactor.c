/* leantea_reactor.c — Non-blocking HTTP/1.1 reactor for LeanTea.Net.ReactorServer.
 *
 * Problem this solves
 * -------------------
 * The FFI FastServer (see leantea_fastnet.c) recovered ~10x throughput
 * by removing the Lean-async / libuv hop from every recv/send. But
 * that model parks one OS thread per open connection: at 10 k idle
 * keep-alive connections you either run out of `LEAN_NUM_THREADS`
 * task workers or run out of OS-thread stack space. Chat / SSE /
 * long-poll workloads can't live on it.
 *
 * This reactor is the standard "one epoll/kqueue thread + non-blocking
 * fds" design. All accepted sockets are set O_NONBLOCK; a single
 * kqueue(2) or epoll(2) thread drains readable/writable events; per-
 * connection state (recv accumulator, send remnant) lives in C; a
 * Lean callback is invoked once per fully-buffered request to produce
 * the response bytes.
 *
 * The Lean side stays functional / linear: the callback is
 * `ByteArray -> IO ByteArray` — request bytes in, response bytes out.
 * No fiber, no promise, no thread-per-conn.
 *
 * Portability
 * -----------
 * kqueue on macOS + BSD, epoll on Linux. The dispatching loop is
 * behind #ifdef; the connection-state code and Lean interop are
 * shared.
 */

#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <unistd.h>

#if defined(__APPLE__) || defined(__FreeBSD__)
  #define LEANTEA_HAVE_KQUEUE 1
  #include <sys/event.h>
  #include <sys/time.h>
#elif defined(__linux__)
  #define LEANTEA_HAVE_EPOLL 1
  #include <sys/epoll.h>
#else
  #error "leantea_reactor: neither kqueue nor epoll available"
#endif

#include <lean/lean.h>

/* ------------------------------------------------------------ */
/* Small buffer type                                             */
/* ------------------------------------------------------------ */

typedef struct {
    uint8_t *data;
    size_t   len;
    size_t   cap;
} buf_t;

static void buf_init(buf_t *b) { b->data = NULL; b->len = 0; b->cap = 0; }

static bool buf_reserve(buf_t *b, size_t need) {
    if (b->cap >= need) return true;
    size_t nc = b->cap ? b->cap * 2 : 4096;
    while (nc < need) nc *= 2;
    uint8_t *p = (uint8_t *)realloc(b->data, nc);
    if (!p) return false;
    b->data = p; b->cap = nc;
    return true;
}

static void buf_free(buf_t *b) {
    free(b->data); b->data = NULL; b->len = 0; b->cap = 0;
}

/* ------------------------------------------------------------ */
/* Per-connection state                                          */
/* ------------------------------------------------------------ */

typedef struct conn_s {
    int fd;
    buf_t inbuf;        /* accumulated recv bytes for the current request */
    buf_t outbuf;       /* response bytes not yet fully sent               */
    size_t out_sent;    /* how many bytes of outbuf we've already written  */
    bool want_close;    /* set once we've decided this is the last request */
    bool writing;       /* whether we're currently mid-write on this fd    */
} conn_t;

static conn_t *conn_new(int fd) {
    conn_t *c = (conn_t *)calloc(1, sizeof(conn_t));
    if (!c) return NULL;
    c->fd = fd;
    buf_init(&c->inbuf); buf_init(&c->outbuf);
    return c;
}

static void conn_free(conn_t *c) {
    if (!c) return;
    buf_free(&c->inbuf); buf_free(&c->outbuf);
    close(c->fd);
    free(c);
}

/* ------------------------------------------------------------ */
/* Reactor state (one per `reactor_create` call)                 */
/* ------------------------------------------------------------ */

typedef struct reactor_s {
    int          poll_fd;     /* kqueue or epoll fd                     */
    int          listen_fd;   /* the (single) listener                  */
    lean_object *handler;     /* Lean closure  ByteArray -> IO ByteArray */
    pthread_t    thread;
    atomic_int   stop;
} reactor_t;

/* Forward decls */
static void reactor_watch_read (reactor_t *r, int fd, void *udata);
static void reactor_watch_write(reactor_t *r, int fd, void *udata);
static void reactor_unwatch    (reactor_t *r, int fd);

/* ------------------------------------------------------------ */
/* Utility: non-blocking on a socket                             */
/* ------------------------------------------------------------ */

static int set_nonblocking(int fd) {
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags < 0) return -1;
    return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

/* ------------------------------------------------------------ */
/* Poll-abstraction: register / arm / unregister                 */
/* ------------------------------------------------------------ */

#ifdef LEANTEA_HAVE_KQUEUE
static void reactor_watch_read(reactor_t *r, int fd, void *udata) {
    struct kevent ev;
    EV_SET(&ev, fd, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, udata);
    kevent(r->poll_fd, &ev, 1, NULL, 0, NULL);
}
static void reactor_watch_write(reactor_t *r, int fd, void *udata) {
    struct kevent ev;
    EV_SET(&ev, fd, EVFILT_WRITE, EV_ADD | EV_ENABLE, 0, 0, udata);
    kevent(r->poll_fd, &ev, 1, NULL, 0, NULL);
}
static void reactor_unwatch(reactor_t *r, int fd) {
    struct kevent ev[2];
    EV_SET(&ev[0], fd, EVFILT_READ,  EV_DELETE, 0, 0, NULL);
    EV_SET(&ev[1], fd, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
    kevent(r->poll_fd, ev, 2, NULL, 0, NULL);
}
#endif

#ifdef LEANTEA_HAVE_EPOLL
static void reactor_arm(reactor_t *r, int fd, uint32_t events, void *udata) {
    struct epoll_event ev;
    memset(&ev, 0, sizeof(ev));
    ev.events = events | EPOLLET;
    ev.data.ptr = udata;
    /* Try ADD; if already registered, MOD. */
    if (epoll_ctl(r->poll_fd, EPOLL_CTL_ADD, fd, &ev) < 0) {
        epoll_ctl(r->poll_fd, EPOLL_CTL_MOD, fd, &ev);
    }
}
static void reactor_watch_read(reactor_t *r, int fd, void *udata) {
    reactor_arm(r, fd, EPOLLIN, udata);
}
static void reactor_watch_write(reactor_t *r, int fd, void *udata) {
    reactor_arm(r, fd, EPOLLOUT, udata);
}
static void reactor_unwatch(reactor_t *r, int fd) {
    epoll_ctl(r->poll_fd, EPOLL_CTL_DEL, fd, NULL);
}
#endif

/* ------------------------------------------------------------ */
/* HTTP header parsing helpers (just enough to slice a request)  */
/* ------------------------------------------------------------ */

/* Find "\r\n\r\n" — return offset of first byte of body, or -1. */
static ssize_t find_header_end(const uint8_t *buf, size_t len) {
    if (len < 4) return -1;
    for (size_t i = 0; i + 3 < len; i++) {
        if (buf[i] == '\r' && buf[i+1] == '\n' &&
            buf[i+2] == '\r' && buf[i+3] == '\n') {
            return (ssize_t)(i + 4);
        }
    }
    return -1;
}

/* Scan headers for "content-length:" — case-insensitive.
   Returns -1 if absent, else the parsed non-negative integer. */
static long parse_content_length(const uint8_t *buf, size_t hdr_len) {
    static const char key[] = "content-length:";
    size_t klen = sizeof(key) - 1;
    for (size_t i = 0; i + klen < hdr_len; i++) {
        bool match = true;
        for (size_t j = 0; j < klen; j++) {
            char c = (char)buf[i + j];
            if (c >= 'A' && c <= 'Z') c = (char)(c + 32);
            if (c != key[j]) { match = false; break; }
        }
        if (!match) continue;
        /* skip whitespace, read digits */
        size_t p = i + klen;
        while (p < hdr_len && (buf[p] == ' ' || buf[p] == '\t')) p++;
        long v = 0;
        while (p < hdr_len && buf[p] >= '0' && buf[p] <= '9') {
            v = v * 10 + (buf[p] - '0');
            p++;
        }
        return v;
    }
    return -1;
}

/* ------------------------------------------------------------ */
/* Invoking the Lean callback                                    */
/* ------------------------------------------------------------ */

/* handler : ByteArray -> IO ByteArray
   We invoke it with the raw request bytes, pull out the response
   ByteArray from the IO result, and return a fresh strong reference. */
static lean_obj_res invoke_handler(lean_object *h, const uint8_t *req, size_t req_len) {
    /* Build a fresh ByteArray owning a copy of the request bytes. */
    lean_object *req_ba = lean_alloc_sarray(1, req_len, req_len);
    memcpy(lean_sarray_cptr(req_ba), req, req_len);

    /* Increment the handler refcount because lean_apply_1 consumes it. */
    lean_inc(h);
    lean_object *io_thunk = lean_apply_1(h, req_ba);
    /* io_thunk : IO ByteArray. Force by applying the RealWorld token. */
    lean_object *io_res = lean_apply_1(io_thunk, lean_box(0));

    if (lean_io_result_is_ok(io_res)) {
        lean_object *ba = lean_io_result_get_value(io_res);
        lean_inc(ba);
        lean_dec(io_res);
        return ba;
    } else {
        lean_dec(io_res);
        /* Return a canned 500 so the connection can be shut cleanly. */
        static const char err[] =
            "HTTP/1.1 500 Internal Server Error\r\n"
            "content-length: 0\r\n"
            "connection: close\r\n\r\n";
        size_t n = sizeof(err) - 1;
        lean_object *ba = lean_alloc_sarray(1, n, n);
        memcpy(lean_sarray_cptr(ba), err, n);
        return ba;
    }
}

/* ------------------------------------------------------------ */
/* Per-connection event handling                                 */
/* ------------------------------------------------------------ */

static void conn_close(reactor_t *r, conn_t *c) {
    reactor_unwatch(r, c->fd);
    conn_free(c);
}

/* Try to drain outbuf via non-blocking send.
   Returns  0: fully sent, 1: partial (EAGAIN), -1: error. */
static int conn_try_write(conn_t *c) {
    while (c->out_sent < c->outbuf.len) {
        ssize_t n = send(c->fd,
                         c->outbuf.data + c->out_sent,
                         c->outbuf.len - c->out_sent,
                         0);
        if (n < 0) {
            if (errno == EINTR) continue;
            if (errno == EAGAIN || errno == EWOULDBLOCK) return 1;
            return -1;
        }
        c->out_sent += (size_t)n;
    }
    /* fully sent */
    c->outbuf.len = 0;
    c->out_sent = 0;
    return 0;
}

/* Called when there's data in inbuf; slice out a full request, invoke
   Lean, buffer the response.  Returns the number of requests handled
   in this call (>=0), or -1 on fatal error. */
static int conn_process(reactor_t *r, conn_t *c) {
    int handled = 0;
    for (;;) {
        ssize_t hdr_end = find_header_end(c->inbuf.data, c->inbuf.len);
        if (hdr_end < 0) return handled;
        long cl = parse_content_length(c->inbuf.data, (size_t)hdr_end);
        if (cl < 0) cl = 0;
        size_t need = (size_t)hdr_end + (size_t)cl;
        if (c->inbuf.len < need) return handled;

        /* We have one full request in [0, need). Invoke Lean. */
        lean_object *resp = invoke_handler(r->handler, c->inbuf.data, need);
        size_t rlen = lean_sarray_size(resp);
        if (!buf_reserve(&c->outbuf, c->outbuf.len + rlen)) {
            lean_dec(resp); return -1;
        }
        memcpy(c->outbuf.data + c->outbuf.len, lean_sarray_cptr(resp), rlen);
        c->outbuf.len += rlen;
        lean_dec(resp);

        /* Consume the request bytes; carry any pipelined bytes forward. */
        size_t rest = c->inbuf.len - need;
        if (rest > 0) {
            memmove(c->inbuf.data, c->inbuf.data + need, rest);
        }
        c->inbuf.len = rest;
        handled++;
    }
}

/* Called by the reactor when the fd is readable. */
static void on_readable(reactor_t *r, conn_t *c) {
    /* Drain non-blocking recv into inbuf. */
    for (;;) {
        if (!buf_reserve(&c->inbuf, c->inbuf.len + 8192)) {
            conn_close(r, c); return;
        }
        ssize_t n = recv(c->fd,
                         c->inbuf.data + c->inbuf.len,
                         c->inbuf.cap - c->inbuf.len,
                         0);
        if (n < 0) {
            if (errno == EINTR) continue;
            if (errno == EAGAIN || errno == EWOULDBLOCK) break;
            conn_close(r, c); return;
        }
        if (n == 0) {
            /* Client closed. If we have a pending response buffered,
               still try to send it, otherwise tear down. */
            c->want_close = true;
            break;
        }
        c->inbuf.len += (size_t)n;
    }

    /* Slice out and process any complete requests. */
    int handled = conn_process(r, c);
    if (handled < 0) { conn_close(r, c); return; }

    /* Try to flush any queued response. */
    if (c->outbuf.len > 0) {
        int w = conn_try_write(c);
        if (w < 0) { conn_close(r, c); return; }
        if (w == 1) {
            /* Partial write — register for EPOLLOUT / EVFILT_WRITE. */
            c->writing = true;
            reactor_watch_write(r, c->fd, c);
            return;
        }
    }

    /* Nothing left to write. If client closed and no response pending,
       we're done. */
    if (c->want_close && c->outbuf.len == 0) {
        conn_close(r, c);
    }
}

/* Called when the fd is writable and we had a partial write. */
static void on_writable(reactor_t *r, conn_t *c) {
    int w = conn_try_write(c);
    if (w < 0) { conn_close(r, c); return; }
    if (w == 1) return;  /* still partial */
    c->writing = false;
    /* Response fully sent. Re-arm for read. */
    reactor_watch_read(r, c->fd, c);
    if (c->want_close) conn_close(r, c);
}

/* Called when the listener is readable — accept N new connections. */
static void on_accept(reactor_t *r) {
    for (;;) {
        struct sockaddr_in cli; socklen_t clen = sizeof(cli);
        int cfd = accept(r->listen_fd, (struct sockaddr *)&cli, &clen);
        if (cfd < 0) {
            if (errno == EINTR) continue;
            if (errno == EAGAIN || errno == EWOULDBLOCK) return;
            return;
        }
        set_nonblocking(cfd);
        int one = 1;
        setsockopt(cfd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));
        conn_t *c = conn_new(cfd);
        if (!c) { close(cfd); continue; }
        reactor_watch_read(r, cfd, c);
    }
}

/* ------------------------------------------------------------ */
/* Reactor thread                                                */
/* ------------------------------------------------------------ */

#ifdef LEANTEA_HAVE_KQUEUE
static void reactor_loop(reactor_t *r) {
    struct kevent evs[128];
    while (!atomic_load(&r->stop)) {
        int n = kevent(r->poll_fd, NULL, 0, evs, 128, NULL);
        if (n < 0) {
            if (errno == EINTR) continue;
            break;
        }
        for (int i = 0; i < n; i++) {
            void *ud = evs[i].udata;
            int fd = (int)evs[i].ident;
            if (fd == r->listen_fd) {
                on_accept(r);
            } else if (ud) {
                conn_t *c = (conn_t *)ud;
                if (evs[i].filter == EVFILT_READ)  on_readable(r, c);
                if (evs[i].filter == EVFILT_WRITE) on_writable(r, c);
            }
        }
    }
}
#endif

#ifdef LEANTEA_HAVE_EPOLL
static void reactor_loop(reactor_t *r) {
    struct epoll_event evs[128];
    while (!atomic_load(&r->stop)) {
        int n = epoll_wait(r->poll_fd, evs, 128, -1);
        if (n < 0) {
            if (errno == EINTR) continue;
            break;
        }
        for (int i = 0; i < n; i++) {
            void *ud = evs[i].data.ptr;
            if (ud == NULL) {
                on_accept(r);
            } else {
                conn_t *c = (conn_t *)ud;
                if (evs[i].events & EPOLLIN)  on_readable(r, c);
                if (evs[i].events & EPOLLOUT) on_writable(r, c);
            }
        }
    }
}
#endif

static void *reactor_thread_main(void *arg) {
    reactor_t *r = (reactor_t *)arg;
    reactor_loop(r);
    return NULL;
}

/* ------------------------------------------------------------ */
/* Lean-facing entry point                                        */
/* ------------------------------------------------------------ */

/* Build+bind+listen a listener with SO_REUSEADDR + SO_REUSEPORT,
   non-blocking. Same shape as lean_ft_bind_reuseport but no fd
   returned (we stash inside the reactor). */
static int build_listener(uint16_t port) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    int one = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
#ifdef SO_REUSEPORT
    setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &one, sizeof(one));
#endif
    setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons(port);

    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) { close(fd); return -1; }
    if (listen(fd, 1024) < 0) { close(fd); return -1; }
    if (set_nonblocking(fd) < 0) { close(fd); return -1; }
    return fd;
}

/* lean_reactor_run(port, handler) : IO Unit
   Spins up a reactor and joins its thread. This call blocks the
   caller for the lifetime of the server — the way the existing
   `serve` functions do. */
LEAN_EXPORT lean_obj_res lean_reactor_run(uint16_t port,
                                          lean_object *handler,
                                          lean_object *w) {
    (void)w;
    reactor_t *r = (reactor_t *)calloc(1, sizeof(reactor_t));
    if (!r) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("reactor: alloc failed")));
    }
    atomic_init(&r->stop, 0);
    r->handler = handler; /* handler owned by the reactor for its lifetime */

#ifdef LEANTEA_HAVE_KQUEUE
    r->poll_fd = kqueue();
#endif
#ifdef LEANTEA_HAVE_EPOLL
    r->poll_fd = epoll_create1(0);
#endif
    if (r->poll_fd < 0) {
        free(r);
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("reactor: poll fd create")));
    }

    r->listen_fd = build_listener(port);
    if (r->listen_fd < 0) {
        close(r->poll_fd); free(r);
        char buf[128];
        snprintf(buf, sizeof(buf), "reactor: bind :%u failed: %s", port, strerror(errno));
        return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(buf)));
    }
    /* Register the listener with a NULL/0 udata so the loop can tell
       "this is the accepter" apart from a regular conn.
       kqueue path uses ident==listen_fd check; epoll uses data.ptr==NULL. */
    reactor_watch_read(r, r->listen_fd, NULL);

    /* Run the loop on this OS thread — no background thread needed.
       Callers already spawn `main` inside its own thread if they want. */
    reactor_loop(r);

    /* Not really reachable in normal ops. */
    close(r->listen_fd);
    close(r->poll_fd);
    free(r);
    return lean_io_result_mk_ok(lean_box(0));
}
