/* leantea_fastnet.c — POSIX-native TCP primitives for LeanTea.Net.FastServer.
 *
 * Why not `Std.Async.TCP` ?
 *   Std.Async.TCP is libuv-backed and every recv/send hops through an
 *   async task + `.block` wake. Per-syscall that costs ~100-500us in
 *   our numbers. A blocking-thread-per-connection model side-steps
 *   the entire async scheduler: a Lean worker thread parks in the
 *   kernel on accept()/recv() and wakes on data. Cheaper per hop; and
 *   more importantly, N listener sockets bound with SO_REUSEPORT are
 *   the only way to scale accept() past one core.
 *
 * Surface (all `@[extern]`-facing):
 *   bindReusePort  : UInt16 -> IO UInt32       -- listener fd
 *   acceptOne      : UInt32 -> IO UInt32       -- client fd
 *   recvBytes      : UInt32 -> UInt32 -> IO ByteArray
 *   sendBytes      : UInt32 -> ByteArray -> IO Unit
 *   shutdownFd     : UInt32 -> IO Unit
 *   closeFd        : UInt32 -> IO Unit
 *
 * Every helper here reports errno via lean_io_result_mk_error with a
 * user-error string. Callers on the Lean side see ordinary IO errors.
 */

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <unistd.h>

#include <lean/lean.h>

static lean_obj_res mk_io_error(const char *msg_prefix) {
    char buf[256];
    snprintf(buf, sizeof(buf), "%s: %s", msg_prefix, strerror(errno));
    return lean_io_result_mk_error(
        lean_mk_io_user_error(lean_mk_string(buf)));
}

/* bindReusePort(port) — create+configure+bind+listen a TCP socket. */
LEAN_EXPORT lean_obj_res lean_ft_bind_reuseport(uint16_t port, lean_object *w) {
    (void)w;
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return mk_io_error("socket");

    int one = 1;
    /* Both are needed: REUSEADDR lets us rebind quickly after crash,
       REUSEPORT lets multiple listeners share the port (kernel
       round-robins accepts across them). */
    if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one)) < 0) {
        close(fd); return mk_io_error("SO_REUSEADDR");
    }
#ifdef SO_REUSEPORT
    if (setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &one, sizeof(one)) < 0) {
        close(fd); return mk_io_error("SO_REUSEPORT");
    }
#endif
    /* Nagle off — small responses hit the wire immediately. */
    setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons(port);

    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        close(fd); return mk_io_error("bind");
    }
    /* Backlog 1024: matches the tuned nginx conf in bench_nginx.conf. */
    if (listen(fd, 1024) < 0) {
        close(fd); return mk_io_error("listen");
    }
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)fd));
}

/* acceptOne(fd) — block until one client connects. */
LEAN_EXPORT lean_obj_res lean_ft_accept_one(uint32_t fd, lean_object *w) {
    (void)w;
    struct sockaddr_in cli;
    socklen_t clen = sizeof(cli);
    int cfd;
    do {
        cfd = accept((int)fd, (struct sockaddr *)&cli, &clen);
    } while (cfd < 0 && errno == EINTR);
    if (cfd < 0) return mk_io_error("accept");
    /* Also disable Nagle on the accepted socket — the listener setting
       doesn't propagate on macOS. */
    int one = 1;
    setsockopt(cfd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)cfd));
}

/* recvBytes(fd, max) — one blocking recv. Returns whatever the kernel
   hands over up to `max`. Empty ByteArray => EOF. */
LEAN_EXPORT lean_obj_res lean_ft_recv_bytes(uint32_t fd, uint32_t max, lean_object *w) {
    (void)w;
    if (max == 0) max = 4096;
    /* Allocate a Lean sarray directly and recv into its buffer.
       We over-allocate to `max` then shrink via the size field. */
    lean_object *ba = lean_alloc_sarray(1, 0, (size_t)max);
    uint8_t *buf = lean_sarray_cptr(ba);
    ssize_t n;
    do {
        n = recv((int)fd, buf, (size_t)max, 0);
    } while (n < 0 && errno == EINTR);
    if (n < 0) {
        lean_dec(ba);
        return mk_io_error("recv");
    }
    lean_sarray_set_size(ba, (size_t)n);
    return lean_io_result_mk_ok(ba);
}

/* sendBytes(fd, ba) — write-all, retrying on partial writes / EINTR. */
LEAN_EXPORT lean_obj_res lean_ft_send_bytes(uint32_t fd, b_lean_obj_arg ba, lean_object *w) {
    (void)w;
    size_t total = lean_sarray_size(ba);
    const uint8_t *data = lean_sarray_cptr(ba);
    size_t sent = 0;
    while (sent < total) {
        ssize_t n = send((int)fd, data + sent, total - sent, 0);
        if (n < 0) {
            if (errno == EINTR) continue;
            return mk_io_error("send");
        }
        sent += (size_t)n;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* shutdownFd(fd) — half-close write side; lets client read final bytes. */
LEAN_EXPORT lean_obj_res lean_ft_shutdown(uint32_t fd, lean_object *w) {
    (void)w;
    shutdown((int)fd, SHUT_WR);
    return lean_io_result_mk_ok(lean_box(0));
}

/* closeFd(fd) — release the descriptor. */
LEAN_EXPORT lean_obj_res lean_ft_close(uint32_t fd, lean_object *w) {
    (void)w;
    close((int)fd);
    return lean_io_result_mk_ok(lean_box(0));
}
