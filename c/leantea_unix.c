/* leantea_unix.c — Unix domain stream sockets for LeanTea.Net.Unix.
 *
 * Std.Async has TCP and UDP but no Unix sockets, so these are blocking
 * POSIX calls, in the style of leantea_fastnet.c: the Lean side runs
 * each connection on a dedicated thread.
 *
 * Surface (all `@[extern]`-facing, fds are UInt32):
 *   listen   : String -> UInt32 (mode) -> IO UInt32
 *   accept   : UInt32 -> IO UInt32
 *   connect  : String -> IO UInt32
 *   recv     : UInt32 -> UInt32 (max) -> UInt32 (timeout ms, 0 = none) -> IO ByteArray
 *   send     : UInt32 -> ByteArray -> IO Unit
 *   shutdown : UInt32 -> UInt8 (0 read, 1 write, 2 both) -> IO Unit
 *   close    : UInt32 -> IO Unit
 *   peerUid  : UInt32 -> IO UInt32
 *
 * Errors come back as IO user errors with strerror(errno). A receive
 * that times out fails with "recv: timed out"; an empty ByteArray is EOF.
 */

#define _GNU_SOURCE  /* struct ucred */
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

#include <lean/lean.h>

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif

static lean_obj_res mk_io_error(const char *what) {
    char buf[512];
    snprintf(buf, sizeof(buf), "%s: %s", what, strerror(errno));
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(buf)));
}

static lean_obj_res mk_io_error_msg(const char *msg) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

/* Fill `addr` from `path`; 0 on success, -1 when it does not fit. */
static int fill_addr(struct sockaddr_un *addr, const char *path) {
    memset(addr, 0, sizeof(*addr));
    addr->sun_family = AF_UNIX;
    size_t n = strlen(path);
    if (n == 0 || n >= sizeof(addr->sun_path)) return -1;
    memcpy(addr->sun_path, path, n);
    return 0;
}

/* Close on exec (portable: no SOCK_CLOEXEC or accept4 on every system),
   and no SIGPIPE where MSG_NOSIGNAL does not exist. */
static void set_cloexec_nosigpipe(int fd) {
    int flags = fcntl(fd, F_GETFD);
    if (flags >= 0) fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
#ifdef SO_NOSIGPIPE
    int one = 1;
    setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof(one));
#endif
}

/* listen(path, mode) — bind a stream socket at `path`, set its mode, then
   listen. A stale socket file at `path` is removed first; any other kind
   of file is left alone and reported. The mode is set before `listen`,
   so no client can connect while the file still has the umask's mode. */
LEAN_EXPORT lean_obj_res lean_unix_listen(b_lean_obj_arg path_obj, uint32_t mode, lean_object *w) {
    (void)w;
    const char *path = lean_string_cstr(path_obj);
    struct sockaddr_un addr;
    if (fill_addr(&addr, path) < 0) return mk_io_error_msg("unix listen: path is empty or too long");
    struct stat st;
    if (lstat(path, &st) == 0) {
        if (!S_ISSOCK(st.st_mode)) return mk_io_error_msg("unix listen: path exists and is not a socket");
        if (unlink(path) < 0) return mk_io_error("unix listen: unlink stale socket");
    }
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) return mk_io_error("unix listen: socket");
    set_cloexec_nosigpipe(fd);
    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        int e = errno; close(fd); errno = e;
        return mk_io_error("unix listen: bind");
    }
    if (chmod(path, (mode_t)mode) < 0) {
        int e = errno; close(fd); unlink(path); errno = e;
        return mk_io_error("unix listen: chmod");
    }
    if (listen(fd, 128) < 0) {
        int e = errno; close(fd); unlink(path); errno = e;
        return mk_io_error("unix listen: listen");
    }
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)fd));
}

LEAN_EXPORT lean_obj_res lean_unix_accept(uint32_t fd, lean_object *w) {
    (void)w;
    int cfd;
    do {
        cfd = accept((int)fd, NULL, NULL);
    } while (cfd < 0 && (errno == EINTR || errno == ECONNABORTED));
    if (cfd < 0) return mk_io_error("unix accept");
    set_cloexec_nosigpipe(cfd);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)cfd));
}

LEAN_EXPORT lean_obj_res lean_unix_connect(b_lean_obj_arg path_obj, lean_object *w) {
    (void)w;
    const char *path = lean_string_cstr(path_obj);
    struct sockaddr_un addr;
    if (fill_addr(&addr, path) < 0) return mk_io_error_msg("unix connect: path is empty or too long");
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) return mk_io_error("unix connect: socket");
    set_cloexec_nosigpipe(fd);
    int r;
    do {
        r = connect(fd, (struct sockaddr *)&addr, sizeof(addr));
    } while (r < 0 && errno == EINTR);
    if (r < 0) {
        int e = errno; close(fd); errno = e;
        return mk_io_error("unix connect");
    }
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)fd));
}

/* recv(fd, max, timeoutMs) — one receive of up to `max` bytes, waiting at
   most `timeoutMs` (0: no limit). Empty result: the peer closed. */
LEAN_EXPORT lean_obj_res lean_unix_recv(uint32_t fd, uint32_t max, uint32_t timeout_ms, lean_object *w) {
    (void)w;
    if (max == 0) max = 65536;
    struct pollfd p = { .fd = (int)fd, .events = POLLIN };
    int r;
    do {
        r = poll(&p, 1, timeout_ms == 0 ? -1 : (int)timeout_ms);
    } while (r < 0 && errno == EINTR);
    if (r < 0) return mk_io_error("recv: poll");
    if (r == 0) return mk_io_error_msg("recv: timed out");
    lean_object *ba = lean_alloc_sarray(1, 0, (size_t)max);
    ssize_t n;
    do {
        n = recv((int)fd, lean_sarray_cptr(ba), (size_t)max, 0);
    } while (n < 0 && errno == EINTR);
    if (n < 0) {
        int e = errno; lean_dec(ba); errno = e;
        if (errno == ECONNRESET) {
            /* A reset is the peer going away: report it as EOF. */
            lean_object *empty = lean_alloc_sarray(1, 0, 0);
            return lean_io_result_mk_ok(empty);
        }
        return mk_io_error("recv");
    }
    lean_sarray_set_size(ba, (size_t)n);
    return lean_io_result_mk_ok(ba);
}

LEAN_EXPORT lean_obj_res lean_unix_send(uint32_t fd, b_lean_obj_arg ba, lean_object *w) {
    (void)w;
    size_t total = lean_sarray_size(ba);
    const uint8_t *data = lean_sarray_cptr(ba);
    size_t sent = 0;
    while (sent < total) {
        ssize_t n = send((int)fd, data + sent, total - sent, MSG_NOSIGNAL);
        if (n < 0) {
            if (errno == EINTR) continue;
            return mk_io_error("send");
        }
        sent += (size_t)n;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lean_unix_shutdown(uint32_t fd, uint8_t how, lean_object *w) {
    (void)w;
    int h = how == 0 ? SHUT_RD : how == 1 ? SHUT_WR : SHUT_RDWR;
    /* ENOTCONN: the peer is already gone, which is what we wanted. */
    if (shutdown((int)fd, h) < 0 && errno != ENOTCONN) return mk_io_error("shutdown");
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lean_unix_close(uint32_t fd, lean_object *w) {
    (void)w;
    if (close((int)fd) < 0 && errno != EINTR) return mk_io_error("close");
    return lean_io_result_mk_ok(lean_box(0));
}

/* peerUid(fd) — the user id of the process at the other end. */
LEAN_EXPORT lean_obj_res lean_unix_peer_uid(uint32_t fd, lean_object *w) {
    (void)w;
#if defined(SO_PEERCRED)
    struct ucred cred;
    socklen_t len = sizeof(cred);
    if (getsockopt((int)fd, SOL_SOCKET, SO_PEERCRED, &cred, &len) < 0)
        return mk_io_error("getsockopt(SO_PEERCRED)");
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)cred.uid));
#else
    uid_t uid; gid_t gid;
    if (getpeereid((int)fd, &uid, &gid) < 0) return mk_io_error("getpeereid");
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)uid));
#endif
}
