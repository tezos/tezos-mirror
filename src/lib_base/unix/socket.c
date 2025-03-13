/* tcp_user_timeout_stubs.c */
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/types.h>

CAMLprim value ocaml_set_tcp_user_timeout(value fd_val, value timeout_val) {
  CAMLparam2(fd_val, timeout_val);
  int fd = Int_val(fd_val);
  int timeout = Int_val(timeout_val);
#define timeout_sz sizeof(timeout)

// TCP_USER_TIMEOUT is not always defined such as in Mac OS/X
#ifdef TCP_USER_TIMEOUT
  if (setsockopt(fd, IPPROTO_TCP, TCP_USER_TIMEOUT, &timeout, timeout_sz) < 0) {
    uerror("setsockopt(TCP_USER_TIMEOUT)", Nothing);
  }
  CAMLreturn(Val_unit);
#else
  caml_failwith("TCP_USER_TIMEOUT not supported on this platform");
#endif
}

CAMLprim value ocaml_set_tcp_keepalive(value fd_val, value duration_val,
                                       value intv_val) {
  CAMLparam3(fd_val, duration_val, intv_val);

  int fd = Int_val(fd_val);

  int enabled = 1;
  int duration = Int_val(duration_val);
  int intv = Int_val(intv_val);
#define enabled_sz sizeof(enabled)
#define duration_sz sizeof(duration)
#define intv_sz sizeof(intv)

/* On linux, the way to set keepalive per socket is
 *  - enable so_keepalive
 *  - set tcp_keepidle to a value in ms
 *  - set tcp_keepalive_interval to the same value in ms
 * (do not set the default number of retransmissions if not acknowledged.
 *  Should be 9 by default).
 * On mac, the equivalent setup is
 *  - enable so_keepalive
 *  - set tcp_keepalive to a value in ms */
#ifdef __linux__
  if (setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, &enabled, enabled_sz) < 0) {
    uerror("setsockopt(TCP_KEEPALIVE)", Nothing);
  }
  if (setsockopt(fd, IPPROTO_TCP, TCP_KEEPIDLE, &duration, duration_sz) < 0) {
    uerror("setsockopt(TCP_KEEPIDLE)", Nothing);
  }
  if (setsockopt(fd, IPPROTO_TCP, TCP_KEEPINTVL, &duration, intv_sz) < 0) {
    uerror("setsockopt(TCP_KEEPINTVL)", Nothing);
  }
#elif defined __APPLE__
  if (setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, &enabled, enabled_sz) < 0) {
    uerror("setsockopt(SO_KEEPALIVE)", Nothing);
  }
  if (setsockopt(fd, IPPROTO_TCP, TCP_KEEPALIVE, &duration, duration_sz) < 0) {
    uerror("setsockopt(TCP_KEEPALIVE)", Nothing);
  }
#else
  caml_failwith("TCP KEEPALIVE not supported on this platform");
#endif
  CAMLreturn(Val_unit);
}
