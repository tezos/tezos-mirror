/* tcp_user_timeout_stubs.c */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

CAMLprim value ocaml_set_tcp_user_timeout(value fd_val, value timeout_val) {
  CAMLparam2(fd_val, timeout_val);
  int fd = Int_val(fd_val);
  socklen_t  timeout = Int_val(timeout_val);

// TCP_USER_TIMEOUT is not always defined such as in Mac OS/X
#ifdef TCP_USER_TIMEOUT
  if (setsockopt(fd, IPPROTO_TCP, TCP_USER_TIMEOUT, &timeout, sizeof(timeout)) < 0) {
    uerror("setsockopt(TCP_USER_TIMEOUT)", Nothing);
  }
  CAMLreturn(Val_unit);
#else
  caml_failwith("TCP_USER_TIMEOUT not supported on this platform");
#endif
}

