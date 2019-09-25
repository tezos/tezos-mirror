#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

CAMLprim value caml_pread
(value v_fd, value v_fd_off, value v_buf, value v_buf_off, value v_len)
{
  CAMLparam5(v_fd, v_fd_off, v_buf, v_buf_off, v_len);

  ssize_t ret;
  size_t fd = Int_val(v_fd);
  size_t fd_off = Int64_val(v_fd_off);
  size_t buf_off = Long_val(v_buf_off);
  size_t len = Long_val(v_len);
  char iobuf[UNIX_BUFFER_SIZE];

  size_t numbytes = (len > UNIX_BUFFER_SIZE) ? UNIX_BUFFER_SIZE : len;
  caml_enter_blocking_section();
  ret = pread(fd, iobuf, numbytes, fd_off);
  caml_leave_blocking_section();

  if (ret == -1) uerror("read", Nothing);
  memcpy(&Byte(v_buf, buf_off), iobuf, ret);

  CAMLreturn(Val_long(ret));
}
