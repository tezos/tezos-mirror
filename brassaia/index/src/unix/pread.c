#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

CAMLprim value caml_index_pread_int
(value v_fd, value v_fd_off, value v_buf, value v_buf_off, value v_len)
{
  CAMLparam5(v_fd, v_fd_off, v_buf, v_buf_off, v_len);

  ssize_t ret;
  size_t fd = Int_val(v_fd);
  size_t fd_off = Long_val(v_fd_off);
  size_t buf_off = Long_val(v_buf_off);
  size_t len = Long_val(v_len);

  size_t numbytes = (len > UNIX_BUFFER_SIZE) ? UNIX_BUFFER_SIZE : len;
  ret = pread(fd, &Byte(v_buf, buf_off), numbytes, fd_off);

  if (ret == -1) uerror("read", Nothing);

  CAMLreturn(Val_long(ret));
}

CAMLprim value caml_index_pread_int64
(value v_fd, value v_fd_off, value v_buf, value v_buf_off, value v_len)
{
  CAMLparam5(v_fd, v_fd_off, v_buf, v_buf_off, v_len);

  ssize_t ret;
  size_t fd = Int_val(v_fd);
  size_t fd_off = Int64_val(v_fd_off);
  size_t buf_off = Long_val(v_buf_off);
  size_t len = Long_val(v_len);

  size_t numbytes = (len > UNIX_BUFFER_SIZE) ? UNIX_BUFFER_SIZE : len;
  ret = pread(fd, &Byte(v_buf, buf_off), numbytes, fd_off);

  if (ret == -1) uerror("read", Nothing);

  CAMLreturn(Val_long(ret));
}
