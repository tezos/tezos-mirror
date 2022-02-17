//   Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

//Provides: hacl_bytes_of_ptr
//Requires: caml_failwith
function hacl_bytes_of_ptr(ptr) {
  if (ptr.length == 4 && ptr[0] == 1 && ptr[1] == 0 && ptr[3] == 1)
    return ptr[2];
  caml_failwith("hacl_bytes_of_ptr: not a valid OCamlRef");
}

//Provides: hacl_create_buffer
//Requires: caml_array_of_bytes, caml_ml_bytes_length, integers_int32_of_uint32, hacl_bytes_of_ptr
//Requires: caml_invalid_argument
function hacl_create_buffer(bytes, len) {
  if (typeof len !== "number")
    caml_invalid_argument(
      "hacl_create_buffer expect an int in second position"
    );
  bytes = hacl_bytes_of_ptr(bytes);
  var a = caml_array_of_bytes(bytes);
  return new joo_global_object.Uint8Array(a.slice(0, len));
}

//Provides: hacl_blit_buf_to_bytes
//Requires: caml_ml_bytes_length, caml_bytes_of_array, caml_blit_bytes, hacl_bytes_of_ptr
//Requires: caml_invalid_argument
function hacl_blit_buf_to_bytes(buf, bytes, len) {
  if (typeof len !== "number")
    caml_invalid_argument(
      "hacl_blit_buf_to_bytes expect an int in third position"
    );
  bytes = hacl_bytes_of_ptr(bytes);
  var buf = caml_bytes_of_array(buf);
  caml_blit_bytes(buf, 0, bytes, 0, len);
  return 0;
}
