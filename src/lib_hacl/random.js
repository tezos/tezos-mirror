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

//Provides: Lib_RandomBuffer_System_randombytes
//Requires: caml_blit_bytes, caml_bytes_of_array, integers_int32_of_uint32, hacl_bytes_of_ptr
function Lib_RandomBuffer_System_randombytes(output, len) {
  len = integers_int32_of_uint32(len);
  output = hacl_bytes_of_ptr(output);
  function browser(output, len) {
    var crypto = self.crypto || self.msCrypto,
      QUOTA = 65536;
    var result = new joo_global_object.Uint8Array(len);
    for (var i = 0; i < len; i += QUOTA) {
      crypto.getRandomValues(result.subarray(i, i + Math.min(len - i, QUOTA)));
    }
    var result = caml_bytes_of_array(result);
    caml_blit_bytes(result, 0, output, 0, len);
    return 1;
  }
  function node(output, len) {
    var result = require("crypto").randomBytes(len);
    var result = caml_bytes_of_array(result);
    caml_blit_bytes(result, 0, output, 0, len);
    return 1;
  }
  var is_browser =
    typeof self !== "undefined" && (self.crypto || self.msCrypto);
  if (is_browser) {
    return browser(output, len);
  } else {
    return node(output, len);
  }
}
