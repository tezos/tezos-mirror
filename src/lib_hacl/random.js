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
