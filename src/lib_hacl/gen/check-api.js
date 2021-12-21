var api_hacl = require.resolve('hacl-wasm/api.json');
var fs = require('fs');
// File destination.txt will be created or overwritten by default.
fs.copyFile(api_hacl, 'api.json.corrected', (err) => {
  if (err) throw err;
  console.log('api.json was copied to api.json.corrected');
});
