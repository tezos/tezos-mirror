function run () {
  var file = process.argv[2];
  process.argv.splice(1,1);
  console.log("Ready to run", file, "with argv = [" + process.argv.toString() + "]");
  // The script doesn't need to know it was started by init.js
  process.argv.splice(1,1);
  require(process.cwd() + "/" + file);
}

function load_hacl_wasm() {
  /* We have to cheat to avoid the noise from hacl-wasm */
  var old_log = console.log;
  console.log = function () {};
  var _HACL_loader = require('hacl-wasm');
  console.log = old_log;
  
  console.log("INITIALISING hacl-wasm");
  var loaded = _HACL_loader.getInitializedHaclModule();
  return loaded.then(function(hacl) {
      global._HACL = hacl;
  })
	.catch(e => console.log(e))
};

load_hacl_wasm().then(run);
