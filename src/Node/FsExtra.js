const fs = require('fs-extra');

exports.outputFileSyncImpl = function(path, data) {
  fs.outputFileSync(path, data);
  return {};
};

exports.pathExistsImpl = function(path) {
  return fs.pathExists(path);
};
