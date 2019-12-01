const fs = require('fs-extra');

exports.pathExistsImpl = function(path) {
  return fs.pathExists(path);
};

exports.outputFileSyncImpl = function(path, data) {
  fs.outputFileSync(path, data);
  return {};
};

exports.readFileSyncImpl = function(path) {
  return fs.readFileSync(path, "utf8");
};

exports.removeSyncImpl = function(path) {
  fs.removeSync(path);
  return {};
};
