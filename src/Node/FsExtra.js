const fs = require('fs-extra');

exports.pathExistsImpl = fs.pathExists;

exports.readFileImpl = function(path) {
  return fs.readFile(path, "utf8");
};

exports.removeImpl = fs.remove;

exports.outputFileSyncImpl = function(path, data) {
  fs.outputFileSync(path, data);
  return {};
};

exports.readFileSyncImpl = function(path) {
  return fs.readFileSync(path, "utf8");
};

exports.removeSyncImpl = fs.removeSync;
