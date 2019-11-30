const execa = require('execa');

exports.pursImpl = function(args) {
  try {
    return execa.sync("purs", args, {preferLocal: true});
  } catch(e) {
    return e;
  }
};
