exports.logImpl = function(a) {
  const getPre = () => window.document.querySelector('pre#console-log');
  const makePre = () => window.document.body.innerHTML += '<pre id="console-log"></pre>';
  const pre = getPre() || (makePre(), getPre());

  pre && (pre.innerText += a + '\n');
  return {};
};

exports.errorImpl = function(a) {
  console.error(a);
  return {};
};
