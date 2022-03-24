"use strict";

const readline = require('readline');

exports._createCompleter = (f) => (s) => {
  const r = f(s);
  return [r.entries, r.substring];
};

exports._createInterface = readline.createInterface;
exports._close = (iface) => iface.close();
exports._prompt = (iface, preserveCursor) => iface.prompt(preserveCursor);
exports._onLine = (iface, callback) => iface.on('line', callback);
