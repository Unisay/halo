"use strict";

const readline = require('readline');

exports.createCompleter = (f) => (s) => {
  const r = f(s);
  console.dir(r);
  return [r.entries, r.substring];
};

exports._createInterface = readline.createInterface;

exports._close = (iface) => iface.close();

exports._prompt = (iface, preserveCursor) => iface.prompt(preserveCursor);

exports._question = (iface, s, callback) => iface.question(s, callback);

exports._onLine = (iface, callback) => iface.on('line', callback);

exports._onHistory = (iface, callback) => iface.on('history', callback);

exports._write = (iface, data, key) => iface.write(data, key);

exports._emitKeypressEvents = (stream) => readline.emitKeypressEvents(stream);

exports._onKeypress = (stream, callback) => {
  if (stream.isTTY) stream.setRawMode(true);
  stream.on('keypress', callback);
}

exports._dir = (o) => console.dir(o);
