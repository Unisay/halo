"use strict";

exports._fullscreen = (t) => t.fullscreen;
exports._getCursorLocation = (t) => t.getCursorLocation();
exports._nextLine = (t, i) => t.nextLine(i);
exports._previousLine = (t, i) => t.previousLine(i);
exports._processExit = (t, i) => t.processExit(i);
exports._grabInput = (t) => t.grabInput;
exports._onKey = (t, cb) => t.on('key', cb);
exports._releaseInput = (t) => t.grabInput(false);
exports._width = (t) => t.width;
exports._height = (t) => t.height;
exports._reset = (t) => t.reset;
exports._print = (t, s) => t(s);
exports._inputField = (t, options) => t.inputField(options).promise;
exports.inputFieldOptions = {}
