
"use strict";
const debug = require('debug')('app:imagemagick');
const spawn = require('child_process').spawn;
const fs = require('fs');
const stream = require('stream');

class ImageMagick {

  constructor () {
    this.operators = [];
  }

  // single op
  arg (val) {
    this.operators.push(val);
    return this;
  }

  args(str) {
    for (let a of str.split(' ')) {
      this.operators.push(a);
    }
    return this;
  }

  spawn() {
    //console.log('[imagemagic] convert', this.operators.join(' '));
    const proc = spawn('convert', this.operators);
    
    proc.stderr.on('data', (data) => {
      console.error(`[imagemagick]: ${data}`);
    });

    const s = new stream.Readable();
    s._read = function noop() {};

    proc.stdout.on('data', (data) => {
      s.push(data);
    });

    proc.stderr.on('data', (data) => {
      console.warn(`[imagemagick] error: ${data}`);
      s.emit('error', new Error(data));
    });

    proc.on('close', (code, signal) => {
      if (code !== 0) {
        s.emit('error', new Error(`ps process exited with code ${code} with signal ${signal}`));
      }
      debug('closed with code: %d %j', code, signal);
      s.push(null);
    });

    return s;
  }


  // private
  onerror (err) {
    if (!isError(err)) err = new Error(err);
    if (!this.listeners('error')) throw err;
    this.emit('error', err);
  }
}

module.exports = (src) => new ImageMagick(src);
