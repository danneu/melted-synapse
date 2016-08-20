

// Node
const nodePath = require('path')
// 3rd
const co = require('co')
const rimraf = require('rimraf')
// 1st
const imagemagick = require('./imagemagick')


function clean (glob) {
  return new Promise(function (resolve, reject) {
    rimraf(glob, function (err) { 
      if (err) return reject(err)
      resolve()
    })
  })
}


co(function * () {
  yield clean(nodePath.join(__dirname, 'output/*'))
  const path = nodePath.join(__dirname, '../../static/maps/micro.png')
  console.log('path:', path)
  yield tilecrop(path, 64, 13, 13)
})
  .then(() => console.log('done'))
  .catch((err) => console.log(err))


function tilecrop (path, tilesize, cols, rows) {
  return new Promise((resolve, reject) => {
    const im = imagemagick()
      .arg(path)
      .args(`-crop ${tilesize}x${tilesize}`)
      .args(`-set filename:tile %[fx:page.x/${tilesize}]_%[fx:page.y/${tilesize}]`)
      .arg('+repage')
      .args(`+adjoin ./output/map_tile_%[filename:tile].png`)
    const stream = im.spawn()
    stream.on('readable', function() {
      if (!stream.read()) {
        resolve()
      }
    })
    stream.on('error', reject)
  })
}
