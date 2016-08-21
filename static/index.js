
// 3rd

require('pxloader/PxLoaderImage.js')
var PxLoader = require('pxloader')


// pull in desired CSS/SASS files


require('./css/index.scss')


// inject bundled Elm app into div#main


var Elm = require('../src/Main')

// show user some text while they wait for assets to laod
document.getElementById('main').innerHTML =
  '<p style="color: white;" class="lead text-center">' +
  '  Loading... <span id="progress"></span>' +
  '</p>'

function onAssetsLoaded () {
  document.getElementById('main').innerHTML = ''
  var app = Elm.Main.embed(document.getElementById('main'))

  // Blur buttons after each click so that spacebar doesn't
  // trigger them (browser behavior).
  document.addEventListener('click', function (e) {
    if (['BUTTON', 'INPUT'].indexOf(e.target.tagName) > -1) {
      e.target.blur()
    }
  }, false)
}


// mount app after preloading assets

var loader = new PxLoader()

// map tiles

;[
  'map_tile_0_0.png',
  'map_tile_0_1.png',
  'map_tile_0_10.png',
  'map_tile_0_11.png',
  'map_tile_0_12.png',
  'map_tile_0_2.png',
  'map_tile_0_3.png',
  'map_tile_0_4.png',
  'map_tile_0_5.png',
  'map_tile_0_6.png',
  'map_tile_0_7.png',
  'map_tile_0_8.png',
  'map_tile_0_9.png',
  'map_tile_10_0.png',
  'map_tile_10_1.png',
  'map_tile_10_10.png',
  'map_tile_10_11.png',
  'map_tile_10_12.png',
  'map_tile_10_2.png',
  'map_tile_10_3.png',
  'map_tile_10_4.png',
  'map_tile_10_5.png',
  'map_tile_10_6.png',
  'map_tile_10_7.png',
  'map_tile_10_8.png',
  'map_tile_10_9.png',
  'map_tile_11_0.png',
  'map_tile_11_1.png',
  'map_tile_11_10.png',
  'map_tile_11_11.png',
  'map_tile_11_12.png',
  'map_tile_11_2.png',
  'map_tile_11_3.png',
  'map_tile_11_4.png',
  'map_tile_11_5.png',
  'map_tile_11_6.png',
  'map_tile_11_7.png',
  'map_tile_11_8.png',
  'map_tile_11_9.png',
  'map_tile_12_0.png',
  'map_tile_12_1.png',
  'map_tile_12_10.png',
  'map_tile_12_11.png',
  'map_tile_12_12.png',
  'map_tile_12_2.png',
  'map_tile_12_3.png',
  'map_tile_12_4.png',
  'map_tile_12_5.png',
  'map_tile_12_6.png',
  'map_tile_12_7.png',
  'map_tile_12_8.png',
  'map_tile_12_9.png',
  'map_tile_1_0.png',
  'map_tile_1_1.png',
  'map_tile_1_10.png',
  'map_tile_1_11.png',
  'map_tile_1_12.png',
  'map_tile_1_2.png',
  'map_tile_1_3.png',
  'map_tile_1_4.png',
  'map_tile_1_5.png',
  'map_tile_1_6.png',
  'map_tile_1_7.png',
  'map_tile_1_8.png',
  'map_tile_1_9.png',
  'map_tile_2_0.png',
  'map_tile_2_1.png',
  'map_tile_2_10.png',
  'map_tile_2_11.png',
  'map_tile_2_12.png',
  'map_tile_2_2.png',
  'map_tile_2_3.png',
  'map_tile_2_4.png',
  'map_tile_2_5.png',
  'map_tile_2_6.png',
  'map_tile_2_7.png',
  'map_tile_2_8.png',
  'map_tile_2_9.png',
  'map_tile_3_0.png',
  'map_tile_3_1.png',
  'map_tile_3_10.png',
  'map_tile_3_11.png',
  'map_tile_3_12.png',
  'map_tile_3_2.png',
  'map_tile_3_3.png',
  'map_tile_3_4.png',
  'map_tile_3_5.png',
  'map_tile_3_6.png',
  'map_tile_3_7.png',
  'map_tile_3_8.png',
  'map_tile_3_9.png',
  'map_tile_4_0.png',
  'map_tile_4_1.png',
  'map_tile_4_10.png',
  'map_tile_4_11.png',
  'map_tile_4_12.png',
  'map_tile_4_2.png',
  'map_tile_4_3.png',
  'map_tile_4_4.png',
  'map_tile_4_5.png',
  'map_tile_4_6.png',
  'map_tile_4_7.png',
  'map_tile_4_8.png',
  'map_tile_4_9.png',
  'map_tile_5_0.png',
  'map_tile_5_1.png',
  'map_tile_5_10.png',
  'map_tile_5_11.png',
  'map_tile_5_12.png',
  'map_tile_5_2.png',
  'map_tile_5_3.png',
  'map_tile_5_4.png',
  'map_tile_5_5.png',
  'map_tile_5_6.png',
  'map_tile_5_7.png',
  'map_tile_5_8.png',
  'map_tile_5_9.png',
  'map_tile_6_0.png',
  'map_tile_6_1.png',
  'map_tile_6_10.png',
  'map_tile_6_11.png',
  'map_tile_6_12.png',
  'map_tile_6_2.png',
  'map_tile_6_3.png',
  'map_tile_6_4.png',
  'map_tile_6_5.png',
  'map_tile_6_6.png',
  'map_tile_6_7.png',
  'map_tile_6_8.png',
  'map_tile_6_9.png',
  'map_tile_7_0.png',
  'map_tile_7_1.png',
  'map_tile_7_10.png',
  'map_tile_7_11.png',
  'map_tile_7_12.png',
  'map_tile_7_2.png',
  'map_tile_7_3.png',
  'map_tile_7_4.png',
  'map_tile_7_5.png',
  'map_tile_7_6.png',
  'map_tile_7_7.png',
  'map_tile_7_8.png',
  'map_tile_7_9.png',
  'map_tile_8_0.png',
  'map_tile_8_1.png',
  'map_tile_8_10.png',
  'map_tile_8_11.png',
  'map_tile_8_12.png',
  'map_tile_8_2.png',
  'map_tile_8_3.png',
  'map_tile_8_4.png',
  'map_tile_8_5.png',
  'map_tile_8_6.png',
  'map_tile_8_7.png',
  'map_tile_8_8.png',
  'map_tile_8_9.png',
  'map_tile_9_0.png',
  'map_tile_9_1.png',
  'map_tile_9_10.png',
  'map_tile_9_11.png',
  'map_tile_9_12.png',
  'map_tile_9_2.png',
  'map_tile_9_3.png',
  'map_tile_9_4.png',
  'map_tile_9_5.png',
  'map_tile_9_6.png',
  'map_tile_9_7.png',
  'map_tile_9_8.png',
  'map_tile_9_9.png'
].forEach(function (filename) {
  loader.addImage('./map/' + filename)
})

// champ sprites

;[
  'attack_0.png',
  'attack_1.png',
  'attack_2.png',
  'attack_3.png',
  'attack_4.png',
  'attack_5.png',
  'attack_6.png',
  'attack_7.png',
  'attack_8.png',
  'idle_0.png',
  'idle_1.png',
  'idle_10.png',
  'idle_11.png',
  'idle_12.png',
  'idle_13.png',
  'idle_14.png',
  'idle_15.png',
  'idle_16.png',
  'idle_2.png',
  'idle_3.png',
  'idle_4.png',
  'idle_5.png',
  'idle_6.png',
  'idle_7.png',
  'idle_8.png',
  'idle_9.png',
  'move_0.png',
  'move_1.png',
  'move_10.png',
  'move_11.png',
  'move_12.png',
  'move_13.png',
  'move_14.png',
  'move_15.png',
  'move_16.png',
  'move_2.png',
  'move_3.png',
  'move_4.png',
  'move_5.png',
  'move_6.png',
  'move_7.png',
  'move_8.png',
  'move_9.png'
].forEach(function (filename) {
  loader.addImage('./img/sprites/champ/' + filename)
})

// misc

;[
  './img/tombstone.png'
].forEach(function (path) {
  loader.addImage(path)
})


loader.addCompletionListener(function () {
  onAssetsLoaded()
})

loader.addProgressListener(function (e) {
  console.log('progress')
  document.getElementById('progress').innerHTML =
    [e.completedCount, '/', e.totalCount].join(' ')
})

loader.start()
