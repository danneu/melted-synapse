

// pull in desired CSS/SASS files


require('./css/index.scss')


// inject bundled Elm app into div#main


var Elm = require('../src/Main')
var app = Elm.Main.embed(document.getElementById('main'))


// Blur buttons after each click so that spacebar doesn't
// trigger them (browser behavior).
document.addEventListener('click', function (e) {
  if (e.target.tagName === 'BUTTON') {
    e.target.blur()
  }
}, false)
