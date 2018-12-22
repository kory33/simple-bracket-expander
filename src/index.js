require('./main.scss');

const { Elm } = require('./elm/Main.elm');
const mountNode = document.getElementById('main');

Elm.Main.init({
  node: document.getElementById('main')
});
