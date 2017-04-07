exports.setScore = function (text) {
  return function() {
    document.getElementById('score').innerHTML = text;
  }
};

exports.showGameOver = function(zeige) {
  return function() {
    var value = "none";
    if (zeige) { value = "block"; }
    document.getElementById('gameOver').style.display = value;
  }
};

