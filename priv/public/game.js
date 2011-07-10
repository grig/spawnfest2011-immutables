Field = (function() {
var PIXEL_SIZE = 20;
var LINE_WIDTH = 3;

function Field(data) {
  this.w = data.width;
  this.h = data.height;
  this.canvas = this.initCanvas();
  this.context = this.canvas.getContext("2d");
  this.context.lineWidth = LINE_WIDTH;
  this.cursor = new Cursor(this, 0, 0);
  this.cells = data.cells || this.initCells();
  window.addEventListener("keydown", _.bind(this.onKeyDown, this), false);
  window.addEventListener("keyup", _.bind(this.onKeyUp, this), false);
  this.repaint();
}

Field.prototype = {
  initCanvas: function() {
    var canvas = $("#field").get(0);
    canvas.width = PIXEL_SIZE * this.w + LINE_WIDTH;
    canvas.height = PIXEL_SIZE * this.h + LINE_WIDTH;
    return canvas;
  },

  initCells: function() {
    _.range(this.w).map(function(x) {
      return _.range(this.h).map(function(y) {
        return 0;
      });
    });
  },

  onKeyDown: function(e) {
    if (_.include([32, 37, 38, 39, 40], e.keyCode)) {
      e.preventDefault();
      var cursor = this.cursor;
      switch(e.keyCode) {
        case 37: cursor.moveLeft(); break;
        case 38: cursor.moveUp(); break;
        case 39: cursor.moveRight(); break;
        case 40: cursor.moveDown(); break;
        case 32: cursor.startPainting(); break;
      }
      this.repaint();
    }
  },
  onKeyUp: function(e) {
    if (e.keyCode === 32) {
      this.cursor.stopPainting();
    }
  },

  paint: function() {
    this.paintXY(this.cursor.x, this.cursor.y);
    $(this.canvas).trigger('paint', [this.cursor.x, this.cursor.y]);
  },

  paintXY: function(x, y) {
    this.cells[x][y] = 1;
    this.repaint();
  },

  repaint: function() {
    this.drawGrid();
    this.drawCells();
    this.drawCursor();
  },

  drawGrid: function() {
    var ctx = this.context;
    ctx.beginPath();
    ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
    for (var x = 0; x <= this.w; x++) {
      ctx.moveTo(LINE_WIDTH / 2 + x*PIXEL_SIZE, 0);
      ctx.lineTo(LINE_WIDTH / 2 + x*PIXEL_SIZE, this.canvas.height);
    }
    for (var y = 0; y <= this.h; y ++) {
      ctx.moveTo(0, LINE_WIDTH / 2 + y*PIXEL_SIZE);
      ctx.lineTo(this.canvas.width, LINE_WIDTH / 2 + y*PIXEL_SIZE);
    }
    ctx.strokeStyle = "#ccc";
    ctx.stroke();
  },
  drawCells: function() {
    var ctx = this.context;
    var columns = this.cells;
    _.each(columns, function(column, x) {
      _.each(column, function(cell, y) {
        if (cell === 1) {
          drawCell(x, y);
        }
      });
    });
    function drawCell(i, j) {
      ctx.beginPath();
      ctx.fillStyle = "#f00";
      var x = i * PIXEL_SIZE + LINE_WIDTH;
      var y = j * PIXEL_SIZE + LINE_WIDTH;
      ctx.fillRect(x, y, PIXEL_SIZE - LINE_WIDTH, PIXEL_SIZE - LINE_WIDTH);
    }
  },

  drawCursor: function() {
    var ctx = this.context;
    ctx.beginPath();
    var x = this.cursor.x * PIXEL_SIZE + LINE_WIDTH / 2;
    var y = this.cursor.y * PIXEL_SIZE + LINE_WIDTH / 2;
    ctx.strokeStyle = "#999";
    ctx.strokeRect(x, y, PIXEL_SIZE, PIXEL_SIZE);
  },
}

function Cursor(field, x, y) {
  this.field = field;
  this.x = x || 0;
  this.y = y || 0;
}

Cursor.prototype = {
  move: function(dx, dy) {
    this.x += dx;
    this.y += dy;
    if (this.x < 0 ) { this.x = 0 };
    if (this.y < 0 ) { this.y = 0 };
    if (this.x >= this.field.w) { this.x = this.field.w - 1 };
    if (this.y >= this.field.h) { this.y = this.field.h - 1 };
    if (this.isPainting) {
      this.field.paint();
    }
  },

  moveLeft:  function() { this.move(-1, 0); },
  moveRight: function() { this.move(1, 0); },
  moveUp: function() { this.move(0, -1); },
  moveDown: function() { this.move(0, 1); },

  startPainting: function() {
    this.isPainting = true;
    this.field.paint();
  },

  stopPainting: function() {
    this.isPainting = false;
  }
}
return Field;
})();
