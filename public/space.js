var Util = {
  each : function(array, block){
    var i = array.length; while (i--) {
      block(array[i]);
    }
  }
}


var Universe = function(P){
  var canvas = P.externals.canvas,
      width = canvas.offsetWidth,
      height = canvas.offsetHeight;

  this.config = {
    width : width,
    height : height,

    sketchLength : 30, // in seconds
    framerate : 12,

    particleCount : 100
  }

  var self = this;
  var buffer = [];
  var frame = 0;
  var totalFrames = this.config.sketchLength * this.config.framerate;

  function getFrameData(){
    var request = new XMLHttpRequest(),
        query = '?frames=' + totalFrames + '&width=' + width + '&height=' + height + '&particle_count=' + self.config.particleCount;
    request.open('GET', '/hatch' + query, false);
    request.send();

    if (request.status == 200) {
      buffer = JSON.parse(request.response);
    } else {
      console.log(request);
      throw 'butts';
    }
  }

  P.setup = function(){
    P.size(width, height);
    P.noStroke();
    P.fill(255);
    P.frameRate(self.config.framerate);

    getFrameData();
  }

  P.draw = function(){
    P.background(0);

    Util.each(buffer, function(particleData){
      var cf = particleData[frame],
          pos = cf.p,
          r = cf.r;

      P.ellipse(pos[0], pos[1], r, r);
    });

    ++frame;
  }
}



function loadUniverse(){
  new Processing('space', Universe);
}

window.onload = loadUniverse;





