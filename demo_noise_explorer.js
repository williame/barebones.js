/* https://github.com/williame/barebones.js project
This file is a simple 3D noise visualiser

BSD LICENSE:

Copyright (c) 2013, William Edwards
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */

function DemoNoiseExplorer() {
	UIViewport.call(this,this);
	var self = this;
	this.camera = {
		centre: [0,0,0],
		up: null,
		eye: [10,10,10],
		insphere: null,
		minZoom: 30,
		maxZoom: 100,
		zoom: 70,
		zoomDiff: 0,
	};
	this.uniforms = {
		texture: programs.blankTex,
		pMatrix: null,
		mvMatrix: null,
		nMatrix: null,
		mvpMatrix: null,
		invMvpMatrix: null,
		spriteScale: 0,
		colour: OPAQUE,
		threshold: 0.6,
	};
	this.w = this.h = this.d = 50; // 50x50x50
	this.vertices = gl.createBuffer();
	this.createVertices();
	this.noiseBuffer = gl.createBuffer();
	this.noiseParams = {
		rnd: [],
		alpha: 1.5,
		beta: 2,
		n: 4,
	};
	this.setNoise();
	loadFile("image","data/circle.png",function(tex) { self.uniforms.texture = tex; });
	this.program = Program(
		"precision mediump float;\n"+
		"varying float value;\n"+
		"attribute vec3 vertex;\n"+
		"attribute float noise;\n"+
		"uniform mat4 mvMatrix, pMatrix, nMatrix;\n"+
		"uniform float spriteScale;\n"+
		"void main() {\n"+
		"	value = noise;\n"+
		"	gl_Position = pMatrix * mvMatrix * vec4(vertex,1.0);\n"+
		"	gl_PointSize = (spriteScale*noise*2.0) / gl_Position.w;\n"+
		"}\n",
		"precision mediump float;\n"+
		"uniform sampler2D texture;\n"+
		"uniform lowp vec4 colour;\n"+
		"uniform float threshold;\n"+
		"varying float value;\n"+
		"void main() {\n"+
		"	if(value < threshold) discard;\n"+
		"	vec4 fragColour = texture2D(texture,gl_PointCoord) * colour * value;\n"+
		"	if(fragColour.a < 0.3) discard;\n"+
		"	gl_FragColor = fragColour;\n"+
		"}\n");
	this.hadMouseDown = false;
	this.viewMode = "3D"; // can be "3D" or "top"
	this.menu = new UIWindow(false,new UIPanel([
			new UILabel("3D noise explorer"),
			new UIButton("pan",function() { self.setTool("pan"); },"tools|pan"),
			new UIButton("rotate",function() { self.setTool("rotate"); },"tools|rotate"),
		],UILayoutRows));
	this.menu.ctrl.allowClickThru = false;
	this.tool = null;
	this.win = new UIWindow(false,this); // a window to host this viewport in
	this.setTool("rotate");
	this.grid = new Cubes([1,1,0,1]);
	this.grid.add(null,[[-this.w/2,-this.h/2,-this.d/2],[this.w/2,this.h/2,this.d/2]]);
}
DemoNoiseExplorer.prototype = {
	__proto__: UIViewport.prototype,
	render: function(ctx) {
		// give the GPU something to be doing
		gl.clearColor(1,1,1,1);
		gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);
		// move viewport?
		var vx, vz, v, t = now(), mv = this.uniforms.mvMatrix;
		if(keys[37] && !keys[39]) // left
			vx = [-mv[0],-mv[4],-mv[8]]; // http://3dengine.org/Right-up-back_from_modelview
		else if(keys[39] && !keys[37]) // right
			vx = [mv[0],mv[4],mv[8]];
		if(keys[38] && !keys[40]) // up
			vz = this.viewMode == "3D"? [-mv[2],0,-mv[10]]: [mv[1],0,mv[9]];			
		else if(keys[40] && !keys[38]) // down
			vz = this.viewMode == "3D"? [mv[2],0,mv[10]]: [-mv[1],0,-mv[9]];
		v = vx&&vz? vec3_add(vx,vz): vx || vz;
		if(v) {
			var elapsed = t - (this.lastRender || t), camera = this.camera;
			v = vec3_scale(vec3_normalise(v),elapsed*0.025);
			this.setCamera(vec3_add(camera.eye,v),vec3_add(camera.centre,v),camera.up);
		}
		// zooming to be done?
		if(this.camera.zoomDiff) {
			var elapsed = t - (this.lastRender || t), camera = this.camera;
			this.camera.zoom += this.camera.zoomDiff * elapsed*0.001;
			this.camera.zoom = Math.max(this.camera.minZoom,Math.min(this.camera.zoom,this.camera.maxZoom));
			this.setCamera(camera.eye,camera.centre,camera.up);
			this.camera.zoomDiff = 0;
		}
		// draw the noise
		gl.lineWidth = 2;
		this.grid.draw(this.uniforms,null,gl.LINES);
		this.program(this._doDraw,this.uniforms,this);
		//Sphere(3).draw(this.uniforms,this.camera.insphere,[1,1,0,1]);
		// done
		if(this.tool && this.tool.draw)
			this.tool.draw();
		this.lastRender = t;
	},
	_doDraw: function(program) {
		gl.bindBuffer(gl.ARRAY_BUFFER,this.vertices);
		gl.vertexAttribPointer(program.vertex,3,gl.SHORT,false,6,0);
		gl.bindBuffer(gl.ARRAY_BUFFER,this.noiseBuffer);
		gl.vertexAttribPointer(program.noise,1,gl.FLOAT,false,4,0);
		//gl.disable(gl.DEPTH_TEST);
		//gl.blendFunc(gl.SRC_ALPHA,gl.ONE);
		gl.drawArrays(gl.POINTS,0,this.w*this.h*this.d);
		gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);
		gl.enable(gl.DEPTH_TEST);
	},
	setTool: function(tool) {
		if(this.tool && this.tool.stop)
			this.tool.stop();
		this.menu.walk(function(ctrl) {
				if(ctrl.tag && startsWith(ctrl.tag,"tools|")) {
					ctrl.bgColour = ctrl.tag == "tools|"+tool? [1,0,0,1]: UI.defaults.btn.bgColour;
					ctrl.dirty();
				}
				return true;
			});
		if(tool == "pan")
			this.tool = new DemoNoiseExplorerPan(this);
		else if(tool == "rotate")
			this.tool = new DemoNoiseExplorerRotate(this);
		else if(tool == null)
			this.tool = null;
		else
			fail("bad tool type: "+tool);
		if(this.tool && this.tool.start)
			this.tool.start();
	},
	setCamera: function(eye,centre,up) {
		this.camera.centre = centre;
		this.camera.up = up || [0,1,0];
		this.camera.eye = vec3_add(centre,vec3_scale(vec3_normalise(vec3_sub(eye,centre)),this.camera.zoom));
		var fovy = 60, aspect = this.width()/this.height();
		this.uniforms.pMatrix = new Float32Array(createPerspective(fovy,aspect,0.01,100));
		this.uniforms.mvMatrix = new Float32Array(createLookAt(this.camera.eye,this.camera.centre,this.camera.up));
		this.uniforms.mvpMatrix = mat4_multiply(this.uniforms.pMatrix,this.uniforms.mvMatrix);
		this.uniforms.invMvpMatrix = mat4_inverse(this.uniforms.mvpMatrix);
		this.uniforms.invMvMatrix = mat4_inverse(this.uniforms.mvMatrix);
		this.uniforms.nMatrix = mat4_transpose(this.uniforms.invMvMatrix);
		this.uniforms.spriteScale = this.height() / (2*Math.tan(0.5*fovy*Math.PI/180.0));
		this.camera.insphere = getFrustumsInsphere(this.viewport,this.uniforms.invMvpMatrix);
	},
	createVertices: function() {
		var	w = this.w, h = this.h, d = this.d,
			hw = w/2, hh = h/2, hd = d/2,
			buf = new Int16Array(w*h*d*3),
			i = 0;
		for(var zi=0; zi<d; zi++) {
			var z = (zi-hd);
			for(var yi=0; yi<h; yi++) {
				var y = (yi-hh);
				for(var xi=0; xi<w; xi++) {
					buf[i++] = (xi-hw);
					buf[i++] = y;
					buf[i++] = z;
				}
			}
		}
		gl.bindBuffer(gl.ARRAY_BUFFER,this.vertices);
		gl.bufferData(gl.ARRAY_BUFFER,buf,gl.STATIC_DRAW);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);
	},
	setPoints: function() {
		var	w = this.w, h = this.h, d = this.d,
			xs = 1/(w/2), ys = 1/(h/2), zs = 1/(d/2),
			buf = new Float32Array(w*h*d),
			i = 0,
			noise = this.noise;
		for(var z=0; z<d; z++)
			for(var y=0; y<h; y++)
				for(var x=0; x<w; x++)
					buf[i++] = (noise.noise3d(x*xs,y*ys,z*zs)+1)/2;
		gl.bindBuffer(gl.ARRAY_BUFFER,this.noiseBuffer);
		gl.bufferData(gl.ARRAY_BUFFER,buf,gl.STATIC_DRAW);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);
		console.log("min",Math.min.apply(Math,buf),"max",Math.max.apply(Math,buf));
	},
	setNoise: function() {
		var i = 0, noiseParams = this.noiseParams, rnd = noiseParams.rnd;
		this.noise = new PerlinNoiseWill({
				random: function() {
					if(i==rnd.length)
						rnd.push(Math.random());
					return rnd[i++];
				}
			},noiseParams.alpha,noiseParams.beta,noiseParams.n);
		addMessage(0,null,"noise: alpha:"+noiseParams.alpha.toFixed(2)+", "+
			"beta:"+noiseParams.beta.toFixed(2)+", "+
			"n:"+noiseParams.n+", "+
			"threshold:"+this.uniforms.threshold.toFixed(2),
			noiseParams);
		this.setPoints();
	},
	mouseRay: function(evt) {
		return unproject(evt.clientX,canvas.height-evt.clientY,
			null,null,
			this.viewport,this.uniforms.invMvpMatrix);
	},
	mousePos: function(evt) {
		return plane_ray_intersection(this.plane,this.mouseRay(evt));
	},
	plane: [[1,0,0],[0,1,0]],
	setSize: function() {
		UIViewport.prototype.setSize.apply(this,arguments);
		this.setCamera(this.camera.eye,this.camera.centre,this.camera.up);
	},
	show: function() {
		this.onResize();
		this.win.show(-1);
		this.menu.show();
	},
	hide: function() {
		this.menu.hide();
		this.win.hide();
		removeMessage(this.noiseParams);
	},
	onResize: function() {
		this.setPos([0,0]);
		this.setSize([canvas.width,canvas.height]);
		this.layout();
	},
	layout: function() {
		this.menu.performLayout();
		this.menu.ctrl.setPosVisible([canvas.width,canvas.height]); // bottom-right
		UIViewport.prototype.layout.call(this);
	},
	onMouseWheel: function(evt,amount) {
		if(this.isMouseInRect(evt)) {
			this.camera.zoomDiff += amount;
			return true;
		}
		return false;
	},
	onMouseDown: function(evt,keys) {
		if(!this.isMouseInRect(evt))
			return false;
		this.hadMouseDown = true;
		if(this.tool && this.tool.onMouseDown)
			this.tool.onMouseDown(evt,keys);
		return true;
	},
	onMouseMove: function(evt,keys,isMouseDown) {
		if(!this.isMouseInRect(evt))
			return false;
		if(this.tool && this.tool.onMouseMove)
			this.tool.onMouseMove(evt,keys,isMouseDown && this.hadMouseDown);
		return true;
	},
	onMouseUp: function(evt,keys) {
		this.hadMouseDown = false;
		if(!this.isMouseInRect(evt))
			return false;
		if(this.tool && this.tool.onMouseUp)
			this.tool.onMouseUp(evt,keys);
		return true;
	},
	onMouseOut: function(evt,keys) {
		this.hadMouseDown = false;
		if(!this.isMouseInRect(evt))
			return false;
		if(this.tool && this.tool.onMouseOut)
			this.tool.onMouseOut(evt,keys);
		return true;
	},
	onKeyDown: function(evt,keys) {
		var	dir = keys[17]? -1: 1, // ctrl
			scale = keys[16]? 0.5: 1; // shift
		switch(evt.which) {
		case 65: // A
			this.noiseParams.alpha += 0.05 * dir * scale;
			break;
		case 66: // B
			this.noiseParams.beta += 0.05 * dir * scale;
			break;
		case 78: // N
			this.noiseParams.n += 1 * dir;
			break;
		case 84: // T
			this.uniforms.threshold -= 0.05 * dir * scale;
			break;
		default:
			console.log("down",evt.which);
			return;
		}
		this.setNoise();
	},
};

function DemoNoiseExplorerPan(view) {
	assert(this !== window);
	this.view = view;
	this.pin = null;
}
DemoNoiseExplorerPan.prototype = {
	onMouseMove: function(evt,keys,isMouseDown) {                                           
		if(!isMouseDown)
			return;
		if(!this.pin)
			this.pin = evt;
		else {
			var	prev = this.view.mousePos(this.pin),
				pos = this.view.mousePos(evt);
			if(!prev || !pos)
				this.pin = null;
			else {
				var	moved = vec3_sub(prev,pos),
					camera = this.view.camera,
					centre = vec3_add(camera.centre,moved),
					eye = vec3_add(camera.eye,moved);
				this.view.setCamera(eye,centre,camera.up);
				this.pin = evt;
			}
		}
	},
	onMouseUp: function() {
		this.pin = null;
	},
};

function DemoNoiseExplorerRotate(view) {
	assert(this !== window);
	this.view = view;
	this.pin = null;
}
DemoNoiseExplorerRotate.prototype = {
	onMouseDown: function(evt,keys) {
		this.pin = null;
		var pos = this.view.mousePos(evt);
		if(pos) {
			var n = vec3_sub(this.view.camera.centre,pos);
			this.pin = Math.atan2(n[2],n[0]);
		}
	},
	onMouseMove: function(evt,keys,isMouseDown) {
		if(!isMouseDown)
			return;
		if(!this.pin) {
			this.onMouseDown(evt,keys);
			return;
		}
		var pos = this.view.mousePos(evt);
		if(!pos) return;
		var	camera = this.view.camera,
			n = vec3_sub(camera.centre,pos),
			angle = Math.atan2(n[2],n[0]);
		var eye = vec3_rotate(vec3_sub(camera.eye,camera.centre),
			(angle-this.pin),
			[0,0,0],
			camera.up);
		this.view.setCamera(vec3_add(camera.centre,eye),camera.centre,camera.up);
	},
	onMouseUp: function() {
		this.pin = null;
	},
};
