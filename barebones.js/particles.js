/* https://github.com/williame/barebones.js project
This file provides basic particle effect.

* see http://williamedwardscoder.tumblr.com/post/43800948133/efficient-procedural-flame-particle-effects

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

function ParticleSystemFire1(rect,sprite,qty) {
	assert(this instanceof ParticleSystemFire1);
	this.size = vec2_sub(rect[1],rect[0]);
	this.rect = rect;
	this.scale = vec2_scale(this.size,0.5);
	this.bounds = [[0,0,-1],[this.size[0],this.size[1],2]];
	this.mvMatrix = mat4_multiply(mat4_translation(this.scale),
		mat4_scale(this.scale[0],this.scale[1],1));
	this.pointSize = Math.min(this.scale[0],this.scale[1])*0.6;
	this.qty = Math.min(ParticleSystemFire1.maxQty,qty||100);
	this.rand = Math.random()*1000;
	this.startTime = now();
	this.sprite = sprite;
	this.tex = getFile("image",sprite);
	if(!this.tex) {
		var self = this;
		loadFile("image",sprite,function(tex) { self.tex = tex; });
	}
	if(modding) {
		this.debugCtx = new UIContext();
		this.debugCtx.drawBox([1,1,1,0.6],-1,-1,1,1);
		this.debugCtx.finish();
	}
	this.art = this;
}
ParticleSystemFire1.load = function(json) {
	return new ParticleSystemFire1(json.rect,json.sprite,json.qty);
}
ParticleSystemFire1.maxQty = 500;
ParticleSystemFire1.prototype = {
	filename: "fire #1 (particles)",
	scale1to1: true,
	type: "particle.fire1",
	ready: true,
	toJSON: function() {
		return {
			particleType: "fire static #1",
			rect: this.rect,
			sprite: this.sprite,
			qty: this.qty,
		};
	},
	zAt: function(rayOrigin) {
		return rayOrigin[0]>=0 && rayOrigin[0]<this.size[0] && rayOrigin[1]>=0 && rayOrigin[1]<this.size[1];
	},
	draw: function(ctx,scene) {
		ctx.inject(this._doDraw,this,scene);
	},
	_doDraw: function(self,scene) { // this==ctx
		if(!self.tex) return;
		var	pMatrix = this.pMatrix,
			animTime = this.pathTime,
			mvMatrix = scene.getMvMatrix(animTime),
			mvp = mat4_multiply(mat4_multiply(pMatrix,mvMatrix),self.mvMatrix),
			colour = self.colour || OPAQUE;
		if(self.debugCtx)
			self.debugCtx.draw(mvp,null,colour);
		gl.bindTexture(gl.TEXTURE_2D,self.tex);
		if(!ParticleSystemFire1.program)
			ParticleSystemFire1.program = createProgram(
				"attribute float aLifetime;\n"+
				"attribute float aXPos;\n"+
				"attribute float aYSpeed;\n"+
				"attribute vec2 aColor;\n"+
				"uniform mat4 mvp;\n"+
				"uniform float uTime;\n"+
				"uniform float uPointSize;\n"+
				"varying float vLifetime;\n"+
				"varying vec2 color;\n"+
				"uniform float randseed;\n"+
				"float rand(float f) {\n"+
				"	return fract(456.789*sin(789.123*f*randseed)*(1.+f));\n"+
				"}\n"+
				"void main(void) {\n"+
				"	float lifetime = mix(0.0,2.0,rand(aLifetime))+1.0;\n"+
				"	vLifetime = mod(uTime,lifetime);\n"+
				"	float ti = 1. - vLifetime/lifetime;\n"+
				"	gl_Position = mvp * vec4(mix(-1.0,1.0,rand(aXPos))*ti,\n"+
				"		mix(0.0,0.7,rand(aYSpeed))*vLifetime - 1.,\n"+
				"		0., 1.);\n"+
				"	vLifetime = 4.*ti*(1. - ti);\n"+
				"	color = aColor;\n"+
				"	gl_PointSize = uPointSize;\n"+
				"}",
				"precision highp float;\n"+
				"uniform sampler2D sTexture;\n"+
				"uniform vec4 uColour;\n"+
				"varying vec2 color;\n"+
				"varying float vLifetime;\n"+
				"void main(void) {\n"+
				"	vec4 texColor = texture2D(sTexture, gl_PointCoord) * uColour;\n"+
				"	gl_FragColor = vec4(color, 0., 1.) * texColor;\n"+
				"	gl_FragColor.a = vLifetime;\n"+
				"}",
				["mvp","uTime","uPointSize","sTexture","randseed","uColour"],
				["aLifetime","aXPos","aYSpeed","aColor"]);
		var program = ParticleSystemFire1.program;
		gl.useProgram(program);
		if(!ParticleSystemFire1.vbo) {
			var pts = [];
			for(var i=0; i<ParticleSystemFire1.maxQty; i++)
				pts.push(
					2*Math.random()+1, // lifetime
					1.5*(Math.random()-0.5), // xPos
					0.7*Math.random(), // ySpeed
					Math.random(),0.2*Math.random()); // colour
			ParticleSystemFire1.vbo = gl.createBuffer();
			gl.bindBuffer(gl.ARRAY_BUFFER,ParticleSystemFire1.vbo);
			gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(pts),gl.STATIC_DRAW);
		} else
			gl.bindBuffer(gl.ARRAY_BUFFER,ParticleSystemFire1.vbo);
		gl.disable(gl.DEPTH_TEST);
		gl.blendFunc(gl.SRC_ALPHA, gl.ONE);
		gl.enableVertexAttribArray(program.aLifetime);
		gl.vertexAttribPointer(program.aLifetime,1,gl.FLOAT,false,20,0);
		gl.enableVertexAttribArray(program.aXPos);
		gl.vertexAttribPointer(program.aXPos,1,gl.FLOAT,false,20,4);
		gl.enableVertexAttribArray(program.aYSpeed);
		gl.vertexAttribPointer(program.aYSpeed,1,gl.FLOAT,false,20,8);
		gl.enableVertexAttribArray(program.aColor);
		gl.vertexAttribPointer(program.aColor,2,gl.FLOAT,false,20,12);
		gl.uniformMatrix4fv(program.mvp,false,mvp);
		gl.uniform1f(program.uPointSize,self.pointSize);
		gl.uniform1f(program.uTime,(now()-self.startTime)/1000);
		gl.uniform1f(program.randseed,self.rand);
		gl.uniform4fv(program.uColour,colour);
		gl.drawArrays(gl.POINTS,0,self.qty);
		gl.disableVertexAttribArray(program.aLifetime);
		gl.disableVertexAttribArray(program.aXPos);
		gl.disableVertexAttribArray(program.aYSpeed);
		gl.disableVertexAttribArray(program.aColor);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);
		gl.useProgram(null);
		gl.bindTexture(gl.TEXTURE_2D,null);
		gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);
	},
};

var ParticleSystems = {
	"fire static #1": {
		create: ParticleSystemFire1,
		load: ParticleSystemFire1.load,
	},
};
