/* https://github.com/williame/barebones.js project
This file is a demo that shows a simple G3D mesh

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

function DemoG3D() {
	UIViewport.call(this,this);
	this.win = new UIWindow(false,this); // a window to host this viewport in
	this.model = new G3D("data/test.g3d"); // model to draw
	var self = this;
	this.menu = new UIWindow(false,new UIPanel([
			new UILabel("g3d demo"),
			new UIButton("show normals",function() { self.showNormals = !self.showNormals; }),
			new UIButton("auto normals",function() { self.model.autoNormals(); }),
		],UILayoutRows));
	this.menu.ctrl.allowClickThru = false;
	loadFile("image","data/opaque.png",function(tex) {
		programs.blankTex = tex;
	});
}
DemoG3D.prototype = {
	__proto__: UIViewport.prototype,
	render: function(ctx) {
		gl.clearColor(0,0,0,0);
		gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);
		if(!this.model.ready) return;
		var	w = this.width(), h = this.height(),
			aspect = w/h,
			zoom = Math.max(w,h)/Math.min(w,h) * 1,
			pMatrix = createPerspective(60.0,aspect,0.1,100),
			mMatrix = createLookAt([0,2,2],[0,0,0],[0,1,0]),
			bounds = this.model.bounds,
			size = vec3_sub(bounds[1],bounds[0]);
		var lightPos = mat4_vec3_multiply(mMatrix,[0,3,2]);
		var vMatrix = mat4_multiply(mMatrix,mat4_scale(1/Math.max.apply(Math,size)));
		vMatrix = mat4_multiply(vMatrix,mat4_translation(vec3_neg(bounds[0])));
		var uniforms = {
			pMatrix: pMatrix,
			mvMatrix: mat4_multiply(vMatrix,mat4_rotation(now()/1000,[0,1,0])),
			colour: OPAQUE,
			fogColour: [0.2,0.2,0.2,1.0],
			fogDensity: 0.02,
			lightPos: lightPos,
			lightColour: this.showNormals? [1,0,0,1]: [1,1,1,1],
			ambientLight: this.showNormals? [0,1,0,1]: [0.2,0.2,0.2,1],
			diffuseLight: [0.8,0.8,0.8,1],
			specularLight: [0,0,0.2,1],
		};
		//Sphere(3).draw(uniforms,[0,0,0,vec3_length(size)/2]);
		var t = (now()/1000)%1;
		if(this.showNormals) {
			this.model.draw(uniforms,t,true);
			gl.lineWidth(1.0);
			this.model.drawNormals(uniforms,t);
			Sphere(2).draw({
					__proto__: uniforms,
					mvMatrix: mMatrix,
				},vec3_vec4(lightPos,0.2),[0.8,0.7,0,1.0]);
		} else
			this.model.draw(uniforms,t);
	},
	show: function() {
		this.onResize();
		this.win.show(-1);
		this.menu.show();
	},
	hide: function() {
		this.win.hide();
		this.menu.hide();
	},
	layout: function() {
		this.menu.performLayout();
		this.menu.ctrl.setPosVisible([canvas.width,canvas.height]); // bottom-right
		UIViewport.prototype.layout.call(this);
	},
	onResize: function() {
		this.setPos([0,0]);
		this.setSize([canvas.width,canvas.height]);
		this.layout();
	},
};
