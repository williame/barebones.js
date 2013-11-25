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
	this.viewMode = "3D";
	this.uniforms = {
		pMatrix: null,
		mvMatrix: null,
		nMatrix: null,
	};
	this.camera = {
		centre: [0,0,0],
		up: null,
		eye: [0,2,2],
		zoom: 1,
	};
	this.setCamera(this.camera.eye,this.camera.centre);
	var self = this;
	var menu = new UIChoiceMenu("g3d demo",[UIViewport.ToolPan,UIViewport.ToolRotate],this);
	menu.addChild(new UIButton("show normals",function() { 
		self.showNormals = !self.showNormals;
		this.bgColour = self.showNormals? UI.defaults.btn.bgColorActive: UI.defaults.btn.bgColour;
		this.dirty();
	}));
	menu.addChild(new UIButton("auto normals",function() { self.model.autoNormals(); }));
	this.menu = new UIWindow(false,menu);
	this.menu.ctrl.allowClickThru = false;
}
DemoG3D.prototype = {
	__proto__: UIViewport.prototype,
	render: function(ctx) {
		gl.clearColor(0,0,0,0);
		gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);
		if(!this.model.ready) return;
		var bounds = this.model.bounds, size = vec3_sub(bounds[1],bounds[0]);
		var lightPos = mat4_vec3_multiply(this.uniforms.mvMatrix,[-0.5,3,2]);
		var mvMatrix = mat4_multiply(this.uniforms.mvMatrix,mat4_scale(1/Math.max.apply(Math,size)));
		mvMatrix = mat4_multiply(mvMatrix,mat4_translation(vec3_neg(bounds[0])));
		var uniforms = {
			__proto__: this.uniforms,
			mvMatrix: mat4_multiply(mvMatrix,mat4_rotation(now()/3000,[0,1,0])),
			colour: OPAQUE,
			fogColour: [0.2,0.2,0.2,1.0],
			fogDensity: this.showNormals? 0: 0.02,
			lightPos: lightPos,
			lightColour: this.showNormals? [1,0,0,1]: [1,1,1,1],
			ambientLight: this.showNormals? [0,0.6,0,1]: [0.2,0.2,0.2,1],
			diffuseLight: [0.8,0.8,0.8,1],
			specularLight: [0,0,0.2,1],
		};
		var t = (now()/1000)%1;
		if(this.showNormals) {
			this.model.draw(uniforms,t,true);
			gl.lineWidth(1.0);
			this.model.drawNormals(uniforms,t);
			Sphere(2).draw({
					__proto__: uniforms,
					mvMatrix: this.uniforms.mvMatrix,
				},vec3_vec4(lightPos,0.2),[0.8,0.7,0,1.0]);
		} else
			this.model.draw(uniforms,t);
	},
	setCamera: function(eye,centre,up) {
		this.camera.centre = centre;
		if(this.viewMode == "3D") {
			this.camera.up = up || [0,1,0];
			this.camera.eye = vec3_add(centre,vec3_scale(vec3_normalise(vec3_sub(eye,centre)),this.camera.zoom));
		} else if(this.viewMode == "top") {
			this.camera.up = up || [0,0,-1];
			this.camera.eye = vec3_sub(centre,[0,-this.camera.zoom,0]);
		} else
			fail("unsupported view mode: "+this.viewMode);
		var aspect = this.width()/this.height();
		this.uniforms.pMatrix = new Float32Array(createPerspective(60.0,aspect,0.01,100));
		this.uniforms.mvMatrix = new Float32Array(createLookAt(this.camera.eye,this.camera.centre,this.camera.up));
		this.uniforms.nMatrix = mat4_mat3(mat4_transpose(mat4_inverse(this.uniforms.mvMatrix)));
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
