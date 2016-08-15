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
	this.models = [
		{
			name: "spider",
			file:"data/test.g3d",
		},
		{
			name:"battle machine",
			file:"data/battle_machine_charging.g3d",
		},
		{
			name:"ornithopter",
			file:"data/ornithopter_walking.g3d",
		},
		{
			name:"normals test",
			file:"data/g3d_advtex_sample/sample_animated.g3d",
		},
	];
	this.model = this.models[0].g3d = new G3D(this.models[0].file);
	this.viewMode = "3D";
	this.uniforms = {
		pMatrix: null,
		mvMatrix: null,
		nMatrix: null,
	};
	this.camera = {
		centre: [1,0,0],
		up: null,
		eye: [0,2,1],
		zoom: false,
	};
	this.win = new UIWindow(false,this); // a window to host this viewport in
	var self = this;
	var menu = new UIChoiceMenu("g3d demo",[UIViewport.ToolPan,UIViewport.ToolRotate],this);
	menu.addChild(new UIComboBox(this.models,0,function(model) {
		self.model = model.g3d || (model.g3d = new G3D(model.file));
	}));
	menu.addChild(new UIButton("show normals",function() { 
		self.showNormals = !self.showNormals;
		this.setActive(self.showNormals);
	}));
	menu.addChild(new UIButton("auto normals",function() { self.model.autoNormals(); }));
	menu.addChild(new UIButton("show grid",function() { 
		self.showGrid = !self.showGrid;
		this.setActive(self.showGrid);
	},"showGrid"));
	this.menu = new UIWindow(false,menu);
	this.menu.ctrl.allowClickThru = false;
	this.showGrid = true;
	this.menu.find("showGrid").setActive(this.showGrid);
}
DemoG3D.prototype = {
	__proto__: UIViewport.prototype,
	render: function(ctx) {
		gl.clearColor(0,0,0,0);
		gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);
		if(!this.model.ready) return;
		var	now = window.now(), t = (now/1000)%1,
			//invMvMatrix = mat4_inverse(this.uniforms.mvMatrix),
			uniforms = {
				__proto__: this.uniforms,
				colour: OPAQUE,
				fogColour: [0.2,0.2,0.2,1.0],
				fogDensity: this.showNormals? 0: 0.02,
				lightColour: this.showNormals? [1,0,0,1]: [1,0,0,1],
				lightPos: mat4_vec3_multiply(this.uniforms.pMatrix, [0,10,-10]),
				ambientLight: this.showNormals? [0,0.6,0,1]: [0.0,0.0,0.0,1],
				diffuseLight: [1,1,1,1],
				specularLight: [0,0,0.2,1],
				shininess: 1,
			};
		gl.lineWidth(1.0);
		if(this.showGrid) {
			var d = 5;
			if(!this.gridLines) {
				var	origin = d/2,
					vertices = [],
					ofs = 0;
				vertices.push(-origin,0,-origin);
				vertices.push(origin,0,origin);
				vertices.push(origin,0,-origin);
				vertices.push(-origin,0,-origin);
				vertices.push(-origin,0,origin);
				vertices.push(origin,0,origin);
				for(var line=0, i=0; line<d; line++) {
					vertices.push(-origin,0,line-origin);
					vertices.push(origin,0,line-origin);
					vertices.push(line-origin,0,-origin);
					vertices.push(line-origin,0,origin);
				}
				this.gridLines = gl.createBuffer();
				gl.bindBuffer(gl.ARRAY_BUFFER,this.gridLines);
				gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(vertices),gl.STATIC_DRAW);
			}
			programs.solidFill(function(program) {
				gl.bindBuffer(gl.ARRAY_BUFFER,this.gridLines);
				gl.vertexAttribPointer(program.vertex,3,gl.FLOAT,false,0,0);
				gl.drawArrays(gl.LINES,6,d*2*2);
				gl.uniform4f(program.colour,0.8,0.6,0.6,1);
				gl.drawArrays(gl.TRIANGLES,0,6);
			},uniforms,this);
			gl.bindBuffer(gl.ARRAY_BUFFER,null);
		}
		var bounds = this.model.bounds, size = vec3_sub(bounds[1],bounds[0]);
		var modelUniforms = {
			__proto__: uniforms,
				
		};
		modelUniforms.mvMatrix = mat4_multiply(modelUniforms.mvMatrix,mat4_scale(1/Math.max.apply(Math,size)));
		modelUniforms.mvMatrix = mat4_multiply(modelUniforms.mvMatrix,mat4_translation(vec3_neg(bounds[0])));
		modelUniforms.mvMatrix = mat4_multiply(modelUniforms.mvMatrix,mat4_rotation(now/4000,[0,1,0]));
		if(this.showNormals) {
			this.model.draw(modelUniforms,t,true);
			this.model.drawNormals(modelUniforms,t);
			Sphere(2).draw(uniforms,vec3_vec4(uniforms.lightPos,0.05),[0.8,0.7,0,1.0]);
		} else
			this.model.draw(modelUniforms,t);
	},
	setCamera: function(eye,centre,up) {
		this.camera.centre = centre;
		if(this.viewMode == "3D") {
			this.camera.up = up || [0,1,0];
			this.camera.eye = this.zoom?
				vec3_add(centre,vec3_scale(vec3_normalise(vec3_sub(eye,centre)),this.camera.zoom)):
				eye;
		} else if(this.viewMode == "top") {
			this.camera.up = up || [0,0,-1];
			this.camera.eye = vec3_sub(centre,[0,-this.camera.zoom,0]);
		} else
			fail("unsupported view mode: "+this.viewMode);
		var aspect = this.width()/this.height();
		this.uniforms.pMatrix = new Float32Array(createPerspective(80.0,aspect,0.01,100));
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
	setSize: function() {
		UIViewport.prototype.setSize.apply(this,arguments);
		this.setCamera(this.camera.eye,this.camera.centre,this.camera.up);
	},
};
