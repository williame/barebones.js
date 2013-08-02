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
			mvMatrix = createLookAt([1,1,1],[0,0,0],[0,1,0]),
			bounds = this.model.bounds,
			size = vec3_sub(bounds[1],bounds[0]);
		mvMatrix = mat4_multiply(mvMatrix,mat4_scale(1/Math.max.apply(Math,size)));
		mvMatrix = mat4_multiply(mvMatrix,mat4_translation(vec3_neg(bounds[0])));
		var uniforms = {
			pMatrix: pMatrix,
			mvMatrix: mvMatrix,
			colour: OPAQUE,
			fogColour: [0.2,0.2,0.2,1.0],
			fogDensity: 0.02,
			lightPos: [1,2,1],
			lightDir: [-1,-1,-1],
			ambientLight: [0.8,0.8,0.8],
		};
		//Sphere(3).draw(uniforms,[0,0,0,vec3_length(size)/2]);
		this.model.draw(uniforms,(now()/1000)%1);
	},
	show: function() {
		this.onResize();
		this.win.show(-1);
	},
	hide: function() {
		this.win.hide();
	},
	onResize: function() {
		this.setPos([0,0]);
		this.setSize([canvas.width,canvas.height]);
		this.layout();
	},
};
