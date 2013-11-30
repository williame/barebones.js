/* https://github.com/williame/barebones.js project
This file is a demo that shows a simple grid-based terrain you can navigate and
select on.  Its an ideal basis for an RTS or something game!

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

function DemoGridTerrain() {
	UIViewport.call(this,this);
	var self = this;
	this.map = new DemoGridTerrainMap(100,100);
	this.camera = {
		centre: [this.map.w/2,0,this.map.h/2],
		up: null,
		eye: [this.map.w/2,10,this.map.h/2+10],
	};
	this.uniforms = {
		t: 0,
		fogColour: [1,1,0.8,1],
		fogDensity: 0.02,
		pMatrix: null,
		mvMatrix: null,
		nMatrix: null,
	};
	this.hadMouseDown = false;
	this.zoomDiff = 0;
	this.zoom = 15;
	this.minZoom = 10;
	this.maxZoom = 30;
	this.viewMode = "3D"; // can be "3D" or "top"
	var menu = new UIChoiceMenu("grid terrain demo",[UIViewport.ToolPan,UIViewport.ToolRotate,DemoGridTerrainPaint],this);
	menu.addChild(new UIButton("show grid",function() { self.map.showGridLines = !self.map.showGridLines; }));
	menu.addChild(new UIButton("3D",function() {
					if(self.viewMode == "3D") {
						self.viewMode = "top";
						self.setCamera(null,self.camera.centre);
					} else if(self.viewMode == "top") {
						self.viewMode = "3D";
						self.setCamera(vec3_add(self.camera.centre,[0,10,10]),self.camera.centre);
					} else
						fail("unsupported view mode: "+self.viewMode);
					this.setText(self.viewMode);
					this.dirty();
			}));
	this.menu = new UIWindow(false,menu,UILayoutRows);
	this.menu.ctrl.allowClickThru = false;
	this.tool = null;
	this.win = new UIWindow(false,this); // a window to host this viewport in
	menu.setTool("Paint");
}
DemoGridTerrain.prototype = {
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
		if(this.zoomDiff) {
			var elapsed = t - (this.lastRender || t), camera = this.camera;
			this.zoom += this.zoomDiff * elapsed*0.001;
			this.zoom = Math.max(this.minZoom,Math.min(this.zoom,this.maxZoom));
			this.setCamera(camera.eye,camera.centre,camera.up);
			this.zoomDiff = 0;
		}
		// draw the main map
		this.map.draw(this.uniforms);
		if(this.tool && this.tool.draw)
			this.tool.draw();
		this.lastRender = t;
		
	},
	setCamera: function(eye,centre,up) {
		if(centre[0] < 0 || centre[0] > this.map.w || centre[2] < 0 || centre[2] > this.map.h) // keep centre on map
			return;
		this.camera.centre = centre;
		if(this.viewMode == "3D") {
			this.camera.up = up || [0,1,0];
			this.camera.eye = vec3_add(centre,vec3_scale(vec3_normalise(vec3_sub(eye,centre)),this.zoom));
		} else if(this.viewMode == "top") {
			this.camera.up = up || [0,0,-1];
			this.camera.eye = vec3_sub(centre,[0,-this.zoom,0]);
		} else
			fail("unsupported view mode: "+this.viewMode);
		var aspect = this.width()/this.height();
		this.uniforms.pMatrix = new Float32Array(createPerspective(60.0,aspect,0.01,100));
		this.uniforms.mvMatrix = new Float32Array(createLookAt(this.camera.eye,this.camera.centre,this.camera.up));
		this.uniforms.nMatrix = mat4_mat3(mat4_transpose(mat4_inverse(this.uniforms.mvMatrix)));
	},
	mousePos: function(evt) {
		return this.map.pos(this.mouseRay(evt));
	},
	mapPos: function(evt,debug) {
		return this.map.mapPos(this.mouseRay(evt),debug);
	},
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
			this.zoomDiff += amount;
			return true;
		}
		return false;
	},
};

function DemoGridTerrainMap(w,h) {
	assert(this !== window);
	this.w = w;
	this.h = h;
	var self = this;
	this.vbo = gl.createBuffer();
	this.tex = gl.createTexture();
	this.program = DemoGridTerrainMap.program = DemoGridTerrainMap.program || 
		createProgram(
			"precision mediump float;\n"+
			"uniform mat4 mvMatrix, pMatrix;\n"+
			"uniform mat3 nMatrix;\n"+
			"uniform float oneOverWidth, oneOverHeight, oneOverAtlasSize;\n"+
			"attribute vec3 vertex, normal;\n"+
			"attribute float corner, tile;\n"+
			"const vec3 lightPos = vec3(1.0,1.0,1.0);\n"+
			"varying vec3 n, lightDir;\n"+
			"varying float tileIdx;\n"+
			"varying vec2 normalTx, tileAtlasTx;\n"+
			"void main() {\n"+
			"	n = nMatrix * normal,0.0;\n"+
			"	vec4 vPos = mvMatrix*vec4(vertex,1.0);\n"+
			"	lightDir = lightPos-vPos.xyz;\n"+
			"	gl_Position = pMatrix * vPos;\n"+
			"	normalTx = vec2(vertex.x*oneOverWidth,vertex.z*oneOverHeight);\n"+
			"	tileAtlasTx = vec2(0.0, tile * oneOverAtlasSize);\n"+
			"	if(corner == 1.0 || corner == 2.0) tileAtlasTx.x = 1.0;\n"+
			"	if(corner == 2.0 || corner == 3.0) tileAtlasTx.y += oneOverAtlasSize;\n"+
			"	tileIdx = tile;\n"+
			"}\n",
			"precision mediump float;\n"+
			"uniform vec4 colour;\n"+
			"uniform float fogDensity;\n"+
			"uniform vec4 fogColour;\n"+
			"varying float tileIdx;\n"+
			"varying vec3 n, lightDir;\n"+
			"varying vec2 normalTx, tileAtlasTx;\n"+
			"uniform sampler2D texNormal, texTileAtlas;\n"+
			"const float LOG2 = 1.442695;\n"+
			"void main() {\n"+
			"	float z = gl_FragCoord.z / gl_FragCoord.w;\n"+
			"	float fogFactor = exp2(-fogDensity*fogDensity*z*z*LOG2);\n"+
			"	fogFactor = clamp(fogFactor,0.0,1.0);\n"+
			"	vec4 fragColour = vec4(texture2D(texNormal,normalTx).rgb,1.0);\n"+
			"	if(tileIdx > 0.0) {\n"+
			"		vec4 tileColour = texture2D(texTileAtlas,tileAtlasTx);\n"+
			"		fragColour = vec4(fragColour.rgb*(1.0-tileColour.a) + tileColour.rgb*tileColour.a,1.0);\n"+
			"	}\n"+
			"	float ambient = min(1.0,max(dot(normalize(n),normalize(lightDir)),0.0));\n"+
 			"	fragColour.xyz *= 0.6+(ambient*0.4);\n"+
 			"	fragColour *= colour;\n"+
			"	gl_FragColor = mix(fogColour,fragColour,fogFactor);\n"+
			"}\n",
			["mvMatrix","pMatrix","nMatrix",
				"colour","fogDensity","fogColour",
				"texNormal","texTileAtlas",
				"oneOverWidth","oneOverHeight","oneOverAtlasSize"],
			["vertex","normal","corner","tile"]);
	this.tileAtlas = programs.BLANK;
	loadFile("image","data/terrain_tiles.png",function(tex) { self.tileAtlas = tex; });
	this.showGridLines = true;
	this.tileMap = {};
	this.generateHeightMap();
	this._mapData = new ArrayBuffer(w*h*4);
	this._mapBmp =  new Uint32Array(this._mapData);
	this.tileVertices = new Float32Array(w*h*8*6);
	for(var y=0; y<h; y++)
		for(var x=0; x<w; x++)
			this.setTile(x,y);
	gl.bindBuffer(gl.ARRAY_BUFFER,this.vbo);
	gl.bufferData(gl.ARRAY_BUFFER,this.tileVertices,gl.STATIC_DRAW);
	gl.bindBuffer(gl.ARRAY_BUFFER,null);
	this.dirty = true;
}
DemoGridTerrainMap.prototype = {
	plane: [[1,0,0],[0,1,0]],
	heightScale: 0.01,
	bgColour: 0xff40a040,
	generateHeightMap: function() {
		var	w = this.w, h = this.h,
			heightMapStride = w+1,
			heightMap = this.heightMap = new Int8Array(heightMapStride*(h+1)),
			macro = [];
		// make a macro heightmap at powers of two
		for(var i=1; (1<<i) <= Math.max(w,h); i++) {
			var temp = [];
			for(var j=0; j<=(h>>i); j++) {
				var temp2 = [];
				for(var k=0; k<=(w>>i); k++)
					temp2.push(Math.random());
				temp.push(temp2);
			}
			macro.push(temp);
		}
		// paint it down
		for(var y=0; y<=h; y++) {
			for(var x=0; x<=w; x++) {
				var z = 0;
				for(var i in macro) {
					i = parseInt(i)+1;
					z += macro[i-1][y>>i][x>>i] * 255/(macro.length-i+1);
				}
				heightMap[y*heightMapStride+x] = z+Math.random();
			}
		}
		// smooth it
		temp = new Int8Array(heightMap.length);
		for(var pass=0; pass<4; pass++) {
			for(var y=0; y<=h; y++)
				for(var x=0; x<=w; x++) {
					var	count = 0, sum = 0,
						visit = function(x,y) {
							if(x>=0 && x<w && y>=0 && y<h) {
								sum += heightMap[y*heightMapStride+x];
								count++;
							}
						};
					for(var i=-1; i<2; i++)
						for(var j=-1; j<2; j++)
							visit(x+i,y+j);
					temp[y*heightMapStride+x] = (sum/count+heightMap[y*heightMapStride+x])/2;
				}
			heightMap.set(temp);
		}
		// compute the normals
		var normalsMap = this.normalsMap = new Float32Array(heightMap.length*3);
		for(var y=0; y<h; y++)
			for(x=0; x<w; x++) {
				var	offsets = this._getOffsets(x,y), // tl,tr,bl,br
					corners = this._getCorners(x,y);
				for(var i=0; i<2; i++) { // tl,t,bl then tr,bl,br over corners
					var n = vec3_normalise(triangle_normal(corners[i+0],corners[i+1],corners[i+2]));
					if(i) n = vec3_neg(n); // flip winding if second triangle
					for(var j=0; j<3; j++) {
						normalsMap[offsets[i+0]*3+j] += n[j];
						normalsMap[offsets[i+1]*3+j] += n[j];
						normalsMap[offsets[i+2]*3+j] += n[j];
					}
				}
			}
		for(var y=0, i=0; y<h; y++)
			for(x=0; x<w; x++) {
				var c = 4; // how many triangles touch this point?
				if(x == 0 || x == this.w) c /= 2;
				if(y == 0 || y == this.h) c /= 2;
				normalsMap[i++] /= c;
				normalsMap[i++] /= c;
				normalsMap[i++] /= c;
			}
	},
	getTile: function(x,y) {
		if(x >= 0 && x < this.w && y >= 0 && y < this.h)
			return this.tileMap[y*this.w+x] || null;
		return null;
	},
	draw: function(uniforms) {
		if(this.dirty) {
			createTexture(this.tex,this.w,this.h,new Uint8Array(this._mapData),true);
			gl.bindBuffer(gl.ARRAY_BUFFER,this.vbo);
			gl.bufferData(gl.ARRAY_BUFFER,this.tileVertices,gl.STATIC_DRAW);
			gl.bindBuffer(gl.ARRAY_BUFFER,null);
			this.dirty = false;
		}
		var program = this.program;
		gl.useProgram(program);
		gl.uniformMatrix4fv(program.pMatrix,false,uniforms.pMatrix);
		gl.uniformMatrix4fv(program.mvMatrix,false,uniforms.mvMatrix);
		gl.uniformMatrix3fv(program.nMatrix,false,uniforms.nMatrix);
		gl.uniform1f(program.oneOverWidth,1/this.w);
		gl.uniform1f(program.oneOverHeight,1/this.h);
		gl.uniform1f(program.oneOverAtlasSize,1/16);
		gl.uniform4f(program.colour,1,1,1,1);
		gl.uniform4f(program.fogColour,1,1,1,1);
		gl.uniform1f(program.fogDensity,0.03);
		gl.activeTexture(gl.TEXTURE1);
		gl.uniform1i(program.texTileAtlas,1);
		gl.bindTexture(gl.TEXTURE_2D,this.tileAtlas);
		gl.activeTexture(gl.TEXTURE0);
		gl.uniform1i(program.texNormal,0);
		gl.bindTexture(gl.TEXTURE_2D,this.tex);
		gl.bindBuffer(gl.ARRAY_BUFFER,this.vbo);
		gl.enableVertexAttribArray(program.vertex);
		gl.vertexAttribPointer(program.vertex,3,gl.FLOAT,false,8*4,0);
		gl.enableVertexAttribArray(program.normal);
		gl.vertexAttribPointer(program.normal,3,gl.FLOAT,false,8*4,3*4);
		gl.enableVertexAttribArray(program.corner);
		gl.vertexAttribPointer(program.corner,1,gl.FLOAT,false,8*4,6*4);
		gl.enableVertexAttribArray(program.tile);
		gl.vertexAttribPointer(program.tile,1,gl.FLOAT,false,8*4,7*4);
		gl.drawArrays(gl.TRIANGLES,0,this.w*this.h*6);
		if(this.showGridLines) {
			gl.uniform4f(program.colour,1,0,1,1);
			gl.lineWidth(3);
			gl.drawArrays(gl.LINES,0,this.w*this.h*6);
		}
		gl.disableVertexAttribArray(program.tile);
		gl.disableVertexAttribArray(program.corner);
		gl.disableVertexAttribArray(program.normal);
		gl.disableVertexAttribArray(program.vertex);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);
		gl.bindTexture(gl.TEXTURE_2D,null);
		gl.useProgram(null);
	},
	mapPos: function(ray) {
		var stop = plane_ray_intersection(this.plane,ray);
		if(!stop) return null;
		var start = [ray[0][0],ray[0][2]];
		stop = [stop[0],stop[2]];
		var	xd = Math.abs(stop[0]-start[0]),
			yd = Math.abs(stop[1]-start[1]),
			sign = function(x) { return x ? x < 0 ? -1 : 1 : 0; },
			xsign = sign(stop[0]-start[0]),
			ysign = sign(stop[1]-start[1]),
			xinc = xsign,
			yinc = ysign;
		if(!xd || !yd) return null;
		if(xd > yd) yinc *= yd/xd; else xinc *= xd/yd;
		var x = start[0], y = start[1], visited = {};
		for(var i=Math.max(xd,yd)+2; i-->0; ) {
			for(var j=0; j<2; j++) {
				var x2 = Math.floor(x+j*xsign);
				if(x2 < 0 || x2 >= this.w)
					continue;
				for(var k=0; k<2; k++) {
					var y2 = Math.floor(y+k*ysign);
					if(y2 < 0 || y2 >= this.h)
						continue;
					var key = ""+x2+","+y2;
					if(key in visited)
						continue;
					visited[key] = true;
					var	c = this._getCorners(x2,y2),
						hit = triangle_ray_intersection(c[0],c[1],c[2],ray[0],ray[1],null,false,true) ||
							triangle_ray_intersection(c[1],c[2],c[3],ray[0],ray[1],null,false,true);
					if(hit)
						return [x2,y2];
				}
			}
			x += xinc;
			y += yinc;
		}
		return null;
	},
	pos: function(ray) {
		return plane_ray_intersection(this.plane,ray);
	},
	setTile: function(x,y,tile,bgColour) {
		if(x<0 || y<0 || x >= this.w || y >= this.h) return;
		var pos = y*this.w+x;
		if(tile)
			this.tileMap[pos] = tile;
		else
			delete this.tileMap[pos];
		this._mapBmp[pos] = bgColour || this.bgColour;
		var 	heightMapStride = this.w+1,
			ofs = (y*this.w+x)*8*6,
			tileVertices = this.tileVertices,
			params = tile? tile.getTileAndOrientation(): [0],
			tl = params[1]||0,
			tr = params[2]||0,
			br = params[3]||0,
			bl = params[4]||0,
			heightMap = this.heightMap,
			heightScale = this.heightScale,
			normalsMap = this.normalsMap,
			emit = function(x,y,corner) {
				tileVertices[ofs++] = x;
				tileVertices[ofs++] = heightMap[y*heightMapStride+x]*heightScale;
				tileVertices[ofs++] = y;
				tileVertices[ofs++] = normalsMap[y*heightMapStride*3+x*3+0];
				tileVertices[ofs++] = normalsMap[y*heightMapStride*3+x*3+1];
				tileVertices[ofs++] = normalsMap[y*heightMapStride*3+x*3+2];
				tileVertices[ofs++] = corner;
				tileVertices[ofs++] = params[0];
			};
		emit(x,y+1,tl); emit(x+1,y+1,tr); emit(x,y,bl);
		emit(x,y,bl); emit(x+1,y+1,tr); emit(x+1,y,br);
		this.dirty = true;
	},
	_getOffsets: function(x,y) {
		assert(x>=0 && x<this.w && y>=0 && y<this.h);
		var heightMapStride = this.w+1, ofs = y*heightMapStride+x, z = this.heightMap;
		return [ofs,ofs+1,ofs+heightMapStride,ofs+heightMapStride+1];
	},
	_getHeights: function(x,y) {
		var ofs = this._getOffsets(x,y), z = this.heightMap;
		return [z[ofs[0]],z[ofs[1]],z[ofs[2]],z[ofs[3]]];
	},
	_getCorners: function(x,y) {
		var z = vec4_scale(this._getHeights(x,y),this.heightScale);
		return [[x,z[0],y],[x+1,z[1],y],[x,z[2],y+1],[x+1,z[3],y+1]];
	},
	getMinHeight: function(x,y) {
		return Math.min.apply(Math,this._getHeights(x,y))*this.heightScale;
	},
	getCentreHeight: function(x,y) {
		var corners = this._getHeights(x,y);
		return ((corners[0]+corners[2])/2)*this.heightScale;
	},
	getMaxHeight: function(x,y) {
		return Math.max.apply(Math,this._getHeights(x,y))*this.heightScale;
	},
	getHeightDiff: function(x,y) {
		var corners = this._getHeights(x,y);
		return (Math.max.apply(Math,corners)-Math.min.apply(Math,corners))*this.heightScale;
	},
	getTilePerimeter: function(x,y,w,h) {
		assert(x >= 0 && y >= 0 && x < this.w && y < this.h);
		if(x+w > this.w) w = this.w-x;
		if(y+h > this.h) h = this.h-y;
		var	lines = new Float32Array((w+h+w+h)*3),
			ofs = 0, heightMapStride = this.w+1,
			heightMap = this.heightMap, heightScale = this.heightScale,
			emit = function(x2,y2) {
				lines[ofs++] = x+x2;
				lines[ofs++] = heightMap[(y+y2)*heightMapStride+x+x2]*heightScale;
				lines[ofs++] = y+y2;
			};
		for(var i=0; i<=w; i++) emit(i,0);
		for(var i=1; i<=h; i++) emit(w,i);
		for(var i=w; i-->0; )   emit(i,h);
		for(var i=h; i-->1; )   emit(0,i);
		assert(ofs == lines.length);
		return lines;
	},
};

function DemoGridTerrainPaint(view) {
	assert(this !== window);
	this.view = view;
	this.selector = gl.createBuffer();
	this.selectorVertices = 0;
	this.lastPos = null;
	this.pos = null;
}
DemoGridTerrainPaint.prototype = {
	name: "Paint",
	onMouseDown: function(evt,keys) {
		this.onMouseMove(evt,keys,true);
	},
	onMouseMove: function(evt,keys,isMouseDown) {
		this.pos = this.view.mapPos(evt,false);
		if(!this.pos) return;
		var x = this.pos[0], y = this.pos[1];
		if(!this.lastPos || this.lastPos[0] != x || this.lastPos[1] != y) {
			this.lastPos = this.pos;
			gl.bindBuffer(gl.ARRAY_BUFFER,this.selector);
			var buf = this.view.map.getTilePerimeter(x,y,1,1);
			this.selectorVertices = buf.length/3;
			gl.bufferData(gl.ARRAY_BUFFER,buf,gl.STATIC_DRAW);
			gl.bindBuffer(gl.ARRAY_BUFFER,null);
		}
		if(isMouseDown) {
			var tile = null;
			if(Math.random() < 0.3) {
				tile = {
					rotations: [[0,1,2,3], // valid rotations of image
						[3,0,1,2],
						[2,3,0,1],
						[1,2,3,0]],
					getTileAndOrientation: function() {
						var r = this.rotations[Math.floor(Math.random()*4)],
							tileIdx = Math.floor(Math.random()*10);
						return [tileIdx,r[0],r[1],r[2],r[3]];
					},
				};
			}
			this.view.map.setTile(this.pos[0],this.pos[1],tile,0xff000000|Math.floor(Math.random()*0xffffff));
		}
	},
	onMouseOut: function() {
		this.pos = null;
	},
	draw: function() {
		if(this.pos) {
			programs.standard(this.drawSelector,{
				__proto__: this.view.uniforms,
				colour: [1,1,1,1],
			},this);
		}
	},
	drawSelector: function(program) {
		gl.bindBuffer(gl.ARRAY_BUFFER,this.selector);
		gl.vertexAttribPointer(program.vertex,3,gl.FLOAT,false,0,0);
		gl.vertexAttribPointer(program.normal,3,gl.FLOAT,false,0,0);
		gl.vertexAttribPointer(program.texCoord,2,gl.FLOAT,false,0,0); // noise
		gl.lineWidth(3);
		gl.drawArrays(gl.LINE_LOOP,0,this.selectorVertices);
	},
};

