/* https://github.com/williame/barebones.js project
This file provides a 3D model loader for the Glest G3D format (exporters exist
for Blender and older copies of 3DSMax).

* semi-transparency seems broken.  Avoid models with semi-transparent textures.

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

function G3D(filename,readyCallback) {
	assert(this !== window);
	var self = this;
	if(readyCallback && readyCallback.wait) // using a Waiter?
		readyCallback = readyCallback.wait(filename);
	this.readyCallbacks = [];
	if(readyCallback)
		this.readyCallbacks.push(readyCallback);
	this.filename = filename;
	this.meshes = [];
	this.meshesLerp = [];
	this.meshesSingle = [];
	this.textureFilenames = [];
	this.ready = false;
	this.showNormals = false;
	this.boundingSphere = [0,0,0,0];
	loadFile("ArrayBuffer",filename,function(arrayBuffer) { self._fileLoaded(arrayBuffer); });
}
G3D.loaded = {};
G3D.load = function(filename,readyCallback) {
	var g3d = G3D.loaded[filename] || null;
	if(g3d) {
		if(readyCallback) {
			if(!g3d.ready)
				g3d.readyCallbacks.push(readyCallback);
			else
				setTimeout(function() { readyCallback(g3d); },0);
		}
	} else
		g3d = G3D.loaded[filename] = new G3D(filename,readyCallback);
	return g3d;
}
G3D.prototype = {
	type: "model.g3d",
	_done: function() {
		for(var callback in this.readyCallbacks)
			this.readyCallbacks[callback](this);
		this.readyCallbacks = [];
	},
	_fileLoaded: function(arrayBuffer) {
		var	self = this,
			done = Callback(this,this._done),
			reader = new BinaryDataReader(arrayBuffer);
		if(reader.uint32()>>24 != 4) throw "unsupported G3D version";
		var meshCount = reader.uint16();
		if(!meshCount) throw "has no meshes";
		if(reader.uint8()) throw "is not a mtMorphMesh";
		this.bounds = [[100000,100000,100000],[-100000,-100000,-100000]];
		for(var i=0; i<meshCount; i++) {
			var mesh = new G3DMesh(this,reader);
			for(var c=0; c<3; c++) { // bounds
				this.bounds[0][c] = Math.min(mesh.bounds[0][c],this.bounds[0][c]);
				this.bounds[1][c] = Math.max(mesh.bounds[1][c],this.bounds[1][c]);
			}
			this.meshes.push(mesh);
			this[mesh.frameCount==1? "meshesSingle":"meshesLerp"].push(mesh);
			// work out what textures we have to load
			if(mesh.textureFilename) {
				if(!this.textureFilenames[mesh.textureFilename])
					this.textureFilenames[mesh.textureFilename] = [];
				this.textureFilenames[mesh.textureFilename].push(mesh);
			}
		}
		if(this.meshesLerp.length) {
			// if some meshes are multi-frame, don't bother with a program switch
			this.meshesLerp.push.apply(this.meshesLerp,this.meshesSingle);
			this.meshesSingle = [];
		}
		this.boundingSphere = vec3_lerp(this.bounds[0],this.bounds[1],0.5);
		this.boundingSphere.push(vec3_length(vec3_sub(this.bounds[1],this.bounds[0])));
		if(reader.ofs != arrayBuffer.byteLength)
			throw "not all bytes consumed by G3D loader!";
		this.ready = (0 == this.textureFilenames.length);
		for(var texture in this.textureFilenames)
			(function(filename,meshes) {
				filename = self.filename.substring(0,self.filename.lastIndexOf("/")+1) + filename;
				loadFile("image",filename,function(tex) {
					for(var mesh in meshes)
						meshes[mesh].texture = tex;
					self.textureFilenames = self.textureFilenames.slice(
						self.textureFilenames.indexOf(filename),1);
					self.ready = (0 == self.textureFilenames.length);
					if(self.ready && self.readyCallbacks)
						setTimeout(done,0);
				});
			})(texture,this.textureFilenames[texture]);
		if(this.ready && this.readyCallbacks)
			setTimeout(done,0);
	},
	draw: function(uniforms,t,untextured,op) {
		if(!this.ready) return;
		op = op || gl.TRIANGLES;
		var	wasCulled = gl.getParameter(gl.CULL_FACE),
			wasMode = gl.getParameter(gl.CULL_FACE_MODE),
			wasFace = gl.getParameter(gl.FRONT_FACE);
		gl.frontFace(gl.CCW);
		gl.cullFace(gl.BACK);
		if(this.meshesSingle.length)
			programs.standard(this._drawSingle,uniforms,this,untextured,op);
		if(this.meshesLerp.length)
			programs.standardLerp(this._drawLerp,uniforms,this,t||0,untextured,op);
		if(wasCulled) gl.enable(gl.CULL_FACE); else gl.disable(gl.CULL_FACE);
		gl.frontFace(wasFace);
		gl.cullFace(wasMode);
	},
	_drawSingle: function(program,untextured,op) {
		for(var mesh in this.meshesSingle)
			this.meshesSingle[mesh]._drawSingle(program,untextured,op);
	},
	_drawLerp: function(program,t,untextured,op) {
		for(var mesh in this.meshesLerp)
			this.meshesLerp[mesh]._drawLerp(program,t,untextured,op);
	},
	drawNormals: function(uniforms,t) {
		if(!this.ready) return;
		programs.solidFillLerp(function(program) {
				for(var mesh in this.meshes)
					this.meshes[mesh]._drawNormals(program,t);
			},uniforms,this);
	},
	lineIntersection: function(lineOrigin,lineDir,t) {
		var	lineLen = vec3_length(lineDir),
			lineSphere = vec3_add(lineOrigin,vec3_scale(lineDir,lineLen/2)),
			hit, k, l, n;
		lineSphere.push(lineLen/2);
		for(var mesh in this.meshes) {
			mesh = this.meshes[mesh];
			mesh.lineIntersection(lineOrigin,lineDir,lineSphere,function(i,I,N) {
				if(!hit || k > i) {
					n = N;
					l = I;
					k = i;
					hit = mesh;
				}
			},Math.floor((t||0)*mesh.frameCount));
		}
		return hit?[hit,k,l,n]:null;
	},
	rayIntersects: function(origin,dir,t) {
		for(var mesh in this.meshes) {
			mesh = this.meshes[mesh];
			if(mesh.rayIntersection(origin,dir,null,
				Math.floor((t||0)*mesh.frameCount)))
				return true;
		}
		return false;
		
	},
	yAt: function(x,z,t) {
		var y = -10, n, hit;
		for(var mesh in this.meshes) {
			mesh = this.meshes[mesh];
			if(x >= mesh.bounds[0][0]-mesh.loci && x <= mesh.bounds[1][0]+mesh.loci &&
				y <= mesh.bounds[1][1] &&
				z >= mesh.bounds[0][2]-mesh.loci && z <= mesh.bounds[1][2]+mesh.loci)
				mesh.rayIntersection([x,y,z],[0,20,0],true,function(i,I,N) {
					if(!n || I[1] > y) {
						y = I[1];
						n = N;
						hit = mesh;
					}
				},Math.floor((t||0)*mesh.frameCount));
		}
		return hit?[y,n,hit]:null;
	},
	zAt: function(rayOrigin,rayDir,t) {
		var z, n, hit;
		for(var mesh in this.meshes) {
			mesh = this.meshes[mesh];
			mesh.rayIntersection(rayOrigin,rayDir,function(i,I,N) {
				var d = vec3_length(vec3_sub(I,rayOrigin));
				if(!n || d < z) {
					z = d;
					n = N;
					hit = mesh;
				}
			},Math.floor((t||0)*mesh.frameCount));
		}
		return hit?[z,n,hit]:null;
	},
	autoNormals: function() {
		for(var mesh in this.meshes)
			this.meshes[mesh].autoNormals();
	},
};

function G3DMesh(g3d,reader) {
	assert(this instanceof G3DMesh);
	this.g3d = g3d;
	this.name = reader.str64();
	this.textureFilename = null;
	this.frameCount = reader.uint32();  if(!this.frameCount) throw "no frames "+this.name+","+this.frameCount;
	this.vertexCount = reader.uint32();  if(!this.vertexCount) throw "no vertices "+this.name+","+this.vertexCount;
	this.indexCount = reader.uint32();  if(!this.indexCount) throw "no indices "+this.name+","+this.indexCount;
	if(this.indexCount%3) throw "bad number of indices "+this.name+","+this.indexCount;
	this.faceCount = this.indexCount/3;
	reader.ofs += 8*4;
	var properties = reader.uint32();
	this.teamColour = properties&1;
	this.twoSided = properties&2;
	this.texture = null;
	this.textures = reader.uint32();
	for(var t=0; t<5; t++) {
		if((1<<t)&this.textures) {
			var textureFilename = reader.str64();
			if(t==0)
				this.textureFilename = textureFilename;
		}
	}
	this.vnVbo = gl.createBuffer();
	gl.bindBuffer(gl.ARRAY_BUFFER,this.vnVbo);
	this.vnData = reader.float32(this.frameCount*this.vertexCount*3*2);
	this.bounds = [[100000,100000,100000],[-100000,-100000,-100000]];
	for(var f=0; f<this.frameCount; f++)
		for(var v=0; v<this.vertexCount; v++)
			for(var i=0; i<3; i++) { // bounds
				this.bounds[0][i] = Math.min(this.bounds[0][i],this.vnData[f*this.vertexCount+v*3+i]);
				this.bounds[1][i] = Math.max(this.bounds[1][i],this.vnData[f*this.vertexCount+v*3+i]);
			}
	this.boundingSphere = vec3_add(this.bounds[0],vec3_scale(vec3_sub(this.bounds[1],this.bounds[0]),0.5));
	this.boundingSphere.push(vec3_length(vec3_sub(this.bounds[1],this.bounds[0]))/2);
	gl.bufferData(gl.ARRAY_BUFFER,this.vnData,gl.STATIC_DRAW);
	if(this.textures) {
		this.texData = reader.float32(this.vertexCount*2);
		for(var i=1; i<this.vertexCount*2; i+=2)
			this.texData[i] = 1-this.texData[i];
		this.tVbo = gl.createBuffer();
		gl.bindBuffer(gl.ARRAY_BUFFER,this.tVbo);
		gl.bufferData(gl.ARRAY_BUFFER,this.texData,gl.STATIC_DRAW);
	}
	this.iVbo = gl.createBuffer();
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,this.iVbo);
	this.iData = new Uint16Array(this.indexCount);
	for(var i=0; i<this.indexCount; i++) {
		this.iData[i] = reader.uint32();
		assert(this.iData[i] >= 0 && this.iData[i] < this.vertexCount);
	}
	gl.bufferData(gl.ELEMENT_ARRAY_BUFFER,this.iData,gl.STATIC_DRAW);
	this.faceSpheres = new Float32Array(this.faceCount*4*this.frameCount);
	this.faceNormals = new Float32Array(this.faceCount*3*this.frameCount);
	var ofs = 0;
	for(var f=0; f<this.frameCount; f++) {
		for(var face=0; face<this.faceCount; face++) {
			var	i = face*3,
				A = f*this.vertexCount*3+this.iData[i]*3,
				B = f*this.vertexCount*3+this.iData[i+1]*3,
				C = f*this.vertexCount*3+this.iData[i+2]*3,
				a = [this.vnData[A],this.vnData[A+1],this.vnData[A+2]],
				b = [this.vnData[B],this.vnData[B+1],this.vnData[B+2]],
				c = [this.vnData[C],this.vnData[C+1],this.vnData[C+2]],
				u = vec3_sub(b,a),
				v = vec3_sub(c,a),
				n = vec3_cross(u,v),
				min = [], max = [];
			for(var j=0; j<3; j++) {
				min.push(Math.min(a[j],b[j],c[j]));
				max.push(Math.max(a[j],b[j],c[j]));
			}
			var centre = vec3_scale(vec3_sub(max,min),0.5),
				radius = Math.sqrt(vec3_dot(centre,centre));
			this.faceSpheres[f*this.faceCount*4+face*4+0] = min[0]+centre[0];
			this.faceSpheres[f*this.faceCount*4+face*4+1] = min[1]+centre[1];
			this.faceSpheres[f*this.faceCount*4+face*4+2] = min[2]+centre[2];
			this.faceSpheres[f*this.faceCount*4+face*4+3] = radius;
			this.faceNormals[f*this.faceCount*3+face*3+0] = n[0];
			this.faceNormals[f*this.faceCount*3+face*3+1] = n[1];
			this.faceNormals[f*this.faceCount*3+face*3+2] = n[2];
		}
	}
}
G3DMesh.prototype = {
	_drawSingle: function(program,untextured,op) {
		if(this.twoSided)
			gl.disable(gl.CULL_FACE);
		else
			gl.enable(gl.CULL_FACE);
		gl.bindTexture(gl.TEXTURE_2D,op==gl.LINES? programs.blankTex: untextured? programs.blankTex: this.texture);
		gl.bindBuffer(gl.ARRAY_BUFFER,this.vnVbo);
		gl.vertexAttribPointer(program.vertex,3,gl.FLOAT,false,3*4,0);			
		gl.vertexAttribPointer(program.normal,3,gl.FLOAT,false,3*4,this.vertexCount*3*4);
		gl.bindBuffer(gl.ARRAY_BUFFER,this.tVbo);
		gl.vertexAttribPointer(program.texCoord,2,gl.FLOAT,false,0,0);
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,this.iVbo);
		gl.drawElements(op,this.indexCount,gl.UNSIGNED_SHORT,0);
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,null);
	},
	_drawLerp: function(program,t,untextured,op) {
		var frame1 = Math.floor(t*this.frameCount),
			frame2 = (frame1+1)%this.frameCount,
			lerp = t*this.frameCount - frame1;
		gl.uniform1f(program.lerp,lerp);
		if(this.twoSided)
			gl.disable(gl.CULL_FACE);
		else
			gl.enable(gl.CULL_FACE);
		gl.bindTexture(gl.TEXTURE_2D,untextured? programs.blankTex: this.texture);
		gl.bindBuffer(gl.ARRAY_BUFFER,this.vnVbo);
		gl.vertexAttribPointer(program.normal1,3,gl.FLOAT,false,3*4,(frame1+this.frameCount)*this.vertexCount*3*4);
		gl.vertexAttribPointer(program.normal2,3,gl.FLOAT,false,3*4,(frame2+this.frameCount)*this.vertexCount*3*4);
		gl.vertexAttribPointer(program.vertex1,3,gl.FLOAT,false,3*4,frame1*this.vertexCount*3*4);
		gl.vertexAttribPointer(program.vertex2,3,gl.FLOAT,false,3*4,frame2*this.vertexCount*3*4);			
		gl.bindBuffer(gl.ARRAY_BUFFER,this.tVbo);
		gl.vertexAttribPointer(program.texCoord,2,gl.FLOAT,false,0,0);
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,this.iVbo);
		gl.drawElements(op,this.indexCount,gl.UNSIGNED_SHORT,0);
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,null);
	},
	_drawNormals: function(program,t) {
		if(!this.drawNormalsVbo) {
			this.drawNormalsVbo = gl.createBuffer();
			gl.bindBuffer(gl.ARRAY_BUFFER,this.drawNormalsVbo);
			var normalsData = new Float32Array(this.frameCount*this.vertexCount*3*2), p = 0;
			for(var f=0; f<this.frameCount; f++)
				for(var v=0; v<this.vertexCount; v++) {
					var n = [0,0,0];
					for(var i=0; i<3; i++) {
						normalsData[p*2+i] = this.vnData[p+i];
						n[i] = this.vnData[this.frameCount*this.vertexCount*3+p+i];
					}
					n = vec3_normalise(n);
					n = vec3_scale(n,0.25);
					for(var i=0; i<3; i++)
						normalsData[p*2+3+i] = this.vnData[p+i] + n[i];
					p += 3;
				}
			gl.bufferData(gl.ARRAY_BUFFER,normalsData,gl.STATIC_DRAW);
		}
		var frame1 = Math.floor(t*this.frameCount),
			frame2 = (frame1+1)%this.frameCount,
			lerp = t*this.frameCount - frame1;
		gl.uniform1f(program.lerp,lerp);
		gl.bindBuffer(gl.ARRAY_BUFFER,this.drawNormalsVbo);
		gl.vertexAttribPointer(program.vertex1,3,gl.FLOAT,false,3*4,frame1*this.vertexCount*3*4*2);
		gl.vertexAttribPointer(program.vertex2,3,gl.FLOAT,false,3*4,frame2*this.vertexCount*3*4*2);
		gl.drawArrays(gl.LINES,0,this.vertexCount*2);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);	
	},
	lineIntersection: function(lineOrigin,lineDir,lineSphere,intersects,frame) {
		if(!sphere_sphere_intersects(this.boundingSphere,lineSphere))
			return;
		for(var face=0; face<this.faceCount; face++) {
			if(!sphere_sphere_intersects(lineSphere,this.faceSpheres,frame*this.faceCount*4+face*4))
				continue;
			var	A = frame*this.vertexCount*3+this.iData[face*3]*3,
				B = frame*this.vertexCount*3+this.iData[face*3+1]*3,
				C = frame*this.vertexCount*3+this.iData[face*3+2]*3,
				a = [this.vnData[A],this.vnData[A+1],this.vnData[A+2]],
				b = [this.vnData[B],this.vnData[B+1],this.vnData[B+2]],
				c = [this.vnData[C],this.vnData[C+1],this.vnData[C+2]],
				n = [	this.faceNormals[frame*this.faceCount*3+face*3+0],
					this.faceNormals[frame*this.faceCount*3+face*3+1],
					this.faceNormals[frame*this.faceCount*3+face*3+2]],
				hit = triangle_ray_intersection(a,b,c,lineOrigin,lineDir,n,true);
			if(hit) intersects(hit[0],hit[1],hit[2]);
		}
	},
	sphereSweepIntersection: function(lineStart,lineStop,lineSphere,lineWidth,frame,callback,ignoreFace) {
		if(!sphere_sphere_intersects(this.boundingSphere,lineSphere))
			return;
		for(var face=0; face<this.faceCount; face++) {
			if(face==ignoreFace)
				continue;
			if(!sphere_sphere_intersects(lineSphere,this.faceSpheres,frame*this.faceCount*4+face*4))
				continue;
			var	A = frame*this.vertexCount*3+this.iData[face*3]*3,
				B = frame*this.vertexCount*3+this.iData[face*3+1]*3,
				C = frame*this.vertexCount*3+this.iData[face*3+2]*3,
				a = vec3(this.vnData,A),
				b = vec3(this.vnData,B),
				c = vec3(this.vnData,C),
				hit = triangle_sphere_sweep(c,b,a,lineStart,lineStop,lineWidth); //CCW->CW
			if(hit)
				callback(mesh,face,[hit[0],vec3_lerp(lineStart,lineStop,hit[0]),hit[1]]);
		}
	},
	rayIntersection: function(rayOrigin,rayDir,intersects,frame) {
		var vertices = this.vnData;
		for(var i=this.indexCount; i>0; ) {
			var	C = frame*this.vertexCount*3+this.iData[--i]*3,
				B = frame*this.vertexCount*3+this.iData[--i]*3,
				A = frame*this.vertexCount*3+this.iData[--i]*3,
				a = [vertices[A],vertices[A+1],vertices[A+2]],
				b = [vertices[B],vertices[B+1],vertices[B+2]],
				c = [vertices[C],vertices[C+1],vertices[C+2]],
				hit = triangle_ray_intersection(c,b,a,rayOrigin,rayDir);
			if(hit) {
				if(intersects)
					intersects(hit[0],hit[1],hit[2]);
				else
					return hit;
			}
		}
		return null;
	},
	autoNormals: function() {
		// explode-up unjoining shared vertices and giving each face a flat normal
		var vnData = new Float32Array(this.frameCount*this.indexCount*3*2); // 3 indices -> 3 components
		for(var f=0; f<this.frameCount; f++) {
			for(var i=0; i<this.indexCount; i+=3) {
				var	A = f*this.vertexCount*3+this.iData[i]*3,
					B = f*this.vertexCount*3+this.iData[i+1]*3,
					C = f*this.vertexCount*3+this.iData[i+2]*3,
					a = [this.vnData[A],this.vnData[A+1],this.vnData[A+2]],
					b = [this.vnData[B],this.vnData[B+1],this.vnData[B+2]],
					c = [this.vnData[C],this.vnData[C+1],this.vnData[C+2]],
					n = vec3_cross(vec3_sub(b,a),vec3_sub(c,a));
				for(var j=0; j<3; j++) {
					vnData[f*this.indexCount*3+i*3+j] = a[j];
					vnData[f*this.indexCount*3+i*3+3+j] = b[j];
					vnData[f*this.indexCount*3+i*3+6+j] = c[j];
					vnData[this.frameCount*this.indexCount*3+f*this.indexCount*3+i*3+j] = n[j];
					vnData[this.frameCount*this.indexCount*3+f*this.indexCount*3+i*3+3+j] = n[j];
					vnData[this.frameCount*this.indexCount*3+f*this.indexCount*3+i*3+6+j] = n[j];
				}
			}
		}
		if(this.textures) {
			var texData = new Float32Array(this.indexCount*2);
			for(var i=0; i<this.indexCount; i++) {
				texData[i*2] = this.texData[this.iData[i]*2];
				texData[i*2+1] = this.texData[this.iData[i]*2+1];
			}
			this.texData = texData;
			gl.bindBuffer(gl.ARRAY_BUFFER,this.tVbo);
			gl.bufferData(gl.ARRAY_BUFFER,this.texData,gl.STATIC_DRAW);
		}
		for(var i=0; i<this.indexCount; i++)
			this.iData[i] = i;
		this.vnData = vnData;
		this.vertexCount = this.indexCount;
		gl.bindBuffer(gl.ARRAY_BUFFER,this.vnVbo);
		gl.bufferData(gl.ARRAY_BUFFER,this.vnData,gl.STATIC_DRAW);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,this.iVbo);
		gl.bufferData(gl.ELEMENT_ARRAY_BUFFER,this.iData,gl.STATIC_DRAW);
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,null);
		if(this.drawNormalsVbo) {
			gl.deleteBuffer(this.drawNormalsVbo);
			this.drawNormalsVbo = null;
		}
	},
};
