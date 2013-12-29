/* https://github.com/williame/barebones.js project
This file contains various utility functions for loading textures, setting up
matrices, doing vector math and determining ray/triangle intersections.

* The capsule intersection functions are broken.  Ignore them.

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
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.*/

"use strict";

gl.activeTexture(gl.TEXTURE0);
gl.enable(gl.DEPTH_TEST);
gl.depthFunc(gl.LEQUAL);
gl.enable(gl.BLEND);
gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);
gl.enable(gl.CULL_FACE);
gl.frontFace(gl.CCW);
gl.clearColor(0.0,0.0,0.0,0.0);
gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);
var	anisotropic = gl.getExtension("EXT_texture_filter_anisotropic") ||
		gl.getExtension("MOZ_EXT_texture_filter_anisotropic") || 
		gl.getExtension("WEBKIT_EXT_texture_filter_anisotropic"),
	max_anisotropy = anisotropic? gl.getParameter(anisotropic.MAX_TEXTURE_MAX_ANISOTROPY_EXT): 0,
	anisotropy = max_anisotropy,
	_textures = [],
	OPAQUE = new Float32Array([1,1,1,1]);

function set_anisotropy(anisotropy) {
	if(!max_anisotropy) return;
	for(var tex in _textures) {
		tex = _textures[tex];
		gl.bindTexture(gl.TEXTURE_2D,tex);
		gl.texParameterf(gl.TEXTURE_2D,anisotropic.TEXTURE_MAX_ANISOTROPY_EXT,anisotropy);
	}
	window.anisotropy = anisotropy;
	gl.bindTexture(gl.TEXTURE_2D,null);
}

function createShader(str,type) {
	if(!createShader.shaders) createShader.shaders = [];
	var shader = createShader.shaders[str+type];
	if(!shader) {
		shader = gl.createShader(type);
		gl.shaderSource(shader,str);
		gl.compileShader(shader);
		if (!gl.getShaderParameter(shader,gl.COMPILE_STATUS)) {
			console.log("bad shader!",type,str);
			console.log("gl says:",gl.getShaderInfoLog(shader));
			fail("error compiling shader");
		}
		createShader.shaders[str+type] = shader;
	}
	return shader;
}

function createProgram(vstr,fstr) {
	if(!createProgram.programs) createProgram.programs = [];
	var program = createProgram.programs[vstr+fstr];
	if(!program) {
		program = gl.createProgram();
		var vshader = createShader(vstr,gl.VERTEX_SHADER);
		var fshader = createShader(fstr,gl.FRAGMENT_SHADER);
		gl.attachShader(program,vshader);
		gl.attachShader(program,fshader);
		gl.linkProgram(program);
		createProgram.programs[vstr+fstr] = program;
		var types = createProgram.uniformTypes;
		if(!types) {
			// https://gist.github.com/szimek/763999 webgl type constants
			types = createProgram.uniformTypes = {};
			types[gl.INT] = gl.uniform1i;
			types[gl.FLOAT] = gl.uniform1f;
			types[gl.SAMPLER_2D] = gl.uniform1i;
			types[gl.FLOAT_VEC2] = gl.uniform2fv;
			types[gl.FLOAT_VEC3] = gl.uniform3fv;
			types[gl.FLOAT_VEC4] = gl.uniform4fv;
			types[gl.FLOAT_MAT3] = gl.uniformMatrix3fv;
			types[gl.FLOAT_MAT4] = gl.uniformMatrix4fv;
		}
		program.uniforms = [];
		for(var i = gl.getProgramParameter(program,gl.ACTIVE_UNIFORMS); i-->0; ) {
			var info = gl.getActiveUniform(program,i);
			assert(!(info.name in program),"unsupported uniform name:",info.name);
			assert(info.type in types,"unsupported uniform type:",info.type,info.name);
			assert(info.size==1,"unsupported uniform array:",info.name);
			var location = gl.getUniformLocation(program,info.name);
			program[info.name] = location;
			program.uniforms.unshift([location,info.name,types[info.type]]);
		}
		program.attributes = {};
		for(var i=gl.getProgramParameter(program, gl.ACTIVE_ATTRIBUTES); i-->0; ) {
			var info = gl.getActiveAttrib(program,i);
			assert(!(info.name in program),"unsupported attribute name:",info.name);
			var location = gl.getAttribLocation(program,info.name);
			program[info.name] = location;
			program.attributes[info.name] = location;
		}
	}
	return program;
}

function Program(vertexShader,fragmentShader) {
	assert(typeof this === "undefined");
	var	init = Array.prototype.slice.call(arguments,0),
		program = null,
		owner = function() {
			var e = new Error();
			if(!e.stack) try { throw e; } catch(_) {}
			return e.stack.split("\n")[3].replace(/^\s+|\s+$/g,'');
		}();
	return function(cb,uniforms,self) {
		assert(typeof cb === "function");
		if(!program) { // create on demand
			program = createProgram.apply(window,init);
			init = null;
		}
		uniforms = uniforms || {};
		gl.useProgram(program);
		for(var i in program.uniforms) {
			var	uniform = program.uniforms[i],
				location = uniform[0],
				name = uniform[1],
				set = uniform[2];
			if(name in uniforms) {
				var value = uniforms[name];
				switch(set) {
				case gl.uniformMatrix4fv:
					assert(value.length == 4*4,name,"should be 4x4",value,owner);
					set.call(gl,location,false,value);
					break;
				case gl.uniformMatrix3fv:
					assert(value.length == 3*3,name,"should be 3x3",value,owner);
					set.call(gl,location,false,value);
					break;
				default:
					set.call(gl,location,value);
				}
			} else switch(name) {
			case "mvp":
				set.call(gl,i,false,mat4_multiply(uniforms.pMatrix,uniforms.mvMatrix));
				break;
			case "nMatrix":
				var nMatrix = mat4_transpose(mat4_inverse(uniforms.mvMatrix));
				if(set === gl.uniformMatrix3fv && nMatrix.length == 4*4)
					set.call(gl,location,false,mat4_mat3(nMatrix));
				else
					set.call(gl,location,false,nMatrix);
				break;
			case "texture":
				gl.uniform1i(location,0);
				break;
			case "aspectRatio":
				var viewport = gl.getParameter(gl.VIEWPORT);
				gl.uniform1f(location,viewport[1]/viewport[3]);
				break;
			default:
				var error = "uniform "+name+" not set";
				if(!program._errors) program._errors = {};
				if(!(error in program._errors)) { // suppress logging each and every time
					program._errors[error] = 1;
					console.log("WARNING!",owner,error);
				}
			}
		}
		if("texture" in program)
			gl.bindTexture(gl.TEXTURE_2D,uniforms.texture||programs.blankTex);
		for(var i in program.attributes)
			gl.enableVertexAttribArray(program.attributes[i]);
		var args = Array.prototype.slice.call(arguments,3);
		args.unshift(program);
		cb.apply(self||window,args);
		for(var i in program.attributes)
			gl.disableVertexAttribArray(program.attributes[i]);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);
		gl.bindTexture(gl.TEXTURE_2D,null);
		gl.useProgram(null);
	};
};

function createTexture(tex,width,height,data,nearestInterpolation) {
	tex = tex || gl.createTexture();
	gl.bindTexture(gl.TEXTURE_2D,tex);
	tex.width = width || data.width;
	tex.height = height || data.height;
	if(width != null)
		gl.texImage2D(gl.TEXTURE_2D,0,gl.RGBA,width,height,0,gl.RGBA,gl.UNSIGNED_BYTE,data || null);
	else
		gl.texImage2D(gl.TEXTURE_2D,0,gl.RGBA,gl.RGBA,gl.UNSIGNED_BYTE,data);
	if(!nearestInterpolation && anisotropic) {
		gl.texParameterf(gl.TEXTURE_2D,anisotropic.TEXTURE_MAX_ANISOTROPY_EXT,anisotropy);
		_textures.push(tex);
	}
	gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_WRAP_S,gl.CLAMP_TO_EDGE);
	gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_WRAP_T,gl.CLAMP_TO_EDGE);
	gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MAG_FILTER,nearestInterpolation?gl.NEAREST:gl.LINEAR);
	if(!nearestInterpolation && !(tex.width&(tex.width-1)) && !(tex.height&(tex.height-1))) { //pow2
		gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MIN_FILTER,gl.LINEAR_MIPMAP_LINEAR);
		gl.generateMipmap(gl.TEXTURE_2D);
	} else
		gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MIN_FILTER,nearestInterpolation?gl.NEAREST:gl.LINEAR);
	gl.bindTexture(gl.TEXTURE_2D,null);
	return tex;
}

function Sphere(iterations) {
	if(!Sphere.spheres)
		Sphere.spheres = [];
	if(iterations in Sphere.spheres)
		return Sphere.spheres[iterations];
	var 	vertices = [],
		vIndex = {},
		indices = [],
		addTriangle = function(a,b,c) {
			indices.push(a);
			indices.push(b);
			indices.push(c);
		},
		addVertex = function(v) {
			if(!(v in vIndex)) {
				vIndex[v] = vertices.length/3;
				vertices.push(v[0]);
				vertices.push(v[1]);
				vertices.push(v[2]);
			}
			return vIndex[v];
		},
		halfway = function(a,b) {
			a = [vertices[a*3],vertices[a*3+1],vertices[a*3+2]];
			b = [vertices[b*3],vertices[b*3+1],vertices[b*3+2]];
			return addVertex(vec3_normalise(vec3_add(a,vec3_scale(vec3_sub(b,a),0.5))));
		},
		bisect = function(a,b,c,iteration) {
			var	ab = halfway(a,b),
				ac = halfway(a,c),
				bc = halfway(b,c),
				func = iteration==iterations? addTriangle: bisect;
			func(a,ab,ac,iteration+1);
			func(b,bc,ab,iteration+1);
			func(c,ac,bc,iteration+1);
			func(ab,bc,ac,iteration+1);
		},
		top = addVertex([0,-1,0]),
		bottom = addVertex([0,1,0]),
		leftFront = addVertex(vec3_normalise([-1,0,-1])),
		leftBack = addVertex(vec3_normalise([-1,0,1])),
		rightFront = addVertex(vec3_normalise([1,0,-1])),
		rightBack = addVertex(vec3_normalise([1,0,1]));
	bisect(leftFront,top,rightFront,0);
	bisect(rightFront,bottom,leftFront,0);
	bisect(leftBack,top,leftFront,0);
	bisect(bottom,leftBack,leftFront,0);
	bisect(rightFront,top,rightBack,0);
	bisect(bottom,rightFront,rightBack,0);
	bisect(rightBack,top,leftBack,0);
	bisect(bottom,rightBack,leftBack,0);
	if(!Sphere.program)
		Sphere.program = Program(
			"precision mediump float;\n"+
			"uniform mat4 mvMatrix, pMatrix;\n"+
			"uniform mat3 nMatrix;\n"+
			"attribute vec3 vertex;\n"+
			"varying vec3 lighting;\n"+
			"void main() {\n"+
			"	vec3 normal = vertex;\n"+
			"	gl_Position = pMatrix * mvMatrix * vec4(vertex,1.0);\n"+
			"	vec3 ambientLight = vec3(0.6,0.6,0.6);\n"+
			"	vec3 lightColour = vec3(0.8,0.9,0.75);\n"+
			"	vec3 lightDir = vec3(0.85,0.8,0.75);\n"+
			"	vec3 transformed = normalize(nMatrix * normal);\n"+
			"	float directional = clamp(dot(transformed,lightDir),0.0,1.0);\n"+
			"	lighting = ambientLight + (lightColour*directional);\n"+
			"}",
			"precision mediump float;\n"+
			"uniform vec4 colour;\n"+
			"varying vec3 lighting;\n"+
			"void main() {\n"+
			"	gl_FragColor = vec4(lighting*colour.rgb,colour.a);\n"+
			"}");
	var self = {
		vVbo: gl.createBuffer(),
		iVbo: gl.createBuffer(),
		indexCount: indices.length,
		draw: function(uniforms,sphere,colour,invert,drawOp) {
			var frontFace = gl.getParameter(gl.FRONT_FACE);
			gl.frontFace(invert? gl.CCW: gl.CW);
			var mvMatrix = uniforms.mvMatrix;
			mvMatrix = mat4_multiply(mvMatrix,mat4_translation(sphere));
			mvMatrix = mat4_multiply(mvMatrix,mat4_scale(sphere[3]));
			Sphere.program(self.doDraw,{
					__proto__: uniforms,
					mvMatrix: mvMatrix,
					colour: colour || uniforms.colour || OPAQUE,
			},self,drawOp);
			gl.frontFace(frontFace);
		},
		doDraw: function(program,drawOp) {
			gl.bindBuffer(gl.ARRAY_BUFFER,self.vVbo);
			gl.vertexAttribPointer(program.vertex,3,gl.FLOAT,false,3*4,0);
			gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,self.iVbo);
			gl.drawElements(drawOp||gl.TRIANGLES,self.indexCount,gl.UNSIGNED_SHORT,0);
		},
	};
	gl.bindBuffer(gl.ARRAY_BUFFER,self.vVbo);
	gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(vertices),gl.STATIC_DRAW);
	gl.bindBuffer(gl.ARRAY_BUFFER,null);
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,self.iVbo);
	gl.bufferData(gl.ELEMENT_ARRAY_BUFFER,new Uint16Array(indices),gl.STATIC_DRAW);
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,null);
	Sphere.spheres[iterations] = self;
	return self;
}

var programs = {
	blankTex: gl.createTexture(),
	solidFill: Program(
		"precision mediump float;\n"+
		"attribute vec3 vertex;\n"+
		"uniform mat4 mvMatrix, pMatrix;\n"+
		"void main() {\n"+
		"	gl_Position = pMatrix * mvMatrix * vec4(vertex,1.0);\n"+
		"}\n",
		"precision mediump float;\n"+
		"uniform vec4 colour;\n"+
		"void main() {\n"+
		"	gl_FragColor = colour;\n"+
		"}\n"),
	solidFillLerp: Program(
		"precision mediump float;\n"+
		"attribute vec3 vertex1, vertex2;\n"+
		"uniform float lerp;\n"+
		"uniform mat4 mvMatrix, pMatrix;\n"+
		"void main() {\n"+
		"	vec3 vertex = mix(vertex1,vertex2,lerp);\n"+
		"	gl_Position = pMatrix * mvMatrix * vec4(vertex,1.0);\n"+
		"}\n",
		"precision mediump float;\n"+
		"uniform vec4 colour;\n"+
		"void main() {\n"+
		"	gl_FragColor = colour;\n"+
		"}\n"),
	standard: Program(
		"precision mediump float;\n"+
		"varying vec2 texel;\n"+
		"varying lowp vec3 lighting;\n"+
		"attribute vec3 vertex;\n"+
		"attribute vec3 normal;\n"+
		"attribute vec2 texCoord;\n"+
		"uniform mat4 mvMatrix, pMatrix;\n"+
		"uniform mat3 nMatrix;\n"+
		"uniform lowp vec3 lightDir, ambientLight, lightColour;\n"+
		"void main() {\n"+
		"	vec3 transformed = normalize(nMatrix*normal);\n"+
		"	float directional = clamp(dot(transformed,lightDir),0.0,1.0);\n"+
		"	lighting = ambientLight + (lightColour*directional);\n"+
		"	texel = texCoord;\n"+
		"	gl_Position = pMatrix * mvMatrix * vec4(vertex,1.0);\n"+
		"}\n",
		"precision mediump float;\n"+
		"varying vec2 texel;\n"+
		"varying lowp vec3 lighting;\n"+
		"uniform sampler2D texture;\n"+
		"uniform lowp vec4 colour;\n"+
		"uniform float fogDensity;\n"+
		"uniform lowp vec4 fogColour;\n"+
		"const float LOG2 = 1.442695;\n"+
		"void main() {\n"+
		"	float z = gl_FragCoord.z / gl_FragCoord.w;\n"+
		"	float fogFactor = exp2(-fogDensity*fogDensity*z*z*LOG2);\n"+
		"	fogFactor = clamp(fogFactor,0.0,1.0);\n"+
		"	vec4 fragColour = texture2D(texture,texel) * colour;\n"+
		"	if(fragColour.a < 0.1) discard;\n"+
		"	fragColour.rgb *= lighting;\n"+
		"	gl_FragColor = mix(fogColour,fragColour,fogFactor);\n"+
		"}\n"),
	standardLerp: Program(
		"precision mediump float;\n"+
		"varying vec2 texel;\n"+
		"varying lowp vec3 normal, eye, lightDir;\n"+
		"uniform float lerp;\n"+
		"attribute vec3 vertex1, vertex2;\n"+
		"attribute vec3 normal1, normal2;\n"+
		"attribute vec2 texCoord;\n"+
		"uniform mat4 mvMatrix, pMatrix;\n"+
		"uniform mat3 nMatrix;\n"+
		"uniform lowp vec3 lightPos;\n"+
		"void main() {\n"+
		"	normal = normalize(mix(normal1,normal2,lerp));\n"+
		"	vec3 vertex = mix(vertex1,vertex2,lerp);\n"+
		"	vec4 pos = mvMatrix * vec4(vertex,1.0);\n"+
		"	eye = -pos.xyz;\n"+
		"	lightDir = (mvMatrix * vec4(lightPos,1.0)).xyz + eye;\n"+
		"	gl_Position = pMatrix * pos;\n"+
		"	texel = texCoord;\n"+
		"}\n",
		"precision mediump float;\n"+
		"varying vec2 texel;\n"+
		"varying lowp vec3 normal, eye, lightDir;\n"+
		"uniform lowp vec4 ambientLight, diffuseLight, specularLight;\n"+
		"uniform float shininess;\n"+
		"uniform sampler2D texture;\n"+
		"uniform lowp vec4 colour;\n"+
		"uniform float fogDensity;\n"+
		"uniform lowp vec4 fogColour;\n"+
		"const float LOG2 = 1.442695;\n"+
		"void main() {\n"+
		"	float z = gl_FragCoord.z / gl_FragCoord.w;\n"+
		"	float fogFactor = exp2(-fogDensity*fogDensity*z*z*LOG2);\n"+
		"	fogFactor = clamp(fogFactor,0.0,1.0);\n"+
		"	vec4 fragColour = texture2D(texture,texel) * colour;\n"+
		"	if(fragColour.a < 0.1) discard;\n"+
		"	vec4 spec = vec4(0.0);\n"+
		"	vec3 n = normalize(normal);\n"+
		"	vec3 l = normalize(lightDir);\n"+
		"	vec3 e = normalize(eye);\n"+
		"	float intensity = max(dot(n,l), 0.0);\n"+
		"	if (intensity > 0.0) {\n"+
		"		vec3 h = normalize(l + e);\n"+
		"		float intSpec = max(dot(h,n), 0.0);\n"+
		"		spec = specularLight * pow(intSpec, shininess);\n"+
		"	}\n"+
		"	fragColour *= max(intensity * diffuseLight + spec, ambientLight);\n"+
		"	gl_FragColor = mix(fogColour,fragColour,fogFactor);\n"+
		"}\n"),
};

if(programs) { // had problems using texture generated from buffer with ATI cards; so force load of an white png
	var image = new Image();
	image.onerror = fail;
	image.onload = function() {
		createTexture(programs.blankTex,null,null,image);
	};
	image.src = "data/opaque.png";
}

function emitCube(blf,trb,array,ofs) {
	ofs = ofs || 0;
	var	left = blf[0], right = trb[0],
		bottom = blf[1], top = trb[1],
		front = blf[2], back = trb[2],
		tlb = [left,top,back],
		trf = [right,top,front],
		tlf = [left,top,front],
		brb = [right,bottom,back],
		blb = [left,bottom,back],
		brf = [right,bottom,front],
		emit = function(vec3) {
			array[ofs++] = vec3[0];
			array[ofs++] = vec3[1];
			array[ofs++] = vec3[2];
		},
		quad = function(normal,a,b,c,d) {
			emit(a); emit(normal);    
			emit(b); emit(normal);
			emit(c); emit(normal);
			emit(a); emit(normal);
			emit(c); emit(normal);
			emit(d); emit(normal);
		};
	quad([1,0,0],blb,tlb,tlf,blf); // left
	quad([-1,0,0],brf,trf,trb,brb); // right
	quad([0,0,-1],blf,tlf,trf,brf); // front
	quad([0,0,1],brb,trb,tlb,blb); // back
	quad([0,-1,0],tlb,trb,trf,tlf); // up
	quad([0,1,0],brb,blb,blf,brf); // down
}

function createCube(blf,trb) {
	var array = new Float32Array(createCube.numVertices);
	emitCube(blf,trb,array,0);
	return array;
}
createCube.numVertices = 6*6; // six faces, each two triangles

function Square() {
	assert(window !== this);
	this.vbo = gl.createBuffer();
	this.buf = new Float32Array(4*2*3*2);
	this.dirty = false;
}
Square.prototype = {
	set: function(ax,az,bx,bz,y) {
		var ofs = 0;
		ofs = this._emit(ax,bz,bx,bz,y,0,-1,0,ofs);
		ofs = this._emit(bx,bz,bx,az,y,0,-1,0,ofs);
		ofs = this._emit(bx,az,ax,az,y,0,-1,0,ofs);
		ofs = this._emit(ax,az,ax,bz,y,0,-1,0,ofs);
		assert(ofs == this.buf.length);
		this.dirty = true;
	},
	_emit: function(ax,az,bx,bz,y,nx,ny,nz,ofs) {
		var buf = this.buf;
		buf[ofs++] = ax; buf[ofs++] = y;  buf[ofs++] = az;
		buf[ofs++] = nx; buf[ofs++] = ny; buf[ofs++] = nz;
		buf[ofs++] = bx; buf[ofs++] = y;  buf[ofs++] = bz;
		buf[ofs++] = nx; buf[ofs++] = ny; buf[ofs++] = nz;
		return ofs;
	},
	draw: function(uniforms,colour) {
		programs.standard(this._draw,{ __proto__: uniforms, colour:colour, },this);
	},
	_draw: function(program) {
		gl.bindBuffer(gl.ARRAY_BUFFER,this.vbo);
		if(this.dirty) {
			gl.bufferData(gl.ARRAY_BUFFER,this.buf,gl.STATIC_DRAW);
			this.dirty = false;
		}
		gl.vertexAttribPointer(program.vertex,3,gl.FLOAT,false,6*4,0);
		if("normal" in program)
			gl.vertexAttribPointer(program.normal,3,gl.FLOAT,false,6*4,3*4);
		if("texCoord" in program)
			gl.vertexAttribPointer(program.texCoord,2,gl.FLOAT,false,0,0); // noise
		gl.drawArrays(gl.LINES,0,4*2);
	},
};

function BlockBuffer(width,refFactory,arrayFactory) {
	assert(this instanceof BlockBuffer);
	this.width = width;
	this.refFactory = refFactory;
	this.arrayFactory = arrayFactory || Float32Array;
	this.len = 0;
	this.refs = [];
	this.size = 32;
	this.buf = new this.arrayFactory(this.offset(this.size));
	this.dirty = true;
	this.ready = false;
}
BlockBuffer.prototype = {
	add: function(ctx,params,ref) {
		if(this.refs.length == this.size) {
			this.size *= 2;
			var old = this.buf;
			this.buf = new this.arrayFactory(this.offset(this.size));
			if(this.buf.set)
				this.buf.set(old);
			else
				for(var i=this.offset(this.refs.length); i-->0; )
					this.buf[i] = old[i];
		}
		if(ref) {
			if(ref.setRefIdx) {
				ref.setRefIdx(this,ctx,this.refs.length);
			} else {
				assert(!ref.idx);
				ref.idx = this.refs.length;
			}
		} else {
			ref = new this.refFactory(this,ctx,this.refs.length);
		}
		this.refs.push(ref);
		if(ref.set)
			ref.set(params);
		this._added(ref);
		return ref;
	},
	_added: function() {
		this.len = this.refs.length;
	},
	offset: function(idx) {
		return idx*this.width;
	},
	toBuffer: function() {
		var size = this.offset(this.refs.length);
		var buf = new this.arrayFactory(size);
		for(var i=size; i-->0; )
			buf[i] = this.buf[i];
		return buf;
	},
	clear: function() {
		this.refs.length = 0;
		this.dirty = true;
	},
	length: function() {
		return this.refs.length;
	},
	isEmpty: function() {
		return !this.refs.length;
	},
	move: function(from,to,array) {
		var sz = this.width;
		from *= sz;
		to *= sz;
		for(var i=0; i<sz; i++,from++,to++)
			array[to] = array[from];
	},
	remove: function(ref) {
		assert(ref === this.refs[ref.idx]);
		var tail = this.refs[this.refs.length-1];
		this.refs[ref.idx] = tail;
		tail.idx = ref.idx;
		this.move(this.refs.length-1,ref.idx,this.buf);
		this.refs.pop();
		this.dirty = true;
	},
};

function ObjectBuffer(refFactory) {
	BlockBuffer.call(this,1,refFactory,Array);
}
ObjectBuffer.prototype = {
	__proto__: BlockBuffer.prototype,
	_added: function(ref) {
		this.buf[ref.idx] = ref;
		BlockBuffer.prototype._added.call(this,ref);
	},
};

function VertexBuffer(width,refFactory,program,arrayFactory) {
	BlockBuffer.call(this,width,refFactory,arrayFactory);
	this.program = program || programs.standard;
	this.vbo = gl.createBuffer();
}
VertexBuffer.prototype = {
	__proto__: BlockBuffer.prototype,
	colour: OPAQUE,
	_added: function() {},
	draw: function(uniforms,program) {
		this.update();
		if(!this.ready || !this.len) return;
		gl.bindBuffer(gl.ARRAY_BUFFER,this.vbo);
		var args = Array.prototype.slice.call(arguments,2);
		args.unshift(this);
		args.unshift({ __proto__: uniforms, colour: vec4_multiply(this.colour,uniforms.colour||OPAQUE), });
		args.unshift(this.doDraw);
		(program||this.program).apply(null,args);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);
	},
	update: function() {
		if(this.dirty) {
			this.len = this.refs.length;
			gl.bindBuffer(gl.ARRAY_BUFFER,this.vbo);
			gl.bufferData(gl.ARRAY_BUFFER,this.buf.subarray(0,this.offset(this.len)),gl.STATIC_DRAW);
			gl.bindBuffer(gl.ARRAY_BUFFER,null);
			this.ready = true;
			this.dirty = false;
		}
	},
};	

function Cubes(colour) {
	VertexBuffer.call(this,createCube.numVertices*6,CubeRef);
	this.colour = colour;
}
Cubes.prototype = {
	__proto__: VertexBuffer.prototype,
	doDraw: function(program,op) {
		gl.vertexAttribPointer(program.vertex,3,gl.FLOAT,false,6*4,0);
		if("normal" in program)
			gl.vertexAttribPointer(program.normal,3,gl.FLOAT,false,6*4,3*4);
		if("texCoord" in program)
			gl.vertexAttribPointer(program.texCoord,2,gl.FLOAT,false,0,0); // noise
		gl.drawArrays(op||gl.TRIANGLES,0,this.len*createCube.numVertices);		
	},
};

function CubeRef(cubes,ctx,idx) {
	assert(this instanceof CubeRef);
	this.cubes = cubes;
	this.ctx = ctx;
	this.idx = idx;
}
CubeRef.prototype = {
	set: function(bounds) {
		assert(this === this.cubes.refs[this.idx]);
		emitCube(bounds[0],
			bounds[1],
			this.cubes.buf,
			this.cubes.offset(this.idx));
		this.cubes.dirty = true;
	},
	remove: function() {
		this.cubes.remove(this);
	},
};

function Line(lines,ctx,idx) {
	this.lines = lines;
	this.ctx = ctx;
	this.idx = idx;
}
Line.prototype = {
	set: function(line) {
		var lines = this.lines, ofs = lines.offset(this.idx), buf = lines.buf;
		buf[ofs++] = line[0][0];
		buf[ofs++] = line[0][1];
		buf[ofs++] = line[0][2];
		buf[ofs++] = line[1][0];
		buf[ofs++] = line[1][1];
		buf[ofs++] = line[1][2];
		lines.dirty = true;
	},
	remove: function() {
		this.lines.remove(this);
	},
};

function Lines(colour,lineWidth) {
	VertexBuffer.call(this,6,Line);
	this.colour = colour;
	this.lineWidth = lineWidth || 1;
}
Lines.prototype = {
	__proto__: VertexBuffer.prototype,
	draw: function() {
		if(this.dirty)
			this.update();
		VertexBuffer.prototype.draw.apply(this,arguments);
	},
	doDraw: function(program) {
		gl.lineWidth(this.lineWidth);
		gl.vertexAttribPointer(program.vertex,3,gl.FLOAT,false,3*4,0);
		if("normal" in program)
			gl.vertexAttribPointer(program.normal,3,gl.FLOAT,false,3*4,0); // noise
		if("texCoord" in program)
			gl.vertexAttribPointer(program.texCoord,2,gl.FLOAT,false,0,0); // noise
		gl.drawArrays(gl.LINES,0,this.len*2);
	},
};

function TextureAtlas(w,h,texture,textureFilename) {
	assert(this instanceof TextureAtlas);
	this.texture = texture;
	if(textureFilename) {
		var self = this;
		loadFile("image",textureFilename,function(texture) { self.texture = texture; });
	}
	this.w = w;
	this.h = h;
	this.rects = new Array(w*h);
	var	sx = 1/w, sy = 1/h,
		xmargin = 0.01, ymargin = 0.01; //### TODO scale properly by input texture size
	for(var y=0; y<h; y++)
		for(var x=0; x<w; x++)
			this.rects[this.getTextureIdx(x,y)] = [
				[x*sx+xmargin,y*sy+ymargin],[x*sx+sx-xmargin,y*sy+ymargin],
				[x*sx+sx-xmargin,y*sy+sy-ymargin],[x*sx+xmargin,y*sy+sy-ymargin]];
}
TextureAtlas.prototype = {
	getTextureIdx: function(x,y) {
		return y*this.w+x;
	},
	getRandomIdx: function() {
		return this.getTextureIdx(Math.floor(Math.random()*this.w),Math.floor(Math.random()*this.h));
	},
	getTextureQuad: function(idx) {
		return this.rects[idx];
	},
};

function Quad(quads,ctx,idx) {
	this.quads = quads;
	this.ctx = ctx;
	this.idx = idx;
}
Quad.prototype = {
	set: function(pts) {
		if(!pts) return;
		var 	quads = this.quads,
			tx = quads.texture,
			ofs = quads.offset(this.idx), 
			buf = quads.buf,
			normal = triangle_normal(pts[0],pts[1],pts[2]),
			emitVec = function(vec,stop,start) {
				stop = stop||vec.length;
				for(var i=start||0; i<stop; )
					buf[ofs++] = vec[i++];
			},
			tile = pts.length == 5?
				quads.textureAtlas.getTextureQuad(pts.pop()):
				null,
			emit = tile? 
				function(idx) { emitVec(pts[idx],3); emitVec(normal); emitVec(tile[idx]); }:
				tx? function(idx) { emitVec(pts[idx],3); emitVec(normal); emit(pts[idx],5,3); }:
				function(idx) { emitVec(pts[idx],3); emitVec(normal); };
		for(var pt in pts)
			assert(pts[pt].length == (tx && !tile? 5: 3),pt,pts);
		emit(0);    
		emit(1);
		emit(2);
		if(pts.length == 4) {
			emit(2);
			emit(3);
			emit(0);
		} else {
			assert(pts.length == 6);
			normal = triangle_normal(pts[3],pts[4],pts[5]);
			emit(3);
			emit(4);
			emit(5);		
		}
		quads.dirty = true;
	},
	remove: function() {
		this.quads.remove(this);
	},
};

function Quads(colour,texture) {
	VertexBuffer.call(this,6*(texture?8:6),Quad);
	this.colour = colour;
	if(texture instanceof TextureAtlas) {
		this.texture = null;
		this.textureAtlas = texture;
	} else {
		this.texture = texture;
		this.textureAtlas = null;
	}
}
Quads.prototype = {
	__proto__: VertexBuffer.prototype,
	draw: function(uniforms,program,op) {
		this.update();
		VertexBuffer.prototype.draw.call(this,{
			__proto__: uniforms,
			tex: this.textureAtlas? this.textureAtlas.texture: this.texture,
		},program,op||gl.TRIANGLES);
	},
	doDraw: function(program,op) {
		var	tex = this.texture||this.textureAtlas,
			stride = 4*(tex?8:6);
		gl.vertexAttribPointer(program.vertex,3,gl.FLOAT,false,stride,0);
		if("normal" in program)
			gl.vertexAttribPointer(program.normal,3,gl.FLOAT,false,stride,3*4);
		if("texCoord" in program)
			gl.vertexAttribPointer(program.texCoord,2,gl.FLOAT,false,stride,tex?6*4:0);
		gl.drawArrays(op,0,this.len*6);
	},
};

