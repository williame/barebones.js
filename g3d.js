
function G3D(filename) {
	var g3d = this;
	this.filename = filename;
	this.meshes = [];
	this.texture_filenames = [];
	this.program = null;
	this.ready = false;
	this._file_loaded = function(arrayBuffer) {
		var pow2 = new Float32Array(24), // massive speedup if precomputed
			one_over_pow2 = new Float32Array(24),
			reader = {
			array: arrayBuffer,
			buffer: new Uint8Array(arrayBuffer),
			ofs: 0,
			_read: function(Type,num_elements) {
				if(!num_elements) {
					var ret = 0;
					for(var i=0; i<Type.BYTES_PER_ELEMENT; i++)
						ret |= this.buffer[this.ofs++] << (i*8);
					return ret;
				}
				var raw = new Type(num_elements),
					stop = this.ofs + raw.byteLength;
				raw.set(this.buffer.subarray(this.ofs,stop));
				this.ofs = stop;
				return raw;
			},
			uint8: function(len) { return this._read(Uint8Array,len); },
			uint16: function(len) { return this._read(Uint16Array,len); },
			uint32: function(len) { return this._read(Uint32Array,len); },
			str64: function() {
				var s = "";
				for(var i=0; i<64; i++) {
					var b = this.buffer[this.ofs++];
					if(b) s += String.fromCharCode(b);
				}
				return s;
			},
			float32: function(len) {
				// do our own unpacking (http://www.terrybutler.co.uk/downloads/web/webgl-md2.htm adapted, speeded up 1000x)
				len = len || 1;
				var as_float = new Float32Array(len);
				for(var j=0; j<len; j++) {
					var value = this.uint32();
					var sign = (value >> 31) & 0x1;
					var nonZero = false;
					var mantissa = 0;
					var exponent = -127;
					// Mantissa
					for(var i = 22; i > -1; i--)
						if((value >> i & 0x1) == 1) {
							mantissa += one_over_pow2[23-i];
							nonZero = true;
						}	
					if(nonZero) mantissa += 1;		
					// Exponent
					for(var i = 30; i > 22; i--)
						if((value >> i & 0x1) == 1)
							exponent += pow2[i-23];
					var total = Math.pow(2, exponent) * mantissa;		
					if(sign) total = -total;		
					as_float[j] = total;
				}
				if(len>1) return as_float;
				return as_float[0];
			},
		};
		for(var i=0; i<pow2.length; i++) {
			pow2[i] = Math.pow(2,i);
			one_over_pow2[i] = 1 / pow2[i];
		}
		console.log("loaded G3D",g3d.filename,arrayBuffer.byteLength,"bytes");
		if(reader.uint32()>>24 != 4) throw "unsupported G3D version";
		var mesh_count = reader.uint16();
		if(!mesh_count) throw "has no meshes";
		if(reader.uint8()) throw "is not a mtMorphMesh";
		for(var i=0; i<mesh_count; i++) {
			var mesh = new G3DMesh(reader);
			g3d.meshes.push(mesh);
			// work out what textures we have to load
			if(mesh.texture_filename) {
				if(!g3d.texture_filenames[mesh.texture_filename])
					g3d.texture_filenames[mesh.texture_filename] = [];
				g3d.texture_filenames[mesh.texture_filename].push(mesh);
			}
		}
		if(reader.ofs != arrayBuffer.byteLength)
			throw "not all bytes consumed by G3D loader!";
		mesh.ready = (0 == g3d.texture_filenames.length);
		for(var texture in g3d.texture_filenames)
			var _ = function(filename,meshes) {
				filename = g3d.filename.substring(0,g3d.filename.lastIndexOf("/")+1) + filename;
				load_file("image",filename,function(tex) {
					for(var mesh in meshes)
						meshes[mesh].texture = tex;
					g3d.texture_filenames = g3d.texture_filenames.slice(
						g3d.texture_filenames.indexOf(filename),1);
					g3d.ready = (0 == g3d.texture_filenames.length);
				});
			}(texture,g3d.texture_filenames[texture]);
	};
	this.draw = function(t,pMatrix,mvMatrix,nMatrix) {
		if(!g3d.ready) return;
		if(!g3d.program) {
			g3d.program = createProgram(
				"precision mediump float;\n"+
				"varying vec3 lighting;\n"+
				"varying vec2 texel;\n"+
				"attribute vec3 vertex0, vertex1;\n"+
				"attribute vec3 normal0, normal1;\n"+
				"attribute vec2 texCoord;\n"+
				"uniform float lerp;\n"+
				"uniform mat4 mvMatrix, pMatrix, nMatrix;\n"+
				"void main() {\n"+
				"	texel = vec2(texCoord.x,1.0-texCoord.y);\n"+
				"	vec3 normal = mix(normal0,normal1,lerp);\n"+
				"	vec3 vertex = mix(vertex0,vertex1,lerp);\n"+
				"	gl_Position = pMatrix * mvMatrix * vec4(vertex,1.0);\n"+
				"	vec3 ambientLight = vec3(0.6,0.6,0.6);\n"+
				"	vec3 lightColour = vec3(0.8,0.9,0.75);\n"+
				"	vec3 lightDir = vec3(0.85,0.8,0.75);\n"+
				"	vec3 transformed = normalize(nMatrix * vec4(normal,1.0)).xyz;\n"+
				"	float directional = clamp(dot(transformed,lightDir),0.0,1.0);\n"+
				"	lighting = ambientLight + (lightColour*directional);\n"+
				"}\n",
				"precision mediump float;\n"+
				"varying vec3 lighting;\n"+
				"varying vec2 texel;\n"+
				"uniform sampler2D texture;\n"+
				"uniform vec4 teamColour;\n"+
				"void main() {\n"+
				"	vec4 tex = texture2D(texture,texel);\n"+
				"	if(0.0 == tex.a) tex = teamColour;\n"+
				"	if(0.0 == tex.a) discard;\n"+
				"	gl_FragColor = vec4(tex.rgb*lighting,tex.a);\n"+
				"}");
			g3d.program.vertex0 = gl.getAttribLocation(g3d.program,"vertex0");
			g3d.program.vertex1 = gl.getAttribLocation(g3d.program,"vertex1");
			g3d.program.normal0 = gl.getAttribLocation(g3d.program,"normal0");
			g3d.program.normal1 = gl.getAttribLocation(g3d.program,"normal1");
			g3d.program.texCoord = gl.getAttribLocation(g3d.program,"texCoord");
			g3d.program.lerp = gl.getUniformLocation(g3d.program,"lerp");
			g3d.program.mvMatrix = gl.getUniformLocation(g3d.program,"mvMatrix");
			g3d.program.pMatrix = gl.getUniformLocation(g3d.program,"pMatrix");
			g3d.program.nMatrix = gl.getUniformLocation(g3d.program,"nMatrix");
			g3d.program.teamColour = gl.getUniformLocation(g3d.program,"teamColour");
			g3d.program.texture = gl.getUniformLocation(g3d.program,"texture");
		}
		gl.useProgram(g3d.program);
		gl.uniformMatrix4fv(g3d.program.pMatrix,false,pMatrix);
		gl.uniformMatrix4fv(g3d.program.mvMatrix,false,mvMatrix);
		gl.uniformMatrix4fv(g3d.program.nMatrix,false,nMatrix);
		gl.activeTexture(gl.TEXTURE0);
		gl.uniform1i(g3d.program.texture,0);
		t = Math.max(0,Math.min(t,1))
		for(var i in g3d.meshes)
			g3d.meshes[i].draw(g3d.program,t);
		gl.useProgram(null);
	};
	load_file("ArrayBuffer",this.filename,this._file_loaded);
};

function G3DMesh(reader) {
	var mesh = this;
	this.name = reader.str64();
	this.texture_filename = null;
	this.frame_count = reader.uint32();  if(!this.frame_count) throw "no frames "+this.name+","+this.frame_count;
	this.vertex_count = reader.uint32();  if(!this.vertex_count) throw "no vertices "+this.name+","+this.vertex_count;
	this.index_count = reader.uint32();  if(!this.index_count) throw "no indices "+this.name+","+this.index_count;
	if(this.index_count%3) throw "bad number of indices "+this.name+","+this.index_count;
	reader.ofs += 8*4;
	this.properties = reader.uint32();
	this.texture = null;
	this.textures = reader.uint32();
	for(var t=0; t<5; t++) {
		if((1<<t)&this.textures) {
			var texture_filename = reader.str64();
			if(t==0)
				this.texture_filename = texture_filename=="war_kite.tga"?"test.png":texture_filename;
		}
	}
	this.vn_vbo = gl.createBuffer();
	gl.bindBuffer(gl.ARRAY_BUFFER,this.vn_vbo);
	var vn_data = reader.float32(this.frame_count*this.vertex_count*3*2);
	gl.bufferData(gl.ARRAY_BUFFER,vn_data,gl.STATIC_DRAW);
	if(this.textures) {
		this.t_vbo = gl.createBuffer();
		gl.bindBuffer(gl.ARRAY_BUFFER,this.t_vbo);
		var tex_data = reader.float32(this.vertex_count*2);
		gl.bufferData(gl.ARRAY_BUFFER,tex_data,gl.STATIC_DRAW);
	}
	this.i_vbo = gl.createBuffer();
	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,this.i_vbo);
	var i_data = new Uint16Array(this.index_count);
	for(var i=0; i<this.index_count; i++)
		i_data[i] = reader.uint32();
	gl.bufferData(gl.ELEMENT_ARRAY_BUFFER,i_data,gl.STATIC_DRAW);
	this.draw = function(program,t) {
		var frame0 = Math.floor(t*mesh.frame_count),
			frame1 = (frame0+1)%mesh.frame_count,
			lerp = t*mesh.frame_count - frame0;
		gl.uniform1f(program.lerp,lerp);
		if(mesh.properties&1)
			gl.uniform4f(program.teamColour,1,0,0,1);
		else
			gl.uniform4f(program.teamColour,0,0,0,0);
		if(mesh.properties&2)
			gl.disable(gl.CULL_FACE);
		else
			gl.enable(gl.CULL_FACE);
		gl.bindTexture(gl.TEXTURE_2D,mesh.texture);
		gl.bindBuffer(gl.ARRAY_BUFFER,mesh.vn_vbo);
		gl.enableVertexAttribArray(program.vertex0);
		gl.vertexAttribPointer(program.vertex0,3,gl.FLOAT,false,3*4,frame0*mesh.vertex_count*3*4);
		gl.enableVertexAttribArray(program.vertex1);
		gl.vertexAttribPointer(program.vertex1,3,gl.FLOAT,false,3*4,frame1*mesh.vertex_count*3*4);
		gl.enableVertexAttribArray(program.normal0);
		gl.vertexAttribPointer(program.normal0,3,gl.FLOAT,false,3*4,(frame0+mesh.frame_count)*mesh.vertex_count*3*4);
		gl.enableVertexAttribArray(program.normal1);
		gl.vertexAttribPointer(program.normal1,3,gl.FLOAT,false,3*4,(frame1+mesh.frame_count)*mesh.vertex_count*3*4);
		gl.bindBuffer(gl.ARRAY_BUFFER,mesh.t_vbo);
		gl.enableVertexAttribArray(program.texCoord);
		gl.vertexAttribPointer(program.texCoord,2,gl.FLOAT,false,0,0);
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,mesh.i_vbo);
		gl.drawElements(gl.TRIANGLES,mesh.index_count,gl.UNSIGNED_SHORT,0);
		gl.disableVertexAttribArray(program.texCoord);
		gl.disableVertexAttribArray(program.normal1);
		gl.disableVertexAttribArray(program.vertex1);
		gl.disableVertexAttribArray(program.normal0);
		gl.disableVertexAttribArray(program.vertex0);
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,null);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);
		gl.bindTexture(gl.TEXTURE_2D,null);
	};
}
