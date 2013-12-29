function isOnGithubPages() {
	return window.location.hostname.indexOf(".github.") > 0;
}

function isOnFileSystem() {
	return "file:" === window.location.protocol;
}

function isLocalHost() {
	return isOnFileSystem() || window.location.hostname == "localhost";
}

function Waiter(readyCallback) {
	var waiter = function(msg) {
		waiter.outstanding.push(msg);
		return function() {
			waiter.outstanding.splice(waiter.outstanding.indexOf(msg),1);
			if(waiter.outstanding.length==0)
				readyCallback();
		};
	};
	waiter.outstanding = [];
	return waiter;
}

function setFile(type,path,bytes) {
	loadFile.loaded = loadFile.loaded || [];
	var	key = ""+type+":"+path,
		obj;
	if(type == "g3d") {
		setFile("ArrayBuffer",path,bytes);
		obj = G3D(path);
	} else if(type == "ArrayBuffer") {
		obj = bytes;
	} else if(type == "json") {
		obj = bytes;
		bytes = JSON.stringify(bytes,null,"\t");
	} else if(type == "image") {
		var image = new Image();
		image.onload = function() {
			loadFile.loaded[key] = createTexture(null,null,null,image);
		};
		image.src = "data:image/png;base64,"+btoa(bytes);
	} else
		fail("unsupported type: "+type+", "+path);
	getDirtyFiles()[key] = bytes;
	if(obj)
		loadFile.loaded[key] = obj;

}

function getDirtyFiles() {
	getDirtyFiles.bytes = getDirtyFiles.bytes || [];
	return getDirtyFiles.bytes;
}

function clearCachedFiles() {
	getDirtyFiles.bytes = [];
	assert(!loadFile.loading || !loadFile.loading.length);
	loadFile.loading = [];
	assert(!loadFile.loadingWait || !loadFile.loading.loadingWait);
	loadFile.loadingWait = [];
	loadFile.loaded = [];
}

function getFile(type,path) {
	var key = ""+type+":"+path;
	return loadFile.loaded? loadFile.loaded[key] || null: null;
}

function loadFile(type,path,callback) {
	var	key = ""+type+":"+path,
		args = Array.prototype.slice.call(arguments).slice(3);
	if(key in loadFile.loaded) {
		console.log("serving",type,path,"from cache...");
		if(callback)
			setTimeout(function() { callback(loadFile.loaded[key]); },0);
		return;
	}
	if(key in loadFile.loading) {
		//console.log("already loading",type,path,"...");
		if(callback)
			loadFile.loading[key].push([callback,args]);
		return;
	}
	console.log("loading",type,path,"...");
	if(cacheHash && path.indexOf("_hash=") == -1)
		path += (path.indexOf("?") == -1? "?": "&") + "_hash="+cacheHash;
	if(splash) splash.dirty();
	loadFile.loading[key] = callback?[[callback,args]]:[];
	if(loadFile.loadingWait) clearTimeout(loadFile.loadingWait);
	loadFile.loadingWait = setTimeout(function() {
		console.log("awaiting load of:",loadFile.loading);
		fail("it's taking a long time to load all the files!  Maybe something is wrong?");
	},30*1000);
	var	fail = Callback(null,function(e) { window.fail("failed to load "+type+" "+path+": "+e); }),
		done = function(arg) {
			try {
				console.log("loaded",key);
				if(splash) splash.dirty();
				arg = loadFile.loaded[key] = loadFile.loaded[key] || arg; // could have been set by setFile() in meantime
				var callbacks = loadFile.loading[key];
				delete loadFile.loading[key];
				if(!loadFile.loading.length) clearTimeout(loadFile.loadingWait);
				for(callback in callbacks) {
					callback = callbacks[callback];
					callback[1].unshift(arg);
					if(callback[0])
						callback[0].apply(this,callback[1]);
				}
			} catch(e) {
				fail(e);
			}
		};
	if(type == "javascript") {
		var script = document.createElement('script');
		script.setAttribute("type","text/javascript");
		script.setAttribute("src",path);
		script.onerror = fail;
		script.async = true;
		script.onload = function() {
			if(!script.readyState || script.readyState == "loaded" || script.readyState == "complete")
				done(script);
			else
				console.log("loading state:",type,path,script.readyState);
		};
		document.getElementsByTagName("head")[0].appendChild(script);
	} else if(type == "image") {
		var image = new Image();
		image.onerror = fail;
		image.onload = function() {
			done(createTexture(null,null,null,image));
		};
		image.src = path;
	} else if(type == "g3d" && window.G3D) {
		var g3d = new G3D(path,function() { done(g3d); });
	} else if(type == "xml") {
		var doc = new XMLHttpRequest();
		doc.open("GET",path,true);
		doc.onerror = fail;
		doc.onreadystatechange = function() {
			if (doc.readyState==4 && (!doc.status || doc.status==200))
				done(doc.responseXML);
		};
		doc.send();
	} else if(type == "json") {
		var doc = new XMLHttpRequest();
		doc.open("GET",path,true);
		doc.overrideMimeType("application/json");
		doc.onerror = fail;
		doc.onreadystatechange = function() {
			if (doc.readyState==4 && (!doc.status || doc.status==200))
				done(JSON.parse(doc.response));
		};
		doc.send();		
	} else if(type == "ArrayBuffer") {
		var doc = new XMLHttpRequest();
		doc.open("GET",path,true);
		doc.responseType = "arraybuffer";
		doc.overrideMimeType('text/plain; charset=x-user-defined');
		doc.onerror = fail;
		doc.onreadystatechange = function() {
			if (doc.readyState==4 && (!doc.status || doc.status==200))
				done(doc.response);
		};
		doc.send();
	} else if(type == "audio") {
		if(!audio)
			done();
		else {
			var doc = new XMLHttpRequest();
			doc.open("GET",path,true);
			doc.responseType = "arraybuffer";
			doc.overrideMimeType('text/plain; charset=x-user-defined');
			doc.onerror = fail;
			doc.onreadystatechange = function() {
				if (doc.readyState==4 && (!doc.status || doc.status==200))
					audio.decodeAudioData(doc.response,done);
			};
			doc.send();
		}
	} else
		fail("unsupported type: "+type+path);
}
loadFile.loading = [];
loadFile.loadingWait = null;
loadFile.loaded = [];

// Data Views are not widely supported in HTML5 browsers yet, so we roll our own and we're happy with it
function BinaryDataReader(arrayBuffer) {
	assert(this instanceof BinaryDataReader);
	this.array = arrayBuffer;
	this.buffer = new Uint8Array(arrayBuffer);
	this.len = arrayBuffer.byteLength;
	this.ofs = 0;
	if(!BinaryDataReader.inited) {
		BinaryDataReader.pow2 = new Float32Array(24); // massive speedup if precomputed
		BinaryDataReader.one_over_pow2 = new Float32Array(24);
		for(var i=0; i<BinaryDataReader.pow2.length; i++) {
			BinaryDataReader.pow2[i] = Math.pow(2,i);
			BinaryDataReader.one_over_pow2[i] = 1 / BinaryDataReader.pow2[i];
		}
		BinaryDataReader.float1 = new Float32Array(1);
		BinaryDataReader.inited = true;
	}
};
BinaryDataReader.prototype = {
	seek: function(ofs) {
		assert(ofs <= this.len);
		this.ofs = ofs;
		return this;
	},
	skip: function(len) {
		assert(this.ofs + len <= this.len);
		this.ofs += len;
		return this;
	},
	_read: function(Type,numElements) {
		numElements = numElements || 1;
		assert(this.ofs + Type.BYTES_PER_ELEMENT*numElements <= this.len);
		if(numElements == 1) {
			var ret = 0;
			for(var i=0; i<Type.BYTES_PER_ELEMENT; i++)
				ret |= this.buffer[this.ofs++] << (i*8);
			return ret;
		}
		var raw = new Type(numElements),
			stop = this.ofs + raw.byteLength;
		raw.set(this.buffer.subarray(this.ofs,stop));
		this.ofs = stop;
		return raw;
	},
	uint8: function(len) { return this._read(Uint8Array,len); },
	uint16: function(len) { return this._read(Uint16Array,len); },
	uint32: function(len) { return this._read(Uint32Array,len); },
	strfixed: function(len) {
		assert(this.ofs + len <= this.len);
		var s = "";
		for(var i=0; i<len; i++) {
			var b = this.buffer[this.ofs++];
			if(b) s += String.fromCharCode(b);
		}
		return s;
	},
	str64: function() { return this.strfixed(64); },
	strz: function() {
		var s = "", b;
		while((b = this.buffer[this.ofs++]))
			s += String.fromCharCode(b);
		return s;
	},
	float32: function(len) {
		// do our own unpacking (http://www.terrybutler.co.uk/downloads/web/webgl-md2.htm adapted, speeded up 1000x)
		len = len || 1;
		var as_float = (len > 1? new Float32Array(len): BinaryDataReader.float1);
		for(var j=0; j<len; j++) {
			var value = this.uint32();
			var sign = (value >> 31) & 0x1;
			var nonZero = false;
			var mantissa = 0;
			var exponent = -127;
			// Mantissa
			for(var i = 22; i > -1; i--)
				if((value >> i & 0x1) == 1) {
					mantissa += BinaryDataReader.one_over_pow2[23-i];
					nonZero = true;
				}	
			if(nonZero) mantissa += 1;		
			// Exponent
			for(var i = 30; i > 22; i--)
				if((value >> i & 0x1) == 1)
					exponent += BinaryDataReader.pow2[i-23];
			var total = Math.pow(2, exponent) * mantissa;		
			if(sign) total = -total;		
			as_float[j] = total;
		}
		if(len>1) return as_float;
		return as_float[0];
	},
};

