var UIWindows = [], UIFonts = [];

function UIFont(xml,texture) {
	var get = function (node,attr) { return parseInt(node.getAttribute(attr)); }
	var common = xml.getElementsByTagName("common")[0];
	this.lineHeight = get(common,"lineHeight");
	this.base = get(common,"base");
	this.scaleW = get(common,"scaleW");
	this.scaleH = get(common,"scaleH");
	this.chars = [];
	var chars = xml.getElementsByTagName("char");
	for(var i=0; i<chars.length; i++) {
		var ch = chars[i];
		this.chars[get(ch,"id")] = {
			x: get(ch,"x"),
			y: get(ch,"y"),
			w: get(ch,"width"),
			h: get(ch,"height"),
			xofs: get(ch,"xoffset"),
			yofs: get(ch,"yoffset"),
			xadv: get(ch,"xadvance"),
		};
	}
	this.kernings = [];
	var kernings = xml.getElementsByTagName("kerning");
	for(var i=0; i<kernings.length; i++) {
		var kerning = kernings[i], first = get(kerning,"first");
		this.kernings[first] = this.kernings[first] || [];
		this.kernings[first][get(kerning,"second")] = get(kerning,"amount");
	}
	this.texture = texture;
	this.measureText = function(text) {
		var prev = 0, x = 0, y = 0;
		for(var ch in text) {
			ch =text.charCodeAt(ch);
			if(ch in this.chars)
				x += this.chars[ch].xadv;
			if(prev in this.kernings)
				x += this.kernings[prev][ch] || 0;
			prev = ch;
		}
		return [x,this.lineHeight];
	};
	this.drawText = function(ctx,colour,x,y,text) {
		var prev = 0;
		for(var ch in text) {
			ch =text.charCodeAt(ch);
			if(ch in this.chars) {
				data = this.chars[ch];
				ctx.drawRect(this.texture,colour,
					x+data.xofs,
					y+data.yofs,
					x+data.xofs+data.w,
					y+data.yofs+data.h,
					data.x/this.scaleW,
					data.y/this.scaleH,
					(data.x+data.w)/this.scaleW,
					(data.y+data.h)/this.scaleH);
				x += data.xadv;
			}
			if(prev in this.kernings)
				x += this.kernings[prev][ch] || 0;
			prev = ch;
		}
		return x;
	};
}

function load_font(name,path,callback) {
	var xml = null, texture = null;
	var done = function() {
		if(xml && texture) {
			console.log("loaded font",name,path);
			UIFonts[name] = new UIFont(xml,texture);
			for(var win in UIWindows) {
				win = UIWindows[win];
				win.layout();
			}
			if(callback)
				callback(UIFonts[name]);
		}
	};
	load_file("image",path+".png",function(arg) {
		texture = arg;
		done();
	});
	load_file("xml",path+".fnt",function(arg) {
		xml = arg;
		done();
	});
}

load_font("default","bitstream_vera_sans");

function UIContext() {
	this.width = this.height = 0;
	this.buffers = [];
	this.blank = window.createTexture? createTexture(1,1,new Uint8Array([255,255,255,255])): null;
	this.corners = [];
	this.program = createProgram(
		"uniform mat4 mvp;\n"+
		"uniform float z;\n"+
		"attribute vec2 vertex;\n"+
		"attribute vec2 texcoord;\n"+
		"varying vec2 tx;\n"+
		"void main() {\n"+
		"	tx = texcoord;\n"+
		"	gl_Position = mvp * vec4(vertex,z,1.0);\n"+
		"}",
		"precision mediump float;\n"+
		"uniform vec4 colour;\n"+
		"varying vec2 tx;\n"+
		"uniform sampler2D texture;\n"+
		"void main() {\n"+
		"	vec4 c = texture2D(texture,tx);\n"+
		"	gl_FragColor = colour * c.a;\n"+
		"}");
	this.program.mvp = gl.getUniformLocation(this.program,"mvp");
	this.program.colour = gl.getUniformLocation(this.program,"colour");
	this.program.z = gl.getUniformLocation(this.program,"z");
	this.program.texture = gl.getUniformLocation(this.program,"texture");
	this.program.vertex = gl.getAttribLocation(this.program,"vertex");
	this.program.texcoord = gl.getAttribLocation(this.program,"texcoord");
	this.clear = function() {
		for(var buffer in this.buffers) {
			buffer = this.buffers[buffer];
			if(buffer.buffer) gl.deleteBuffer(buffer.buffer);
		}
		this.buffers = [];
	};
	this.set = function(texture,colour) {
		if(!this.buffers.length || this.buffers[this.buffers.length-1].texture != texture || this.buffers[this.buffers.length-1].colour != colour)
			this.buffers.push({
				texture: texture,
				colour: colour,
				buffer: null,
				data: [],
			});
	};
	this.drawText = function(font,colour,x,y,text) { return font? font.drawText(this,colour,x,y,text): false; };
	this.measureText = function(font,text) { return font? font.measureText(text): [0,0]; };
	this.drawRect = function(texture,colour,x1,y1,x2,y2,tx1,ty1,tx2,ty2) {
		this.set(texture,colour);
		this.buffers[this.buffers.length-1].data = this.buffers[this.buffers.length-1].data.concat([
			x1,y1,tx1,ty1, x2,y1,tx2,ty1, x1,y2,tx1,ty2,
			x2,y1,tx2,ty1, x1,y2,tx1,ty2, x2,y2,tx2,ty2]);
	};
	this.fillRect = function(colour,x1,y1,x2,y2) {
		this.drawRect(this.blank,colour,x1,y1,x2,y2,0,0,1,1);
	};
	this.makeCorners = function(r) {
		var pts = [],
			x = r, y = 0,
			theta = 2 * Math.PI / (r*4),
			cos = Math.cos(theta), sin = Math.sin(theta);
		for(var i=0; i<r; i++) {
			var px = x, py = y;
			x = cos * x - sin * y;
			y = sin * px + cos * y;
			pts.push([px,py,x,y]);
		}
		return pts;
	};
	this.fillRoundedRect = function(colour,margin,x1,y1,x2,y2) {
		var corner = this.corners[margin] = this.corners[margin] || this.makeCorners(margin),
			pts = [], p = 0,
			addPoint = function(pt,x,xdir,y,ydir) {
				pts[p++] = x + xdir*pt[0]; pts[p++] = y + ydir*pt[1];
				pts[p++] = 0; pts[p++] = 0;
				pts[p++] = x + xdir*pt[2]; pts[p++] = y + ydir*pt[3];
				pts[p++] = 1; pts[p++] = 0;
				pts[p++] = x; pts[p++] = y;
				pts[p++] = 1; pts[p++] = 1;
			};
		for(var pt in corner) {
			pt = corner[pt];
			addPoint(pt,x1,-1,y1,-1);
			addPoint(pt,x2,+1,y1,-1);
			addPoint(pt,x1,-1,y2,+1);
			addPoint(pt,x2,+1,y2,+1);
		}
		this.drawRect(this.blank,colour,x1,y1-margin,x2,y2+margin,0,0,1,1); // sets up right texture and colour buffer
		this.drawRect(this.blank,colour,x1-margin,y1,x1,y2,0,0,1,1);
		this.drawRect(this.blank,colour,x2,y1,x2+margin,y2,0,0,1,1);
		this.buffers[this.buffers.length-1].data = this.buffers[this.buffers.length-1].data.concat(pts);
	};
	this.drawRoundedRect = function(colour,margin,width,x1,y1,x2,y2) {
		var corner = this.corners[margin] = this.corners[margin] || this.makeCorners(margin),
			pts = [], p = 0, scale = 1.0 - width/margin;
			addPoint = function(pt,x,xdir,y,ydir) {
				pts[p++] = x + xdir*pt[0]; pts[p++] = y + ydir*pt[1];
				pts[p++] = 0; pts[p++] = 0;
				pts[p++] = x + xdir*pt[2]; pts[p++] = y + ydir*pt[3];
				pts[p++] = 1; pts[p++] = 0;
				pts[p++] = x + xdir*pt[0]*scale; pts[p++] = y + ydir*pt[1]*scale;
				pts[p++] = 1; pts[p++] = 1;
				pts[p++] = x + xdir*pt[2]; pts[p++] = y + ydir*pt[3];
				pts[p++] = 1; pts[p++] = 0;
				pts[p++] = x + xdir*pt[0]*scale; pts[p++] = y + ydir*pt[1]*scale;
				pts[p++] = 1; pts[p++] = 1;
				pts[p++] = x + xdir*pt[2]*scale; pts[p++] = y + ydir*pt[3]*scale;
				pts[p++] = 1; pts[p++] = 1;
			};
		for(var pt in corner) {
			pt = corner[pt];
			addPoint(pt,x1,-1,y1,-1);
			addPoint(pt,x2,+1,y1,-1);
			addPoint(pt,x1,-1,y2,+1);
			addPoint(pt,x2,+1,y2,+1);
		}
		this.drawRect(this.blank,colour,x1,y1-margin,x2,y1-margin+width,0,0,1,1); // sets up right texture and colour buffer
		this.drawRect(this.blank,colour,x1,y2+margin-width,x2,y2+margin,0,0,1,1);
		this.drawRect(this.blank,colour,x1-margin,y1,x1-margin+width,y2,0,0,1,1);
		this.drawRect(this.blank,colour,x2+margin-width,y1,x2+margin,y2,0,0,1,1);
		this.buffers[this.buffers.length-1].data = this.buffers[this.buffers.length-1].data.concat(pts);
	};
	this.draw = function(mvp) {
		gl.useProgram(this.program);
		gl.uniformMatrix4fv(this.program.mvp,false,mvp);
		gl.uniform1i(this.program.texture,0);
		gl.uniform1i(this.program.z,0.6);
		gl.activeTexture(gl.TEXTURE0);
		for(var buffer in this.buffers) {
			buffer = this.buffers[buffer];
			if(!buffer.data.length) continue;
			gl.bindTexture(gl.TEXTURE_2D,buffer.texture);
			gl.uniform4fv(this.program.colour,buffer.colour);
			if(!buffer.buffer) {
				buffer.buffer = gl.createBuffer();
				gl.bindBuffer(gl.ARRAY_BUFFER,buffer.buffer);
				gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(buffer.data),gl.STATIC_DRAW);
			} else
				gl.bindBuffer(gl.ARRAY_BUFFER,buffer.buffer);
			gl.enableVertexAttribArray(this.program.vertex);
			gl.vertexAttribPointer(this.program.vertex,2,gl.FLOAT,false,16,0);
			gl.enableVertexAttribArray(this.program.texcoord);
			gl.vertexAttribPointer(this.program.texcoord,2,gl.FLOAT,false,16,8);
			gl.drawArrays(gl.TRIANGLES,0,buffer.data.length/4);
		}
		gl.disableVertexAttribArray(this.program.vertex);
		gl.disableVertexAttribArray(this.program.texcoord);
		gl.bindTexture(gl.TEXTURE_2D,null);
	}
};

function UIWindow(modal,tree) {
	if(this == window)
		return new UIWindow(modal,tree);
	this.modal = modal;
	this.tree = tree;
	this.ctx = new UIContext();
	this.isDirty = true;
	this.dirty = function() { this.isDirty = true; }
	this.draw = function(canvas) {
		if(this.ctx.width != canvas.offsetWidth || this.ctx.height != canvas.offsetHeight) {
			this.ctx.width = canvas.offsetWidth;
			this.ctx.height = canvas.offsetHeight;
			this.isDirty = true;
			this.mvp = new Float32Array(createOrtho2D(0,this.ctx.width,this.ctx.height,0));
		}
		if(this.isDirty) {
			this.ctx.clear();
			if(this.modal)
				this.ctx.fillRect([0.3,0.3,0.3,0.6],0,0,this.ctx.width,this.ctx.height);
			var draw = function(node,ctx) {
				node.isDirty = false;
				node.draw(ctx);
				for(var child in node.children)
					node.children[child].draw(ctx);
			};
			draw(tree,this.ctx);
			this.isDirty = false;
		}
		this.ctx.draw(this.mvp);
	};
	this.hide = function() {
		var idx = UIWindows.indexOf(this);
		if(idx != -1)
			UIWindows.splice(idx,1);
	};
	this.show = function() {
		this.hide();
		this.layout();
		if(this.modal)
			UIWindows.push(this);
		else
			UIWindows.unshift(this);
	};
	this.getFont = function() {
		return UIFonts.indexOf("default") != -1? UIFonts["default"]: null;
	};
	this.getBgColour = function() {
		return UIDefaults.bgColour;
	};
	this.getFgColour = function() {
		return UIDefaults.fgColour;
	};
	this.layout = function() {
		this.tree.layout();
	};
	tree.setParent(this);
	tree.layout();
	return this;
};

function drawUI(canvas) {
	for(var window in UIWindows)
		UIWindows[window].draw(canvas);
}

var UIDefaults = {
	hpadding: 10,
	bgColour: [0.3,0.2,0.2,1.0],
	fgColour: [1.0,0.8,0.8,1.0],
};

function UIComponent() {
	if(this == window)
		return new UIComponent();
	this.x1 = this.x2 = 0;
	this.y1 = this.y2 = 0;
	this.pos = function() {
		return [this.x1,this.y1];
	}
	this.setPos = function(pos) {
		if(pos == this.pos()) return;
		this.x2 = this.width() + pos[0];
		this.y2 = this.height() + pos[1];
		this.x1 = pos[0];
		this.y1 = pos[1];
		this.dirty();
	}
	this.setSize = function(size) {
		if(size == this.size()) return;
		this.x2 = this.x1 + size[0];
		this.y2 = this.y1 + size[1];
		this.dirty();
	}
	this.width = function() {
		return this.x2 - this.x1;
	}
	this.height = function() {
		return this.y2 - this.y1;
	}
	this.size = function() {
		return [this.width(),this.height()];
	}
	this.preferredSize = function() {
		return this.size(); 
	}
	this.font = null;
	this.getFont = function() {
		return this.font || this.parent.getFont();
	}
	this.fgColour = null;
	this.getFgColour = function() {
		return this.fgColour || this.parent.getFgColour();
	}
	this.bgColour = null;
	this.getBgColour = function() {
		return this.bgColour || this.parent.getBgColour();
	}
	this.layout = function() {
		for(var child in this.children)
			this.children[child].layout();
		this.setSize(this.preferredSize());
	}
	this.draw = function(ctx) {}
	this.isDirty = true;
	this.dirty = function() {
		if(this.isDirty) return;
		this.isDirty = true;
		this.parent.dirty();
	}
	this.children = [];
	this.parent = null;
	this.setParent = function(parent) {
		this.parent = parent;
		this.isDirty = true;
		for(var child in this.children)
			this.children[child].setParent(this);
	}
	return this;
}

function UILabel(text) {
	var ctrl = UIComponent(); // poor man's inheritence
	ctrl.text = text;
	ctrl.preferredSize = function() {
		var font = ctrl.getFont();
		return font? font.measureText(ctrl.text): [0,0];
	}
	ctrl.draw = function(ctx) {
		console.log("drawing",text,ctrl);
		ctx.fillRect(ctrl.getBgColour(),ctrl.x1,ctrl.y1,ctrl.x2,ctrl.y2);
		ctx.drawText(ctrl.getFont(),ctrl.getFgColour(),ctrl.x1,ctrl.y1,ctrl.text);
	}
	return ctrl;
}
	
function UILayoutFlow(ctrl,hpadding) {
	hpadding = hpadding || UIDefaults.hpadding;
	return function() {
		var h = 0;
		for(var child in ctrl.children) {
			child = ctrl.children[child];
			child.layout();
			child.setSize(child.preferredSize());
			h = Math.max(h,child.height());
		}
		var x = 0;
		for(var child in ctrl.children) {
			if(x) x += hpadding;
			child = ctrl.children[child];
			child.setPos([ctrl.x+x,ctrl.y1+(h-child.height())/2]);
			x += child.width();
		}
		ctrl.setSize([x,h]);
	}
}

