var UIWindows = [], UIFonts = [];

function UIFont(xml,texture) {
	var	font = {},
		get = function (node,attr) { return parseInt(node.getAttribute(attr)); },
		common = xml.getElementsByTagName("common")[0];
	font.lineHeight = get(common,"lineHeight");
	font.base = get(common,"base");
	font.scaleW = get(common,"scaleW");
	font.scaleH = get(common,"scaleH");
	font.chars = [];
	var chars = xml.getElementsByTagName("char");
	for(var i=0; i<chars.length; i++) {
		var ch = chars[i];
		font.chars[get(ch,"id")] = {
			x: get(ch,"x"),
			y: get(ch,"y"),
			w: get(ch,"width"),
			h: get(ch,"height"),
			xofs: get(ch,"xoffset"),
			yofs: get(ch,"yoffset"),
			xadv: get(ch,"xadvance"),
		};
	}
	font.kernings = [];
	var kernings = xml.getElementsByTagName("kerning");
	for(var i=0; i<kernings.length; i++) {
		var kerning = kernings[i], first = get(kerning,"first");
		font.kernings[first] = font.kernings[first] || [];
		font.kernings[first][get(kerning,"second")] = get(kerning,"amount");
	}
	font.texture = texture;
	font.measureText = function(text) {
		var prev = 0, x = 0, y = 0;
		for(var ch in text) {
			ch =text.charCodeAt(ch);
			if(ch in font.chars)
				x += font.chars[ch].xadv;
			if(prev in font.kernings)
				x += font.kernings[prev][ch] || 0;
			prev = ch;
		}
		return [x,font.lineHeight];
	};
	font.drawText = function(ctx,colour,x,y,text) {
		var prev = 0;
		for(var ch in text) {
			ch = text.charCodeAt(ch);
			if(ch in font.chars) {
				var data = font.chars[ch];
				ctx.drawRect(font.texture,colour,
					x+data.xofs,
					y+data.yofs,
					x+data.xofs+data.w,
					y+data.yofs+data.h,
					data.x/font.scaleW,
					data.y/font.scaleH,
					(data.x+data.w)/font.scaleW,
					(data.y+data.h)/font.scaleH);
				x += data.xadv;
			}
			if(prev in font.kernings)
				x += font.kernings[prev][ch] || 0;
			prev = ch;
		}
		return x;
	};
	return font;
}

function load_font(name,path,callback) {
	var xml = null, texture = null;
	var done = function() {
		if(xml && texture) {
			console.log("loaded font",name,path);
			UIFonts[name] = UIFont(xml,texture);
			for(var win in UIWindows) {
				win = UIWindows[win];
				win.layout();
			}
			if(callback)
				callback(UIFonts[name]);
		}
	};
	loadFile("image",path+".png",function(arg) {
		texture = arg;
		done();
	});
	loadFile("xml",path+".xml",function(arg) {
		xml = arg;
		done();
	});
}

load_font("default","bitstream_vera_sans");

function UIContext() {
	var ctx = {};
	ctx.width = ctx.height = 0;
	ctx.buffers = [];
	ctx.data = [];
	ctx.vbo = null;
	ctx.blank = programs.blankTex;
	ctx.corners = [];
	if(!UIContext.program) {
		UIContext.program = createProgram(
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
			"	gl_FragColor = colour * c;\n"+
			"}");
		UIContext.program.mvp = gl.getUniformLocation(UIContext.program,"mvp");
		UIContext.program.colour = gl.getUniformLocation(UIContext.program,"colour");
		UIContext.program.z = gl.getUniformLocation(UIContext.program,"z");
		UIContext.program.texture = gl.getUniformLocation(UIContext.program,"texture");
		UIContext.program.vertex = gl.getAttribLocation(UIContext.program,"vertex");
		UIContext.program.texcoord = gl.getAttribLocation(UIContext.program,"texcoord");
	}
	ctx.clear = function() {
		ctx.data = [];
		ctx.buffers = [];
	};
	ctx.finish = function() {
		if(!ctx.vbo) ctx.vbo = gl.createBuffer();
		if(ctx.data.length) {
			gl.bindBuffer(gl.ARRAY_BUFFER,ctx.vbo);
			gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(ctx.data),gl.STATIC_DRAW);
			gl.bindBuffer(gl.ARRAY_BUFFER,null);
		}
		ctx.data = ctx.data.length; // better to make it crash
	};
	ctx.set = function(texture,colour) {
		if(!ctx.buffers.length || 
			ctx.buffers[ctx.buffers.length-1].texture != texture ||
			ctx.buffers[ctx.buffers.length-1].colour != colour) {
			if(ctx.buffers.length)
				ctx.buffers[ctx.buffers.length-1].stop = ctx.data.length;
			ctx.buffers.push({
				texture: texture,
				colour: colour,
				start: ctx.data.length,
				stop: -1, // marker to say until end of buffer
			});
		}
	};
	ctx.drawText = function(font,colour,x,y,text) { return font? font.drawText(this,colour,x,y,text): 0; };
	ctx.measureText = function(font,text) { return font? font.measureText(text): [0,0]; };
	ctx.drawRect = function(texture,colour,x1,y1,x2,y2,tx1,ty1,tx2,ty2) {
		ctx.set(texture,colour);
		ctx.data = ctx.data.concat([
			x1,y2,tx1,ty2, x2,y1,tx2,ty1, x1,y1,tx1,ty1, //CCW
			x2,y2,tx2,ty2, x2,y1,tx2,ty1, x1,y2,tx1,ty2]);
	};
	ctx.fillRect = function(colour,x1,y1,x2,y2) {
		ctx.drawRect(ctx.blank,colour,x1,y1,x2,y2,0,0,1,1);
	};
	ctx.makeCorners = function(r) {
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
	ctx.fillRoundedRect = function(colour,margin,x1,y1,x2,y2) {
		var corner = ctx.corners[margin] = ctx.corners[margin] || ctx.makeCorners(margin),
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
		ctx.drawRect(ctx.blank,colour,x1,y1-margin,x2,y2+margin,0,0,1,1); // sets up right texture and colour buffer
		ctx.drawRect(ctx.blank,colour,x1-margin,y1,x1,y2,0,0,1,1);
		ctx.drawRect(ctx.blank,colour,x2,y1,x2+margin,y2,0,0,1,1);
		ctx.data = ctx.data.concat(pts);
	};
	ctx.drawRoundedRect = function(colour,margin,width,x1,y1,x2,y2) {
		var corner = ctx.corners[margin] = ctx.corners[margin] || ctx.makeCorners(margin),
			pts = [], p = 0, scale = 1.0 - width/margin,
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
		ctx.drawRect(ctx.blank,colour,x1,y1-margin,x2,y1-margin+width,0,0,1,1); // sets up right texture and colour buffer
		ctx.drawRect(ctx.blank,colour,x1,y2+margin-width,x2,y2+margin,0,0,1,1);
		ctx.drawRect(ctx.blank,colour,x1-margin,y1,x1-margin+width,y2,0,0,1,1);
		ctx.drawRect(ctx.blank,colour,x2+margin-width,y1,x2+margin,y2,0,0,1,1);
		ctx.data = ctx.data.concat(pts);
	};
	ctx.draw = function(mvp) {
		gl.useProgram(UIContext.program);
		gl.disable(gl.CULL_FACE);
		gl.disable(gl.DEPTH_TEST);
		gl.uniformMatrix4fv(UIContext.program.mvp,false,mvp);
		gl.uniform1i(UIContext.program.texture,0);
		gl.uniform1i(UIContext.program.z,0.6);
		gl.bindBuffer(gl.ARRAY_BUFFER,ctx.vbo);
		gl.activeTexture(gl.TEXTURE0);
		gl.enableVertexAttribArray(UIContext.program.vertex);
		gl.enableVertexAttribArray(UIContext.program.texcoord);
		for(var buffer in ctx.buffers) {
			buffer = ctx.buffers[buffer];
			var len = (buffer.stop >= 0? buffer.stop: ctx.data)-buffer.start;
			if(!len) continue;
			gl.bindTexture(gl.TEXTURE_2D,buffer.texture);
			gl.uniform4fv(UIContext.program.colour,buffer.colour);
			gl.vertexAttribPointer(UIContext.program.vertex,2,gl.FLOAT,false,16,0);
			gl.vertexAttribPointer(UIContext.program.texcoord,2,gl.FLOAT,false,16,8);
			gl.drawArrays(gl.TRIANGLES,buffer.start/4,len/4);
		}
		gl.disableVertexAttribArray(UIContext.program.vertex);
		gl.disableVertexAttribArray(UIContext.program.texcoord);
		gl.bindTexture(gl.TEXTURE_2D,null);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);
		gl.enable(gl.DEPTH_TEST);
		gl.enable(gl.CULL_FACE);
	}
	return ctx;
};

function UIWindow(modal,ctrl) {
	var win = {};
	win.modal = modal;
	win.ctrl = null;
	win.ctx = UIContext();
	win.mvp = null;
	win.isDirty = true;
	win.dirty = function() { win.isDirty = true; }
	win.draw = function(canvas) {
		if(win.ctx.width != canvas.offsetWidth || win.ctx.height != canvas.offsetHeight || !win.mvp) {
			win.ctx.width = canvas.offsetWidth;
			win.ctx.height = canvas.offsetHeight;
			win.isDirty = true;
			win.mvp = new Float32Array(createOrtho2D(0,win.ctx.width,win.ctx.height,0));
		}
		if(win.isDirty) {
			win.ctx.clear();
			if(win.modal)
				win.ctx.fillRect(UIDefaults.modalClear,0,0,win.ctx.width,win.ctx.height);
			var draw = function(node) {
				node.isDirty = false;
				node.draw(win.ctx);
				return true;
			};
			win.isDirty = false;
			win.walk(draw);
			win.ctx.finish();
		}
		win.ctx.draw(win.mvp);
	};
	win.walk = function(cb) {
		var visit = function(ctrl) {
			if(cb(ctrl))
				for(var child in ctrl.children)
					visit(ctrl.children[child]);
		};
		if(win.ctrl)
			visit(win.ctrl);
	},
	win.isShown = function() {
		return UIWindows.indexOf(win) != -1;
	};
	win.hide = function() {
		var idx = UIWindows.indexOf(win);
		if(idx != -1)
			UIWindows.splice(idx,1);
	};
	win.show = function() {
		win.hide();
		win.layout();
		if(win.modal)
			UIWindows.push(win);
		else
			UIWindows.unshift(win);
	};
	win.dismiss = function() {
		if(win.modal && win.isShown()) {
			win.hide();
			if(win.onDismiss)
				win.onDismiss();
		}
	}	
	win.getFont = function() { return "default" in UIFonts? UIFonts["default"]: null; };
	win.getBgColour = function() { return UIDefaults.bgColour; };
	win.getFgColour = function() { return UIDefaults.fgColour; };
	win.layout = function() { if(win.ctrl) win.ctrl.layout(); };
	win.window = function() { return win; }
	win.onClick = function(evt,keys) { return win.ctrl? win.ctrl.onClick(evt,keys): false; }
	win.onContextMenu = function(evt,keys) { win.dismiss(); return win.modal; }
	win.setCtrl = function(ctrl) {
		win.ctrl = ctrl;
		win.dirty();
		if(!ctrl)
			win.children = [];
		else {
			win.children = [ctrl];
			ctrl.setParent(win);
			ctrl.layout();
		}
	};
	win.setCtrl(ctrl);
	return win;
};

function drawUI(canvas) {
	for(var window in UIWindows)
		UIWindows[window].draw(canvas);
}

function onMouseDownUI(evt,keys) {
	for(var i=UIWindows.length; i-->0; ) {
		var window = UIWindows[i];
		if(window.onClick(evt,keys) || window.modal)
			return true;
	}
	return false;
}

function onMouseUpUI(evt,keys) {
	return false;
}

function onContextMenuUI(evt,keys) {
	for(var i=UIWindows.length; i-->0; ) {
		var window = UIWindows[i];
		if((window.onContextMenu && window.onContextMenu(evt,keys)) || window.modal)
			return true;
	}
	return false;
}

var UIDefaults = {
	hpadding: 5,
	vpadding: 5,
	modalClear: [0.9,0.9,1,0.5],
	btn:{
		bgColour: [0.3,0.3,0.8,1],
		txtOutline: [0,0,0,0.5],
		fgColour: [1,1,1,1],
	},
	bgColour: [0.3,0.2,0.2,0.5],
	fgColour: [1.0,0.0,0.5,1.0],
};

function UIComponent() {
	var ctrl = {};
	ctrl.x1 = ctrl.x2 = 0;
	ctrl.y1 = ctrl.y2 = 0;
	ctrl.pos = function() {
		return [ctrl.x1,ctrl.y1];
	};
	ctrl.setPos = function(pos) {
		var x = pos[0]-ctrl.x1, y = pos[1]-ctrl.y1;
		if(!x && !y) return;
		for(var child in ctrl.children) {
			child = ctrl.children[child];
			child.setPos([child.x1+x,child.y1+y]);
		}
		ctrl.x1 += x; ctrl.y1 += y;
		ctrl.x2 += x; ctrl.y2 += y;
		ctrl.dirty();
	};
	ctrl.setPosVisible = function(pos) {
		ctrl.setPos([
			Math.max(0,Math.min(pos[0],canvas.width-ctrl.width())),
			Math.max(0,Math.min(pos[1],canvas.height-ctrl.height()))]);
	};
	ctrl.setSize = function(size) {
		if(size == ctrl.size()) return;
		ctrl.x2 = ctrl.x1 + size[0];
		ctrl.y2 = ctrl.y1 + size[1];
		ctrl.dirty();
	};
	ctrl.width = function() {
		return ctrl.x2 - ctrl.x1;
	};
	ctrl.height = function() {
		return ctrl.y2 - ctrl.y1;
	};
	ctrl.size = function() {
		return [ctrl.width(),ctrl.height()];
	};
	ctrl.preferredSize = function() {
		return ctrl.size();
	};
	ctrl.font = null;
	ctrl.getFont = function() {
		return ctrl.font || ctrl.parent.getFont();
	};
	ctrl.fgColour = null;
	ctrl.getFgColour = function() {
		return ctrl.fgColour || ctrl.parent.getFgColour();
	};
	ctrl.bgColour = null;
	ctrl.getBgColour = function() {
		return ctrl.bgColour || ctrl.parent.getBgColour();
	};
	ctrl.layout = function() {
		for(var child in ctrl.children)
			ctrl.children[child].layout();
		ctrl.setSize(ctrl.preferredSize());
	};
	ctrl.draw = function(ctx) {}
	ctrl.isDirty = true;
	ctrl.dirty = function() {
		if(ctrl.isDirty) return;
		ctrl.isDirty = true;
		ctrl.parent.dirty();
	};
	ctrl.children = [];
	ctrl.parent = null;
	ctrl.setParent = function(parent) {
		ctrl.parent = parent;
		ctrl.isDirty = true;
		for(var child in ctrl.children)
			ctrl.children[child].setParent(ctrl);
	};
	ctrl.addChild = function(child) {
		ctrl.children.push(child);
		child.setParent(ctrl);
		ctrl.window().layout();
	};
	ctrl.replaceChild = function(from,to) {
		var i = ctrl.children.indexOf(from);
		if(i == -1)
			ctrl.children.push(to);
		else
			ctrl.children[i] = to;
		to.setParent(ctrl);
		ctrl.window().layout();
	};
	ctrl.destroy = function() {
		if(!ctrl.parent) return;
		var idx = ctrl.parent.children.indexOf(ctrl);
		if(idx != -1) {
			ctrl.parent.children.splice(idx,1);
			ctrl.parent.window().layout();
		}
	};
	ctrl.window = function() { return ctrl.parent.window(); }
	ctrl.onClick = function(evt,keys) {
		var	x = evt.clientX-evt.target.offsetLeft,
			y = evt.clientY-evt.target.offsetTop;
		if(x<ctrl.x1 || x>=ctrl.x2 ||
			y<ctrl.y1 || y>=ctrl.y2)
			return false;
		for(var child in ctrl.children)
			if(ctrl.children[child].onClick(evt,keys))
				return true;
		return false;
	};
	return ctrl;
}

function UILabel(text) {
	var ctrl = UIComponent(); // poor man's inheritence
	ctrl.text = text;
	ctrl.outline = null;
	ctrl.preferredSize = function() {
		var font = ctrl.getFont();
		if(!font) return [0,0];
		var ret = font.measureText(ctrl.text);
		if(ctrl.outline)
			return [ret[0]+3,ret[1]+3];
		return ret;
	}
	ctrl.draw = function(ctx) {
		if(ctrl.outline) {
			ctx.drawText(ctrl.getFont(),ctrl.outline,ctrl.x1,ctrl.y1,ctrl.text);
			ctx.drawText(ctrl.getFont(),ctrl.outline,ctrl.x1,ctrl.y1,ctrl.text);
			ctx.drawText(ctrl.getFont(),ctrl.outline,ctrl.x1+2,ctrl.y1+2,ctrl.text);
			ctx.drawText(ctrl.getFont(),ctrl.outline,ctrl.x1+2,ctrl.y1+2,ctrl.text);
			ctx.drawText(ctrl.getFont(),ctrl.getFgColour(),ctrl.x1+1,ctrl.y1+1,ctrl.text);
		} else
			ctx.drawText(ctrl.getFont(),ctrl.getFgColour(),ctrl.x1,ctrl.y1,ctrl.text);
	}
	return ctrl;
}

function UIButton(text,onClick,tag) {
	var	label = UILabel(text),
		ctrl = UIPanel([label]);
	label.outline = UIDefaults.btn.txtOutline;
	ctrl.bgColour = UIDefaults.btn.bgColour;
	ctrl.fgColour = UIDefaults.btn.fgColour;
	ctrl.canFocus = true;
	if(tag)
		ctrl.tag = tag;
	label.onClick = function(evt) {
		onClick(evt);
		return true;
	}
	return ctrl;
}

function UIPanel(children,layout) {
	var ctrl = UIComponent();
	ctrl.children = children || ctrl.children;
	ctrl.layout = (layout || UILayoutFlow)(ctrl);
	ctrl.draw = function(ctx) {
		var margin = Math.min(UIDefaults.hpadding,UIDefaults.vpadding);
		ctx.fillRoundedRect(ctrl.getBgColour(),margin,
			ctrl.x1+margin,ctrl.y1+margin,ctrl.x2-margin,ctrl.y2-margin);
	};
	return ctrl;
}
	
function UILayoutFlow(ctrl) {
	return function() {
		var h = 0;
		for(var child in ctrl.children) {
			child = ctrl.children[child];
			child.layout();
			child.setSize(child.preferredSize());
			h = Math.max(h,child.height());
		}
		h += UIDefaults.vpadding*2;
		var x = UIDefaults.hpadding;
		for(var child in ctrl.children) {
			child = ctrl.children[child];
			child.setPos([ctrl.x1+x,ctrl.y1+(h-child.height())/2]);
			x += child.width() + UIDefaults.hpadding;
		}
		ctrl.setSize([x,h]);
	}
}

function UILayoutRows(ctrl) {
	return function() {
		var w = 0, h = UIDefaults.vpadding;
		for(var child in ctrl.children) {
			child = ctrl.children[child];
			child.layout();
			child.setSize(child.preferredSize());
			child.setPos([ctrl.x1+UIDefaults.hpadding,ctrl.y1+h]);
			w = Math.max(w,child.width());
			h += child.height() + UIDefaults.vpadding;
		}
		ctrl.setSize([w+UIDefaults.hpadding*2,h]);
	}
}

function Perf() {
	var perf = UIComponent();
	UIWindow(false,perf); // creates a window for it
	perf.label = UILabel("fps");
	perf.addChild(UIPanel([perf.label]));
	perf.data = new Float32Array(2*6*60*3),
	perf.slot = 0,
	perf.now = function() { return Date.now()/1000.0; };
	perf.start = perf.now();
	perf.tick = function() {
		var t = perf.now() - perf.start, w = 1.0/60;
		var pts = [t,1,t+w,1,t,0,t+w,1,t,0,t+w,0]; // 2 triangles
		perf.data.set(pts,perf.slot);
		perf.slot += pts.length;
		if(perf.slot >= perf.data.length) perf.slot = 0;
		perf.label.text = perf.fps(3).toFixed(2)+" fps";
		perf.setPos([canvas.width-(perf.children[0].preferredSize()[0]+10),10]);
		perf.layout();
	};
	perf.fps = function(secs) {
		var min = 0, count = 0,
			now = perf.now() - perf.start,
			since = now - secs;
		for(var tick=0; tick<perf.data.length; tick+=6) {
			var t = perf.data[tick];
			if(t && t > since) {
				min = Math.min(t,min) || t;
				count++;
			}
		}
		if(min < now-Math.min(1,secs))
			return count / (now-min);
		return -1;
	};
	return perf;
}
