/* https://github.com/williame/barebones.js project
This file provides a very basic 'window and control-based' UI toolkit that
draws in webGL.  It contains code for printing text messages using bitmap fonts.

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

function UIFont(xml,texture) {
	assert(this instanceof UIFont);
	var	get = function (node,attr) { return parseInt(node.getAttribute(attr)); },
		common = xml.getElementsByTagName("common")[0];
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
	this.em = (this.chars[' '] || {}).w || this.lineHeight;
	this.kernings = [];
	var kernings = xml.getElementsByTagName("kerning");
	for(var i=0; i<kernings.length; i++) {
		var kerning = kernings[i], first = get(kerning,"first");
		this.kernings[first] = this.kernings[first] || [];
		this.kernings[first][get(kerning,"second")] = get(kerning,"amount");
	}
	this.texture = texture;
}
UIFont.prototype = {
	measureText: function(text) {
		var prev = 13, x = 0, w = 0, y = this.lineHeight;
		for(var ch in text) {
			ch = text.charCodeAt(ch);
			if(ch == 10) {
				w = x;
				x = 0;
				y += this.lineHeight;
			} else if(ch in this.chars)
				x += this.chars[ch].xadv;
			if(prev in this.kernings)
				x += this.kernings[prev][ch] || 0;
			prev = ch;
		}
		return [Math.max(x,w),y];
	},
	drawText: function(ctx,colour,x,y,text) {
		var prev = 0, left = x;
		for(var ch in text) {
			ch = text.charCodeAt(ch);
			if(ch == 10) {
				x = left;
				y += this.lineHeight;
			} else if(ch in this.chars) {
				var data = this.chars[ch];
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
		return [x,y];
	},
};

function UIContext() {
	assert(this instanceof UIContext);
	this.width = this.height = 0;
	this.buffers = [];
	this.data = [];
	this.vbo = null;
	this.corners = [];
	this._transforms = [];
	this.drawCount = 0;
	if(!UIContext.program)
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
};
UIContext.corners = {};
UIContext.prototype = {
	clear: function() {
		this.data = [];
		this.buffers = [];
	},
	isEmpty: function() {
		return this.buffers.length == 0;
	},
	finish: function() {
		if(!this.vbo) this.vbo = gl.createBuffer();
		if(this.data.length) {
			gl.bindBuffer(gl.ARRAY_BUFFER,this.vbo);
			gl.bufferData(gl.ARRAY_BUFFER,new Float32Array(this.data),gl.STATIC_DRAW);
			gl.bindBuffer(gl.ARRAY_BUFFER,null);
		}
		this.data = this.data.length; // better to make it crash
		this.drawCount = 0;
	},
	inject: function(callback) {
		if(this.buffers.length)
			this.buffers[this.buffers.length-1].stop = this.data.length;
		var args = Array.prototype.slice.call(arguments,1);
		this.buffers.push({
			inject: callback,
			injectArgs: args,
			texture: "invalid",
			start: this.data.length,
			stop: -1,
		});
	},
	transform: function(callback) { // give it a callback that gets called at each draw to modify the mvp matrix
		if(this.buffers.length)
			this.buffers[this.buffers.length-1].stop = this.data.length;
		this.buffers.push({
			transform: callback,
			transformArgs: arguments.length>1? Array.prototype.slice.call(arguments,1): null,
			texture: "invalid",
			start: this.data.length,
			stop: -1,
		});
	},
	pushTransform: function(mvp) {
		this.transform(this._pushTransform,mvp);
	},
	popTransform: function() {
		this.transform(this._popTransform);
	},
	_pushTransform: function(a,b) {
		this._transforms.push(a);
		return mat4_multiply(a,b);
	},
	_popTransform: function() {
		return this._transforms.pop();
	},
	set: function(texture,colour,mode) {
		if(this.buffers.length) {
			var buffer = this.buffers[this.buffers.length-1];
			if(buffer.texture == texture && buffer.colour == colour && buffer.mode == mode)
				return;
			buffer.stop = this.data.length;
		}
		this.buffers.push({
			texture: texture,
			colour: colour,
			transform: null,
			mode: mode,
			start: this.data.length,
			stop: -1, // marker to say until end of buffer
		});
	},
	drawText: function(font,colour,x,y,text) { return font? font.drawText(this,colour,x,y,text): 0; },
	drawTextOutlined: function(font,fgColour,outlineColour,x,y,text) {
		this.drawText(font,outlineColour,x,y,text);
		this.drawText(font,outlineColour,x,y+2,text);
		this.drawText(font,outlineColour,x+2,y,text);
		this.drawText(font,outlineColour,x+2,y+2,text);
		return this.drawText(font,fgColour,x+1,y+1,text)+1;
	},
	measureText: function(font,text) { return font? font.measureText(text): [0,0]; },
	drawRect: function(texture,colour,x1,y1,x2,y2,tx1,ty1,tx2,ty2) {
		this.set(texture,colour,gl.TRIANGLES);
		this.data.push(
			x1,y2,tx1,ty2, x2,y1,tx2,ty1, x1,y1,tx1,ty1, //CCW
			x2,y2,tx2,ty2, x2,y1,tx2,ty1, x1,y2,tx1,ty2);
	},
	drawQuad: function(texture,colour,a,b,c,d,ta,tb,tc,td) {
		this.set(texture,colour,gl.TRIANGLES);
		this.data.push(
			b[0],b[1],tb[0],tb[1], a[0],a[1],ta[0],ta[1], c[0],c[1],tc[0],tc[1], //CCW
			b[0],b[1],tb[0],tb[1], c[0],c[1],tc[0],tc[1], d[0],d[1],td[0],td[1]);
	},
	fillConvexPolygon: function(colour,vertices) {
		this.set(programs.blankTex,colour,gl.TRIANGLES);
		var ox = vertices[0][0], oy = vertices[0][1];
		for(var i=1; i<vertices.length-1; i++)
			this.data.push(ox,oy,0,0,
				vertices[i][0],vertices[i][1],0,1,
				vertices[i+1][0],vertices[i+1][1],1,1);
	},
	fillRect: function(colour,x1,y1,x2,y2) {
		this.drawRect(programs.blankTex,colour,x1,y1,x2,y2,0,0,1,1);
	},
	drawLine: function(colour,x1,y1,x2,y2,width) {
		if(!width) {
			this.set(programs.blankTex,colour,gl.LINES);
			this.data.push(x1,y1,0,0,x2,y2,1,1);
		} else {
			this.set(programs.blankTex,colour,gl.TRIANGLES);
			var	angle = Math.atan2(y2 - y1, x2 - x1),
				cos = width/2 * Math.cos(angle),
				sin = width/2 * Math.sin(angle);
			this.data.push(
			    x1 + sin, y1 - cos, 1, 0,
			    x2 + sin, y2 - cos, 1, 0,
			    x2 - sin, y2 + cos, 0, 1,
			    x2 - sin, y2 + cos, 0, 1,
			    x1 - sin, y1 + cos, 0, 1,
			    x1 + sin, y1 - cos, 1, 0);
		}
		return this;
	},
	drawBox: function(colour,x1,y1,x2,y2) {
		this.	drawLine(colour,x1,y1,x2,y1).
			drawLine(colour,x1,y2,x2,y2).
			drawLine(colour,x1,y1,x1,y2).
			drawLine(colour,x2,y1,x2,y2);
	},
	fillCircle: function(colour,x1,y1,radius) {
		this.fillRoundedRect(colour,radius,x1,y1,x1,y1);
	},
	_makeCorners: function(r) {
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
	},
	fillRoundedRect: function(colour,margin,x1,y1,x2,y2) {
		var	corner = UIContext.corners[margin] = UIContext.corners[margin] || this._makeCorners(margin),
			pts = [],
			addPoint = this._fillRoundedRect_addPoint,
			drawRect = this.drawRect;
		for(var pt in corner) {
			pt = corner[pt];
			addPoint(pts,pt,x1,-1,y1,-1);
			addPoint(pts,pt,x2,+1,y1,-1);
			addPoint(pts,pt,x1,-1,y2,+1);
			addPoint(pts,pt,x2,+1,y2,+1);
		}
		drawRect.call(this,programs.blankTex,colour,x1,y1-margin,x2,y2+margin,0,0,1,1); // sets up right texture and colour buffer
		drawRect.call(this,programs.blankTex,colour,x1-margin,y1,x1,y2,0,0,1,1);
		drawRect.call(this,programs.blankTex,colour,x2,y1,x2+margin,y2,0,0,1,1);
		this.data.push.apply(this.data,pts);
	},
	_fillRoundedRect_addPoint: function(pts,pt,x,xdir,y,ydir) {
		pts.push(
			x + xdir*pt[0], y + ydir*pt[1],
			0, 0,
			x + xdir*pt[2], y + ydir*pt[3],
			1, 0,
			x, y,
			1, 1
		);
	},
	drawRoundedRect: function(colour,margin,width,x1,y1,x2,y2) {
		var corner = UIContext.corners[margin] = UIContext.corners[margin] || this._makeCorners(margin),
			pts = [],
			scale = 1.0 - width/margin,
			addPoint = this._drawRoundedRect_addPoint,
			drawRect = this.drawRect;
		for(var pt in corner) {
			pt = corner[pt];
			addPoint(pts,scale,pt,x1,-1,y1,-1);
			addPoint(pts,scale,pt,x2,+1,y1,-1);
			addPoint(pts,scale,pt,x1,-1,y2,+1);
			addPoint(pts,scale,pt,x2,+1,y2,+1);
		}
		drawRect.call(programs.blankTex,colour,x1,y1-margin,x2,y1-margin+width,0,0,1,1); // sets up right texture and colour buffer
		drawRect.call(programs.blankTex,colour,x1,y2+margin-width,x2,y2+margin,0,0,1,1);
		drawRect.call(programs.blankTex,colour,x1-margin,y1,x1-margin+width,y2,0,0,1,1);
		drawRect.call(programs.blankTex,colour,x2+margin-width,y1,x2+margin,y2,0,0,1,1);
		this.data.apply(this.data,pts);
	},
	_drawRoundedRect_addPoint: function(pts,scale,pt,x,xdir,y,ydir) {
		pts.push(
			x + xdir*pt[0], y + ydir*pt[1],
			0, 0,
			x + xdir*pt[2], y + ydir*pt[3],
			1, 0,
			x + xdir*pt[0]*scale, y + ydir*pt[1]*scale,
			1, 1,
			x + xdir*pt[2], y + ydir*pt[3],
			1, 0,
			x + xdir*pt[0]*scale, y + ydir*pt[1]*scale,
			1, 1,
			x + xdir*pt[2]*scale, y + ydir*pt[3]*scale,
			1, 1
		);
	},
	drawBorderedRect: function(atlas,colour,x1,y1,x2,y2,borderSize,margin,cornerIdx,sideIdx,backgroundIdx) {
		var corner, tx1, ty1, tx2, ty2, xofs, xlen, xremainder, xscale, yofs, ylen, yremainder, yscale;
		if(backgroundIdx) {
			corner = atlas.getTextureQuad(backgroundIdx);
			tx1 = corner[1][0]; ty1 = corner[1][1];
			tx2 = corner[0][0]; ty2 = corner[3][1];
			xofs = x1 + margin;
			xremainder = (x2-x1)-(margin*2);
			while(xremainder > 0) {
				xlen = Math.min(borderSize,xremainder);
				xscale = lerp(tx1,tx2,xlen/borderSize);
				yofs = y1 + margin;
				yremainder = (y2-y1)-(margin*2);
				while(yremainder > 0) {
					ylen = Math.min(borderSize,yremainder);
					this.drawRect(atlas.texture,colour,
						xofs,yofs,xofs+xlen,yofs+ylen,
						tx1,ty1,
						xscale,lerp(ty1,ty2,ylen/borderSize));
					yofs += ylen;
					yremainder -= ylen;
				}
				xofs += xlen;
				xremainder -= xlen;
			}
		}
		if(cornerIdx) {
			corner = atlas.getTextureQuad(cornerIdx);
			tx1 = corner[1][0]; ty1 = corner[1][1];
			tx2 = corner[0][0]; ty2 = corner[3][1];
			this.drawRect(atlas.texture,colour,x1,y1,x1+borderSize,y1+borderSize,tx1,ty1,tx2,ty2);
			this.drawRect(atlas.texture,colour,x2-borderSize,y1,x2,y1+borderSize,tx2,ty1,tx1,ty2);
			this.drawRect(atlas.texture,colour,x1,y2-borderSize,x1+borderSize,y2,tx1,ty2,tx2,ty1);
			this.drawRect(atlas.texture,colour,x2-borderSize,y2-borderSize,x2,y2,tx2,ty2,tx1,ty1);
		}
		if(sideIdx) {
			corner = atlas.getTextureQuad(sideIdx);
			tx1 = corner[1][0]; ty1 = corner[1][1];
			tx2 = corner[0][0]; ty2 = corner[3][1];
			xofs = x1 + borderSize;
			xremainder = (x2-x1)-(borderSize*2);
			while(xremainder > 0) {
				xlen = Math.min(borderSize,xremainder);
				xscale = lerp(tx1,tx2,xlen/borderSize);
				this.drawRect(atlas.texture,colour,xofs,y1,xofs+xlen,y1+borderSize,tx1,ty1,xscale,ty2);
				this.drawRect(atlas.texture,colour,xofs,y2,xofs+xlen,y2-borderSize,tx1,ty1,xscale,ty2);
				xofs += xlen;
				xremainder -= xlen;
			}
			yofs = y1 + borderSize;
			yremainder = (y2-y1)-(borderSize*2);
			while(yremainder > 0) {
				ylen = Math.min(borderSize,yremainder);
				yscale = ylen/borderSize;
				this.drawQuad(atlas.texture,colour,
					[x1,yofs],[x1+borderSize,yofs],[x1,yofs+ylen],[x1+borderSize,yofs+ylen],
					corner[0],corner[3],
					vec2_lerp(corner[0],corner[1],yscale),vec2_lerp(corner[3],corner[2],yscale));
				this.drawQuad(atlas.texture,colour,
					[x2,yofs],[x2-borderSize,yofs],[x2,yofs+ylen],[x2-borderSize,yofs+ylen],
					corner[0],corner[3],
					vec2_lerp(corner[0],corner[1],yscale),vec2_lerp(corner[3],corner[2],yscale));
				yofs += ylen;
				yremainder -= ylen;
			}
		}
		return this;
	},
	_initShader: function(mvp,program) {
		gl.useProgram(program);
		gl.disable(gl.CULL_FACE);
		gl.disable(gl.DEPTH_TEST);
		gl.uniformMatrix4fv(program.mvp,false,mvp);
		gl.uniform1i(program.texture,0);
		gl.uniform1f(program.z,0.6);
		gl.bindBuffer(gl.ARRAY_BUFFER,this.vbo);
		gl.activeTexture(gl.TEXTURE0);
		gl.enableVertexAttribArray(program.vertex);
		gl.enableVertexAttribArray(program.texcoord);
	},
	_deinitShader: function(program) {
		gl.disableVertexAttribArray(program.vertex);
		gl.disableVertexAttribArray(program.texcoord);
		gl.bindTexture(gl.TEXTURE_2D,null);
		gl.bindBuffer(gl.ARRAY_BUFFER,null);
		gl.enable(gl.DEPTH_TEST);
		gl.enable(gl.CULL_FACE);
		gl.useProgram(null);
	},
	draw: function(mvp,program,colour) {
		this.mvp = mvp;
		program = program || UIContext.program;
		this.drawCount++;
		var inited = false, len;
		for(var buffer in this.buffers) {
			buffer = this.buffers[buffer];
			if(buffer.inject) {
				if(inited) {
					this._deinitShader(program);
					inited = false;
				}
				buffer.inject.apply(this,buffer.injectArgs);
			} else if(buffer.transform) {
				this.mvp = mvp = buffer.transformArgs?
					buffer.transform.apply(this,[mvp].concat(buffer.transformArgs)):
					buffer.transform.call(this,mvp);
				if(inited)
					gl.uniformMatrix4fv(program.mvp,false,mvp);
			} else if(len = (buffer.stop >= 0? buffer.stop: this.data)-buffer.start) {
				if(!inited) {
					this._initShader(mvp,program);
					inited = true;
				}
				gl.bindTexture(gl.TEXTURE_2D,buffer.texture);
				if(colour)
					gl.uniform4fv(program.colour,[buffer.colour[0]*colour[0],buffer.colour[1]*colour[1],buffer.colour[2]*colour[2],buffer.colour[3]*colour[3]]);
				else
					gl.uniform4fv(program.colour,buffer.colour);
				gl.vertexAttribPointer(program.vertex,2,gl.FLOAT,false,16,0);
				gl.vertexAttribPointer(program.texcoord,2,gl.FLOAT,false,16,8);
				gl.drawArrays(buffer.mode,buffer.start/4,len/4);
			}
		}
		if(inited)
			this._deinitShader(program);
		this.mvp = null;
	},
};

function UIWindow(modal,ctrl,tag) {
	assert(this instanceof UIWindow);
	this.mvp = null;
	this.transform = null;
	this.isDirty = true;
	this.needsLayout = true;
	this.showScheduled = false;
	this.hideScheduled = false;
	this.modal = modal;
	this.ctrl = null;
	this.ctx = new UIContext();
	this.id = UI.windowIdSeq++; // for debug info
	this.tag = tag;
	this.setCtrl(ctrl);
};
UIWindow.prototype = {
	dirty: function() { this.isDirty = true; },
	getProjectionMatrix: function() {
		return createOrtho2D(0,this.ctx.width,this.ctx.height,0);
	},
	draw: function(canvas) {
		if(this.ctx.width != canvas.offsetWidth || this.ctx.height != canvas.offsetHeight || !this.mvp) {
			this.ctx.width = canvas.offsetWidth;
			this.ctx.height = canvas.offsetHeight;
			this.isDirty = true;
			this.mvp = new Float32Array(this.getProjectionMatrix());
		}
		if(this.needsLayout)
			this.performLayout();
		if(this.isDirty) {
			this.ctx.clear();
			if(this.modal)
				this.ctx.fillRect(UI.defaults.modalClear,0,0,this.ctx.width,this.ctx.height);
			this.isDirty = false;
			this.walk(this._draw);
			this.ctx.finish();
		}
		this.ctx.draw(this.transform? mat4_multiply(this.mvp,this.transform): this.mvp);
	},
	_draw: function(ctrl) {
		if(!ctrl.visible)
			return false;
		ctrl.isDirty = false;
		ctrl.draw(this.ctx);
		return true;
	},
	walk: function(cb,ctrl) {
		ctrl = ctrl || this.ctrl;
		if(!ctrl) return;
		if(cb.call(this,ctrl)) {
			for(var child in ctrl.children) {
				child = ctrl.children[child];
				if(child)
					this.walk(cb,child);
			}
		}
	},
	find: function(tag) {
		var ret = null;
		this.walk(function(ctrl) {
			if(ctrl && ctrl.tag && ctrl.tag == tag)
				ret = ctrl;
			return !ret;
		});
		return ret;
	},
	_changeVisibility: function() {
		if(this.showScheduled) {
			this.showScheduled = false;
			if(this.isShown())
				this.hide();
			else
				this.onResize();
			if(this.modal) {
				UI.windows.push(this);
			} else {
				if(this._z != -1) { //### TODO support various _z indices
					for(var i=UI.windows.length-1; i>=0; i--)
						if(!UI.windows[i].modal) {
							UI.windows.splice(i+1,0,this);
							return;
						}
				}
				UI.windows.unshift(this);
			}
		} else if(this.hideScheduled) {
			this.hideScheduled = false;
			var idx = UI.windows.indexOf(this);
			if(idx != -1)
				UI.windows.splice(idx,1);
		}
	},
	isShown: function() { return this.showScheduled || (UI.windows.indexOf(this) != -1 && !this.hideScheduled); },
	hide: function() {
		var self = this;
		this.showScheduled = false;
		this.hideScheduled = true;
		schedule(function() { self._changeVisibility(); });
	},
	show: function(z) {
		var self = this;
		this._z = z|0;
		this.showScheduled = true;
		this.hideScheduled = false;
		schedule(function() { self._changeVisibility(); });
	},
	dismiss: function() {
		if(this.modal && this.isShown()) {
			this.hide();
			if(this.ctrl && this.ctrl.onDismiss)
				this.ctrl.onDismiss();
			if(this.onDismiss)
				this.onDismiss();
		}
	},
	getFont: function() { return UI.fonts["default"] || null; },
	getBgColour: function() { return UI.defaults.bgColour; },
	getFgColour: function() { return UI.defaults.fgColour; },
	layout: function() { this.needsLayout = true; }, // schedule the control to be laid out next time its drawn
	performLayout: function() { // perform the layout immediately
		this.needsLayout = false;
		if(this.ctrl)
			this.ctrl.performLayout();
		this.isDirty = true;
	},
	window: function() { return this; },
	onMouseDown: function(evt,keys) { return this.ctrl && this.ctrl.onMouseDown? this.ctrl.onMouseDown(evt,keys): false; },
	onMouseMove: function(evt,keys,isMouseDown) { return this.ctrl && this.ctrl.onMouseMove? this.ctrl.onMouseMove(evt,keys,isMouseDown): false; },
	onMouseUp: function(evt,keys) { return this.ctrl && this.ctrl.onMouseUp? this.ctrl.onMouseUp(evt,keys): false; },
	onKeyDown: function(evt,keys) { 
		var consume = this.ctrl && this.ctrl.onKeyDown? this.ctrl.onKeyDown(evt,keys): false;
		if(!consume && this.modal && evt.which == 27) { // escape key dismisses a modal dialog
			this.dismiss();
			consume = true;
		}
		return consume;
	},
	onKeyUp: function(evt,keys) { return this.ctrl && this.ctrl.onKeyUp? this.ctrl.onKeyUp(evt,keys): false; },
	onMouseWheel: function(evt,amount) { return this.ctrl && this.ctrl.onMouseWheel? this.ctrl.onMouseWheel(evt,amount): false; },
	onContextMenu: function(evt,keys) {
		if(this.ctrl && this.ctrl.onContextMenu)
			return this.ctrl.onContextMenu(evt,keys);
		this.dismiss();
		return this.modal;
	},
	onResize: function(evt) {
		if(this.ctrl && this.ctrl.onResize)
			this.ctrl.onResize(evt);
	},
	setCtrl: function(ctrl) {
		this.ctrl = ctrl;
		this.dirty();
		if(!ctrl)
			this.children = [];
		else {
			this.children = [ctrl];
			ctrl.setParent(this);
			ctrl.performLayout();
		}
	},
};

var UI = {
	windows: [],
	windowIdSeq: 0,
	fonts: {},
	loadFont: function(name,path,callback) {
		var xml = null, texture = null;
		var done = function() {
			if(xml && texture) {
				console.log("loaded font",name,path);
				UI.fonts[name] = new UIFont(xml,texture);
				if(name == "default")
					UI.defaults.lineHeight = UI.fonts[name].lineHeight;
				if(callback)
					callback(UI.fonts[name]);
				window.setTimeout(Callback(window,window.onresize),0);
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
	},
	draw: function(canvas) {
		for(var window in this.windows)
			this.windows[window].draw(canvas);
	},
	onMouseDown: function(evt,keys) {
		for(var i=this.windows.length; i-->0; ) {
			var window = this.windows[i];
			if(window.onMouseDown(evt,keys) || window.modal)
				return true;
		}
		return false;
	},
	onMouseMove: function(evt,keys,isMouseDown) {
		for(var i=this.windows.length; i-->0; ) {
			var window = this.windows[i];
			if(window.onMouseMove(evt,keys,isMouseDown) || window.modal)
				return true;
		}
		return false;
	},
	onMouseUp: function(evt,keys) {
		for(var i=this.windows.length; i-->0; ) {
			var window = this.windows[i];
			if(window.onMouseUp(evt,keys) || window.modal)
				return true;
		}
		return false;
	},
	onKeyDown: function(evt,keys) {
		for(var i=this.windows.length; i-->0; ) {
			var window = this.windows[i];
			if(window.onKeyDown(evt,keys) || window.modal)
				return true;
		}
		return false;
	},
	onKeyUp: function(evt,keys) {
		for(var i=this.windows.length; i-->0; ) {
			var window = this.windows[i];
			if(window.onKeyUp(evt,keys) || window.modal)
				return true;
		}
		return false;
	},
	onMouseWheel: function(evt,amount) {
		for(var i=this.windows.length; i-->0; ) {
			var window = this.windows[i];
			if(window.onMouseWheel(evt,amount) || window.modal)
				return true;
		}
		return false;
	},
	onContextMenu: function(evt,keys) {
		for(var i=this.windows.length; i-->0; ) {
			var window = this.windows[i];
			if((window.onContextMenu && window.onContextMenu(evt,keys)) || window.modal)
				return true;
		}
		return false;
	},
	onResize: function(evt) {
		for(var i=this.windows.length; i-->0; ) {
			var window = this.windows[i];
			window.onResize(evt);
		}
	},
	addMessage: function(secs,from,text,tag) {
		var 	f = from? new UILabel(from): null,
			message = new UIPanel(f?[f,new UILabel(text)]:[new UILabel(text)]);
		message.bgColour = this.defaults.messages.text;
		message.tag = tag;
		if(f) f.fgColour = this.defaults.messages.from;
		if(!this._messages) {
			this._messages = new UIWindow(false,new UIPanel([],UILayoutRows),"messages");
			this._messages.show();
		}
		var old = tag? this.getMessage(tag): null;
		if(old) {
			old.parent.replaceChild(old,message);
			if(old.dismisser)
				clearTimeout(old.dismisser);
		} else
			this._messages.ctrl.addChild(message);
		if(secs)
			this._messages.dismisser = setTimeout(function() { message.destroy(); },secs*1000);
	},
	getMessage: function(tag) {
		if(!this._messages) return null;
		for(var message in this._messages.ctrl.children) {
			message = this._messages.ctrl.children[message];
			if(message.tag == tag)
				return message;
		}
		return null;
	},
	removeMessage: function(tag) {
		var message = this.getMessage(tag);
		if(message) {
			message.destroy();
			if(message.dismisser)
				clearTimeout(message.dismisser);
		}
		return message != null;
	},
	removeAllMessages: function() {
		if(!this._messages) return;
		this._messages.hide();
		this._messages = null;
	},
	defaults: {
		hpadding: 5,
		vpadding: 5,
		ihpadding: 1,
		ivpadding: 1,
		modalClear: [0.9,0.9,1,0.5],
		btn:{
			bgColour: [0.3,0.3,0.8,1.0],
			txtOutline: [0,0,0,0.5],
			fgColour: [1.0,1.0,1.0,1.0],
			active: {
				bgColour: [1.0,0.3,0.3,1.0],
				fgColour: [1.0,1.0,1.0,1.0],
			},
			disabled: {
				bgColour: [0.3,0.3,0.3,1.0],
				fgColour: [0.5,0.5,0.5,1.0],
			},
		},
		messages:{
			from: [0.8,1,0.8,1],
			text: [0.2,0.2,0.2,1],
		},
		fgColour: [1.0,0.0,0.5,1.0],
		bgColour: [0.3,0.2,0.2,0.5],
		dividerColour: [0.9,0.9,0.9,0.5],
		lineHeight: 13,
		spacerHeight: 3,
	},
};
UI.loadFont("default","data/bitstream_vera_sans");

function UIComponent() {
	assert(this instanceof UIComponent);
	this.children = [];
	this.isDirty = true;
	this.parent = null;
	this.x1 = this.y1 = this.x2 = this.y2 = 0;
}
UIComponent.prototype = {
	visible: true,
	enabled: true,
	allowClickThru: true, // set to false to consume clicks on it even if no child consumes it 
	pos: function() { return [this.x1,this.y1]; },
	setPos: function(pos) {
		var x = pos[0]-this.x1, y = pos[1]-this.y1;
		if(!x && !y) return;
		for(var child in this.children) {
			child = this.children[child];
			if(child)
				child.setPos([child.x1+x,child.y1+y]);
		}
		this.x1 += x; this.y1 += y;
		this.x2 += x; this.y2 += y;
		this.dirty();
	},
	setPosVisible: function(pos,rect) {
		if(this.window().needsLayout)
			this.window().performLayout();
		rect = rect || [0,0,canvas.width,canvas.height];
		this.setPos([
			Math.max(rect[0],Math.min(pos[0],rect[2]-this.width())),
			Math.max(rect[1],Math.min(pos[1],rect[3]-this.height()))]);
	},
	setSize: function(size) {
		if(size == this.size()) return;
		this.x2 = this.x1 + size[0];
		this.y2 = this.y1 + size[1];
		this.dirty();
	},
	setWidth: function(width) {
		if(width == this.width()) return;
		this.x2 = this.x1 + width;
		this.dirty();
	},
	setRect: function(rect) {
		this.setPos([rect[0],rect[1]]);
		this.setSize([rect[2],rect[3]]);
		this.dirty();
	},
	x: function() { return this.x1; },
	y: function() { return this.y1; },
	width: function() { return this.x2 - this.x1; },
	height: function() { return this.y2 - this.y1; },
	size: function() { return [this.width(),this.height()]; },
	rect: function() { return [this.x1,this.y1,this.x2,this.y2]; },
	preferredSize: function() { return this.size(); },
	font: null,
	getFont: function() { return this.font || this.parent.getFont(); },
	fgColour: null,
	getFgColour: function() { return this.fgColour || this.parent.getFgColour(); },
	bgColour: null,
	getBgColour: function() { return this.bgColour || this.parent.getBgColour(); },
	layout: function() {
		if(this.parent)
			this.parent.layout();
	},
	performLayout: function() {
		if(this.layoutManager) {
			this.layoutManager.layout(this);
		} else {
			for(var child in this.children) {
				child = this.children[child];
				if(child)
					child.performLayout();
			}
			this.setSize(this.preferredSize());
		}
	},
	draw: function(ctx) {},
	drawBg: function(ctx) {
		var	border = this.border,
			margin = border && border.margin? border.margin:
				border && border.size? border.size/2:
				Math.min(UI.defaults.hpadding,UI.defaults.vpadding);
		if(!border || !border.backgroundTile)
			ctx.fillRoundedRect(border && border.colour? border.colour: this.getBgColour(),
				border && border.width? border.width: margin,
				this.x1+margin,this.y1+margin,this.x2-margin,this.y2-margin);
		if(border && border.atlas && (border.cornerTile||border.sideTile))
			ctx.drawBorderedRect(border.atlas,border.colour||OPAQUE,this.x1,this.y1,this.x2,this.y2,
				border.size||margin,border.width||margin,
				border.cornerTile,border.sideTile,border.backgroundTile);
		return this;
	},
	dirty: function() {
		if(this.isDirty) return;
		this.isDirty = true;
		this.parent.dirty();
	},
	setParent: function(parent) {
		this.parent = parent;
		this.isDirty = true;
		for(var child in this.children) {
			child = this.children[child];
			if(child)
				child.setParent(this);
		}
	},
	addChild: function(child) {
		this.children.push(child);
		child.setParent(this);
		this.layout();
	},
	replaceChild: function(from,to) {
		var i = this.children.indexOf(from);
		if(i == -1)
			this.children.push(to);
		else if(to) {
			this.children[i].setParent(null);
			this.children[i] = to;
			to.setParent(this);
		} else
			this.children.splice(i,1);
		this.layout();
	},
	removeChild: function(child) {
		this.replaceChild(child);
	},
	destroy: function() {
		if(!this.parent) return;
		var idx = this.parent.children.indexOf(this);
		if(idx != -1) {
			this.parent.children.splice(idx,1);
			this.parent.layout();
		}
	},
	window: function() { return this.parent? this.parent.window(): null; },
	isMouseInRect: function(evt) {
		return mousePos && this._isMouseInRect(mousePos[0],mousePos[1]);
	},
	isMouseOver: function(pos) {
		pos = pos || mousePos;
		return pos? this._isMouseInRect(pos[0],pos[1]): false;
	},
	_isMouseInRect: function(x,y) {
		return this.visible && 
			x>=this.x1 && x<this.x2 &&
			y>=this.y1 && y<this.y2;
	},
	onMouseDown: function(evt,keys) {
		if(!this.enabled || !this.isMouseInRect(evt))
			return false;
		if(this.onClicked)
			return this.onClicked(evt,keys);
		else
			for(var child in this.children) {
				child = this.children[child];
				if(child && child.onMouseDown(evt,keys))
					return true;
			}
		return !this.allowClickThru;
	},
	onMouseMove: function(evt,keys,isMouseDown) {
		if(!this.enabled || !this.isMouseInRect(evt))
			return false;
		for(var child in this.children) {
			child = this.children[child];
			if(child && child.onMouseMove(evt,keys,isMouseDown))
				return true;
		}
		return !this.allowClickThru;
	},
	onResize: function(evt) {
		for(var child in this.children) {
			child = this.children[child];
			if(child && child.onResize)
				child.onResize(evt);
		}
	},
};

var UILayoutFlow = {
	layout: function(ctrl) {
		var	h = 0,
			margin = ctrl.border? ctrl.border.margin || ctrl.border.size || 0: 0,
			hpadding = this.hpadding || UI.defaults.hpadding,
			vpadding = this.vpadding || UI.defaults.vpadding,
			ipadding = this.ipadding || UI.defaults.ihpadding;
		for(var child in ctrl.children) {
			child = ctrl.children[child];
			if(!child || !child.visible) continue;
			child.performLayout();
			child.setSize(child.preferredSize());
			h = Math.max(h,child.height());
		}
		h += vpadding*2 + margin;
		var x = hpadding + margin;
		for(var child in ctrl.children) {
			child = ctrl.children[child];
			if(!child || !child.visible) continue;
			child.setPos([ctrl.x1+x,ctrl.y1+(h-child.height())/2]);
			x += child.width() + ipadding;
		}
		x += (hpadding-ipadding);
		ctrl.setSize([x+margin,h+margin]);
	},
};

var UILayoutRows = {
	layout: function(ctrl) {
		var	w = 0,
			margin = ctrl.border? ctrl.border.margin || ctrl.border.size || 0: 0,
			hpadding = this.hpadding || UI.defaults.hpadding,
			vpadding = this.vpadding || UI.defaults.vpadding,
			ipadding = this.ipadding || UI.defaults.ivpadding,
			h = vpadding + margin;
		for(var child in ctrl.children) {
			child = ctrl.children[child];
			if(!child || !child.visible) continue;
			child.performLayout();
			child.setSize(child.preferredSize());
			child.setPos([ctrl.x1+hpadding+margin,ctrl.y1+h]);
			w = Math.max(w,child.width());
			h += child.height() + ipadding;
		}
		h += (vpadding-ipadding);
		for(var child in ctrl.children) {
			child = ctrl.children[child];
			if(!child || !child.visible) continue;
			child.setWidth(w);
		}
		ctrl.setSize([w+hpadding*2+margin*2,h+margin]);
	},
};

function UIPanel(children,layout) {
	UIComponent.call(this);
	this.children = children || this.children;
	this.layoutManager = layout || this.layoutManager;
	this.atlas = null;
	this.cornerTile = this.sideTile = 0;
}
UIPanel.prototype = {
	__proto__: UIComponent.prototype,
	draw: UIComponent.prototype.drawBg,
	layoutManager: UILayoutFlow,
};

function UILabel(text,outline,tag) {
	UIComponent.call(this);
	this.text = String(text);
	this.outline = outline || this.outline;
	this.tag = tag;
}
UILabel.prototype = {
	__proto__: UIComponent.prototype,
	outline: false,
	preferredSize: function() {
		var font = this.getFont();
		if(!font || !this.text) return [0,0];
		var ret = font.measureText(this.text);
		if(this.outline)
			return [ret[0]+3,ret[1]+3];
		return ret;
	},
	setText: function(text) {
		this.text = text;
		this.layout();
	},
	draw: function(ctx) {
		var font = this.getFont();
		if(this.outline)
			ctx.drawTextOutlined(font,this.getFgColour(),this.outline,this.x1,this.y1,this.text);
		else
			ctx.drawText(font,this.getFgColour(),this.x1,this.y1,this.text);
	},
};

function UICtrlIcon(name) {
	UIComponent.call(this);
	this.setIcon(name);
}		
UICtrlIcon.prototype = {
	__proto__: UIComponent.prototype,
	_mapping: ["combo","submenu","checked","unchecked"],
	preferredSize: function() { return [UI.defaults.lineHeight,UI.defaults.lineHeight]; },
	setIcon: function(name) {
		this.iconName = name;
		this.idx = this._mapping.indexOf(name);
		assert(this.idx >= 0,"unsupported mapping: "+name);
	},
	draw: function(ctx) {
		if(!this.tex) return;
		var numIcons = this.tex.height / this.tex.width;
		ctx.drawRect(this.tex,this.getFgColour(),
			Math.max(this.x1,this.x2-this.height()),this.y1,
			this.x2,this.y2,
			0,this.idx/numIcons,
			1,(this.idx+1)/numIcons);
	},
};
loadFile("image","data/ctrl_icons.png",function(tex) { UICtrlIcon.prototype.tex = tex; });

function UIButton(text,onClick,tag,leftIcon,rightIcon) {
	this.label = new UILabel(text,UI.defaults.btn.txtOutline);
	UIPanel.call(this,[leftIcon,this.label,rightIcon]);
	this.onClicked = onClick;
	this.tag = tag;
}
UIButton.prototype = {
	__proto__: UIPanel.prototype,
	getBgColour: function() {
		return this.bgColour || (this.enabled? this.active? UI.defaults.btn.active.bgColour: UI.defaults.btn.bgColour: UI.defaults.btn.disabled.bgColour);
	},
	getFgColour: function() {
		return this.fgColour || (this.enabled? this.active? UI.defaults.btn.active.fgColour: UI.defaults.btn.fgColour: UI.defaults.btn.disabled.fgColour);
	},
	canFocus: true,
	setText: function(text) {
		this.label.setText(text);
	},
	getText: function() {
		return this.label.text;
	},
	setActive: function(active) {
		this.active = !!active;
		this.dirty();
	},
};

function UICheckbox(text,onClick,tag,checked) {
	this._checked = checked;
	this.box = new UICtrlIcon(checked?"checked":"unchecked");
	var	self = this,
		clicked = function() {
			self.setChecked(!self.isChecked());
			onClick.call(this,arguments);
			return true;
		};
	UIButton.call(this,text,clicked,tag,this.box);
}
UICheckbox.prototype = {
	__proto__: UIButton.prototype,
	isChecked: function() { return this._checked; },
	setChecked: function(checked) {
		this._checked = !!checked;
		this.box.setIcon(checked?"checked":"unchecked");
		this.layout();
	},
};

function UIViewport(view) {
	UIComponent.call(this);
	this.view = view;
	this.viewport = null;
	this.tool = null;
	this.hadMouseDown = false;
	this.setViewport();
}
UIViewport.prototype = {
	__proto__: UIComponent.prototype,
	draw: function(ctx) {
		ctx.inject(this.doDraw,this);
	},
	doDraw: function(self) {
		// this == UIContext
		var fullsize = self.isFullSize();
		if(!fullsize) {
			var	oldViewport = gl.getParameter(gl.VIEWPORT),
				viewport = self.viewport;
			gl.viewport(viewport[0],viewport[1],viewport[2],viewport[3]);
			gl.enable(gl.SCISSOR_TEST);
			gl.scissor(viewport[0],viewport[1],viewport[2],viewport[3]);
		}
		self.view.render(self.viewport);
		if(!fullsize) {
			gl.disable(gl.SCISSOR_TEST);
			gl.viewport(oldViewport[0],oldViewport[1],oldViewport[2],oldViewport[3]);
		}
	},
	isFullSize: function() {
		return this.x1==0 && this.y1==0 && this.x2 == canvas.width && this.y2 == canvas.height;
	},
	setViewport: function() {
		this.viewport = [this.x1,canvas.height-this.y1-this.height(),this.width(),this.height()];
	},
	setPos: function(pos) {
		UIComponent.prototype.setPos.call(this,pos);
		this.setViewport();
	},
	setSize: function(size) {
		UIComponent.prototype.setSize.call(this,size);
		this.setViewport();
	},
	mouseRay: function(evt) {
		return unproject(evt.clientX,canvas.height-evt.clientY,
			this.uniforms.mvMatrix,this.uniforms.pMatrix,
			this.viewport);
	},
	mousePos: function(evt) {
		var ray = this.mouseRay(evt), plane = [[1,0,0],[0,1,0]];
		return plane_ray_intersection(plane,ray);
	},
	onMouseDown: function(evt,keys) {
		if(!this.isMouseInRect(evt))
			return false;
		this.hadMouseDown = true;
		if(this.tool && this.tool.onMouseDown)
			this.tool.onMouseDown(evt,keys);
		return true;
	},
	onMouseMove: function(evt,keys,isMouseDown) {
		if(!this.isMouseInRect(evt))
			return false;
		if(this.tool && this.tool.onMouseMove)
			this.tool.onMouseMove(evt,keys,isMouseDown && this.hadMouseDown);
		return true;
	},
	onMouseUp: function(evt,keys) {
		this.hadMouseDown = false;
		if(!this.isMouseInRect(evt))
			return false;
		if(this.tool && this.tool.onMouseUp)
			this.tool.onMouseUp(evt,keys);
		return true;
	},
	onMouseOut: function(evt,keys) {
		this.hadMouseDown = false;
		if(!this.isMouseInRect(evt))
			return false;
		if(this.tool && this.tool.onMouseOut)
			this.tool.onMouseOut(evt,keys);
		return true;
	},
};

UIViewport.ToolPan = function(view) {
	assert(this instanceof UIViewport.ToolPan);
	assert(view instanceof UIViewport);
	this.view = view;
	this.pin = null;
}
UIViewport.ToolPan.prototype = {
	name: "Pan",
	onMouseMove: function(evt,keys,isMouseDown) {                                           
		if(!isMouseDown)
			return;
		if(!this.pin)
			this.pin = evt;
		else {
			var	prev = this.view.mousePos(this.pin),
				pos = this.view.mousePos(evt);
			if(!prev || !pos)
				this.pin = null;
			else {
				var	moved = vec3_sub(prev,pos),
					camera = this.view.camera,
					centre = vec3_add(camera.centre,moved),
					eye = vec3_add(camera.eye,moved);
				this.view.setCamera(eye,centre,camera.up);
				this.pin = evt;
			}
		}
	},
	onMouseUp: function() {
		this.pin = null;
	},
};

UIViewport.ToolRotate = function(view) {
	assert(this instanceof UIViewport.ToolRotate);
	assert(view instanceof UIViewport);
	this.view = view;
	this.pin = null;
}
UIViewport.ToolRotate.prototype = {
	name: "Rotate",
	onMouseDown: function(evt,keys) {
		this.pin = null;
		var pos = this.view.mousePos(evt);
		if(pos) {
			var n = vec3_sub(this.view.camera.centre,pos);
			this.pin = Math.atan2(n[2],n[0]);
		}
	},
	onMouseMove: function(evt,keys,isMouseDown) {
		if(!isMouseDown)
			return;
		if(!this.pin) {
			this.onMouseDown(evt,keys);
			return;
		}
		var pos = this.view.mousePos(evt);
		if(!pos) return;
		var	camera = this.view.camera,
			n = vec3_sub(camera.centre,pos),
			angle = Math.atan2(n[2],n[0]);
		if(this.view.viewMode == "3D") {
			var eye = vec3_rotate(vec3_sub(camera.eye,camera.centre),
				(angle-this.pin),
				[0,0,0],
				camera.up);
			this.view.setCamera(vec3_add(camera.centre,eye),camera.centre,camera.up);
		} else if(this.view.viewMode == "top") {
			var up = vec3_rotate(camera.up,
				(angle-this.pin),
				[0,0,0],
				vec3_sub(camera.eye,camera.centre));
			this.view.setCamera(camera.eye,camera.centre,vec3_normalise(up));
		} else
			fail("rotate doesn't support view mode: "+this.view.viewMode);
	},
	onMouseUp: function() {
		this.pin = null;
	},
};

function UIChoiceMenu(title,tools,view) {
	assert(this instanceof UIChoiceMenu);
	assert(view instanceof UIViewport);
	this.tools = tools;
	this.view = view;
	var self = this, rows = [];
	if(title) rows.push(new UILabel(title));
	for(var tool in tools) {
		tool = tools[tool];
		var name = tool.prototype.name;
		assert(name);
		rows.push(new UIButton(name,(function(name) { return function() { self.setTool(name); }; })(name),"tools|"+name));
	}
	UIPanel.call(this,rows,UILayoutRows);
}
UIChoiceMenu.prototype = {
	__proto__: UIPanel.prototype,
	setTool: function(name) {
		console.log("set tool:",name);
		if(this.view.tool) {
			if(this.view.tool.name == name)
				return;
			if(this.view.tool.stop)
				this.view.tool.stop();
			this.view.tool = null;
		}
		this.window().walk(function(ctrl) {
				if(ctrl.tag && startsWith(ctrl.tag,"tools|"))
					ctrl.setActive(ctrl.tag == "tools|"+name);
				return true;
			});
		for(var t in this.tools)
			if(this.tools[t].prototype.name == name) {
				this.view.tool = new this.tools[t](this.view);
				break;
			}
		assert(this.view.tool,"bad tool type: "+name);
		if(this.view.tool.start)
			this.view.tool.start();
	},
};

function VSpacer(pixels) {
	UIComponent.call(this);
	this.pixels = pixels;
}
VSpacer.prototype = {
	__proto__: UIComponent.prototype,
	preferredSize: function() { return [0,this.pixels || UI.defaults.spacerHeight]; },
};

function UIComboBox(options,idx,onSelect,tag,title) {
	this.options = options;
	this.idx = idx;
	this.onSelect = onSelect;
	this.title = title;
	this.highlight = -1;
	title = title || options[idx].name || options[idx];
	UIButton.call(this,title,this.onClicked,tag,null,new UICtrlIcon("combo"));
	this.label.preferredSize = function() {
		var	ret = [0,0],
			font = this.getFont();
		if(!font) return ret;
		if(this.title)
			ret = font.measureText(title);
		else
			for(var idx in options) {
				var sz = font.measureText(options[idx].name || options[idx]);
				ret[0] = Math.max(ret[0],sz[0]);
				ret[1] = Math.max(ret[1],sz[1]);
			}
		if(this.outline)
			return [ret[0]+3,ret[1]+3];
		return ret;
	};
}
UIComboBox.prototype = {
	__proto__: UIButton.prototype,
	onClicked: function(evt,keys) {
		var	ctrl = this,
			list = new UIPanel([],UILayoutRows),
			menu = new UIWindow(true,list);
		for(var idx in this.options) {
			list.addChild(new UIButton(this.options[idx].name || this.options[idx],function() {
				ctrl.setIdx(this.tag);
				menu.dismiss();
				if(ctrl.onSelect)
					ctrl.onSelect(ctrl.options[ctrl.idx]);
				return true;
			},idx,new UICtrlIcon(idx==this.idx?"checked":"unchecked")));
		}
		menu.layout();
		list.setPosVisible(this.pos());
		menu.show();
		return true;
	},
	setIdx: function(idx) {
		this.idx = idx;
		var label = this.title || this.options[idx].name || ""+this.options[idx];
		this.setText(label);		
	},
};

function UIProperties(keys,title) {
	assert(this instanceof UIProperties);
	UIComponent.call(this);
	this.keys = keys;
	this.data = null;
	this.title = title;
	this.keyWidth = this.valueWidth = 0;
}
UIProperties.prototype = {
	__proto__: UIComponent.prototype,
	_empty: [0,0],
	setData: function(data) {
		this.data = data;
		this.layout();
	},
	_getValue: function(key) {
		return this.data?
			""+(key.getValue?
				key.getValue(this.data):
				this.data[key.key||key.label]):
			"";
	},
	performLayout: function() {
		var font = this.getFont();
		if(!font)
			return;
		var	margin = this.border? this.border.margin || this.border.size || 0: 0,
			hpadding = UI.defaults.hpadding,
			vpadding = UI.defaults.vpadding,
			ihpadding = UI.defaults.ihpadding + font.em,
			ivpadding = UI.defaults.ivpadding,
			k = this.keyWidth, v = 0;
		for(var key in this.keys) {
			key = this.keys[key];
			if(!this.keyWidth)
				k = Math.max(k,font.measureText(key.label)[0]);
			v = Math.max(v,font.measureText(this._getValue(key))[0]);
		}
		var	w = Math.max(font.measureText(this.title)[0],k+ihpadding+v) + hpadding*2 + margin*2,
			h = font.lineHeight;
		h += (h+ivpadding)*this.keys.length + vpadding*2 + margin*2;
		this.keyWidth = k;
		this.valueWidth = v;
		this.setSize([w,h]);
	},
	draw: function(ctx) {
		this.drawBg(ctx);
		var font = this.getFont();
		if(!font)
			return;
		var	margin = (this.border? this.border.margin || this.border.size || 0: 0) || Math.min(hpadding,vpadding),
			hpadding = UI.defaults.hpadding,
			vpadding = UI.defaults.vpadding,
			ihpadding = UI.defaults.ihpadding + font.em,
			ivpadding = UI.defaults.ivpadding,
			bgColour = this.getBgColour(), fgColour = this.getFgColour(),
			v,
			x = this.x1+hpadding+margin, y = this.y1+vpadding+margin;
		var tx = x+this.keyWidth+ihpadding*0.5, ty = y+font.lineHeight+ivpadding*0.5;
		ctx.drawLine(UI.defaults.dividerColour,this.x1+margin,ty,this.x2-margin,ty);
		ctx.drawLine(UI.defaults.dividerColour,tx,ty,tx,this.y2-margin);
		for(var i=0; i<this.keys.length; i+=2) {
			ty = y + (i+1)*(ivpadding + font.lineHeight);
			ctx.fillRect([0.9,0.9,0.9,0.3],this.x1+margin,ty,this.x2-margin,ty+font.lineHeight);
		}
		ctx.drawText(font,fgColour,x,y,this.title);
		for(var key in this.keys) {
			key = this.keys[key];
			y += ivpadding + font.lineHeight;
			ctx.drawText(font,fgColour,x,y,key.label);
			v = this._getValue(key);
			ctx.drawText(font,fgColour,x+ihpadding+this.keyWidth,y,v);
		}
	},
};

function PerformanceCounter(slots) {
	assert(this instanceof PerformanceCounter);
	this.data = new Array(slots || 10);
	this.idx = 0;
}
PerformanceCounter.prototype = {
	now: Date.now,
	tick: function() {
		if(this.idx >= this.data.length)
			this.idx = 0;
		this.data[this.idx++] = this.now();
	},
	tps: function(secs) {
		secs = secs || 3;
		var	min, count = 0,
			now = this.now(),
			since = now - secs*1000;
		for(var t in this.data) {
			t = this.data[t];
			if(t >= since)
				min = count++? Math.min(min,t): t;
		}
		if(count && min < now)
			return count / (now-min) * 1000;
		return -1;
	},		
};

function Perf() {
	var perf = new UIComponent();
	new UIWindow(false,perf); // creates a window for it
	perf.label = new UILabel("fps");
	perf.addChild(new UIPanel([perf.label]));
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
