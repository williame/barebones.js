/* https://github.com/williame/barebones.js project
This file is a simple platform game

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

function DemoPlatformer() {
	UIComponent.call(this,this);
	var win = new UIWindow(false,this);
	var ofs_x = 100, ofs_y = 100, width = 2000, height = 2000;
	this.scene = new QuadTree(rect(ofs_x,ofs_y,ofs_x+width,ofs_y+height));
	for(var i=Math.floor(Math.random()*50)+1050; i-->0; ) {
		var	x = Math.random()*width+ofs_x-51, y = Math.random()*height+ofs_y-51,
			w = Math.random()*50+1, h = Math.random()*50+1,
			r = [x,y,x+w,y+h],
			c = [Math.random(),Math.random(),Math.random(),1];
		this.scene.add(r,[r,c]);
	}
	this.left = 0;
	this.top = 0;
	this.border = 112;
	var self = this;
	var win_draw = win.draw;
	win.draw = function() {
		self.tick();
		win_draw.apply(win,arguments);
	};
	this.lastTick = now();
}
DemoPlatformer.prototype = {
	__proto__: UIComponent.prototype,
	preferredSize: function() { return [canvas.width,canvas.height]; },
	draw: function(ctx) {
		var visible = this.scene.getInRect(this.viewport);
		var ofs = [-this.left,-this.top];
		for(var i in visible) {
			var r = rect_add_vec2(visible[i][0],ofs), c = visible[i][1];
			ctx.drawRect(programs.blankTex,c,r[0],r[1],r[2],r[3],0,0,1,1);
		}
		bounds = rect_add_vec2(this.viewport,ofs);
		ctx.drawBox(OPAQUE,bounds[0],bounds[1],bounds[2],bounds[3]);
		ctx.drawText(this.getFont(),OPAQUE,this.border,this.border,"("+this.left.toFixed(1)+","+this.top.toFixed(1)+")");
	},
	tick: function() {
		var t = now();
		elapsed = (t - this.lastTick) / 1000;
		if(keys[37] && !keys[39]) // left
			this.moveViewport(100*elapsed,0);
		else if(keys[39] && !keys[37]) // right
			this.moveViewport(-100*elapsed,0);
		if(keys[38] && !keys[40]) // up
			this.moveViewport(0,100*elapsed);
		else if(keys[40] && !keys[38]) // down
			this.moveViewport(0,-100*elapsed);
		this.lastTick = t;
	},
	hide: function() {
		window.render = null;
		this.window().hide();
	},
	show: function() {
		this.window().show(-1);
	},
	moveViewport: function(xadj,yadj) {
		this.left += xadj || 0;
		this.top += yadj || 0;
		this.viewport = rect(this.left+this.border,
			this.top+this.border,
			this.left+this.width()-this.border,
			this.top+this.height()-this.border);
		this.dirty();
		
	},
	onResize: function() {
		this.moveViewport();
	},
};

