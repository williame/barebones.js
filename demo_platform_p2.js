/* https://github.com/williame/barebones.js project
This file is a simple platform game using the p2 physics engine

BSD LICENSE:

Copyright (c) 2014, William Edwards
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

function BodyP2() {
	assert(this instanceof BodyP2);
	p2.Body.apply(this,arguments);
	this.colour = [Math.random()*0.5+0.5,Math.random()*0.5+0.5,Math.random()*0.5+0.5,1];
	var self = this, saved_mvp = null;
	this.begin_render = function(mvp) {
		assert(!saved_mvp);
		saved_mvp = mvp;
		var trans = mat4_multiply(mvp, mat4_translation(self.position));
		if(self.angle)
			trans = mat4_multiply(trans, mat4_rotation(-self.angle, [0, 0, 1]));
		return trans;
	};
	this.end_render = function(mvp) {
		assert(saved_mvp);
		mvp = saved_mvp;
		saved_mvp = null;
		return mvp;
	};
}
BodyP2.prototype = {
	__proto__: p2.Body.prototype,
	draw: function(ctx) {
		ctx.transform(this.begin_render);
		for(var shape in this.shapes) {
			shape = this.shapes[shape];
			if(shape.vertices) {
				ctx.fillRect(this.colour,-shape.width/2,-shape.height/2,shape.width/2,shape.height/2);
				//ctx.fillConvexPolygon(this.colour,shape.vertices);
			} else if(shape instanceof p2.Plane) {
				ctx.fillRect(this.colour,-10000, -10, 10000, 0);
			} else
				console.log("cannot draw shape", this, shape);
		}
		ctx.transform(this.end_render);	
	},
};

function make_chain(world, material, pos, len, num_links) {
	var prev = null;
	for(var i=0; i<num_links; i++) {
		var link = new BodyP2({
			mass: prev? (num_links-i) * (i/num_links): 0,
			position: [pos[0], pos[1] - i * len],
		});
		var shape = new p2.Rectangle(len/5, len);
		shape.material = material || new p2.Material();
		link.addShape(shape);
		world.addBody(link);
		if(prev) {
			var constraint = new p2.RevoluteConstraint(link, prev, {
				localPivotA: [0, len/2],
				localPivotB: [0, -len/2],
			});
			constraint.setLimits(-0.5, 0.5);
			world.addConstraint(constraint);
				
		}
		prev = link;
	}
}

function DemoPlatformerP2() {
	UIComponent.call(this,this);
	var walkSpeed = 20, jumpSpeed = 70;
	var win = new UIWindow(false,this);
	var lastStep = now(), self = this, win_draw = win.draw;
	var airborne = true, facing = 1, x_axis = [1, 0], y_axis = [0, 1];
	win.draw = function() {
		var now = window.now();
		var t = now - lastStep;
		if(keys[37] && !keys[39]) { // left
			facing = -1;
			characterBody.velocity[0] = -walkSpeed;
		} else if(keys[39] && !keys[37]) { // right
			facing = 1;
			characterBody.velocity[0] = walkSpeed;
		} //else
		//	characterBody.velocity[0] = 0;
		if(airborne) {
			for(var i in world.narrowphase.contactEquations) {
				var c = world.narrowphase.contactEquations[i];
				if(c.bodyA === characterBody || c.bodyB === characterBody) {
					var x = p2.vec2.dot(c.normalA, x_axis);
					var y = p2.vec2.dot(c.normalA, y_axis);
					if(c.bodyA === characterBody) {
						x *= -1;
						y *= -1;
					}
					if(y >= 0) { // touched down; 0 == side
						console.log("touched down", x, y);
						airborne = false;
						break;
					}
					console.log("airborne, contact", c, x, y);
				}
			}
		}
		if(keys[38] && !keys[40] && !airborne) {
			var canJump = false;
			for(var i=0; i<world.narrowphase.contactEquations.length; i++) {
				var c = world.narrowphase.contactEquations[i];
				if(c.bodyA === characterBody || c.bodyB === characterBody) {
					var x = p2.vec2.dot(c.normalA, x_axis);
					var y = p2.vec2.dot(c.normalA, y_axis);
					if(c.bodyA === characterBody) {
						x *= -1;
						y *= -1;
					}
					console.log("jumping, contact", c, x, y);
					if(y == -1) { // something above
						canJump = false;
						break;
					} else if(x == 0) // on something, and not beside it
						canJump = true;
				}
			}
			if(canJump) {
				airborne = true;
				characterBody.velocity[1] = jumpSpeed;
			}
		}
		for(var i=0; i<movingPlatforms.length; i++)
			movingPlatforms[i].velocity[0] = Math.sin(now * 0.001) * 10;
		this.lastTick = now;
		self.world.step(1/60, lastStep);
		lastStep = now;
		win_draw.apply(win, arguments);
	};
	win.getProjectionMatrix = function() {
		return createOrtho2D(0,this.ctx.width,0,this.ctx.height); // 0 is bottom
	};
	
	// Init world
	var world = this.world = new p2.World();
	//world.defaultContactMaterial.friction = 0.5;
	//world.setGlobalStiffness(1e5);
	// Init materials
	var	groundMaterial = new p2.Material(),
		    characterMaterial = new p2.Material(),
		    boxMaterial = new p2.Material();
	// Add a character body
	var characterShape = new p2.Rectangle(15,30);
	var characterBody = this.character = new BodyP2({
		  mass: 10,
		  position:[540,200],
		  fixedRotation: true,
	});
	characterBody.addShape(characterShape);
	world.addBody(characterBody);
	characterShape.material = characterMaterial;
	characterBody.damping = 0.3;
	world.on("beginContact", function() { console.log("begin contact", arguments); });
	world.on("endContact", function() { console.log("end contact", arguments); });
	world.on("impact", function() { console.log("impact", arguments); });
	// Add a ground plane
	var planeShape = new p2.Plane();
	var planeBody = new BodyP2({
		position:[0,100],
	});
	planeBody.addShape(planeShape);
	world.addBody(planeBody);
	planeShape.material = groundMaterial;
	// some platforms
	var	movingPlatforms = [],
		platformPositions = [[500,200],[600,180],[700,210],[800,100]],
		platformShape = new p2.Rectangle(50,25),
		boxShape = new p2.Rectangle(20,20);
	for(var i=0; i<platformPositions.length; i++) {
		var platformBody = new BodyP2({
			mass: 0, // Static
			position:platformPositions[i],
		});
		platformBody.type = p2.Body.KINEMATIC;
		platformBody.addShape(platformShape);
		world.addBody(platformBody);
		if(i % 2 == 0)
			movingPlatforms.push(platformBody);
		var boxBody = new BodyP2({
			mass: 1,
			angle: Math.random(), 
			position: vec2_add(platformPositions[i], [0, boxShape.height*(2+10*Math.random())]),
		});
		boxBody.addShape(boxShape);
		world.addBody(boxBody);
	}
	platformShape.material = groundMaterial;
	boxShape.material = boxMaterial;
	// chains
	make_chain(world, groundMaterial, [900,400], 10, 25);
	make_chain(world, groundMaterial, [1000,400], 10, 25);
	// Init contactmaterials
	world.addContactMaterial(new p2.ContactMaterial(groundMaterial, characterMaterial, {
		friction: 3.0, // No friction between character and ground
	}));
	world.addContactMaterial(new p2.ContactMaterial(boxMaterial, characterMaterial, {
		friction: 0.0, // No friction between character and boxes
	}));
	world.addContactMaterial(new p2.ContactMaterial(boxMaterial, groundMaterial, {
		friction: 0.6, // Between boxes and ground
	}));
}

DemoPlatformerP2.prototype = {
	__proto__: UIComponent.prototype,
	preferredSize: function() { return [canvas.width,canvas.height]; },
	draw: function(ctx) {
		var world = this.world; 
		for(var i in world.bodies) {
			var body = world.bodies[i];
			if(body.draw)
				body.draw(ctx);
			else
				console.log(body);
		}
	},
	hide: function() {
		this.window().hide();
	},
	show: function() {
		this.window().show(-1);
	},
};
