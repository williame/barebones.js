// framework code follows:

var gl, canvas, splash, audio, keys = {}, loading = true, mousePos = [-1,-1], cacheHash, unhandledEventHandler;

function init(canvas) {
	if(fail.error) {
		console.log("barebones.js failed to load properly");
		return;
	}
	window.canvas = canvas;
	try {
		console.log("barebones.js is initing...");
		try {
			var params = {
				alpha: false,
				premultipliedAlpha: false,
				stencil: true,
			};
			gl = canvas.getContext("webgl",params) || canvas.getContext("experimental-webgl",params);
		} catch(e) {
			console.log("Error initializing webGL:",e);
		}
		if(!gl) {
			console.log(gl);
			document.getElementById("errWebGL").style.display = "block";
			return;
		}
		canvas.addEventListener("webglcontextlost",function(evt) {
			canvas.style.display = "none";
			document.getElementById("errWebGL").style.display = "block";
			console.log(evt);
			evt.preventDefault();
		},false);
		if(gl.getSupportedExtensions)
			console.log("GL Extensions: "+gl.getSupportedExtensions());
		var handleEvent = function(name) {
			var args = Array.prototype.slice.call(arguments,1);
			(window.UI && name in UI && UI[name].apply(UI,args)) ||
				(unhandledEventHandler && name in unhandledEventHandler &&
					unhandledEventHandler[name].apply(unhandledEventHandler,args)) ||
				(name in window && window[name].apply(window,args));
		};
		window.onresize = function(evt) {
			canvas.style.height = (window.innerHeight - canvas.offsetTop)+"px";
			canvas.width = canvas.offsetWidth;
			canvas.height = canvas.offsetHeight;
			gl.viewport(0,0,canvas.offsetWidth,canvas.offsetHeight);
			if(splash) splash.dirty();
			handleEvent("onResize",evt);
		};
		window.onresize();
		var audioFactory = window.audioContext || window.webkitAudioContext || window.mozAudioContext;
		if(audioFactory)
			audio = new audioFactory();
		var	isMouseDown = false,
			onMouseDown = function(evt) {
				evt.cancelBubble = true;
				evt.preventDefault();
				isMouseDown = true;
				mousePos = [evt.pageX-evt.target.offsetLeft,evt.pageY-evt.target.offsetTop];
				if(evt.target !== canvas)
					return false;
				handleEvent("onMouseDown",evt,keys);
				schedule.run();
				return false;
			},
			onMouseMove = function(evt) {
				evt.cancelBubble = true;
				evt.preventDefault();
				mousePos = [evt.pageX-evt.target.offsetLeft,evt.pageY-evt.target.offsetTop];
				if(evt.target !== canvas)
					return;
				handleEvent("onMouseMove",evt,keys,isMouseDown);
				schedule.run();
			},
			onMouseOver = function(evt) {
				evt.cancelBubble = true;
				evt.preventDefault();
				if(isMouseDown)
					onMouseUp(evt);
				isMouseDown = false;
				handleEvent("onMouseOver",evt);
			},
			onMouseOut = function(evt) {
				evt.cancelBubble = true;
				if(isMouseDown)
					onMouseUp(evt);
				isMouseDown = false;
				handleEvent("onMouseOut",evt);
				mousePos = null;
			},
			onMouseUp = function(evt) {
				evt.cancelBubble = true;
				isMouseDown = false;
				mousePos = [evt.pageX-evt.target.offsetLeft,evt.pageY-evt.target.offsetTop];
				if(evt.target !== canvas)
					return false;
				handleEvent("onMouseUp",evt,keys);
				schedule.run();
				return false;
			},
			onContextMenu = function(evt) {
				evt.cancelBubble = true;
				evt.preventDefault();
				mousePos = [evt.pageX-evt.target.offsetLeft,evt.pageY-evt.target.offsetTop];
				if(evt.target !== canvas)
					return false;
				handleEvent("onContextMenu",evt,keys);
				schedule.run();
				return false;
			},
			onKeyDown = function(evt) {
				evt.cancelBubble = true;
				evt.preventDefault();
				if(!keys[evt.which]) {
					keys[evt.which] = true;
					handleEvent("onKeyDown",evt,keys);
				}
				schedule.run();
				return false;
			},
			onKeyUp = function(evt) {
				evt.cancelBubble = true;
				evt.preventDefault();
				if(keys[evt.which]) {
					keys[evt.which] = false;
					handleEvent("onKeyUp",evt,keys);
				}
				schedule.run();
				return false;
			},
			onMouseWheel = function(evt) {
				evt.cancelBubble = true;
				evt.preventDefault();
				mousePos = [evt.pageX-evt.target.offsetLeft,evt.pageY-evt.target.offsetTop];
				// http://stackoverflow.com/questions/5527601/normalizing-mousewheel-speed-across-browsers
				var d = evt.detail, w = evt.wheelDelta;
				// Normalize delta
				d = d ? w && (f = w/d) ? d/f : -d/1.35 : w/120;
				// Quadratic scale if |d| > 1
				d = d < 1 ? d < -1 ? (-Math.pow(d, 2) - 255) / 255 : d : (Math.pow(d, 2) + 254) / 255;
				// Delta *should* not be greater than 2...
				d = Math.min(Math.max(d / 2, -1), 1);
				handleEvent("onMouseWheel",evt,d*100);
			};
		var loaded = Waiter(function() {
				canvas.addEventListener("mousedown",onMouseDown,true);
				canvas.addEventListener("mouseup",onMouseUp,true);
				canvas.addEventListener("mousemove",onMouseMove,true);
				canvas.addEventListener("mouseover",onMouseOver,true);
				canvas.addEventListener("mouseout",onMouseOut,true);
				canvas.addEventListener("mousewheel",onMouseWheel,true); //chrome
				canvas.addEventListener("DOMMouseScroll",onMouseWheel,true); //FF
				canvas.addEventListener("contextmenu",onContextMenu,true);
				canvas.addEventListener("keydown",onKeyDown,true);
				canvas.addEventListener("keyup",onKeyUp,true);
				canvas.onselectstart = function(){ return false; } // otherwise Chrome doesn't show custom cursors
				if(!canvas.tabIndex <= 0)
					canvas.tabIndex = 1;
				canvas.focus();
				// splash
				var splashCtrl = new UIComponent();
				loadFile("image","data/logo.png",function() { splashCtrl.dirty(); });
				splashCtrl.setSize([canvas.width,canvas.height]);
				splashCtrl.draw = function(ctx) {
					var font = splashCtrl.getFont(), alpha = splash.alpha;
					drawLogo(ctx,alpha);
					if(!font) return;
					var	label = "loading...",
						sz = ctx.measureText(font,label),
						x = (canvas.width-sz[0])*0.5,
						y = (canvas.height-sz[1])*0.75;
					ctx.fillRoundedRect([0.7,0.7,0,0.9*alpha],3,x,y,x+sz[0],y+sz[1]);
					ctx.drawTextOutlined(font,[1,1,1,alpha],[0,0,0,alpha],x-1,y-1,label);
				};
				splash = new UIWindow(false,splashCtrl);
				splash.alpha = 1;
				window.requestAnimFrame(loadLoop);
				// hand over to game
				window.focus();
				loading = false; // game can set it to true again if they want to keep splash
				game();
			});
		if(!isOnGithubPages() && !isLocalHost() && window.location.pathname.indexOf("/branch/")==-1) {
			var doc = new XMLHttpRequest();
			doc.open("POST","/api/get_hash",false);
			doc.send();
			if(doc.status == 200 || doc.status == 0)
				cacheHash = JSON.parse(doc.response).hash;
		}
		loadFile("javascript","barebones.js/vec.js",loaded());
		loadFile("javascript","barebones.js/gl.js",loaded());
		loadFile("javascript","barebones.js/ui.js",loaded());
		if(isOnGithubPages()) {
			var ghProject = "https://github.com/"+
				window.location.hostname.substr(0,window.location.hostname.indexOf(".github."))+"/"+
				window.location.pathname.split("/")[1],
				band = document.createElement("div");
			band.innerHTML = '<a href="'+ghProject+'" style="position: absolute; top: 0; right:0; border: 0;">'+
				'<img src="https://s3.amazonaws.com/github/ribbons/forkme_right_red_aa0000.png" alt="Fork me on GitHub"></a>';
			document.body.appendChild(band);
		}
	} catch(e) {
		fail(e);
	}
}

function drawLogo(ctx,alpha) {
	ctx.clear();
	ctx.fillRect([0,0,0,alpha],0,0,canvas.width,canvas.height);
	var logo = getFile("image","data/logo.png");
	if(logo) {
		var	x = (canvas.width-logo.width)/2,
			y = (canvas.height-logo.height)/2;
		ctx.drawRect(logo,[1,1,1,alpha],
			Math.max(x,0),Math.max(y,0),
			Math.min(x+logo.width,canvas.width),
			Math.min(y+logo.height,canvas.height),
			0,0,1,1);
	}
}

function loop() {
	if(fail.error) return;
	try {
		window.requestAnimFrame(loop);
		schedule.run();
		if(canvas.width && canvas.height) {
			if(window.perf)
				perf.tick();
			if(window.render)
				render();
			UI.draw(canvas);
			if(splash) {
				var fade = now()-splash.fade, timeout = 3000;
				if(fade > timeout)
					splash = null;
				else {
					splash.alpha = 1-fade/timeout;
					splash.dirty();
					splash.draw(canvas);
				}
			}
		}
	} catch(e) {
		fail(e);
	}
}

function loadLoop() {
	if(fail.error) return;
	try {
		if(!loading || (window.isLoadingComplete && window.isLoadingComplete())) {
			loading = false;
			splash.fade = now();
			loop();
			return;
		}
		window.requestAnimFrame(loadLoop);
		schedule.run();
		if(canvas.width && canvas.height && splash) {
			gl.clearColor(0,0,0,1);
			gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);
			splash.draw(canvas,1);
		}
	} catch(e) {
		fail(e);
	}
}

function randomSound(list,volume) {
	if(list.length && audio)
		playSound(choose(list),volume);
}

function playSound(clip,volume) {
	if(!clip) return;
	try {
		var source = audio.createBufferSource();
		source.buffer = clip;
		if(volume) {
			var gainNode = audio.createGainNode();
			source.connect(gainNode);
			gainNode.connect(audio.destination);
			gainNode.gain.value = volume;
		} else
			source.connect(audio.destination);
		source.noteOn(0);
	} catch(error) {
		// not fatal
		console.log("ERROR playing sound: "+clip+": "+error);
	}
}

function schedule(cb) { // most recently scheduled are run first
	assert(cb);
	schedule._items.push(Array.prototype.slice.call(arguments,0));
}

schedule._items = [];

schedule.run = function() {
	for(var items = schedule._items; items.length; items = schedule._items) {
		schedule._items = [];
		for(var i=items.length; i-->0; ) { // most recently pushed first
			var item = items[i], cb = item[0], args = item.slice(1);
			cb.apply(window,args);
		}
	}
};

function Callback(self,callback) {
	var args = arguments.length > 2? Array.prototype.slice.call(arguments,3): null;
	return function() {
		var a = Array.prototype.slice.call(arguments,0);
		Array.prototype.unshift.apply(a,args);
		return callback.apply(self,a);
	}
}

window.requestAnimFrame = 
	window.requestAnimationFrame ||
	window.webkitRequestAnimationFrame ||
	window.mozRequestAnimationFrame ||
	window.oRequestAnimationFrame ||
	window.msRequestAnimationFrame ||
	function(callback) {
		window.setTimeout(callback,1000/60);
	};

