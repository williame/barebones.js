/* https://github.com/williame/barebones.js project

Contains a basic web-worker wrapper.  You can also instansiate it in the browser page thread too if you choose,
which may simplify debugging or for browsers that do not support web workers.

* You make the game() function do whatever you like e.g. create a UIViewport control
  to draw a 3D scene in

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
 
function arrayToString() {
	var args = [];
	for(var arg in arguments) {
		arg = arguments[arg];
		if(arg instanceof Array)
			arg = "["+arrayToString.apply(null,arg)+"]";
		else if(arg instanceof Object) try {
			arg = JSON.stringify(arg);
		} catch(error) {}
		args.push(String(arg));
	}
	return args.length == 1? args[0]: args.join();
}

var fail = fail || function(error) {
	if(!(error instanceof Error))
		error = new Error(arrayToString.apply(null,arguments));
	fail.error = error;
	console.log("error",error,"at",error.stack);
	if(window.document) {
		var div = window.document.getElementById("error");
		div.innerHTML = "<b>AN ERROR OCCURRED</b><pre>"+error.stack;
		div.style.display = "block";
	}
	throw error;
}

var assert = assert || function(condition,msg) {
	if(!condition)
		fail.apply(this,Array.prototype.slice.call(arguments,1));
}

var isInWebWorker = !self.document;

if(!isInWebWorker) {
	window.WebWorker = function(scriptFilenames,dispatch) {
		this.scriptFilenames = scriptFilenames; // array of strings, in import order
		this.dispatch = dispatch; // name of main dispatch function
		this.worker = null;
		this.inflight = [];
	}
	WebWorker.prototype = {
		synthesised: !window.Worker || window.location.search.indexOf("NoWebWorkers") != -1,
		send: function(data,transferrable,callback,ctx) {
			if(!callback) {
				callback = this.defaultCallback;
				ctx = this;
			} else
				ctx = ctx||window;
			if(this.synthesised) {
				if(!this.worker) {
					var	self = this,
						load = function() {
							if(self.scriptFilenames.length) {
								var scriptFilename = self.scriptFilenames.shift();
								var scripts = document.getElementsByTagName("script");
								for(var i=0; i<scripts.length; i++)
									if(scripts[i].src == scriptFilename) {
										console.log(scriptFilename,"already loaded");
										load();
										return;
									}
								console.log("loading",scriptFilename);
								var script = document.createElement('script');
								script.setAttribute("type","text/javascript");
								script.setAttribute("src",scriptFilename);
								script.onerror = window.onerror;
								script.async = true;
								script.onload = function() {
									if(!script.readyState || script.readyState == "loaded" || script.readyState == "complete")
										load();
								};
								document.getElementsByTagName("head")[0].appendChild(script);
							} else {
								self.dispatch = window[self.dispatch] || fail("bad dispatch: "+self.dispatch);
								for(var queued in self.queued) {
									queued = self.queued[queued];
									try {
										var ret = self.dispatch.call(self.worker,queued[0]);
										queued[1].call(queued[2],ret);
									} catch(error) {
										fail(error+" "+error.stack);
									}
								}
								self.queued = null;
							}
						};
					this.worker = {};
					this.queued = [[data,callback,ctx]];
					load();
				} else if(this.queued) {
					this.queued.push([data,callback,ctx]);
				} else try {
					ret = this.dispatch.call(this.worker,data);
					callback.call(ctx,ret);
				} catch(error) {
					fail(error+" "+error.stack);
				}
			} else {
				if(!this.worker) {
					var self = this;
					this.worker = new Worker("web_worker.js");
					this.worker.onerror = window.onerror;
					this.worker.onmessage = function() { self.receive.apply(self,arguments); };
					this.worker.postMessage({
							scriptFilenames: this.scriptFilenames,
							dispatch: this.dispatch,
						});
				}
				var args = Array.prototype.slice.call(arguments,4);
				this.worker.postMessage(data,transferrable||[]);
				this.inflight.push([callback,ctx,args]);
			}
		},
		receive: function(evt) {
			var data = evt.data;
			if(data.error) {
				console.log(data);
				fail(data.error);
			}
			if("ret" in data) {
				var callback = this.inflight.shift();
				if(callback) {
					var ctx = callback[1], args = callback[2], callback = callback[0];
					if(typeof data.ret !== "undefined")
						args.unshift(data.ret);
					callback.apply(ctx,args);
				}
			} else if("console" in data) {
				console.log("(from WebWorker): ",data.console);
			} else if("message" in data) {
				this.onmessage(data.message);
			} else if("UI" in data) {
				UI[data["UI"]].apply(UI,data.args);
			} else
				fail("unsupported message from webWorker:",data);
		},
		onmessage: function(data) {
			console.log("unhandled message from web worker:",data);
		},
		defaultCallback: function(data) {
			console.log("unhandled response from web worker:",data);
		},
	};
} else {
	self.window = self;
	self.console = {
		log: function() {
			postMessage({ console: arrayToString.apply(null,arguments), });
		},
	};
	self.UI = {
		addMessage: function(secs,from,text,tag) {
			postMessage({ UI:"addMessage", args:Array.prototype.slice.call(arguments,0), });
		},
	};
	self.onmessage = function(evt) {
		try {
			if(fail.error)
				throw fail.error;
			var cmd = evt.data;
			if(!self.dispatch) {
				for(var script in cmd.scriptFilenames) {
					console.log("loading",cmd.scriptFilenames[script]);
					importScripts(cmd.scriptFilenames[script]);
					console.log("loaded",cmd.scriptFilenames[script]);
				}
				self.dispatch = self[cmd.dispatch];
				assert(self.dispatch,"dispatch not specified");
			} else
				postMessage({ ret:self.dispatch(cmd), console:console.lines, });
		} catch(error) {
			postMessage({ ret:true, error:""+error, stack:error.stack, });
		}
	};
	self.sendMessage = function(data) {
		postMessage({ message: data, });
	};
}
