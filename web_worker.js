/* https://github.com/williame/barebones.js project
The WebWorker object (index.html) uses this for web-worker plumbing.

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


function fail(msg) {
	fail.error = new Error(msg? msg.stack? msg+"\n"+msg.stack: msg: "an error occurred");
	throw fail.error;
}

function assert(expr,msg) {
	if(!expr)
		fail(msg);
}

var	window = self,
	gl = null, // use this to know if you should create graphical assets, to know if you are in a worker or the main thread
	console = {
		log: function() {
			console.lines.push(Array.prototype.join.call(arguments));
		},
		lines: [],
	};

onmessage = function(evt) {
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
			postMessage({ ret: self.dispatch(cmd), console: console.lines, });
	} catch(error) {
		postMessage({ error:""+error, stack:error.stack, console: console.lines, });
	}
	console.lines = [];
};
