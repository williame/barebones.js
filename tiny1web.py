import os, subprocess, base64
import tornado.ioloop
import tornado.websocket
import tornado.web
from tornado.options import define, options, parse_command_line

define("port",default=8888,type=int)
define("branch",default="master")
define("access",type=str,multiple=True)
define("local",type=bool)

class EchoWebSocket(tornado.websocket.WebSocketHandler):
    def allow_draft76():
    	    print "draft76 rejected"
    	    return False
    def open(self):
        print "WebSocket opened"
    def on_message(self,message):
        self.write_message(message)
    def on_close(self):
        print "WebSocket closed"


class MainHandler(tornado.web.RequestHandler):
    def get(self,path):
        # check user access
        auth_header = self.request.headers.get('Authorization') or ""
        authenticated = not len(options.access)
        if not authenticated and auth_header.startswith('Basic '):
            authenticated = base64.decodestring(auth_header[6:]) in options.access
        if not authenticated:
            self.set_status(401)
            self.set_header('WWW-Authenticate', 'Basic realm=Restricted')
            self._transforms = []
            self.finish()
            return
        # check not escaping chroot
        if os.path.commonprefix([os.path.abspath(path),os.getcwd()]) != os.getcwd():
            raise tornado.web.HTTPError(418)
        # get the file to serve
        body = None
        if options.local:
        	try:
        		with open(path,"r") as f:
        			body = f.read()
		except IOError:
			pass
	if not body:
		try:
		    body = subprocess.check_output(["git","show","%s:%s"%(options.branch,path)])
		except subprocess.CalledProcessError:
		    raise tornado.web.HTTPError(404)
        # and set its content-type
        self.set_header("Content-Type",subprocess.Popen(["file","-i","-b","-"],stdout=subprocess.PIPE,
            stdin=subprocess.PIPE, stderr=subprocess.STDOUT).communicate(input=body)[0].split(";")[0])
        # serve it
        self.write(body)
        

application = tornado.web.Application([
    (r"/ws-echo", EchoWebSocket),
    (r"/(.*)", MainHandler),
])

if __name__ == "__main__":
    parse_command_line()
    application.listen(options.port)
    try:
        tornado.ioloop.IOLoop.instance().start()
    except KeyboardInterrupt:
        print "bye!"
