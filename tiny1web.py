#!/usr/bin/env python

# https://github.com/williame/barebones.js project
# This file is a stand-alone mini webserver that can serve from git (even bare)
# repos and support basic in-secure access control, uploading and archiving.
# 
# BSD LICENSE:
# 
# Copyright (c) 2013, William Edwards
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of the <organization> nor the
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import os, subprocess, base64, tempfile, shutil, sys
import tornado.ioloop
import tornado.web
from tornado.options import define, options, parse_command_line

define("port",default=8888,type=int)
define("branch",default="HEAD")
define("enable_upload",default=None,multiple=True,type=str)
define("access",type=str,multiple=True)

class BaseHandler(tornado.web.RequestHandler):
    auth_user = None
    def check_auth(self,access=None):
        if access is None: access = options.access
        if not access: return
        # check authenticated
        auth_header = self.request.headers.get("Authorization") or ""
        if not auth_header.startswith("Basic "):
            raise tornado.web.HTTPError(401)
        user = base64.decodestring(auth_header[6:])
        if user not in access:
            raise tornado.web.HTTPError(401)
        self.auth_user = user[:user.find(":")]
        return self.auth_user
    def get_commit_hash(self):
        return self.get_argument("_hash",None)
    def check_path(self,path):
        # check not escaping chroot
        if os.path.commonprefix([os.path.abspath(path),home]) != home:
            raise tornado.web.HTTPError(403)
    def write_error(self,status_code,**kwargs):
        if status_code == 401:
            self.set_header("WWW-Authenticate","Basic realm=Restricted")
        tornado.web.RequestHandler.write_error(self,status_code,**kwargs)
    def _request_summary(self):
        return self.request.method + " " + self.request.uri + \
            " (" + self.request.remote_ip + ") "+(self.auth_user or "") 


class MainHandler(BaseHandler):
    def get(self,path):
        self.check_auth()
        self.check_path(path)
        body = None
        commit = self.get_commit_hash()
        if commit and commit in self.request.headers.get("If-None-Match",""):
            self.set_status(304)
            return
        # get the file to serve
        if not commit and not bare:
            try:
                with open(path,"rb") as f:
                    body = f.read()
            except IOError as e:
                print "An error occurred:",path,e
        if not body:
            try:
                body = subprocess.check_output(["git","show","%s:%s"%(commit or options.branch,path)])
            except subprocess.CalledProcessError:
                raise tornado.web.HTTPError(404)
        # and set its content-type
        if path.endswith(".js"):
            content_type = "text/javascript"
        elif path.endswith(".png"):
            content_type = "image/png"
        else:
            content_type = subprocess.Popen(["file","--mime-type","-b","-"],stdout=subprocess.PIPE,
                stdin=subprocess.PIPE, stderr=subprocess.STDOUT).communicate(input=body)[0].strip().split(";")[0]
        self.set_header("Content-Type",content_type)
        if commit:
            self.set_header("ETag",'"%s"'%commit)
            self.set_header("Expires","Fri, 10 Oct 2030 14:19:41 GMT") # counts as forever
            self.set_header("Cache-Control","315360000")
        # serve it
        self.finish(body)
        
class ZipBallHandler(BaseHandler):
    def get(self):
        self.check_auth()
        body = subprocess.check_output(["git","archive","--format","zip",options.branch])
        self.set_header("Content-Type","application/zip")
        self.write(body)
        
class UploadHandler(BaseHandler):
    def post(self):
        if not options.enable_upload:
            raise tornado.web.HTTPError(403,"uploads not allowed")
        user = self.check_auth(options.enable_upload)
        # get the uploaded file
        message = self.get_argument("message")
        folder = self.get_argument("folder")
        if folder.startswith("/"):
            folder = folder[1:]
        folder = os.path.normpath(folder)
        files = self.request.files["files"]
        filenames = [os.path.join(folder,upload["filename"]) for upload in files]
        for filename in filenames:
            self.check_path(filename)
        bodies = [upload["body"] for upload in files]
        # check out the repo to a temporary folder
        working_dir = tempfile.mkdtemp() if bare else home
        print "user",user,"uploading",",".join(filenames),"(%d bytes)"%sum(len(body) for body in bodies),"using",working_dir,"..."
        try:
            if bare:
                os.chdir(working_dir)
                subprocess.check_call(["git","clone",home,working_dir])
                subprocess.check_call(["git","checkout",options.branch])
            dirty = False
            for filename,body in zip(filenames,bodies):
                with open(filename,"w") as f:
                    f.write(body)
                subprocess.check_call(["git","add",filename,"-v"])
                dirty = dirty or subprocess.call(["git","diff","--quiet",options.branch,filename])
            if dirty:
                subprocess.check_call(["git","commit","-m",message,"--author=%s <none@none>"%user])
                if bare:
                    subprocess.check_call(["git","push","origin",options.branch])
            else:
                print "(change to",",".join(filenames)," was no-op)"
        finally:
            if bare:
                print "cleaning up",working_dir,"..."
                os.chdir(home)
                shutil.rmtree(working_dir)
                
class APIHandler(BaseHandler):
    def post(self,entryPoint):
        self.check_auth()
        if entryPoint == "get_hash":
            if not bare:
                dirty = subprocess.check_output(["git","status","--porcelain"]).strip()
                if dirty:
                    self.set_status(503) # busy
                    return
            body = subprocess.check_output(["git","rev-parse",options.branch]).strip()
            self.set_header("Content-Type","application/json")
            self.write({"hash":body})
        else:
            raise tornado.web.HTTPError(404)

if __name__ == "__main__":
    home = os.getcwd()
    bare = not os.path.isdir(".git")
    parse_command_line()
    zipballFilename = r"/%s.zip"%(os.path.splitext(os.path.split(home)[1])[0])
    application = tornado.web.Application((
        (r"/api/(.*)",APIHandler),
        (r"/upload",UploadHandler),
        (zipballFilename,ZipBallHandler),
        (r"/(.*)",MainHandler),
    ))
    print "serving",options.branch,"on port",options.port,"(zipball is %s)"%zipballFilename
    application.listen(options.port)
    try:
        tornado.ioloop.IOLoop.instance().start()
    except KeyboardInterrupt:
        print "bye!"
