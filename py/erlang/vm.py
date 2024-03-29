
import atexit
import hashlib
import os
import signal
import socket
import struct
import subprocess as sp
import sys
import textwrap


import erlang
a = erlang.Atom


class VM(object):
    def __init__(self):
        self.pipe = None
        self.conn = None
        atexit.register(self.close)

    def __repr__(self):
        return "<VM %s>" % id(self)

    def __enter__(self):
        if self.conn is None:
            self.boot()
        return self

    def __exit__(self, etype, evalue, trace):
        self.close()

    def close(self):
        if self.conn:
            try:
                self.conn.close()
            except:
                pass
        if self.pipe and self.pipe.returncode is None:
            try:
                os.kill(self.pipe.pid, signal.SIGKILL)
            except:
                pass
        self.pipe = None
        self.conn = None

    def boot(self):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(1.0)
        sock.bind(("127.0.0.1", 0))
        sock.listen(1)
        port = sock.getsockname()[1]
        cmd = [find_escript(), sys.argv[0], "--port", str(port)]
        self.pipe = sp.Popen(cmd)
        self.conn, _ = sock.accept()
        self.conn.setblocking(1)
        sock.close()
        return self

    def call(self, mod, fun, args=None, timeout=None):
        if args is None:
            args = []
        elif not isinstance(args, list):
            args = [args]
        if timeout is None:
            timeout = a.infinity
        body = (a.call, timeout, a(mod), a(fun), args)
        return self.request(body)

    def compile(self, script):
        script = textwrap.dedent(script.lstrip("\n"))
        modname = "nutty_" + hashlib.sha1(script).hexdigest().upper()
        script = "-module(%s).\n-export([main/1]).\n\n%s\n" % (modname, script)
        body = (a.compile, a(modname), script)
        resp = self.request(body)
        if resp != a.ok:
            raise ValueError("Error compiling script: %s" % (resp,))
        return modname

    def run(self, script, arg=None, timeout=None):
        if not (script.startswith("nutty_") and len(script) == 46):
            script = self.compile(script)
        if timeout is None:
            timeout = a.infinity
        body = (a.run, timeout, a(script), arg)
        return self.request(body)

    def request(self, body):
        body = erlang.serialize(body)
        data = struct.pack("!I", len(body)) + body
        self.conn.sendall(data)
        buf = ""
        while len(buf) < 4:
            buf += self.conn.recv(4 - len(buf))
        size = struct.unpack("!I", buf)[0]
        ret = []
        while size > 0:
            ret.append(self.conn.recv(size))
            size -= len(ret[-1])
        ret = erlang.parse("".join(ret))
        if ret == a.timeout:
            raise RuntimeError("Timeout executing request")
        assert ret[0] == a.resp
        return ret[1]


def find_escript():
    def is_exe(fn):
        return os.path.exists(fn) and os.access(fn, os.X_OK)
    for path in os.environ["PATH"].split(os.pathsep):
        fn = os.path.join(path, "escript")
        if is_exe(fn):
            return fn

