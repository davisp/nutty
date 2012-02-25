
import socket
import subprocess as sp
import sys


import erlang
a = erlang.Atom


class VM(object):
    def __init__(self):
        self.booted = False

    def boot(self):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(1.0)
        sock.bind(("127.0.0.1", 0))
        sock.listen(1)
        port = sock.getsockname()[1]
        cmd = ["escript", sys.argv[0], "--port", str(port)]
        self.pipe = sp.Popen(" ".join(cmd), shell=True)
        self.conn, _ = sock.accept()
        sock.close()
        self.booted = True

    def call(self, mod, fun, args, timeout=None):
        if timeout is None:
            timeout = a.infinity
        body = tuple(a.call, timeout, a(mod), a(fun), args)
        return self.request(body)

    def compile(self, script):
        modname = "nutty_" + hashlibg.sha1(script).hexdigest().upper()
        script = "-module(%s).\n-export([main/0]).\n\n" + script
        body = tuple(a.compile, Atom(modname), script)
        resp = self.request(body)
        if resp != a.ok:
            raise ValueError("Error compiling script: %s" % resp)
        return modname

    def run(self, script, arg=None, timeout=None):
        if not (script.startswith("nutty_") and len(script) == 46):
            script = self.compile(script)
        if timeout is None:
            timeout = a.infinity
        body = tuple(a.run, timeout, Atom(modname), arg)
        return self.request(body)

    def request(self, body):
        body = erlang.serialize(body)
        data = struct.pack("!I", len(body)) + body
        self.conn.sendall(data)
        buf = ""
        while len(buf) < 4:
            self.conn.recv(4 - len(buf))
        size = struct.unpack("!I", buf)[0]
        ret = []
        while size > 0:
            ret.append(self.conn.recv(size))
            size -= len(ret[-1])
        return "".join(ret)
