
import socket
import subprocess as sp
import sys


class VM(object):
    def __init__(self):
        pass

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

