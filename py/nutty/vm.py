
import subprocess as sp
import sys

def boot():
    sp.check_call("escript %s" % sys.argv[0], shell=True)
