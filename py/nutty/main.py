
import erlang

def run():
    print "This is python!"
    vm = erlang.VM().boot()
    print vm.call("erlang", "now", timeout=1000)
