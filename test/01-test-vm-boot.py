import t

import erlang
a = erlang.Atom

def test_basic():
    vm = erlang.VM().boot()
    resp = vm.call(a.erlang, a.now)
    t.eq(resp[0], a.resp)
    t.eq(isinstance(resp[1], tuple), True)
    for i in range(3):
        print resp[1][i]
        t.eq(isinstance(resp[1][i], (int, long)), True)
        t.gt(resp[1][i], 0)
