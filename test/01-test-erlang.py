import t
import h

import erlang
a = erlang.Atom

@h.vm()
def test_basic(vm):
    resp = vm.call(a.erlang, a.now)
    t.eq(resp[0], a.resp)
    t.eq(isinstance(resp[1], tuple), True)
    for i in range(3):
        t.eq(isinstance(resp[1][i], (int, long)), True)
        t.gt(resp[1][i], 0)

@h.vm()
def test_compile(vm):
    script = """
    main(_) ->
        ok.
    """
    sig = vm.compile(script)
    t.eq(a.ok, vm.run(sig))
    t.eq(a.ok, vm.run(script))
