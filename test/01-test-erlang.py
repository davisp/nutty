import t
import h

import erlang
a = erlang.Atom

@h.vm()
def test_basic(vm):
    resp = vm.call(a.erlang, a.now)
    t.eq(isinstance(resp, tuple), True)
    for i in range(3):
        t.eq(isinstance(resp[i], (int, long)), True)
        t.gt(resp[i], 0)

@h.vm()
def test_compile(vm):
    script = """
    main(_) ->
        ok.
    """
    sig = vm.compile(script)
    t.eq(a.ok, vm.run(sig))
    t.eq(a.ok, vm.run(script))
