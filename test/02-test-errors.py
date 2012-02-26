import t
import h


import erlang
a = erlang.Atom


@h.vm()
def test_no_fun(vm):
    t.eq(vm.call(a.foo, a.bar)[0], a.undef)


@h.vm()
def test_syntax_error(vm):
    t.raises(ValueError, vm.compile, "main(_) foo.")


@h.vm()
def test_repeated_errors(vm):
    for i in range(100):
        t.raises(ValueError, vm.compile, "main(_) foo.")
