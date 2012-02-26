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
    script = "main(_) -> ok."
    sig = vm.compile(script)
    t.eq(a.ok, vm.run(sig))
    t.eq(a.ok, vm.run(script))


DATA = [
    ('<<4:5>>', None),
    ('1', 1),
    ('65537', 65537),
    ('1.5', 1.5),
    ('list_to_atom([97 || _ <- lists:seq(0, 5)])', None),
    ('make_ref()', None),
    ('open_port({spawn, "/bin/sh"}, [])', None),
    ('self()', None),
    ('{0, self()}', None),
    ('list_to_tuple(lists:seq(0, 5))', tuple(range(6))),
    ("bang", "bang"),
    ('[0, 1]', "".join(map(chr, [0, 1]))),
    ('lists:seq(0, 5)', "".join(map(chr, range(6)))),
    ('<<"bing">>', "bing"),
    ('Var = 3, fun(P) -> ok end', None),
    ('fun erlang:now/0', None),
    ('foo', a.foo)
]


def check_parser(vm, script, exp):
    resp = vm.run(script)
    if exp is not None:
        t.eq(resp, exp)
    t.eq(resp, erlang.parse(erlang.serialize(resp)))

def test_parser():
    with erlang.VM() as vm:
        for (e, p) in DATA:
            script = "main(_) -> %s." % e
            yield check_parser, vm, script, p

