
import decimal
import functools
import struct
import types


from erltypes import ErlType, Atom, Ref, Port, Pid, Fun, NewFun, ExpFun


_SERIALIZER_FUNS = {}


class treg(object):
    def __init__(self, typ):
        self.typ = typ
    def __call__(self, fun):
        assert self.typ not in _SERIALIZER_FUNS
        _SERIALIZER_FUNS[self.typ] = fun
        return fun


@treg(types.NoneType)
def _none(val):
    assert val is None
    return Atom("nil").to_binary()


@treg(bool)
def _bool(val):
    if val:
        return Atom("true").to_binary()
    else:
        return Atom("false").to_binary()


@treg(int)
@treg(long)
def _int(val):
    if 0 <= val < 256:
        return chr(97) + chr(val)
    elif -214748364 <= val < 2147483647:
        return chr(98) + struct.pack("!i", val)
    else:
        return _bigint(val)


@treg(float)
def _float(val):
    return chr(70) + struct.pack("!d", val)


@treg(str)
def _binary(val):
    return chr(109) + struct.pack("!I", len(val)) + val


@treg(unicode)
def _ubinary(val):
    val = val.encode("utf-8")
    return chr(109) + struct.pack("!I", len(val)) + val


@treg(tuple)
def _tuple(val):
    body = "".join(map(serialize, val))
    if len(val) < 256:
        return chr(104) + chr(len(val)) + body
    else:
        return chr(105) + struct.pack("!I", len(val)) + body


@treg(list)
def _list(val):
    body = "".join(map(serialize, val))
    return chr(108) + struct.pack("!I", len(val)) + body + chr(106)


@treg(dict)
def _dict(val):
    return _list(val.items())


def serialize(val):
    if isinstance(val, ErlType):
        return val.to_binary()
    fun = _SERIALIZER_FUNS.get(type(val))
    if not fun:
        raise TypeError("Unable to serialize %s" % type(val))
    return fun(val)
