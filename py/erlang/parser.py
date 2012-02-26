
import decimal
import functools
import struct
import zlib


from erlang.erltypes import Atom, BitString, Ref, Port, Pid, \
                                Fun, NewFun, ExpFun, Nil


_PARSER_FUNS = {}


class tag(object):
    def __init__(self, val):
        self.val = val
    def __call__(self, fun):
        assert self.val not in _PARSER_FUNS
        _PARSER_FUNS[self.val] = fun
        return fun


class dlen(object):
    def __init__(self, val):
        self.val = val
    def __call__(self, fun):
        @functools.wraps(fun)
        def _wrapped(data):
            if len(data) < self.val:
                raise ValueError("Insufficient data: %d" % len(data))
            return fun(data)
        return _wrapped


class ulen(object):
    def __init__(self, val, extra=0):
        assert val in (1, 2, 4)
        self.val = val
        self.extra = extra
    def __call__(self, fun):
        @functools.wraps(fun)
        def _wrapped(data):
            if len(data) < self.val + 1:
                raise ValueError("Insufficient data: %d" % len(data))
            if self.val == 1:
                length = ord(data[0])
                data = data[1:]
            elif self.val == 2:
                length = struct.unpack("!H", data[:2])[0]
                data = data[2:]
            elif self.val == 4:
                length = struct.unpack("!I", data[:4])[0]
                data = data[4:]
            if len(data) < length + self.extra:
                args = fun.func_name, length
                raise ValueError("Invalid length: %s :: %d" % args)
            return fun(length, data)
        return _wrapped


@tag(70)
@dlen(8)
def _new_float(data):
    ret = struct.unpack("!d", data[:8])[0]
    return ret, data[8:]


@tag(77)
@ulen(4, 1)
def _bitstring(length, data):
    ret = BitString(data[1:length]).bits(length)
    return ret, data[length+1:]


@tag(97)
@dlen(1)
def _small_int(data):
    return ord(data[0]), data[1:]


@tag(98)
@dlen(4)
def _int(data):
    ret = struct.unpack("!i", data[:4])[0]
    return ret, data[4:]


@tag(99)
@dlen(31)
def _float(data):
    return float(data[:31]), data[31:]


@tag(100)
@ulen(2)
def _atom(length, data):
    return Atom(data[:length]), data[length:]


@tag(101)
def _ref(data):
    node, rest = _parse(data)
    if not isinstance(node, Atom):
        raise TypeError("Invalid node decoded")
    if len(rest) < 5:
        raise ValueError("Insufficient data: %d" % len(rest))
    return Ref(node, ret[:4], rest[4]), rest[5:]


@tag(102)
def _port(data):
    node, rest = _parse(data)
    if not isinstance(node, Atom):
        raise TypeError("Invalid node decoded")
    if len(rest) < 5:
        raise ValueError("Insufficient data: %d" % len(rest))
    return Port(node, rest[:4], rest[4]), rest[5:]


@tag(103)
def _pid(data):
    node, data = _parse(data)
    if not isinstance(node, Atom):
        raise TypeError("Invalid node decoded")
    if len(data) < 9:
        raise ValueError("Insufficient data: %d" % len(data))
    ret = Pid(node, data[:4], data[4:8], data[8])
    return ret, data[9:]


@tag(104)
@ulen(1)
def _small_tuple(length, data):
    return _tuple(length, data)


@tag(105)
@ulen(4)
def _large_tuple(length, data):
    return _tuple(length, data)


def _tuple(length, data):
    ret = []
    for i in range(length):
        term, data = _parse(data)
        ret.append(term)
    return tuple(ret), data


@tag(106)
def _nil(data):
    return Nil(), data


@tag(107)
@ulen(2)
def _string(length, data):
    return data[:length], data[length:]


@tag(108)
@ulen(4)
def _list(length, data):
    ret = []
    for i in range(length):
        term, data = _parse(data)
        ret.append(term)
    # parse tail
    term, data = _parse(data)
    if not isinstance(term, Nil):
        ret.append(term)
    return ret, data


@tag(109)
@ulen(4)
def _binary(length, data):
    return data[:length], data[length:]


@tag(110)
@ulen(1, 1)
def _small_bigint(length, data):
    return _bigint(length, data)


@tag(111)
@ulen(4, 1)
def _large_bigint(length, data):
    return _bigint(length, data)


def _bigint(length, data):
    ret = decimal.Decimal(0)
    sign = data[0]
    for i in range(length):
        ret += ord(data[i+1]) * (256 ** i)
    if sign:
        ret *= -1
    return ret, data[length+1:]


@tag(112)
def _new_fun(data):
    size = struct.unpack("!I", data[:4])[0]
    if size > len(data):
        raise ValueError("Insufficient data: %d" % len(data))
    retdata = data[size:]
    data = data[4:size]
    arity, data = ord(data[0]), data[1:]
    uniq, data = data[0:16], data[16:]
    idx, data = struct.unpack("!I", data[:4])[0], data[4:]
    num_free, data = struct.unpack("!I", data[:4])[0], data[4:]
    mod, data = _parse(data)
    oldidx, data = _parse(data)
    oldunq, data = _parse(data)
    pid, data = _parse(data)
    free = []
    for i in range(num_free):
        term, data = _parse(data)
        free.append(free)
    assert len(data) == 0
    ret = NewFun(arity, uniq, idx, mod, oldidx, oldunq, pid, free)
    return ret, retdata


@tag(113)
def _export(data):
    mod, data = _parse(data)
    fun, data = _parse(data)
    ari, data = _parse(data)
    return ExpFun(mod, fun, ari), data


@tag(114)
def _new_ref(data):
    length = struct.unpack("!H", data[:2])[0]
    node, data = _parse(data[2:])
    if len(data) < 1 + (4 * length):
        raise ValueError("Insufficient data: %d" % len(data))
    if len(data[1:]) / 4 != length:
        raise ValueError("Missing ref info")
    ret = Ref(node, data[1:], data[0])
    return ret, data[1 + (4 * length):]


@tag(115)
@ulen(1)
def _small_atom(length, data):
    return Atom(data[:length]), data[length:]


@tag(117)
@ulen(4)
def _fun(num_free, data):
    pid, data = _parse(data)
    mod, data = _parse(data)
    idx, data = _parse(data)
    unq, data = _parse(data)
    free = []
    for i in range(num_free):
        term, data = _parse(data)
        free.append(term)
    return Fun(pid, mod, idx, unq, free), data


def _parse(data):
    tag = ord(data[0])
    fun = _PARSER_FUNS.get(tag)
    if fun is None:
        raise ValueError("Invalid element tag: %d" % tag)
    return fun(data[1:])


def parse(data):
    if ord(data[0]) != 131:
        raise ValueError("Invalid magic byte: %d" % ord(data[0]))
    if ord(data[1]) == 80:
        ret, tail = _parse(zlib.decompress(data[6:]))
    else:
        ret, tail =  _parse(data[1:])
    if len(tail) != 0:
        raise ValueError("Trailing garbage bytes: %d" % len(tail))
    return ret
