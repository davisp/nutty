
import struct


import erlang


class ErlType(object):
    def to_binary(self):
        raise NotImplementedError()


class AtomType(type):
    def __getattr__(cls, key):
        return cls(key)


class Atom(ErlType, str):
    __metaclass__ = AtomType

    def to_binary(self):
        if len(self) > 255:
            raise ValueError("Atom value too long: %d" % len(self))
        return chr(100) + struct.pack("!H", len(self)) + self


class BitString(ErlType, str):
    def bits(self, n):
        self._bits = n
        return self

    def to_binary(self):
        return chr(77) + struct.pack("!I", len(self)) + chr(self._bits) + self


class Ref(ErlType):
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id
        self.creation = creation

    def __eq__(self, o):
        return _erl_eq(Ref, self, o, "node id creation")

    def to_binary(self):
        return "".join([
            chr(114),
            struct.pack("!H", len(self.id) / 4),
            erlang.serialize(self.node, False),
            self.creation,
            self.id
        ])


class Port(ErlType):
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id
        self.creation = creation

    def __eq__(self, o):
        return _erl_eq(Port, self, o, "node id creation")

    def to_binary(self):
        return "".join([
            chr(102),
            erlang.serialize(self.node, False),
            self.id,
            self.creation
        ])


class Pid(ErlType):
    def __init__(self, node, id, serial, creation):
        self.node = node
        self.id = id
        self.serial = serial
        self.creation = creation

    def __eq__(self, o):
        return _erl_eq(Pid, self, o, "node id serial creation")

    def to_binary(self):
        return "".join([
            chr(103),
            erlang.serialize(self.node, False),
            self.id,
            self.serial,
            self.creation
        ])


class Fun(ErlType):
    def __init__(self, pid, mod, idx, unq, free):
        self.pid = pid
        self.mod = mod
        self.idx = idx
        self.unq = unq
        self.free = free

    def __eq__(self, o):
        return _erl_eq(Fun, self, o, "pid mod idx unq free")

    def to_binary(self):
        ret = [
            chr(117),
            struct.pack("!I", len(self.free)),
            erlang.serialize(self.pid, False),
            erlang.serialize(self.mod, False),
            erlang.serialize(self.idx, False),
            erlang.serialize(self.unq, False)
        ] + map(lambda f: erlang.serialize(f, False), self.free)
        return "".join(ret)


class NewFun(ErlType):
    def __init__(self, arity, unq, idx, mod, oldidx, oldunq, pid, free):
        self.arity = arity
        self.unq = unq
        self.idx = idx
        self.mod = mod
        self.oldidx = oldidx
        self.oldunq = oldunq
        self.pid = pid
        self.free = free

    def __eq__(self, o):
        return _erl_eq(NewFun, self, o,
                        "arity unq idx mod oldidx oldunq pid free")

    def to_binary(self):
        ret = [
            chr(self.arity),
            self.unq,
            struct.pack("!I", self.idx),
            struct.pack("!I", len(self.free)),
            erlang.serialize(self.mod, False),
            erlang.serialize(self.oldidx, False),
            erlang.serialize(self.oldunq, False),
            erlang.serialize(self.pid, False)
        ] + map(lambda f: erlang.serialize(f, False), self.free)
        ret = "".join(ret)
        return chr(112) + struct.pack("!I", len(ret) + 4) + ret


class ExpFun(ErlType):
    def __init__(self, mod, fun, arity):
        self.mod = mod
        self.fun = fun
        self.arity = arity

    def __eq__(self, o):
        return _erl_eq(ExpFun, self, o, "mod fun arity")

    def to_binary(self):
        return "".join([
            chr(113),
            erlang.serialize(self.mod, False),
            erlang.serialize(self.fun, False),
            erlang.serialize(self.arity, False)
        ])


class Nil(ErlType):
    def to_binary(self):
        return chr(106)


def _erl_eq(cls, a, b, attrs):
    if not isinstance(a, cls):
        return False
    if not isinstance(b, cls):
        return False
    for attr in attrs.split():
        # Slightly awkard to force the use of the
        # __eq__ comparison
        if getattr(a, attr) == getattr(b, attr):
            continue
        return False
    return True

