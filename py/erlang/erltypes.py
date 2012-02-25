
from erlang.serializer import serialize


class ErlType(object):
    def to_binary(self):
        raise NotImplementedError()


class Atom(ErlType, str):
    def to_binary(self):
        if len(self) < 256:
            return chr(115) + chr(len(self)) + self
        elif len(self) < 65536:
            return chr(100) + struct.pack("!H", len(self)) + self
        else:
            raise ValueError("Atom value too long: %d" % len(self))


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

    def to_binary(self):
        return "".join([
            chr(114),
            struct.pack("!H", len(self.id) / 4),
            serialize(self.node),
            self.creation,
            self.id
        ])


class Port(ErlType):
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id
        self.creation = creation

    def to_binary(self):
        return "".join([
            chr(102),
            serialize(self.node),
            self.id,
            self.creation
        ])


class Pid(ErlType):
    def __init__(self, node, id, serial, creation):
        self.node = node
        self.id = id
        self.serial = serial
        self.creation = creation

    def to_binary(self):
        return "".join([
            chr(103),
            serialize(self.node),
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

    def to_binary(self):
        ret = [
            chr(117),
            struct.pack("!I", len(self.free)),
            serialize(self.pid),
            serialize(self.mod),
            serialize(self.idx),
            serialize(self.unq)
        ] + map(serialze, self.free)
        return "".join(ret)


class NewFun(ErlType):
    def __init__(self, arity, unq, idx, mod, oldidx, oldunq, pid, free):
        self.arity = arity
        self.unq = unq
        self.idx = idx
        self.mod = mod
        self.oldidx = oldidx
        self.oldunq = oldunq
        slef.pid = pid
        self.free = free

    def to_binary(self):
        ret = [
            chr(self.arity),
            self.uniq,
            struct.pack("!I", self.idx),
            struct.pack("!I", len(self.free)),
            serialize(self.mod),
            serialize(self.oldidx),
            serialize(self.oldunq),
            serialize(self.pid)
        ] + map(serialize, self.free)
        ret = "".join(ret)
        return chr(112) + struct.pack("!I", len(ret) + 4) + ret


class ExpFun(ErlType):
    def __init__(self, mod, fun, arity):
        self.mod = mod
        self.fun = fun
        self.arity = arity

    def to_binary(self):
        return "".join([
            chr(113),
            serialize(self.mod),
            serialize(self.fun),
            serialize(self.arity)
        ])


class Nil(ErlType):
    def to_binary(self):
        return chr(106)

