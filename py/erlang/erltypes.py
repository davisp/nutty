
class ErlType(object):
    pass


class Atom(ErlType, str):
    pass


class BitString(ErlType, str):
    def bits(self, n):
        self._bits = n
        return self


class Ref(ErlType):
    def __init__(self, node, id, creation):
        self.node = node
        self.id = rid
        self.creation = creation


class Port(ErlType):
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id
        self.creation = creation


class Pid(ErlType):
    def __init__(self, node, id, serial, creation):
        self.node = node
        self.id = id
        self.serial = serial
        self.creation = creation


class Fun(ErlType):
    def __init__(self, pid, mod, idx, unq, free):
        self.pid = pid
        self.mod = mod
        self.idx = idx
        self.unq = unq
        self.free = free


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


class ExpFun(ErlType):
    def __init__(self, mod, fun, arity):
        self.mod = mod
        self.fun = fun
        self.arity = arity


class Nil(ErlType):
    pass

