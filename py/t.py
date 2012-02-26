

def eq(a, b):
    assert a == b, "%r != %r" % (a, b)


def ne(a, b):
    assert a != b, "%r == %r" % (a, b)


def lt(a, b):
    assert a < b, "%r >= %r" % (a, b)


def gt(a, b):
    assert a > b, "%r <= %r" % (a, b)


def isin(a, b):
    assert a in b, "%r is not in %r" % (a, b)


def isnotin(a, b):
    assert a not in b, "%r is in %r" % (a, b)


def has(a, b):
    assert hasattr(a, b), "%r has no attribute %r" % (a, b)


def hasnot(a, b):
    assert not hasattr(a, b), "%r has an attribute %r" % (a, b)


def raises(etype, func, *args, **kwargs):
    try:
        func(*args, **kwargs)
    except etype, inst:
        return
    else:
        fname = getattr(func, "func_name", "<builtin_function>")
        ename = getattr(etype, "__name__", "<unknown type>")
        raise AssertionError("Function %s did not raise %s" % (fname, ename))

