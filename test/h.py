
import functools


import erlang


class vm(object):
    def __init__(self, *args, **kwargs):
        self.args = args
        self.kwargs = kwargs
    def __call__(self, fun):
        @functools.wraps(fun)
        def _wrapper(*args, **kwargs):
            vm = erlang.VM(*self.args, **self.kwargs).boot()
            try:
                return fun(vm, *args, **kwargs)
            finally:
                vm.close()
        return _wrapper
