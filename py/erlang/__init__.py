

import erlang.parser
import erlang.serializer

from erlang.erltypes import Atom
from erlang.vm import VM


def parse(data):
    return erlang.parser.parse(data)


def serialize(term, include_magic=True):
    ret = erlang.serializer.serialize(term)
    if not include_magic:
        return ret
    return chr(131) + ret
