

import erlang.parser
import erlang.serializer

from erlang.erltypes import Atom
from erlang.vm import VM


def parse(data):
    return erlang.parser.parse(data)


def serialize(term):
    ret = erlang.serializer.serialize(term)
    return chr(131) + ret
