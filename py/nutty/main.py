
import os
import pkgutil
import sys


import nose.core


def run():
    nose.core.run()


def _nose_usage(cls):
    return pkgutil.get_data("nose.core", "usage.txt")
nose.core.TestProgram.usage = classmethod(_nose_usage)
