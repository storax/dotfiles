from __future__ import print_function

import sys

import rlcompleter
import pdb
pdb.Pdb.complete = rlcompleter.Completer(locals()).complete

# return to debugger after fatal exception (Python cookbook 14.5):
def info(type, value, tb):
    if hasattr(sys, 'ps1') or not sys.stderr.isatty():
        sys.__excepthook__(type, value, tb)
    import traceback, pdb
    traceback.print_exception(type, value, tb)
    print
    pdb.pm()

sys.excepthook = info

# Cleanup any variables that could otherwise clutter up the namespace.
try:
    del info
    del pdb
    del rlcompleter
except NameError:
    # Probably this is a second pdbrc that has been loaded.
    pass
