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

# From (on my machine) /usr/local/lib/python2.3/less user.py:
import os
home = os.curdir                        # Default
if 'HOME' in os.environ:
    home = os.environ['HOME']
elif os.name == 'posix':
    home = os.path.expanduser("~/")
elif os.name == 'nt':                   # Contributed by Jeff Bauer
    if 'HOMEPATH' in os.environ:
        if 'HOMEDRIVE' in os.environ:
            home = os.environ['HOMEDRIVE'] + os.environ['HOMEPATH']
        else:
            home = os.environ['HOMEPATH']
# Make sure home always ends with a directory separator:
home = os.path.realpath(home) + os.sep

# Try to execute local file (if any) unless we are in the home dir.
if home != os.path.realpath(os.curdir) + os.sep:
    # Avoid recursively loading this file.
    try:
        _already_loaded += 1
    except NameError:
        _already_loaded = 1
    if _already_loaded < 3:
        try:
            if sys.version_info[0] == 2:
                execfile("pdbrc.py")
            else:
                exec(open("pdbrc.py").read())
        except IOError:
            pass

# Cleanup any variables that could otherwise clutter up the namespace.
try:
    del home
    del info
    del os
    del pdb
    del rlcompleter
    # careful here:
    del _already_loaded
except NameError:
    # Probably this is a second pdbrc that has been loaded.
    pass
