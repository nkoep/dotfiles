import sys
import os

if sys.version_info.major == 2:
    import numpy as np
    import numpy.linalg as la
    import numpy.random as rnd
else:
    import gi
    from importlib import import_module

    modules = [("GObject", None), ("GLib", None) , ("Gio", None),
               ("Gtk", "3.0"), ("Gst", "1.0")]

    globals_ = globals()
    for module, version in modules:
        if version is not None:
            try:
                gi.require_version(module, version)
            except ValueError:
                continue
        try:
            globals_[module] = import_module("gi.repository.%s" % module)
        except ImportError:
            pass

