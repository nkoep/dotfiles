import sys
import os

if sys.version_info.major == 3:
    from importlib import import_module

    globals_ = globals()
    for module in "GObject GLib Gio Gtk Gst".split():
        try:
            globals_[module] = import_module("gi.repository.%s" % module)
        except ImportError:
            pass

