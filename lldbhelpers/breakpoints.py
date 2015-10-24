import lldb

NAME = "exceptions"


def add(debugger, command, result, internal_dict):
    debugger.HandleCommand("breakpoint delete %s" % NAME)
    debugger.HandleCommand("breakpoint set -E c++ -N %s" % NAME)
    debugger.HandleCommand("breakpoint set -E objc -N %s" % NAME)
    debugger.HandleCommand("breakpoint set -E swift -N %s" % NAME)


def register_handlers(debugger, namespace_name):
    lldb.debugger.HandleCommand('command script add -f breakpoints.add exceptions')


def __lldb_init_module(debugger, internal_dict):
    register_handlers(debugger, __name__)
