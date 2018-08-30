NAME = "exceptions"


def add(debugger, command, result, internal_dict):
    debugger.HandleCommand("breakpoint delete %s" % NAME)
    debugger.HandleCommand("breakpoint set -E c++ -N %s" % NAME)
    debugger.HandleCommand("breakpoint set -E objc -N %s" % NAME)
    debugger.HandleCommand("breakpoint set -E swift -N %s" % NAME)


def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand("command script add -f breakpoints.add exceptions")
