import lldb

_NAME = "exceptions"


@lldb.command()
def exceptions(debugger, _ignored, context, result, _):
    debugger.HandleCommand(f"breakpoint delete {_NAME}")
    debugger.HandleCommand(f"breakpoint set -E c++ -N {_NAME}")
    debugger.HandleCommand(f"breakpoint set -E objc -N {_NAME}")
    debugger.HandleCommand(f"breakpoint set -E swift -N {_NAME}")
