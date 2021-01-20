import lldb


@lldb.command()
def bhere(debugger, _ignored, context, result, _):
    pc = exe_ctx.frame.GetPC()
    debugger.HandleCommand("breakpoint set -a {}".format(pc))
