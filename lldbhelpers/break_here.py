def bhere(debugger, command, context, result, internal_dict):
    frame = context.GetFrame()
    pc = frame.GetPC()
    debugger.HandleCommand("breakpoint set -a {}".format(pc))


def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand("command script add -f break_here.bhere bhere")
