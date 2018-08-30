import lldb
import re


def seq(debugger, commands, context, result, _):
    """
    (lldb) seq command1; command2; ...

    Run multiple separate lldb commands in order, and print the output
    """
    interpreter = debugger.GetCommandInterpreter()
    for command in re.split(r"; *", commands):
        return_value = lldb.SBCommandReturnObject()
        interpreter.HandleCommand(command, context, return_value)
        print >> result, return_value
        if not return_value.Succeeded():
            break


def __lldb_init_module(debugger, _):
    debugger.HandleCommand("command script add -f seq.seq seq")
