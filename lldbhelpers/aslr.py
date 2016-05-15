import lldb
import re


def ASLR(debugger, command, result, internal_dict, debug=True):
    """
    Returns the offset of the first image address (ASLR offset).
    """
    interpreter = debugger.GetCommandInterpreter()

    return_obj = lldb.SBCommandReturnObject()
    interpreter.HandleCommand('image list -o', return_obj)

    output = return_obj.GetOutput().split("\n")[0]
    match = re.match(r'.+(0x[0-9a-fA-F]+)', output)
    if debug and match:
        print "ASLR offset is:", match.group(1)

    return int(match.group(1), 16) if match else None


def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('command script add -f aslr.ASLR aslr')
