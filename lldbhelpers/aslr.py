import lldb


def _aslr_for_module(target, module):
    """
    Get the aslr offset for a specific module

    :param target: the lldb.SBTarget that is currently being debugged
    :param module: the lldb.SBModule to find the offset for
    :returns: the offset as an int, this should be converted to hex for display
    """
    header_address = module.GetObjectFileHeaderAddress()
    load_address = header_address.GetLoadAddress(target)
    return load_address - header_address.GetFileAddress()


def ASLR(debugger, command, result, internal_dict):
    """
    (lldb) aslr

    Returns the aslr offset for the executable image being debugged

    Note: for executables launched with lldb this will be 0x0
    """
    target = debugger.GetSelectedTarget()
    module = target.FindModule(target.GetExecutable())
    offset = hex(_aslr_for_module(target, module))

    result.AppendMessage(offset)
    result.SetStatus(lldb.eReturnStatusSuccessFinishResult)


def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand("command script add -f aslr.ASLR aslr")
