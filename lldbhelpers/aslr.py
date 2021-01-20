import lldb


def aslr_for_module(target, module):
    """
    Get the aslr offset for a specific module

    - parameter target: lldb.SBTarget that is currently being debugged
    - parameter module: lldb.SBModule to find the offset for

    - returns: the offset as an int
    """
    header_address = module.GetObjectFileHeaderAddress()
    load_address = header_address.GetLoadAddress(target)
    return load_address - header_address.GetFileAddress()


@lldb.command()
def aslr(debugger, _ignored, context, result, _):
    """
    Prints the aslr offset for the executable image being debugged

    NOTE: for executables launched with lldb this will be 0x0 since aslr is
          automatically disabled in that case
    """
    target = debugger.GetSelectedTarget()
    module = target.FindModule(target.GetExecutable())
    offset = hex(aslr_for_module(target, module))

    result.AppendMessage(offset)
    result.SetStatus(lldb.eReturnStatusSuccessFinishResult)
