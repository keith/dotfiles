import aslr
import lldb


def _address_for_symbol(target, symbol_context, include_symbol):
    symbol = symbol_context.GetSymbol()
    module = symbol_context.GetModule()
    offset = aslr._aslr_for_module(target, module)
    address = hex(offset + symbol.GetStartAddress().GetFileAddress())

    if include_symbol:
        return "{}: {}".format(symbol.GetName(), address)
    else:
        return "{}".format(address)


def address_for_symbol(debugger, command, result, internal_dict):
    """
    Get the address for a symbol regardless of the ASLR offset

    (lldb) afs viewDidLoad
    -[UIViewController viewDidLoad]: 0x1146c79a4
    ...

    (lldb) afs -[MyViewController viewDidLoad]:
    0x1146c79a4
    """
    # TODO: Allow user to pass module for symbol
    symbol = command.strip()
    target = debugger.GetSelectedTarget()
    functions = target.FindFunctions(symbol)

    if len(functions) == 0:
        result.AppendWarning("No symbol found for '{}'".format(symbol))
        result.SetStatus(lldb.eReturnStatusFailed)
    elif len(functions) == 1:
        result.AppendMessage(_address_for_symbol(target, functions[0], False))
        result.SetStatus(lldb.eReturnStatusSuccessFinishResult)
    else:
        for context in functions:
            result.AppendMessage(_address_for_symbol(target, context, True))
        result.SetStatus(lldb.eReturnStatusSuccessFinishResult)


def __lldb_init_module(debugger, internal_dict):
    handle = debugger.HandleCommand
    handle('command script add -f symbol_address.address_for_symbol afs')
