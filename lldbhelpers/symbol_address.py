import aslr
import lldb


def _address_for_symbol(target, symbol_context, include_symbol):
    symbol = symbol_context.GetSymbol()
    function = symbol_context.GetFunction()
    start_address = None
    name = None

    # Try to fetch the symbol's address first, and fallback to the function's
    # It seems as though symbols contexts fetched with the "full" option (at
    # least for swift symbols) contain an invalid symbol, but a valid function,
    # yet functions fetched in other ways, have the opposite, a valid symbol,
    # and invalid function. Now we check to see if they're valid, fetching the
    # necessary properties for both, and fallback to failing entirely
    if symbol.IsValid():
        start_address = symbol.GetStartAddress()
        name = symbol.GetName()
    elif function.IsValid():
        start_address = function.GetStartAddress()
        name = function.GetDisplayName()
    else:
        raise Exception("Neither function nor symbol is valid")

    if not start_address.IsValid():
        raise Exception("Start address is invalid")

    module = symbol_context.GetModule()
    offset = aslr.aslr_for_module(target, module)
    address = hex(offset + start_address.GetFileAddress())

    if include_symbol:
        return "{}: {}".format(name, address)
    else:
        return "{}".format(address)


@lldb.command()
def afs(debugger, command, context, result, _):
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
    if "?" in symbol:
        result.AppendWarning(
            "Symbol must not have Swift syntax sugar '{}'".format(symbol)
        )
        result.SetStatus(lldb.eReturnStatusFailed)
        return

    target = debugger.GetSelectedTarget()

    # Search for functions with both the original, and auto, arguments
    # The default to this argument is "any" yet it seems as though you
    # don't actually search for any. For example:
    # argument | swift symbol                               | result
    # ---------+--------------------------------------------+-------------
    # any      | ViewController.viewDidLoad                 | valid symbol
    # any      | module.ViewController.viewDidLoad () -> () | none
    # full     | ViewController.viewDidLoad                 | none
    # full     | module.ViewController.viewDidLoad () -> () | valid symbol
    #
    # In this case, I would expect any + a full symbol to still return
    # the full symbol. This is also supposed to be a bitwise field, so I've
    # also tried passing multiple options, with no success
    functions = target.FindFunctions(symbol, lldb.eFunctionNameTypeFull)
    for symbol_context in target.FindFunctions(symbol):
        functions.Append(symbol_context)

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
