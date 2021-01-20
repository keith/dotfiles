import lldb
import re

SYMBOL_REGEX = re.compile("^([+-])\[([^\s\]\(]+)(\([^\s]+\))?\s+([^\s\]]+)\]$")


def _output_for_command(debugger, command):
    interpreter = debugger.GetCommandInterpreter()
    result = lldb.SBCommandReturnObject()
    interpreter.HandleCommand(command, result)

    if result.GetStatus() == 2:
        return result.GetOutput()
    else:
        return ""


def _address_for_command(debugger, command):
    output = _output_for_command(debugger, command).split()

    try:
        return int(output[-1], 16)
    except IndexError:
        return 0


def _get_command(function, *args):
    arguments = ", ".join([str(x) for x in args])
    return "expr -l objc++ -- (void *){}({})".format(function, arguments)


@lldb.command()
def aff(debugger, command, context, result, _):
    signature = command.strip()
    match = SYMBOL_REGEX.match(signature)
    if not match:
        print("Usage: +/-[Class selector:]")
        return

    scope_identifier = match.group(1)
    class_name = match.group(2)
    selector_name = match.group(4)

    class_argument = '(id)NSClassFromString(@"%s")' % class_name
    selector_argument = "@selector(%s)" % selector_name

    get_method_function = "class_getInstanceMethod"
    if scope_identifier == "+":
        get_method_function = "class_getClassMethod"

    get_method_command = _get_command(
        get_method_function, class_argument, selector_argument
    )
    method_address = _address_for_command(debugger, get_method_command)
    if method_address == 0:
        print("'%s' doesn't exist" % signature)
        return

    get_implementation_command = _get_command(
        "method_getImplementation", method_address
    )
    print(hex(_address_for_command(debugger, get_implementation_command)))
