import lldb
import re

SYMBOL_REGEX = re.compile("^([+-])\[([^\s\]\(]+)(\([^\s]+\))?\s+([^\s\]]+)\]$")


def output_for_command(debugger, command):
    interpreter = debugger.GetCommandInterpreter()
    result = lldb.SBCommandReturnObject()
    interpreter.HandleCommand(command, result)

    if result.GetStatus() == 2:
        return result.GetOutput()
    else:
        return ""


def address_for_command(debugger, command):
    output = output_for_command(debugger, command).split()

    try:
        return int(output[-1], 16)
    except IndexError:
        return 0


def get_command(function, *args):
    arguments = ", ".join([str(x) for x in args])
    return "expr -l objc++ -- (void *){}({})".format(function, arguments)


def address_for_function(debugger, command, result, internal_dict):
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

    get_method_command = get_command(
        get_method_function, class_argument, selector_argument
    )
    method_address = address_for_command(debugger, get_method_command)
    if method_address == 0:
        print("'%s' doesn't exist" % signature)
        return

    get_implementation_command = get_command(
        "method_getImplementation", method_address
    )
    print(hex(address_for_command(debugger, get_implementation_command)))


def __lldb_init_module(debugger, internal_dict):
    handle = debugger.HandleCommand
    handle("command script add -f function_address.address_for_function aff")
