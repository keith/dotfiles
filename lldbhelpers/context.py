import lldb


def _output_for_command(debugger, command):
    interpreter = debugger.GetCommandInterpreter()
    result = lldb.SBCommandReturnObject()
    interpreter.HandleCommand(command, result)

    if result.Succeeded():
        return str(result.GetOutput())
    else:
        return ""


def _index_of(target, function):
    for i, x in enumerate(target):
        if function(x):
            return i

    return None


@lldb.command()
def objc_context(debugger, _ignored, context, result, _):
    function_name = context.frame.GetFunctionName()
    print(function_name)
    print()

    argument_count = function_name.count(":")
    argument_names = function_name.split(" ")[-1].split(":")
    for i in range(3, 3 + argument_count):
        argument_value = _output_for_command(debugger, "po $arg{}".format(i))
        argument_name = argument_names[i - 3]
        print("{}: {}".format(argument_name, argument_value))

    disassembly = context.frame.Disassemble().split("\n")
    index = _index_of(disassembly, lambda x: x.startswith("-> "))
    assert index
    disassembly = "\n".join(disassembly[index - 1 : index + 4]).strip()

    try:
        import pygments.lexers
        import pygments.formatters

        print(
            pygments.highlight(
                disassembly,
                pygments.lexers.NasmLexer(),
                pygments.formatters.TerminalFormatter(),
            )
        )
    except ImportError:
        print(disassembly)
