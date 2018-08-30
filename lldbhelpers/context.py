import lldb


def output_for_command(debugger, command):
    interpreter = debugger.GetCommandInterpreter()
    result = lldb.SBCommandReturnObject()
    interpreter.HandleCommand(command, result)

    if result.Succeeded():
        return str(result.GetOutput())
    else:
        return ""


def index_of(target, function):
    for i, x in enumerate(target):
        if function(x):
            return i

    return None


def objc_context(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    process = target.GetProcess()
    thread = process.GetSelectedThread()
    frame = thread.GetSelectedFrame()

    function_name = frame.GetFunctionName()
    print(function_name)
    print

    argument_count = function_name.count(":")
    argument_names = function_name.split(" ")[-1].split(":")
    for i in range(3, 3 + argument_count):
        argument_value = output_for_command(debugger, "po $arg{}".format(i))
        argument_name = argument_names[i - 3]
        print("{}: {}".format(argument_name, argument_value))

    disassembly = frame.Disassemble().split("\n")
    index = index_of(disassembly, lambda x: x.startswith("-> "))
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


def __lldb_init_module(debugger, internal_dict):
    handle = debugger.HandleCommand
    handle("command script add -f context.objc_context objc_context")
