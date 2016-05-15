import lldb

INTEL_ARGS_REGISTERS = [
    "rdx", "rcx", "r8", "r9",
]


def class_from_reciever(reciever):
    return reciever.split()[0].strip("<:")


def selector_from_register(register):
    return register.split()[-1].strip('"')


def output_for_command(debugger, command):
    interpreter = debugger.GetCommandInterpreter()
    result = lldb.SBCommandReturnObject()
    interpreter.HandleCommand(command, result)

    if result.GetStatus() == 2:
        return result.GetOutput()
    else:
        return ""


def objc_context(debugger, command, result, internal_dict):
    reciever = output_for_command(debugger, "po $rdi").strip()
    selector = selector_from_register(output_for_command(debugger, "x/s $rsi"))
    number_of_arguments = selector.count(":")
    labels = selector.split(":")
    body = "Reciever: {} Selector: {}\nArgs:\n".format(reciever, selector)
    for i in range(number_of_arguments):
        register = INTEL_ARGS_REGISTERS[i]
        arg = output_for_command(debugger, "po $%s" % register).strip()
        body += "\n{}: {}\n".format(labels[i], arg)

    body += "\n" + output_for_command(debugger, "disassemble -c 5")

    print(body)


def __lldb_init_module(debugger, internal_dict):
    handle = debugger.HandleCommand
    handle('command script add -f context.objc_context objc_context')
