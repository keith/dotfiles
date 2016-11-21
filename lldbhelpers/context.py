import lldb


def objc_context(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    process = target.GetProcess()
    thread = process.GetSelectedThread()
    frame = thread.GetFrameAtIndex(0)

    print frame.GetFunctionName()
    print
    print frame.get_arguments()
    print
    disassembly = frame.Disassemble()

    try:
        import pygments.lexers
        import pygments.formatters
        print(pygments.highlight(disassembly, pygments.lexers.NasmLexer(),
                                 pygments.formatters.TerminalFormatter()))
    except ImportError:
        print(disassembly)


def __lldb_init_module(debugger, internal_dict):
    handle = debugger.HandleCommand
    handle("command script add -f context.objc_context objc_context")
