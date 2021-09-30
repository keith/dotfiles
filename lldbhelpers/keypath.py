import lldb


class KeypathException(Exception):
    pass


def _run_command(debugger, command):
    interpreter = debugger.GetCommandInterpreter()
    result = lldb.SBCommandReturnObject()
    interpreter.HandleCommand(command, result)

    if not result.Succeeded():
        raise KeypathException(result.GetError())


def _is_swift_frame(frame):
    language = frame.GetFunction().GetLanguage()
    return language == lldb.eLanguageTypeSwift


def _swift_statement_for_target(target):
    if target.startswith("0x"):
        return "unsafeBitCast({}, to: NSObject.self)".format(target)

    return target


def _run_swift_command(debugger, result, target, path):
    swift_command = '{}.value(forKeyPath: "{}")'.format(
        _swift_statement_for_target(target), path
    )

    debugger_command = "po "
    debugger_command += "if let value = %s {" % swift_command
    debugger_command += "    print(value)"
    debugger_command += "} else {"
    debugger_command += '    print("nil")'
    debugger_command += "}"

    try:
        _run_command(debugger, debugger_command)
        result.SetStatus(lldb.eReturnStatusSuccessFinishResult)
    except KeypathException as e:
        result.AppendWarning(str(e))
        result.SetStatus(lldb.eReturnStatusFailed)


def _run_objc_command(debugger, result, target, path):
    objc_command = '[(id){} valueForKeyPath:@"{}"]'.format(target, path)
    debugger_command = "po "
    debugger_command += "id value = {};".format(objc_command)
    debugger_command += 'printf("%s\\n",'
    debugger_command += "[(NSString *)[value debugDescription]"
    debugger_command += "                    UTF8String]);"

    try:
        _run_command(debugger, debugger_command)
        result.SetStatus(lldb.eReturnStatusSuccessFinishResult)
    except KeypathException as e:
        result.AppendWarning(str(e))
        result.SetStatus(lldb.eReturnStatusFailed)


@lldb.command()
def keypath(debugger, command, context, result, _):
    """
    Print the value for the given keypath on the given object from Objective-C
    or Swift

    (lldb) keypath self.foo.bar
      output or nil

    (lldb) keypath 0x1234.foo.bar
      output or nil
    """
    command = command.strip()

    if "." not in command:
        result.AppendWarning("Invalid command, must contain `.`")
        result.SetStatus(lldb.eReturnStatusFailed)
        return

    parts = command.rsplit(".", 1)
    target = parts[0].strip()
    path = parts[1].strip()

    if _is_swift_frame(context.frame):
        _run_swift_command(debugger, result, target, path)
    else:
        _run_objc_command(debugger, result, target, path)
