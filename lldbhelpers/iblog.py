import lldb
import re


class ModuleNotFound(Exception):
    pass


class SectionNotFound(Exception):
    pass


def _extract_address_from(memory_string):
    result = re.match(r"^data found at location: (0x[0-9a-fA-F]+)$",
                      memory_string, re.MULTILINE)
    return result.group(1)


def _find_section(module, section_name, segment_name):
    for section in module.section_iter():
        if section.GetName() != section_name:
            continue

        for subsection in section:
            if subsection.GetName() != segment_name:
                continue

            return subsection

    raise SectionNotFound()


def _find_module(target, name):
    for module in target.module_iter():
        if module.GetFileSpec().GetFilename() == name:
            return module

    raise ModuleNotFound()


def _output_for_command(debugger, command):
    interpreter = debugger.GetCommandInterpreter()
    result = lldb.SBCommandReturnObject()
    interpreter.HandleCommand(command, result)

    if result.Succeeded():
        return str(result.GetOutput())
    else:
        return ""


def iblog(debugger, command, context, result, internal_dict):
    target = context.GetTarget()
    module = _find_module(target, "UIKit")
    section = _find_section(module, "__TEXT", "__cstring")
    load_address = section.GetLoadAddress(target)
    end_address = load_address + section.GetByteSize()

    frame = context.GetFrame()
    cmd = """memory find --count 1 --string \
'Could not load the "%@" image referenced from a nib in the bundle \
with identifier "%@"' {} {}""".format(hex(load_address), hex(end_address))

    output = _output_for_command(debugger, cmd)
    string_addr = _extract_address_from(output)
    debugger.HandleCommand(
        "br set --name NSLog --condition '(void *)[$arg1 cString] == {}'"
        .format(string_addr))


def __lldb_init_module(debugger, internal_dict):
    handle = debugger.HandleCommand
    handle("command script add -f iblog.iblog iblog")
