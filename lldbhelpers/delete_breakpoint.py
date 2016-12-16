import lldb


def delete_breakpoint(debugger, command, result, internal_dict):
    target = debugger.GetSelectedTarget()
    process = target.GetProcess()
    thread = process.GetSelectedThread()

    if thread.GetStopReason() != lldb.eStopReasonBreakpoint:
        return

    count = thread.GetStopReasonDataCount()
    assert(count == 2)

    breakpoint_id = thread.GetStopReasonDataAtIndex(0)
    location_id = thread.GetStopReasonDataAtIndex(1)

    breakpoint = target.FindBreakpointByID(breakpoint_id)
    if not breakpoint:
        print("Breakpoint '{}.{}' not found"
              .format(breakpoint_id, location_id))
        return

    location = breakpoint.FindLocationByID(location_id)
    if not location:
        print("Location '{}.{}' not found".format(breakpoint_id, location_id))
        return

    location.SetEnabled(False)


def __lldb_init_module(debugger, internal_dict):
    handle = debugger.HandleCommand
    handle("command script add -f delete_breakpoint.delete_breakpoint db")
