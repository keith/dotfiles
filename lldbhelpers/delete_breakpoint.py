import lldb


@lldb.command()
def db(debugger, _ignored, context, result, _):
    target = debugger.GetSelectedTarget()
    thread = context.thread

    if thread.GetStopReason() != lldb.eStopReasonBreakpoint:
        return

    count = thread.GetStopReasonDataCount()
    assert count == 2

    breakpoint_id = thread.GetStopReasonDataAtIndex(0)
    location_id = thread.GetStopReasonDataAtIndex(1)

    breakpoint = target.FindBreakpointByID(breakpoint_id)
    if not breakpoint:
        result.SetError(
            "Breakpoint '{}.{}' not found".format(breakpoint_id, location_id)
        )
        return

    location = breakpoint.FindLocationByID(location_id)
    if not location:
        result.SetError(
            "Location '{}.{}' not found".format(breakpoint_id, location_id)
        )
        return

    location.SetEnabled(False)
    assert not location.IsEnabled()
    result.SetStatus(lldb.eReturnStatusSuccessContinuingResult)
    debugger.HandleCommand("continue")
