# https://gist.github.com/kastiglione/d7fc5b3b3ebab1248333b858591e06b0
import lldb
import os

kNoResult = 0x1001


@lldb.command("load_swift")
def load_swift(debugger, path, ctx, result, _):
    with open(os.path.expanduser(path)) as f:
        contents = f.read()

    options = lldb.SBExpressionOptions()
    options.SetLanguage(lldb.eLanguageTypeSwift)
    error = ctx.frame.EvaluateExpression(contents, options).error

    if error.fail and error.value != kNoResult:
        result.SetError(error)
