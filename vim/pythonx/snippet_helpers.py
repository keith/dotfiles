import re
import vim


def objc_comment_block():
    line_num = current_line_number() + 1
    lines = line_in_range(line_num, line_num + 5)
    method = lines[0]
    return lines
    if not (method.startswith("-") or method.startswith("+")):
        return ""

    while "{" not in method and line_num < len(lines):
        line_num += 1
        print "getting line " + line_num
        method += " " + lines[line_num]

    r = re.compile(":\s*\(\w+\s*\*?\)\s*(\w+)")
    variables = r.findall(method)
    return comment_block(variables_string(variables))


def variables_string(variables):
    string = ""
    for v in variables:
        string += " *  @param %s    TODO\n" % v

    return string


def comment_block(variables):
    comment = "/**\n *  TODO\n *\n%s *\n *  @return TODO\n */" % variables
    return comment


def current_line_number():
    return int(vim.eval("line('.')"))


def line_in_range(start, end):
    return vim.eval("getline(%d, %d)" % (start, end))
