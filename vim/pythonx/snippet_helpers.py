import re
import vim


def objc_comment_block():
    line_num = current_line_number() + 1
    method = line_at_number(line_num)
    r = re.compile(":\s*\(\w+\s*\*?\)\s*(\w+)")
    while "{" not in method:
        line_num += 1
        method += " " + line_at_number(line_num)

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


def line_at_number(num):
    return vim.eval("getline('%s')" % num)
