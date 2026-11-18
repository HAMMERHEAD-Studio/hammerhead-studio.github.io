# ============================================================
# Simple template engine
#
# usage: awk -f template.awk <page> [<name>=<value> ...]
#        <name>=<value> overrides definitions in the page and the layout.
#
# ============================================================
# Example:
#
# Define variables
#
# {{ title=ultra cool title }}
# {{ description = super cool description }}
# {{ greeting = hello }}
#
# Use variables
#
# <title>{{ title }}</title>
# <meta name="description" content="{{description}}">
# <p>{{ greeting }}</p>
# ↓
# <title>ultra cool title</title>
# <meta name="description" content="super cool description">
# <p>hello</p>
#
# Variable starts with `#` are reserved
#
# {{ #include=<path> }}
# {{ #layout=<path> }}
# {{ #content }}
# ============================================================

function fail(msg) {
  if (path_stack["\001"] > 0) {
    msg = prettify_path(path_stack[path_stack["\001"] - 1]) ": " msg
  }
  print msg | "cat 1>&2"
  close("cat 1>&2")
  exit 1
}

function strip_head(s) {
  gsub(/^[[:space:]]+/, "", s)
  return s
}

function strip_tail(s) {
  gsub(/[[:space:]]+$/, "", s)
  return s
}

function strip(s) {
  return strip_head(strip_tail(s))
}

function skip_line(s,    c) {
  while (s != "") {
    c = substr(s, 1, 1)
    if (c !~ /[[:space:]]/) { break }
    s = substr(s, 2)
    if (c == "\n") { break }
  }
  return s
}

function push_stack(stack, value) {
  stack[stack["\001"]] = value
  stack["\001"] = stack["\001"] + 1
}

function pop_stack(stack,    n, value) {
  n = stack["\001"] - 1
  if (n < 0) { fail("internal error: path stack underflow") }
  value = stack[n]
  delete stack[n]
  stack["\001"] = n
  return value
}

function is_valid_define_name(s) {
  if (substr(s, 1, 1) == "#") { s = substr(s, 2) }
  return s ~ /^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_]+$/
}

function prettify_path(p,    is_abs, n, a, i, m, parts, out) {
  is_abs = (substr(p, 1, 1) == "/")
  n = split(p, a, "/")
  m = 0
  for (i = 1; i <= n; i++) {
    if (a[i] == "" || a[i] == ".") { continue }
    if (a[i] == ".." && m > 0 && parts[m] != "..") {
      m--
      continue
    }
    if (a[i] == ".." && is_abs) { continue }
    m++
    parts[m] = a[i]
  }
  out = is_abs ? "/" : ""
  for (i = 1; i <= m; i++) { out = out (i > 1 ? "/" : "") parts[i] }
  return out == "" ? "." : out
}

function resolve_path(base, path,    i, dir) {
  if (path == "") { fail("empty include path") }
  if (substr(path, 1, 1) == "/") { return path }
  dir = ""
  for (i = length(base); i > 0; i--) {
    if (substr(base, i, 1) == "/") {
      dir = substr(base, 1, i)
      break
    }
  }
  if (dir == "") { dir = "./" }
  return dir path
}

function read_file(path,    s, line, r) {
  s = ""
  while ((r = (getline line < path)) > 0) {
    sub(/\r$/, "", line)
    s = s line "\n"
  }
  if (r < 0) { fail("cannot read: " path) }
  close(path)
  return s
}

# 0: found (exp_before, exp_content and exp_rest are set)
# 1: not found (exp_before = whole text)
# 2: unterminated "{{"
function take_expression(src,    start, end) {
  start = index(src, "{{")
  if (!start) {
    exp_before = src
    return 1
  }
  end = index(substr(src, start + 2), "}}")
  if (!end) { return 2 }
  exp_before = substr(src, 1, start - 1)
  exp_content = substr(src, start + 2, end - 1)
  exp_rest = substr(src, start + end +  3)
  return 0
}

# 0: found (exp_left and exp_right set)
# 1: not found
function split_expression(content, delim,    head, rest) {
  content = strip(content)
  if (!match(content, /^#?[^[:space:]|=]+/)) { return 1 }
  head = substr(content, 1, RLENGTH)
  rest = strip_head(substr(content, RLENGTH + 1))
  if (delim != "" && substr(rest, 1, length(delim)) != delim) { return 1 }
  exp_left = head
  exp_right = strip_head(substr(rest, length(delim) + 1))
  return 0
}

# 0: found     (def_before, def_name, def_value, def_rest are set)
# 1: not found (def_before = whole text)
# 2: unterminated "{{"
function take_definition(src, want,    before, rest, status) {
  before = ""
  rest = src
  while (1) {
    status = take_expression(rest)
    if (status == 1) {
      def_before = src
      return 1
    }
    if (status == 2) { return 2 }
    if (split_expression(exp_content, "=") == 0) {
      if (want == "" || exp_left == want) {
        def_before = before exp_before
        def_name = exp_left
        def_value = exp_right
        def_rest = exp_rest
        return 0
      }
    }
    before = before exp_before "{{" exp_content "}}"
    rest = exp_rest
  }
}

# ------------------------------------------------------------

function collect_variables(path, depth, variables,    inc_variables, rest, status) {
  if (depth >= MAX_DEPTH) { fail("max include depth " MAX_DEPTH " exceeded") }
  split("", inc_variables)
  rest = read_file(path)
  push_stack(path_stack, path)
  while (1) {
    status = take_definition(rest, "")
    if (status == 2) { fail("unterminated '{{'") }
    if (status == 1) {
      pop_stack(path_stack)
      return
    }
    rest = def_rest
    if (def_name == "#include") {
      split("", inc_variables)
      collect_variables(resolve_path(path, def_value), depth + 1, inc_variables)
      for (key in inc_variables) { variables[key] = inc_variables[key] }
      continue
    }
    if (def_name == "#layout") {
      if ("#layout" in variables) { fail("multiple #layout") }
      variables["#layout"] = def_value
      split("", inc_variables)
      collect_variables(resolve_path(path, def_value), 0, inc_variables)
      for (key in inc_variables) {
        if (!(key in variables)) { variables[key] = inc_variables[key] }
      }
      continue
    }
    if (!is_valid_define_name(def_name)) { fail("invalid variable name: " def_name) }
    if (substr(def_name, 1, 1) == "#") { fail("names starting with # are reserved: " def_name) }
    variables[def_name] = def_value
  }
}

function expand_expressions(path, depth, variables,    out, rest, status) {
  if (depth >= MAX_DEPTH) { fail("max include depth " MAX_DEPTH " exceeded") }
  rest = read_file(path)
  push_stack(path_stack, path)
  out = ""
  while (1) {
    status = take_expression(rest)
    if (status == 1) {
      pop_stack(path_stack)
      return out exp_before
    }
    if (status == 2) { fail("unterminated '{{'") }
    status = split_expression(exp_content, "")
    if (status == 1) { fail("invalid expression: " exp_content) }
    if (exp_right != "" && substr(exp_right, 1, 1) !~ /[|=]/) { fail("invalid expression: " exp_content) }
    if (substr(exp_right, 1, 1) == "=") {
      if (exp_left == "#include") {
        rest = skip_line(exp_rest)
        out = out exp_before
        out = out expand_expressions(resolve_path(path, strip(substr(exp_right, 2))), depth + 1, variables)
        continue
      }
      rest = skip_line(exp_rest)
      out = out exp_before
      gsub(/^\n/, "", rest)
      continue
    }
    if (exp_left in variables) {
      rest = exp_rest
      out = out exp_before variables[exp_left]
      continue
    }
    if (substr(exp_right, 1, 1) == "|") {
      rest = exp_rest
      out = out exp_before strip(substr(exp_right, 2))
      continue
    }
    fail("undefined variable: " exp_left)
  }
}

BEGIN {
  # Constants
  MAX_DEPTH = 32

  split("", path_stack)
  path_stack["\001"] = 0

  # `take_expression()` result
  exp_before = ""
  exp_content = ""
  exp_rest = ""

  # `split_expression()` result
  exp_left = ""
  exp_right = ""

  # `take_definition()` result
  def_before = ""
  def_name = ""
  def_value = ""
  def_rest = ""

  # Variable tables
  split("", variables)
  # ------------------------------------------------------------

  if (ARGC < 2 || ARGV[1] == "-h" || ARGV[1] == "--help") {
    print "usage: awk -f template.awk <page> [<name>=<value> ...]" | "cat 1>&2"
    close("cat 1>&2")
    exit 1
  }
  path = resolve_path("./", ARGV[1])

  collect_variables(path, 0, variables)
  for (i = 2; i < ARGC; i++) {
    arg = ARGV[i]
    eq = index(arg, "=")
    cmd_name = substr(arg, 1, eq - 1)
    if (!eq || cmd_name == "" || cmd_name ~ /[[:space:]]/) {
      fail("invalid variable argument: " arg)
    }
    variables[cmd_name] = substr(arg, eq + 1)
  }

  out = expand_expressions(path, 0, variables)
  if ("#layout" in variables) {
    variables["#content"] = strip(out)
    out = expand_expressions(resolve_path(path, variables["#layout"]), 0, variables)
  }
  printf "%s", out
}
