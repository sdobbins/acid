# @author Scott Dobbins
# @version 0.1
# @date 2018-01-09 18:00

# transcribes acid.R code into Python for easy simultaneous maintenance

extra_functions <- c(
  "", 
  "def non_empty_string(string):", 
  '    return type(string) is str and string != ""', 
  "", 
  "def reduce_concat(stuff):", 
  "    return functools.reduce(lambda x, y: x + y, stuff)", 
  "", 
  "def collapse_bar(strings):", 
  "    return functools.reduce(lambda x, y: str(x) + '|' + str(y), strings)", 
  "", 
  "def paste0(*lists):", 
  "    return list(map(lambda x: reduce_concat(x), zip(*lists)))", 
  "", 
  "def any_of(strings):", 
  '    return "(" + collapse_bar(strings) + ")"', 
  "", 
  "def flat_concat(ls):", 
  "    return reduce_concat([subls if type(subls) is list else [subls] for subls in ls])", 
  ""
)

transcribe_R_file_to_Python_file <- function(R_file, Python_file, append = TRUE, start = 1L, finish = Inf) {
  R_file_connection <- file(R_file)
  R_lines <- readLines(R_file_connection)
  close(R_file_connection)
  if (is.infinite(finish)) {
    finish <- length(R_lines)
  }
  Python_lines <- transcribe_R_lines_to_Python(R_lines[start:finish])
  Python_lines <- c("import functools", "import re", "import numpy as np", "", extra_functions, "", Python_lines, "")
  Python_file_connection <- file(Python_file)
  if (append) {
    # dunno
  } else {
    writeLines(text = Python_lines, con = Python_file_connection)
  }
  close(Python_file_connection)
  invisible(Python_lines)
}

transcribe_R_lines_to_Python <- function(R_lines) {
  lines <- R_lines
  
  ### fix simple language differences
  
  # remove R's integer markers
  lines <- gsub(lines, pattern = "([0-9]+)L", replacement = "\\1")
  
  # change NA to None
  lines <- gsub(lines, pattern = " NA", fixed = TRUE, replacement = " None")
  
  # change TRUE to True
  lines <- gsub(lines, pattern = " TRUE", fixed = TRUE, replacement = " True")
  
  # change FALSE to False
  lines <- gsub(lines, pattern = " FALSE", fixed = FALSE, replacement = " False")
  
  # fix R's non-vectorized logic
  lines <- gsub(lines, pattern = "||", replacement = "or", fixed = TRUE)
  lines <- gsub(lines, pattern = "&&", replacement = "and", fixed = TRUE)
  
  # remove double spaces between letters if any
  lines <- gsub(lines, pattern = "([^\t ]) +([^\t ])", replacement = "\\1 \\2")
  
  # change any tabs to double spaces
  lines <- gsub(lines, pattern = "\t", replacement = "  ")
  
  # fix spacing (R tabs are 2 spaces, python tabs are 4 spaces)
  lines <- gsub(lines, pattern = "^((  )*)([^ ])", replacement = "\\1\\1\\3")
  
  # fix brackets
  lines <- gsub(lines, pattern = " *\\{", replacement = ":")
  lines <- lines %which!like% "^ *\\} *$"
  lines <- grem(lines, "\\} *")
  
  containing_code <- lines %like% "^ *[^ #]"
  
  # fix function definitions
  lines[containing_code] <- gsub(lines[containing_code], pattern = "^( *)([^ <-]*) *<- *function\\(([^)]*)\\):", replacement = "\\1def \\2(\\3):")
  
  # fix else if to elif
  lines[containing_code] <- gsub(lines[containing_code], pattern = "else if", replacement = "elif")
  
  # delete unnecessary parentheses around conditionals and for/while loops
  lines[containing_code] <- gsub(lines[containing_code], pattern = "((el)?if|for|while) *\\((.*)\\):", replacement = "\\1 \\3:")
  
  # delete unnecessary parentheses around return statements
  lines[containing_code] <- gsub(lines[containing_code], pattern = "return \\((.*)\\)", replacement = "return \\1")
  
  ### fix functions
  
  # fix vector assignments (flat)
  lines <- gsub(lines, pattern = '( |\\()c\\((("[^"]+"(, *)?)+)\\)', replacement = "\\1[\\2]")
  
  # fix vector assignments (not flat)
  lines <- fix_function_call_pattern(lines, "c", pattern = "( |\\()c\\(", before = "flat_concat([", after = "])", offset = 1L)
  
  # fix unique([x]) to list(set(x))
  lines <- fix_function_call(lines, "unique", before = "list(set(", after = "))")
  
  # fix unique(x) to np.unique(x)
  lines[containing_code] <- gsub(lines[containing_code], pattern = "unique(", fixed = TRUE, replacement = "np.unique(")
  # fix sort(x) to np.sort(x)
  lines[containing_code] <- gsub(lines[containing_code], pattern = "sort(", fixed = TRUE, replacement = "np.sort(")
  
  # fix ending_with_word(...) to "\\b" + ... + "$"
  lines <- fix_function_call(lines, "ending_with_word", before = '"\\\\b" + ', after = ' + "$"')
  
  # fix ending_with(...) to ... + "$"
  lines <- fix_function_call(lines, "ending_with", after = ' + "$"')
  
  # fix beginning_with_word(...) to "^" + ... + "\\\\b"
  lines <- fix_function_call(lines, "beginning_with_word", before = '"^" + ', after = ' + "\\\\b"')
  
  # fix beginning_with(...) to "^" + ...
  lines <- fix_function_call(lines, "beginning_with", before = '"^" + ', after = "")
  
  # fix capturing_group(...) to "(" + ... + ")"
  lines <- fix_function_call(lines, "capturing_group", before = '"(" + ', after = ' + ")"')
  
  # fix isnt_empty(x) to len(x) != 0
  lines[containing_code] <- gsub(lines[containing_code], pattern = "isnt_empty\\(([^)]*)\\)", replacement = "len(\\1) != 0")
  
  # fix endsWith(x, y) to x.endswith(y)
  lines[containing_code] <- gsub(lines[containing_code], pattern = 'endsWith\\(([a-z]*), *("[^"]+")\\)', replacement = "\\1.endswith(\\2)")
  
  # fix endsWithAny(x, [y, z]) to np.any([x.endswith(string) for string in (y, z))], axis = 0)
  lines[containing_code] <- gsub(lines[containing_code], pattern = 'endsWithAny\\(([a-z]*), *\\[([^])]+)\\]\\)', replacement = "np.any([\\1.endswith(string) for string in (\\2)], axis = 0)")
  # fix endsWithAny(x, flat_concat([...])) to np.any([x.endswith(string) for string in flat_concat([...])], axis = 0)
  lines[containing_code] <- gsub(lines[containing_code], pattern = 'endsWithAny\\(([a-z]*), *(flat_concat\\(\\[[^])]+\\]\\))\\)', replacement = "np.any([\\1.endswith(string) for string in \\2], axis = 0)")
  # fix endsWithAny(x, y) to np.any([x.endswith(string) for string in tuple(y))], axis = 0)
  lines[containing_code] <- gsub(lines[containing_code], pattern = 'endsWithAny\\(([a-z]*), *([^)]+)\\)', replacement = "np.any([\\1.endswith(string) for string in \\2], axis = 0)")
  
  # fix remove_last_n_chars(something, num) to [string[:-num] for string in something]
  lines[containing_code] <- gsub(lines[containing_code], pattern = 'remove_last_n_chars\\(([^,]+), *([0-9]+)\\)', replacement = "[string[:-\\2] for string in \\1]")
  
  # fix replace_last_n_chars_with(something, num, replacement) to [string[:-num] + replacement for string in something]
  lines[containing_code] <- gsub(lines[containing_code], pattern = 'replace_last_n_chars_with\\(([^,]+), *([0-9]+), *("[^"]*")\\)', replacement = "[string[:-\\2] + \\3 for string in \\1]")
  
  # fix gsub(something, pattern = some_pattern, replacement = "text") to [re.sub(string = string, pattern = some_pattern, repl = "text") for string in something]
  lines[containing_code] <- gsub(lines[containing_code], pattern = 'gsub\\(([^,]+), *pattern = (.+), *replacement = ("[^"]*")\\)', replacement = "[re.sub(string = string, pattern = \\2, repl = \\3) for string in \\1]")
  
  # fix grem(something, "text") to [re.sub(string = string, pattern = "text") for string in something]
  lines[containing_code] <- gsub(lines[containing_code], pattern = 'grem\\(([^,]+), *("[^"]+")\\)', replacement = '[re.sub(string = string, pattern = \\2, repl = "") for string in \\1]')
  
  # fix reduce_nor(x1, x2, ...) to np.logical_not(np.any((x1, x2, ...), axis = 0))
  lines[containing_code] <- gsub(lines[containing_code], pattern = 'reduce_nor\\(([^)]*)\\)', replacement = "np.logical_not(np.any((\\1), axis = 0))")
  
  # fix R's %in% and %!in% to python's in and not in (also %c% and %!c%, which are the same as %in% and %!in%)
  lines[containing_code] <- gsub(lines[containing_code], pattern = "%(c|in)%", replacement = "in")
  lines[containing_code] <- gsub(lines[containing_code], pattern = "%!(c|in)%", replacement = "not in")
  
  # # fix x %whichlike% y to [string %like% y for string in x]
  # lines[containing_code <- gsub(lines[containing_code], "%whichlike%", between = "")]
  
  # fix x %like% y and x %!like% y to x.find(y) > 0 and x.find(y) < 0 respectively
  lines[containing_code] <- fix_infix_operator(lines[containing_code], "%like%", between = ".find(", after = ") > 0", parenthesize = TRUE)
  lines[containing_code] <- fix_infix_operator(lines[containing_code], "%!like%", between = ".find(", after = ") < 0", parenthesize = TRUE)
  
  # fix paste0 function when first arg is a one-element list (e_rules)
  lines <- gsub(lines, pattern = 'paste0\\(e_rules, *("[^"]+")\\)', replacement = "(e_rules + \\1)")
  
  # fix paste0 function in most cases (i.e. paste0(something, "text") to [thing + "text" for thing in something])
  lines <- gsub(lines, pattern = 'paste0\\(([^,]+), *("[^"]+")\\)', replacement = "[string + \\2 for string in \\1]")
  
  ### fix logic
  
  # fix R's ! to numpy's ~ (all negations should be element-wize)
  lines[containing_code] <- gsub(lines[containing_code], pattern = "!([^=])", replacement = "~\\1")
  # fix R's !() to python's not() (later to np.logical_not() as necessary) # no need to fix since ~ is valid numpy
  # lines[containing_code] <- fix_unary_operator(lines[containing_code], "!", "![^=]", func = "not(", np_func = "np.logical_not(")

  # # fix R's vectorized & to numpy's logical_and(x1, x2, ...) # no need to fix since & is also valid numpy
  # lines[containing_code] <- fix_binary_operator(lines[containing_code], "&")
  # # fix R's vectorized | to numpy's logical_or(x1, x2, ...) # no need to fix since | is also valid numpy
  # lines[containing_code] <- fix_binary_operator(lines[containing_code], "|")
  
  ### fix last simple language changes
  
  # add in line continuations (single backslash)
  lines[containing_code] <- gsub(lines[containing_code], pattern = "([&|]) *$", replacement = "\\1 \\\\")
  
  # fix assignment operator to equals
  lines[containing_code] <- gsub(lines[containing_code], pattern = "<-", replacement = "=")
  
  return (lines)
}

num_of_first_line_that_matches <- function(lines, pattern, exact = FALSE) {
  return (grep(lines, pattern = pattern, fixed = exact)[1L])
}

fix_function_call <- function(lines, op, before = "", after = ")") {
  pattern <- paste0(op, "(")
  l <- num_of_first_line_that_matches(lines, pattern, exact = TRUE)
  while (!is.na(l)) {
    current_line <- lines[l]
    pos <- regexpr(current_line, pattern = pattern, fixed = TRUE)[[1L]]
    phrase_info <- find_phrase_to_right(current_line, op, pos)
    stri_sub(lines[l], from = pos, to = phrase_info[["end"]]) <- paste0(before, phrase_info[["string"]], after)
    l <- num_of_first_line_that_matches(lines, pattern, exact = TRUE)
  }
  return (lines)
}

fix_function_call_pattern <- function(lines, op, pattern, before = "", after = ")", offset = 0L) {
  l <- num_of_first_line_that_matches(lines, pattern)
  while (!is.na(l)) {
    current_line <- lines[l]
    pos <- regexpr(current_line, pattern = pattern)[[1L]] + offset
    phrase_info <- find_phrase_to_right(current_line, op, pos)
    stri_sub(lines[l], from = pos, to = phrase_info[["end"]]) <- paste0(before, phrase_info[["string"]], after)
    l <- num_of_first_line_that_matches(lines, pattern)
  }
  return (lines)
}

fix_infix_operator <- function(lines, op, between, after = ")", parenthesize = FALSE) {
  l <- num_of_first_line_that_matches(lines, op, exact = TRUE)
  while (!is.na(l)) {
    current_line <- lines[l]
    pos <- regexpr(current_line, pattern = op, fixed = TRUE)[[1L]]
    left_phrase_info <- find_phrase_to_left(current_line, op, pos)
    right_phrase_info <- find_phrase_to_right(current_line, op, pos)
    if (parenthesize) {
      stri_sub(lines[l], from = left_phrase_info[["start"]], to = right_phrase_info[["end"]]) <- paste0("(", left_phrase_info[["string"]], between, right_phrase_info[["string"]], after, ")")
    } else {
      stri_sub(lines[l], from = left_phrase_info[["start"]], to = right_phrase_info[["end"]]) <- paste0(left_phrase_info[["string"]], between, right_phrase_info[["string"]], after)
    }
    l <- num_of_first_line_that_matches(lines, op, exact = TRUE)
  }
  return (lines)
}

# operators in order (first in list is first to evaluate) in regex form
# actual operator itself (without context) is captured within capturing group \\2
operators_in_order <- list("name_:" = c("((::))([^:]|$)", "((:::))"), 
                           "slot_$" = c("(($))", "((@))"), 
                           "slot_[" = c("((\\[))([^[]|$)", "((\\[\\[))s"), 
                           "exp"    = c("((\\^))"), 
                           #"unary" = c("-", "+"), # unary + and - (as in -2 degrees C vs +2 degrees C)
                           "seq_:"  = c("((:))([^:]|$)"), 
                           "infix"  = c("((%[^%]*%))"), # infix operators of the form %any% including %% and %/%
                           "mult"   = c("((\\*))", "((/))"), 
                           "add"    = c("((\\+))", "(^|[^<-])(-)([^>-]|$)"), 
                           "comp"   = c("((<))([^=<-]|$)", "(^|[^->])(>)([^=]|$)", "((<=))", "((>=))", "((==))", "((!=))"), 
                           "negate" = c("((!))([^=]|$)"), 
                           "log_&"  = c("((&))([^&]|$)", "((&&))"), 
                           "log_|"  = c("((\\|))([^|]|$)", "((\\|\\|))"), 
                           "by"     = c("((~))"), 
                           "asn_rg" = c("((->))([^>]|$)", "((->>))"), 
                           "asn_lf" = c("(^|[^<])(<-)", "((<<-))"), 
                           "asn_eq" = c("(^|[^!=><])(=)([^=]|$)"), 
                           "help"   = c("((\\?))"))

is_operator_binary <- c("name_:" = FALSE, 
                        "slot_$" = FALSE, 
                        "slot_[" = FALSE, 
                        "exp"    = TRUE, 
                        #"unary" = FALSE, # unary + and - (as in -2 degrees C vs +2 degrees C)
                        "seq_:"  = TRUE, 
                        "infix"  = TRUE, # infix operators of the form %any% including %% and %/%
                        "mult"   = TRUE, 
                        "add"    = TRUE, 
                        "comp"   = TRUE, 
                        "negate" = FALSE, 
                        "log_&"  = TRUE, 
                        "log_|"  = TRUE, 
                        "by"     = TRUE, 
                        "asn_rg" = TRUE, 
                        "asn_lf" = TRUE, 
                        "asn_eq" = TRUE, 
                        "help"   = FALSE)

unary_operators <- operators_in_order[!is_operator_binary]
binary_operators <- operators_in_order[is_operator_binary]

get_stopping_operators <- function(op) {
  # stopping operators are those with *lower* or equal precedence
  #*** the below is merely good enough for now for my utils_characters.R code--not generalizable
  if (op %like% "%") {
    return (c(",", "%", "&", "|"))
  }else if (op == "!") {
    return (c(",", "&", "|"))
  } else if (op == "&") {
    return (c(",", "&", "|"))
  } else if (op == "|") {
    return (c(",", "|"))
  } else { # default to some function call
    return (c(" ", ",", "%", "&", "|"))
  }
}

get_stopping_operators_dir <- function(op, dir) {
  if (dir == "left") {
    return (c(get_stopping_operators(op), "(", "["))
  } else {
    return (c(get_stopping_operators(op), ")", "]"))
  }
}

remove_unnecessary_parens <- function(string) {
  left_parens <- attr(regexpr(string, pattern = "^\\(*"), 'match.length')
  right_parens <- attr(regexpr(string, pattern = "\\)*$"), 'match.length')
  # pmin is vectorized (element-wise) min
  # the extra 1 is because string position 1 is the first one and string position -1 is the last one
  # i.e. (..., from = 1, to = -1) would not do anything, which is what we want for 0 parens
  # and (..., from = n, to = -n) would remove n-1 parens
  removable_parens <- pmin(left_parens, right_parens) + 1L
  return (stri_sub(string, from = removable_parens, to = -removable_parens))
}

corresponding_Python_operator <- list("&" = "np.all((", 
                                      "|" = "np.any((")

find_phrase_to_left <- function(line, op, pos) {
  # note that the "string" component of the return statement is not the same as 
  #   the substring from the "start" component to the "end" component, 
  #   due to the remove_unnecessary_parens() step
  chars <- strsplit(line, "")[[1L]]
  non_space_positions <- which(chars != " ")
  end <- max(non_space_positions %[<]% pos)
  assign_pos <- regexpr(line, pattern = " +<- +")
  if (assign_pos[[1L]] > 0L && assign_pos[[1L]] < pos) {
    start_limit <- assign_pos[[1L]] + attr(assign_pos, 'match.length')[[1L]]
  } else {
    start_limit <- regexpr(line, pattern = "[^ ]")[[1L]]
  }
  stopper_at <- start_limit - 1L
  stopper <- "\n" # code defaults to stopping at end of text
  stoppers <- get_stopping_operators_dir(op, "left")
  paren_stack_height <- 0L
  cur_pos <- end + 1L
  while (cur_pos > start_limit) {
    cur_pos <- cur_pos - 1L
    cur_char <- chars[[cur_pos]]
    if (cur_char == "'" || cur_char == '"') {#*** maybe also need to fast-forward through infix operators (%etc%)
      quote_char <- cur_char
      cur_pos <- cur_pos - 1L
      cur_char <- chars[[cur_pos]]
      while (cur_char != quote_char) {
        cur_pos <- cur_pos - 1L
        cur_char <- chars[[cur_pos]]
        if (cur_char == quote_char && chars[[cur_pos - 1L]] == "\\") {
          cur_pos <- cur_pos - 1L
          cur_char <- chars[[cur_pos]]
        }
      }
    } else if (cur_char == ")") {
      paren_stack_height <- paren_stack_height + 1L
    } else if (cur_char %in% stoppers) {
      if (paren_stack_height == 0L) {
        stopper <- cur_char
        stopper_at <- cur_pos
        start_limit <- Inf # stop loop
      } else if (cur_char == "(") {
        paren_stack_height <- paren_stack_height - 1L
      }
    }
  }
  start <- min(non_space_positions %[>]% stopper_at)
  return (list("start" = start, "end" = end, "string" = remove_unnecessary_parens(stri_sub(line, from = start, to = end)), "stopper" = stopper, "stopper_pos" = stopper_at))
}

find_phrase_to_right <- function(line, op, pos) {
  # note that the "string" component of the return statement is not the same as 
  #   the substring from the "start" component to the "end" component, 
  #   due to the remove_unnecessary_parens() step
  chars <- strsplit(line, "")[[1L]]
  non_space_positions <- which(chars != " ")
  start <- min(non_space_positions %[>=]% (pos + nchar(op)))
  comment_start_position <- regexpr(line, pattern = "#", fixed = TRUE)[[1L]]
  if (comment_start_position > 0L) {
    end_limit <- comment_start_position - 1L
  } else {
    end_limit <- max(non_space_positions)
  }
  assign_pos <- regexpr(line, pattern = "<-")[[1L]]
  if (assign_pos > 0L && assign_pos > pos) {
    end_limit <- min(end_limit, assign_pos - 1L)
  } 
  stopper_at <- end_limit + 1L
  stopper <- "\n" # code defaults to stopping at end of text
  stoppers <- get_stopping_operators_dir(op, "right")
  paren_stack_height <- 0L
  cur_pos <- start - 1L
  while (cur_pos < end_limit) {
    cur_pos <- cur_pos + 1L
    cur_char <- chars[[cur_pos]]
    if (cur_char == "'" || cur_char == '"') {#*** maybe also need to fast-forward through infix operators (%etc%)
      quote_char <- cur_char
      cur_pos <- cur_pos + 1L
      cur_char <- chars[[cur_pos]]
      while (cur_char != quote_char) {
        cur_pos <- cur_pos + 1L
        cur_char <- chars[[cur_pos]]
        if (cur_char == "\\" && chars[[cur_pos + 1L]] == quote_char) {
          cur_pos <- cur_pos + 1L
          cur_char <- chars[[cur_pos]]
        }
      }
    } else if (cur_char == "(") {
      paren_stack_height <- paren_stack_height + 1L
    } else if (cur_char %in% stoppers) {
      if (paren_stack_height == 0L) {
        stopper <- cur_char
        stopper_at <- cur_pos
        end_limit <- -Inf # stop loop
      } else if (cur_char == ")") {
        paren_stack_height <- paren_stack_height - 1L
      }
    }
  }
  end <- max(non_space_positions %[<]% stopper_at)
  return (list("start" = start, "end" = end, "string" = remove_unnecessary_parens(stri_sub(line, from = start, to = end)), "stopper" = stopper, "stopper_pos" = stopper_at))
}

fix_unary_operator <- function(lines, op, pattern, func, np_func) {
  l <- num_of_first_line_that_matches(lines, pattern = pattern)
  while (!is.na(l)) {
    current_line <- lines[l]
    pos <- regexpr(current_line, pattern = pattern)[[1L]]
    phrase_info <- find_phrase_to_right(current_line, op, pos)
    element_wise_context <- current_line %like% "[&|]" # already removed && and ||
    if (element_wise_context) {
      sub_function <- np_func
    } else {
      sub_function <- func
    }
    stri_sub(lines[l], from = pos, to = phrase_info[["end"]]) <- paste0(sub_function, phrase_info[["string"]], ")")
    l <- num_of_first_line_that_matches(lines, pattern = pattern)
  }
}

fix_binary_operator <- function(lines, op) {
  Python_op <- corresponding_Python_operator[[op]]
  
  op_for_matching <- paste0(" ", op, " ") # lazily get around conflicts with non-logical regex uses of | and & as I always put spaces around | and &
  
  l <- num_of_first_line_that_matches(lines, op_for_matching, exact = TRUE)
  while (!is.na(l)) {
    current_line <- lines[l]
    pos <- regexpr(current_line, pattern = op_for_matching, fixed = TRUE)[[1L]] + 1L #* later keep attributes of regexpr if match.length is needed
    phrase_info <- find_phrase_to_left(current_line, op, pos)
    current_phrase <- paste0(stri_sub(current_line, from = 1L, to = (phrase_info[["start"]]-1L)), 
                             Python_op, 
                             phrase_info[["string"]], 
                             ", ")
    after_stopper <- pos + 1L
    continue_on_current_line <- stri_sub(current_line, from = after_stopper) %like% "[^ ]"
    if (continue_on_current_line) {
      current_line <- paste0(current_phrase, stri_sub(current_line, from = after_stopper))
      pos <- nchar(current_phrase)
    } else {
      lines[l] <- current_phrase
      l <- l + 1L
      current_line <- lines[l]
      current_phrase <- stri_sub(current_line, to = regexpr(current_line, pattern = "[^ ]")[[1L]] - 1L)
      pos <- 0L
    }
    ended_with_op <- TRUE
    while (ended_with_op) {
      phrase_info <- find_phrase_to_right(current_line, op, pos)
      ended_with_op <- phrase_info[["stopper"]] == op
      if (ended_with_op) {
        current_phrase <- paste0(current_phrase, 
                                 phrase_info[["string"]], 
                                 ", ")
        after_stopper <- phrase_info[["stopper_pos"]] + 1L
        continue_on_current_line <- stri_sub(current_line, from = after_stopper) %like% "[^ ]"
        if (continue_on_current_line) {
          current_line <- paste0(current_phrase, stri_sub(current_line, from = after_stopper))
          pos <- nchar(current_phrase)
        } else {
          lines[l] <- current_phrase
          l <- l + 1L
          current_line <- lines[l]
          current_phrase <- stri_sub(current_line, to = regexpr(current_line, pattern = "[^ ]")[[1L]] - 1L)
          pos <- 0L
        }
      } else {
        current_phrase <- paste0(current_phrase, 
                                 phrase_info[["string"]], 
                                 "), axis = 0)", 
                                 stri_sub(current_line, from = phrase_info[["stopper_pos"]]))
        current_line <- current_phrase
      }
    }
    lines[l] <- current_line
    l <- num_of_first_line_that_matches(lines, op_for_matching, exact = TRUE)
  }
  return (lines)
}
