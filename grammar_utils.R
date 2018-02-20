# @author Scott Dobbins
# @version 0.5.0.2
# @date 2018-01-13 14:30

### ACID
# contains, pluralizer, singularizer, and lemmatizer
# the lemmatizer "digests" words down into their 
#   simplest root form automatically, without any need 
#   to supply part of speech information
# lemmatizer also available in Python version


### Util Functions ----------------------------------------------------------

# dependecy checking
is_package_loaded <- function(package_name) {
  return (paste0("package:", package_name) %in% search())
}

# vector management
if (is_package_loaded("rlang") | is_package_loaded("purrr")) {
  isnt_empty <- function(thing) {
    return (!is_empty(thing))
  }
} else {
  is_empty <- function(thing) {
    return (length(thing) == 0L)
  }
  isnt_empty <- function(thing) {
    return (length(thing) != 0L)
  }
}
if (is_package_loaded("data.table")) {
  re_name <- function(vec, new_names) {
    setattr(vec, "names", new_names)
  }
} else {
  re_name <- function(vec, new_names) {
    global_vec_name <- deparse(substitute(vec))
    names(vec) <- new_names
    assign(global_vec_name, vec, envir = parent.frame())
    invisible(vec)
  }
}

# infix operators
`%!in%` <- function(a, b) {
  return (!(a %in% b))
}
`%like%` <- function(a, b) grepl(pattern = b, a)
`%!like%` <- function(a, b) !grepl(pattern = b, a)
`%exactlylike%` <- function(a, b) grepl(pattern = b, a, fixed = TRUE)
`%whichlike%` <- function(a, b) a[a %like% b]

# regex helpers
non_capturing_group <- function(strings) {
  return (paste0("(?:", strings, ")"))
}
any_of <- function(strings) {
  return (non_capturing_group(paste0(strings, collapse = "|")))
}
beginning_with <- function(strings) {
  return (paste0("^", strings))
}
beginning_with_word <- function(strings) {
  return (paste0("^", strings, "\\b"))
}
ending_with <- function(strings) {
  return (paste0(strings, "$"))
}
ending_with_word <- function(strings) {
  return (paste0("\\b", strings, "$"))
}
endsWithAny <- function(strings, endings) {
  results <- endsWith(strings, endings[[1]])
  for (i in seq_along(endings)[-1L]) {
    results <- results | endsWith(strings, endings[[i]])
  }
  return (results)
}
grem <- function(strings, pattern, exact = FALSE) {
  return (gsub(pattern = pattern, replacement = "", x = strings, fixed = exact))
}
remove_quotes <- function(strings) {
  return (grem(pattern = "\"", strings))
}

# slicing
`%[==]%` <- function(a, b) a[a == b]
`%[!=]%` <- function(a, b) a[a != b]
`%[>]%`  <- function(a, b) a[a > b]
`%[>=]%` <- function(a, b) a[a >= b]
`%[<]%`  <- function(a, b) a[a < b]
`%[<=]%` <- function(a, b) a[a <= b]

# mapped logic
map_not_vec <- function(a) sapply(a, `!`)
map_or  <- function(a, b) Map(`|`, a, b)
reduce_or <- function(...) unlist(Reduce(map_or, list(...)), use.names = FALSE)
reduce_nor <- function(...) map_not_vec(Reduce(map_or, list(...)))

# string handling
if (is_package_loaded("stringi")) {
  remove_last_n_chars <- function(strings, n) {
    return (stri_sub(strings, to = -(n + 1L)))
  }
} else {
  remove_last_n_chars <- function(strings, n) {
    return (substr(strings, start = 1L, stop = nchar(strings) - n))
  }
}
replace_last_n_chars_with <- function(strings, n, replacements) {
  return (paste0(remove_last_n_chars(strings, n), replacements))
}
