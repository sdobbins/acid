# @author Scott Dobbins
# @version 0.5.0.2
# @date 2018-01-13 14:30

### ACID
# contains, pluralizer, singularizer, and lemmatizer
# the lemmatizer "digests" words down into their 
#   simplest root form automatically, without any need 
#   to supply part of speech information
# lemmatizer also available in Python version


### Constants ---------------------------------------------------------------

languages_supported <- c("Latin", "Greek", "French", "Italian", "Hebrew", "Slavic", "Japanese", "Maori")
num_languages_supported <- length(languages_supported)

English_invariant_words <- c("bison", "buffalo", "cannon", "carp", "cod", "deer", "fish", "hi", "moose", "pike", "salmon", "sheep", "shrimp", "squid", "swine", "trout")
English_invariant_words_s_string <- "(((ser|spec)ie)|chassi|preci|rendezvou|chao|molasse)s$"
English_ie_singulars_string <- "(anom|badd|beast|bigg|bird|boog|boot|brown|calor|camarader|charcuter|coll|comm|cook|coot|cowr|dear|dogg|doug|food|gen|goal|good|group|hipp|hood|hott|junk|kidd|kitt|\\bl|magp|mean|mov|newb|(\\b|pot|sweet(ie|y))p|patisser|pix|prar|prem|quick|rever|rook|room|rotisser|smooth|soft|sweet|(\\b|hog|neck)t|talk|tough|town|vegg|wheel|yupp|zomb)ies$"
English_oe_singulars_string <- "(\\bal|\\bob|\\br|\\bsh|\\bt|\\bw)oes$"
English_zz_singulars_string <- "(bu|fi|fri|fu|ja|piza|ra)zz$"
English_zz_singulars_plurals_string <- "(bu|fi|fri|fu|ja|piza|ra)zzes$"
English_s_singulars_string <- "(alia|apparatu|asbesto|atla|bia|bonu|cactu|campu|canva|caucu|citru|ga|ibi|iri|len|lori|mucu|new|octopu|oop|pelvi|porticulli|rucku|statu|trelli|tucku|viru|ye)s$"
English_s_singulars_plurals_string <- "((alia|apparatu|asbesto|atla|bia|bonu|cactu|campu|canva|caucu|citru|ga|ibi|iri|len|lori|mucu|new|octopu|pelvi|porticulli|rucku|statu|trelli|tucku|viru|ye)s)es$"
English_ves_plurals_singulars_string <- "(cal|dwar|el|hal|hoo|lea|loa|scar|sel|shel||thie|wol)f|(kni|li|wi)fe$"
English_ves_plurals_string <- "(cal|dwar|el|hoo|lea|loa|scar|sel|shel|thie|wol)ves$"
English_ves_plurals_e_string <- "(kni|li|wi)ves$"
English_us_plurals <- c("bayous", "caribous", "emus", "gnus", "menus", "tiramisus", "tutus")
English_is_plurals <- c("khakis", "skis", "taxis")

words_with_plain_plurals <- c("canto", "hereto", "kimono", "photo", "piano", "portico", "pro", "quarto", "zero")
Latin_us_to_i_singulars <- c("alumnus", "cactus", "focus", "fungus", "succubus", "syllabus", "terminus", "uterus")
Latin_us_to_i_plurals <- c("alumni", "cacti", "foci", "fungi", "succubi", "syllabi", "termini", "uteri")
Latin_us_to_a_plurals <- c("addenda", "auditoria", "collisea", "compendia", "media", "memoranda", "millennia", "ova", "referenda", "spectra", "stadia", "strata", "symposia")
Latin_a_to_ae_singulars <- c("alga", "alumna", "antenna", "fauna", "fistula", "flora", "formula", "fovea", "hernia", "larva", "trachea")
Latin_is_singulars_string <- "(\\bax|cris|genes|kines|nemes|nos|oas|parenthes|test|thes|tos)is$"
Latin_es_plurals_string <- "(\\bax|cris|genes|kines|nemes|nos|oas|parenthes|test|thes|tos)es$" #* bases could be base or basis (asbestoses could be asbestosis but more likely asbestos)
Japanese_words_in_English <- c("bento", "katana", "kimono", "ninja", "otaku", "samurai", "sushi", "tsunami")
Maori_words_in_English <- c("kakapo", "kiwi", "waka")
other_foreign_is_plurals <- paste0(c(Japanese_words_in_English, Maori_words_in_English), "s") %whichlike% "is$"
all_is_plurals <- c(English_is_plurals, other_foreign_is_plurals)


### Argument Interpreter ----------------------------------------------------

handle_language_arguments <- function(...) {
  arguments_line <- remove_quotes(deparse(substitute(...)))
  if (arguments_line == "NULL") {
    return (character(0L))
  } else {
    arguments <- strsplit(arguments_line, "[ ,]+")[[1]] %[!=]% ""
    negations <- tolower(arguments) == "no"
    if (any(negations)) {
      which_negations <- which(negations)
      which_after_negations <- which_negations %[<=]% length(arguments)
      arguments[which_after_negations] <- paste0("-", capitalize(arguments[which_after_negations]))
      arguments <- arguments[-which_negations]
    }
    return (capitalize_first_letters(arguments))
  }
}


### Singulars and Plurals ---------------------------------------------------

pluralize <- function(words, ...) {
  arguments <- handle_language_arguments(...)
  return (pluralize_(words, arguments))
}

pluralize_ <- function(words, ...) {
  # requires lower or proper noun case to work
  # languages to turn on
  other_languages <- unlist(list(...), use.names = FALSE)
  if (is_empty(other_languages) || (isnt_empty(other_languages) && other_languages[[1]] == "All")) {
    use_language <- rep(TRUE, 8L)
  } else {
    use_language <- rep(FALSE, 8L)
  }
  re_name(use_language, languages_supported)
  for (item in other_languages) {
    if (item %contain% languages_supported) {
      if (item %like% "^-") {
        use_language[[grem(item, "-", exact = TRUE)]] <- FALSE
      } else {
        use_language[[item]] <- TRUE
      }
    }# else ignore
  }
  
  # invariants
  is_invariant <- words %like% ending_with(any_of(English_invariant_words)) | 
    words %like% English_invariant_words_s_string | 
    words %endswith% "nese"
  
  # Anglo-Saxon oddities
  is_person <- endsWith(words, "person")
  is_child <- endsWith(words, "child")
  is_brother <- endsWith(words, "brother")
  is_man <- endsWith(words, "man") & !endsWith(words, "human")
  is_oo <- words %like% "(tooth|foot|\\bgoose)$"
  is_ouse <- words %like% "((\\b|book|head)l|(\\b|dor|field|shrew|tit)m)ouse$"
  is_ox <- words %like% "\\box$"
  is_die <- words %like% "\\bdie$"
  
  rule_not_applied <- reduce_nor(is_invariant, is_person, is_child, is_brother, is_man, is_oo, is_ouse, is_ox, is_die)
  
  # Japanese
  if (use_language[["Japanese"]]) {
    is_japanese_invariant <- words %like% ending_with(any_of(Japanese_words_in_English)) & rule_not_applied
    rule_not_applied <- rule_not_applied & !is_japanese_invariant
  }
  
  # Maori
  if (use_language[["Maori"]]) {
    is_maori_invariant <- words %like% ending_with(any_of(Maori_words_in_English)) & rule_not_applied
    rule_not_applied <- rule_not_applied & !is_maori_invariant
  }
  
  # Hebrew
  if (use_language[["Hebrew"]]) {
    need_im <- endsWithAny(words, c("cherub", "kibbutz", "seraph")) & rule_not_applied
    need_ot <- endsWith(words, "matzah") & rule_not_applied
    
    hebrew_rule_applies <- reduce_or(need_im, 
                                     need_ot)
    rule_not_applied <- rule_not_applied & !hebrew_rule_applies
  }
  
  # Slavic
  if (use_language[["Slavic"]]) {
    need_a_slavic <- endsWith(words, "kniazhestvo") & rule_not_applied
    need_i_slavic <- words %like% "\\b(kobzar|oblast)$" & rule_not_applied
    
    slavic_rule_applies <- reduce_or(need_a_slavic, 
                                     need_i_slavic)
    rule_not_applied <- rule_not_applied & !slavic_rule_applies
  }
  
  # Greek
  if (use_language[["Greek"]]) {
    need_ta <- endsWith(words, "ma") & rule_not_applied
    need_a_greek <- endsWithAny(words, c("automaton", "criterion", "hedron", "menon")) & rule_not_applied
    need_ides <- endsWith(words, "itis") & rule_not_applied
    need_eis <- endsWith(words, "polis") & rule_not_applied
    
    greek_rule_applies <- reduce_or(need_ta, 
                                    need_a_greek, 
                                    need_ides, 
                                    need_eis)
    rule_not_applied <- rule_not_applied & !greek_rule_applies
  }
  
  # Italian
  if (use_language[["Italian"]]) {
    need_i_italian <- endsWith(words, "cello") & rule_not_applied
    rule_not_applied <- rule_not_applied & !need_i_italian
  }
  
  # French
  if (use_language[["French"]]) {
    need_x <- endsWith(words, "eau") & rule_not_applied
    rule_not_applied <- rule_not_applied & !need_x
  }
  
  # Latin
  if (use_language[["Latin"]]) {
    need_a <- endsWith(words, "um") & rule_not_applied
    need_e <- words %like% ending_with(any_of(Latin_a_to_ae_singulars)) & rule_not_applied
    need_i <- (endsWith(words, "ius") | words %like% ending_with(any_of(Latin_us_to_i_singulars))) & rule_not_applied
    need_era <- endsWithAny(words, c("genus", "viscus")) & rule_not_applied
    need_ora <- endsWith(words, "corpus") & rule_not_applied
    need_ices <- endsWithAny(words, c("dex", "dix", "tex", "tix", "trex", "trix")) & rule_not_applied
    need_es_latin <- endsWithAny(words, c("testis", "sis", "xis")) & rule_not_applied
    
    latin_rule_applies <- reduce_or(need_a, 
                                    need_e, 
                                    need_i, 
                                    need_era, 
                                    need_ora, 
                                    need_ices, 
                                    need_es_latin)
    rule_not_applied <- rule_not_applied & !latin_rule_applies
  }
  
  # English leftovers
  need_zes <- words %like% "[aeiou]z$"
  need_ses <- ((words %like% "[aeio]s$" & words %!like% English_s_singulars_string & !endsWithAny(words, c("itis", "polis", "testis", "sis", "xis"))) | endsWith(words, "bus")) & rule_not_applied
  need_es <- ((words %like% "([sxz]|[cs]h|[^aeiouy]o)$" | words %like% English_s_singulars_string) & !need_zes & !need_ses & words %!like% ending_with(any_of(words_with_plain_plurals))) & rule_not_applied
  need_ies <- words %like% "[^aeiou]y$" & rule_not_applied
  need_ves <- words %like% "[ailr]fe?$" & rule_not_applied
  
  english_rule_applies <- reduce_or(need_es, 
                                    need_ies, 
                                    need_ses, 
                                    need_ves, 
                                    need_zes)
  rule_not_applied <- rule_not_applied & !english_rule_applies
  
  # catch-all generic English plural
  need_s <- rule_not_applied
  
  # fix Anglo-Saxon oddities
  words[is_person] <- replace_last_n_chars_with(words[is_person], 4L, "ople")
  words[is_child] <- paste0(words[is_child], "ren")
  words[is_brother] <- replace_last_n_chars_with(words[is_brother], 5L, "ethren")
  words[is_man] <- replace_last_n_chars_with(words[is_man], 2L, "en")
  words[is_oo] <- gsub(words[is_oo], pattern = "oo([a-z]{1,2})$", replacement = "ee\\1")
  words[is_ouse] <- replace_last_n_chars_with(words[is_ouse], 4L, "ice")
  words[is_ox] <- paste0(words[is_ox], "en")
  words[is_die] <- replace_last_n_chars_with(words[is_die], 1L, replacement = "ce")
  
  # fix French
  if (use_language[["French"]]) {
    words[need_x] <- paste0(words[need_x], "x")
  }
  
  # fix Greek
  if (use_language[["Greek"]]) {
    words[need_ta] <- paste0(words[need_ta], "ta")
    words[need_a_greek] <- replace_last_n_chars_with(words[need_a_greek], 2L, "a")
    words[need_ides] <- replace_last_n_chars_with(words[need_ides], 2L, "ides")
    words[need_eis] <- replace_last_n_chars_with(words[need_eis], 2L, "eis")
  }
  
  # fix Hebrew
  if (use_language[["Hebrew"]]) {
    words[need_im] <- paste0(words[need_im], "im")
    words[need_ot] <- replace_last_n_chars_with(words[need_ot], 2L, "ot")
  }
  
  # fix Italian
  if (use_language[["Italian"]]) {
    words[need_i_italian] <- replace_last_n_chars_with(words[need_i_italian], 1L, "i")
  }
  
  # fix Latin
  if (use_language[["Latin"]]) {
    words[need_a] <- replace_last_n_chars_with(words[need_a], 2L, "a")
    words[need_e] <- paste0(words[need_e], "e")
    words[need_i] <- replace_last_n_chars_with(words[need_i], 2L, "i")
    words[need_era] <- replace_last_n_chars_with(words[need_era], 2L, "era")
    words[need_ora] <- replace_last_n_chars_with(words[need_ora], 2L, "ora")
    words[need_es_latin] <- replace_last_n_chars_with(words[need_es_latin], 2L, "es")
    words[need_ices] <- replace_last_n_chars_with(words[need_ices], 2L, "ices")
  }
  
  # fix Slavic
  if (use_language[["Slavic"]]) {
    words[need_a_slavic] <- replace_last_n_chars_with(words[need_a_slavic], 1L, "a")
    words[need_i_slavic] <- paste0(words[need_i_slavic], "i")
  }
  
  # fix English leftovers
  words[need_zes] <- paste0(words[need_zes], "zes")
  words[need_ses] <- paste0(words[need_ses], "ses")
  words[need_es] <- paste0(words[need_es], "es")
  words[need_ies] <- replace_last_n_chars_with(words[need_ies], 1L, "ies")
  words[need_ves] <- gsub(words[need_ves], pattern ="fe?$", replacement = "ves")
  
  # catch-all
  words[need_s] <- paste0(words[need_s], "s")
  
  return (words)
}

singularize <- function(words) {
  # invariants
  is_invariant <- words %like% ending_with(any_of(English_invariant_words)) | 
    words %like% English_invariant_words_s_string | 
    words %like% ending_with(any_of(Japanese_words_in_English)) | 
    words %like% ending_with(any_of(Maori_words_in_English)) | 
    endsWith(words, "nese")
  
  # Anglo-Saxon oddities
  is_person <- endsWith(words, "people")
  remove_last3 <- endsWith(words, "children")
  is_brother <- endsWith(words, "brethren")
  is_man <- endsWith(words, "men") & words %!like% "(\\b[ao]|abdo|acu|albu|bitu|fora|hy|lu|ra|regi|ru|se|speci|sta)men$"
  is_oo <- endsWithAny(words, c("teeth", "feet", "geese"))
  is_ouse <- words %like% "((\\b|book|head)l|(\\b|dor|field|shrew|tit)m)ice$"
  remove_last2 <- words %like% "\\boxen$"
  is_die <- words %like% "\\bdice$"
  
  rule_not_found <- reduce_nor(is_invariant, is_person, remove_last3, is_brother, is_man, is_oo, is_ouse, remove_last2, is_die)
  
  # foreign language rules
  remove_last <- endsWithAny(words, c("kobzari", "oblasti", "eaux", "ae")) & rule_not_found
  need_o <- endsWithAny(words, c("kniazhestva", "celli")) & rule_not_found
  rule_not_found <- rule_not_found & reduce_nor(remove_last, need_o)
  
  need_itis <- endsWith(words, "itides") & rule_not_found
  rule_not_found <- rule_not_found & !need_itis
  
  need_on <- endsWithAny(words, c("automata", "criteria", "hedra", "mena")) & rule_not_found
  rule_not_found <- rule_not_found & !need_on
  
  remove_last2 <- remove_last2 | (endsWithAny(words, c("im", "mata")) & rule_not_found)
  need_ah <- endsWith(words, "ot") & rule_not_found
  rule_not_found <- rule_not_found & reduce_nor(remove_last2, need_ah)
  
  need_ma <- endsWith(words, "mata") & rule_not_found
  need_us <- endsWith(words, "i") & rule_not_found
  need_us_special <- endsWithAny(words, c("corpora", "genera", "viscera")) & rule_not_found
  rule_not_found <- rule_not_found & reduce_nor(need_ma, need_us, need_us_special)
  
  need_um <- endsWith(words, "a") & rule_not_found
  rule_not_found <- rule_not_found & !need_um
  
  need_is_latin <- words %like% Latin_es_plurals_string & words %!like% "((\\b|brown|bull|hard|hook|shovel|arabi|flavi|fura|man|pyra)n|(hep|lac|mal|pen)t)oses$" & rule_not_found
  rule_not_found <- rule_not_found & !need_is_latin
  
  need_ex <- endsWithAny(words, c("codices", "cortices", "indices", "vortices")) & rule_not_found
  need_ix <- endsWithAny(words, c("radices", "trices")) & rule_not_found
  need_is_greek <- endsWith(words, "eis") & words %!like% "(\\bl|sens)eis$" & rule_not_found
  rule_not_found <- rule_not_found & reduce_nor(need_ex, need_ix, need_is_greek)
  
  need_f <- words %like% English_ves_plurals_string & rule_not_found
  need_fe <- words %like% English_ves_plurals_e_string & rule_not_found
  need_y <- endsWith(words, "ies") & words %!like% English_ie_singulars_string & rule_not_found
  rule_not_found <- rule_not_found & reduce_nor(need_f, need_fe, need_y)
  
  remove_last3 <- remove_last3 | ((endsWith(words, "busses") | 
                                     (endsWith(words, "zzes") & 
                                        words %!like% English_zz_singulars_plurals_string)) & 
                                    rule_not_found)
  rule_not_found <- rule_not_found & !remove_last3
  
  remove_last <- remove_last | ((words %like% English_ie_singulars_string | 
                                   words %like% English_oe_singulars_string | 
                                   words %like% "[aeiouy][^aeioux]es$" | 
                                   endsWith(words, "mmes") | 
                                   words %like% "(([bcdfglprstz][glr])|(l[csv])|(n[cgrs])|(r[cgsv])|(s[c])|(u)|(((\\b|back|belly|head|stomach|tooth)a|ca|mousta|pana|pista)ch)|(rr)|(tt)|(\\b(ba|ca|ge|ha|mo|pa|pi|ta|wa|cha|try|arti|bati|ripo|langou)st))es$") & 
                                  words %!like% English_s_singulars_plurals_string & 
                                  words %!like% ending_with_word(any_of(paste0(Latin_us_to_i_singulars, "es"))) & 
                                  rule_not_found)
  rule_not_found <- rule_not_found & !remove_last
  
  remove_last2 <- remove_last2 | (words %like% "[^e]es$" & rule_not_found)
  rule_not_found <- rule_not_found & !remove_last2
  
  remove_last <- remove_last | (endsWith(words, "s") & rule_not_found)
  
  # fix English rules
  words[is_person] <- replace_last_n_chars_with(words[is_person], 4L, "rson")
  words[is_brother] <- replace_last_n_chars_with(words[is_brother], 6L, "other")
  words[is_man] <- replace_last_n_chars_with(words[is_man], 2L, "an")
  words[is_oo] <- gsub(words[is_oo], pattern = "ee([a-z]{1,2})$", replacement = "oo\\1")
  words[is_ouse] <- replace_last_n_chars_with(words[is_ouse], 3L, "ouse")
  words[is_die] <- replace_last_n_chars_with(words[is_die], 3L, "ie")
  words[need_f] <- replace_last_n_chars_with(words[need_f], 3L, "f")
  words[need_fe] <- replace_last_n_chars_with(words[need_fe], 3L, "fe")
  words[need_y] <- replace_last_n_chars_with(words[need_y], 3L, "y")
  
  # fix foreign rules
  words[need_o] <- replace_last_n_chars_with(words[need_o], 1L, "o")
  words[need_itis] <- replace_last_n_chars_with(words[need_itis], 6L, "itis")
  words[need_ah] <- replace_last_n_chars_with(words[need_ah], 2L, "ah")
  words[need_ma] <- replace_last_n_chars_with(words[need_ma], 2L, "ma")
  words[need_on] <- replace_last_n_chars_with(words[need_on], 1L, "on")
  words[need_us] <- replace_last_n_chars_with(words[need_us], 1L, "us")
  words[need_us_special] <- replace_last_n_chars_with(words[need_us_special], 3L, "us")
  words[need_um] <- replace_last_n_chars_with(words[need_um], 1L, "um")
  words[need_ex] <- replace_last_n_chars_with(words[need_ex], 4L, "ex")
  words[need_ix] <- replace_last_n_chars_with(words[need_ix], 4L, "ix")
  words[need_is_greek] <- replace_last_n_chars_with(words[need_is_greek], 3L, "is")
  words[need_is_latin] <- replace_last_n_chars_with(words[need_is_latin], 2L, "is")
  
  # fix generic rules
  words[remove_last3] <- remove_last_n_chars(words[remove_last3], 3L)
  words[remove_last2] <- remove_last_n_chars(words[remove_last2], 2L)
  words[remove_last] <- remove_last_n_chars(words[remove_last], 1L)
  
  return (words)
}

make_plural <- function(words, ...) {
  arguments <- handle_language_arguments(...)
  are_singular <- is_singular(words)
  of_indeterminate_number <- is.na(are_singular)
  if (any(of_indeterminate_number) & !any(c("All", "Japanese", "Maori") %in% arguments)) {
    words[of_indeterminate_number] <- pluralize_(words[of_indeterminate_number], arguments)
    words[are_singular & !of_indeterminate_number] <- pluralize_(words[are_singular & !of_indeterminate_number], arguments)
  } else {
    words[are_singular] <- pluralize_(words[are_singular], arguments)
  }
  return (words)
}

make_singular <- function(words) {
  are_plural <- is_plural(words)
  of_indeterminate_number <- is.na(are_plural)
  can_be_made_singular <- are_plural & !of_indeterminate_number
  words[can_be_made_singular] <- singularize(words[can_be_made_singular])
  return (words)
}

get_singular_and_plural <- function(words, data_in = NULL) {
  if (is.null(data_in)) {
    are_plural <- is_plural(words)
    are_indeterminate <- is.na(are_plural)
    are_plural[are_indeterminate] <- FALSE
    are_singular <- !are_indeterminate & !are_plural
    indeterminates <- words[are_indeterminate]
    plurals <- c(words[are_plural], pluralize(words[are_singular]))
    singulars <- c(words[are_singular], singularize(words[are_plural]))
    return (c(indeterminates, singulars, plurals))
  } else if (data_in == 'singular') {
    are_indeterminate <- is.na(is_plural(words))
    indeterminates <- words[are_indeterminate]
    singulars <- words[!are_indeterminate]
    plurals <- pluralize(words[!are_indeterminate])
    return (c(indeterminates, singulars, plurals))
  } else if (data_in == 'plural') {
    are_indeterminate <- is.na(is_plural(words))
    indeterminates <- words[are_indeterminate]
    plurals <- words[!are_indeterminate]
    singulars <- singularize(words[!are_indeterminate])
    return (c(indeterminates, singulars, plurals))
  } else {
    message('gets_singular_and_plual() only takes "singular" and "plural" as possible data_in arguments; defaulting to null case')
    return (gets_singular_and_plural(words))
  }
}


### Number Testers ----------------------------------------------------------

is_singular <- function(words) {
  return (!is_plural(words))
}

is_plural <- function(words) {
  is_singular_with_s <- words %like% English_s_singulars_string | 
    (words %like% "[^e]iu?s$" & !endsWithAny(words, all_is_plurals)) | 
    words %like% ending_with(any_of(Latin_us_to_i_singulars)) | 
    endsWithAny(words, c("corpus", "genus", "viscus")) | 
    words %like% Latin_is_singulars_string | 
    endsWith(words, "itis") | 
    endsWith(words, "ss") | 
    (endsWith(words, "us") & words %!like% ending_with(any_of(English_us_plurals)) & !endsWith(words, "eaus"))
  
  is_plural_without_s <- endsWith(words, "people") | 
    endsWithAny(words, c("brethren", "children")) | 
    (endsWith(words, "men") & words %!like% "(\\b[ao]|abdo|acu|albu|bitu|fora|hy|lu|ra|regi|ru|se|speci|sta)men$") | 
    endsWithAny(words, c("teeth", "feet", "geese")) | 
    words %like% "((\\b|book|head)l|(\\b|dor|field|shrew|tit)m)ice$" | 
    words %like% "\\boxen$" | 
    words %like% "\\bdice$" | 
    endsWithAny(words, c("kobzari", "oblasti")) | 
    endsWith(words, "eaux") | 
    endsWith(words, "ae") | 
    endsWith(words, "kniazhestva") | 
    endsWith(words, "celli") | 
    endsWithAny(words, c("cherubim", "kibbutz", "seraph")) | 
    endsWith(words, "matzot") | 
    endsWithAny(words, c("hedra", "mata", "mena", "ria")) | 
    endsWithAny(words, c("genera", "viscera", "corpora")) | 
    words %like% ending_with(any_of(Latin_us_to_i_plurals)) | 
    words %like% ending_with(any_of(Latin_us_to_a_plurals))
  
  is_indeterminate <- words %like% ending_with(any_of(English_invariant_words)) | 
    words %like% English_invariant_words_s_string | 
    words %like% ending_with(any_of(Japanese_words_in_English)) | 
    words %like% ending_with(any_of(Maori_words_in_English)) | 
    endsWith(words, "nese")
  
  is_plural <- !is_indeterminate & 
    (is_plural_without_s | (endsWith(words, "s") & !is_singular_with_s))
  
  results <- logical(length(words))
  results[is_indeterminate] <- NA
  results[is_plural] <- TRUE
  
  return (results)
}
