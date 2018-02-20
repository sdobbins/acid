# @author Scott Dobbins
# @version 0.6.1
# @date 2018-02-18 22:00

### ACID
# contains, pluralizer, singularizer, and lemmatizer
# the lemmatizer "digests" words down into their 
#   simplest root form automatically, without any need 
#   to supply part of speech information
# lemmatizer also available in Python version


### Lemmatizer --------------------------------------------------------------

English_ly_nouns <- c("ally", "anomaly", "assembly", "belly", "bully", "butterfly", "contumely", "doily", "dragonfly", "gadfly", "family", "filly", "firefly", "fly", "folly", "gully", "holly", "homily", "horsefly", "housefly", "jelly", "lily", "melancholy", "monopoly", "oligopoly", "panoply", "rally", "sandfly", "tally")
English_ly_verbs <- c("apply", "bely", "bully", "comply", "dally", "dilly-dally", "imply", "multiply", "ply", "rally", "rely", "reply", "sally", "shilly-shally", "supply", "tally")
English_ly_adjectives <- c("billy", "dilly", "early", "filly", "holy", "likely", "nilly", "only", "silly", "smily", "willy")
English_ly_keepers <- unique(c(English_ly_nouns, English_ly_verbs, English_ly_adjectives))
English_ly_to_le_words <- c("doubly", "cycly", "muscly", "crackly", "crinkly", "fickly", "knuckly", "sparkly", "tinkly", "wrinkly", "crumply", "dimply", "druply", "riply", "rumply", "simply", "triply", "tuply", "bristly", "gently", "gristly", "rattly", "subtly", "thistly")

English_anti_keepers <- c("anticipat", "antidote", "antilog", "antimony", "anting", "antiquari", "antiquary", "antiquat", "antique", "antiqui", "antiquit", "antistrophe")
English_dis_keepers <- c("discreet", "discret(?:e|ion)", "discrepan", "discriminat", "disk", "dish", "display", "dismay", "dismal", "dismiss", "dispel", "discern", "discipl", "dispute", "distribu", "disrupt", "disturb", "discus", "diss", "dispose", "disgust", "dismiss", "distill", "disdain", "distort", "disease", "disco$", "discograph", "discover", "district", "distinct", "distinguish", "distan", "disten", "distress")
English_imbmp_keepers <- c("imbib", "imbitter", "imbolden", "imbecil", "imblaz", "imbroglio", "imbue", "immediat", "imp$", "impair", "impal", "impeach", "imped", "imperitive", "impertinent", "import", "implement", "imply", "implic", "impregnat", "improp", "impuls", "impresario", "impose", "imposit", "impetuous", "imperil", "imperial", "impact", "implod", "implos", "impress", "imprint", "imput", "impel", "impromptu", "implant", "impish", "impound", "impunit", "improv", "implor", "impuls", "imping", "immanenc", "immigrat", "immun", "immur", "immers", "immanent", "immens")
English_in_keepers <- c("in$", "inside$", "into$", "inane", "inanit", "inaug", "inbound", "inbre", "inch", "incas", "incens", "incentiv", "incept", "incid", "incis", "incit", "inclin", "inclos", "includ", "inclus", "incom[ei]", "increas", "increment", "incub", "inculca", "incur", "indeed", "indemn", "indent", "index", "india", "indic", "indie", "indig", "individual", "induc", "indulg", "industr", "indy", "inert", "infant", "inertia", "infatua", "infect", "infer", "infest", "infix", "inflat", "inflect", "inflict", "influen", "info", "infra", "infring", "infus", "ingest", "ingot", "ingrain", "ingrati", "ingredient", "ingroup", "inhabit", "inhal", "inherent", "inherit", "inhibit", "initia", "inject", "injure", "ink", "inlay", "inmate", "inn", "inositol", "input", "inquir", "insert", "insid", "insinuat", "insip", "insist", "insinuat", "inspect", "inspir", "install", "instan", "instat", "instead", "instigat", "instill", "instruct", "instrum", "institut", "insul", "insur", "intact", "integ", "intell", "inten", "inter", "intestin", "intimat", "intomb", "intro", "intru", "intubat", "intuit", "inundat", "inur", "invad", "invas", "invent", "invers", "invert", "invest", "invit", "invok", "invoc", "involv", "inward")
English_mis_keepers <- c("missile", "mission", "miser", "mischiev", "miscible", "misceg", "miscell", "misses", "miss$", "missed", "missing", "mishap", "mist", "miso", "mississippi")
English_sub_keepers <- c("sub$", "submit", "submar", "subtl", "subb(?:ed|ing)", "subject", "suburb", "subdu(?:e|ing)", "subway", "subsequent", "subvene", "subpena", "subduce", "subvert", "subsidy", "subside", "subsist", "sublime", "subtend", "submer[gs]e", "subtract", "substan[ct]", "subscri[bp]", "substitut", "subsidiar", "substrate")#***
English_super_keepers <- c("super$", "superfluous", "superior", "superlativ")
English_un_keepers <- c("uncle", "union", "unif", "univer", "unilat", "uniloc", "unifol", "uniform", "unit", "unival", "univar", "univoc", "unicycl", "uniling", "unilin", "unicam", "uniplan", "unipot", "unicol", "unitar", "unicorn", "uniax", "unique", "unison", "uniface", "unisex", "unless", "until")
English_under_keepers <- c("under$", "underneath$", "understand", "understood")
English_other_keepers <- c("anti$", "hyper$", "hypo$", "hypothe", "over$", "overly$", "under$", "underwh")
English_prefix_keepers <- c(English_anti_keepers, English_dis_keepers, English_imbmp_keepers, English_in_keepers, English_mis_keepers, English_sub_keepers, English_super_keepers, English_un_keepers, English_under_keepers, English_other_keepers)

English_iable_keepers <- c("amiable", "liable", "viable")
English_able_keepers <- c("able", "available", "cable", "fable", "gable", "horrible", "parable", "probable", "reliable", "stable", "table", "timetable", "vegetable", "vulnerable", English_iable_keepers)
English_ible_keepers <- c("bible", "compatible", "eligible", "feasible", "horrible", "possible", "responsible", "terrible")
English_eal_keepers <- c("anneal", "appeal", "conceal", "congeal", "deal", "\\bmeal", "ordeal", "\\breal", "repeal", "reveal", "seal", "squeal", "steal")
English_ial_keepers <- c("artificial", "axial", "colloquial", "congenial", "cordial", "crucial", "jovial", "judicial", "material", "nubial", "social", "special", "superficial", "trial", "trivial", "venial", "vivial")
English_ual_keepers <- c("actual", "casual", "dual", "equal", "eventual", "individual", "lingual", "manual", "menstrual", "mutual", "ritual", "usual", "victual", "visual")
English_al_keepers <- c("aboriginal", "animal", "arsenal", "capital", "cardinal", "carnival", "cathedral", "charcoal", "chemical", "coal", "crystal", "decimal", "\\bdent", "eternal", "federal", "final", "fiscal", "funeral", "general", "hospital", "integral", "international", "interval", "journal", "lateral", "legal", "liberal", "literal", "local", "loyal", "mammal", "marital", "medieval", "mental", "mineral", "moral", "municipal", "naval", "normal", "numeral", "\\boval", "primeval", "principal", "radical", "rival", "rural", "scandal", "secular", "several", "spectrum", "spiral", "temporal", "thermal", "total", "vassal", "vertical", "virtual", "vital", English_eal_keepers, English_ial_keepers, English_ual_keepers) #*** integral to integrate?
English_ist_keepers <- c("assist", "artist", "checklist", "chemist", "cist", "consist", "dentist", "enlist", "exist", "feist", "fist", "foist", "gist", "heist", "hoist", "insist", "list", "joist", "mist", "moist", "persist", "playlist", "protist", "resist", "schist", "shist", "twist", "wishlist", "wrist") #, "florist"
English_ism_keepers <- c("animism", "atheism", "autism", "baptism", "catechism", "deism", "fascism", "sadism", "sophism", "theism")
English_ian_keepers <- c("lesbian", "thespian")
English_age_removers <- c("acreage", "anchorage", "appendage", "baronage", "binage", "bondage", "breakage", "cellarage", "coinage", "corkage", "cousinage", "coverage", "creepage", "drainage", "factorage", "flowerage", "footage", "frontage", "fruitage", "gallonage", "graftage", "harborage", "herbage", "hermitage", "innage", "layerage", "leafage", "leakage", "layerage", "lighterage", "linkage", "meltage", "meterage", "mileage", "moorage", "orphanage", "package", "parentage", "passage", "patronage", "percentage", "pilotage", "portage", "porterage", "postage", "poundage", "pressage", "quarterage", "reportage", "roughage", "seepage", "sewerage", "shortage", "shrinkage", "signage", "siphonage", "spillage", "soilage", "steerage", "stowage", "surplusage", "tankage", "tillage", "tinage", "towage", "tutorage", "voltage", "wagonage", "wattage", "wharfage", "yardage")
English_ish_keepers <- c("abolish", "blish", "blemish", "burnish", "dish", "fish", "fetish", "finish", "flourish", "foolish", "garish", "guish", "hashish", "lavish", "monish", "parish", "perish", "plish", "plenish", "polish", "publish", "quish", "ravish", "relish", "wish")
English_ment_keepers <- c("parliament", "tournament", "testament", "ornament", "torment", "armament", "garment", "element", "plement", "department", "environment", "segment", "aliment", "moment", "comment", "condiment", "experiment", "ndiment", "pliment", "regiment", "sediment", "sentiment", "triment", "argument", "document", "instrument", "monument")
English_ize_keepers <- c("baptize", "braize", "maize", "ognize", "organize", "ostracize", "prize", "seize", "size")

English_able_double_consonants <- strrep(c("b", "d", "g", "m", "n", "p", "r", "t"), 2)
English_doubled_consonants_able <- paste0(English_able_double_consonants, "able")
English_ism_double_consonants <- strrep(c("b", "d", "g", "l", "n", "p", "t", "z"), 2)
English_doubled_consonants_ism <- paste0(English_ism_double_consonants, "ism")
English_er_double_consonants <- strrep(c("b", "d", "g", "m", "n", "p", "t"), 2)
English_doubled_consonants_er <- paste0(English_er_double_consonants, "er")
English_est_double_consonants <- strrep(c("b", "d", "g", "m", "n", "p", "t"), 2)
English_doubled_consonants_est <- paste0(English_est_double_consonants, "est")
English_ed_double_consonants <- strrep(c("b", "d", "g", "l", "m", "n", "p", "r", "t", "v", "z"), 2)
English_doubled_consonants_ed <- paste0(English_ed_double_consonants, "ed")
English_ing_double_consonants <- strrep(c("b", "d", "g", "l", "m", "n", "p", "r", "t", "v", "z"), 2)
English_doubled_consonants_ing <- paste0(English_ing_double_consonants, "ing")

English_eer_keepers <- c("beer", "career", "cheer", "deer", "domineer", "engineer", "killdeer", "jeer", "leer", "peer", "pioneer", "queer", "reindeer", "schmeer", "sheer", "sneer", "steer", "veer", "veneer", "volunteer")
English_ier_keepers <- c("brier", "cashier", "cavalier", "chandelier", "courier", "frontier", "glacier", "\\bpier", "premier", "soldier", "\\bspier", "\\btier")
English_er_keepers <- c("under", "whether", "\\bever", "whenever", "wherever", "whichever", "whoever", "whomever", "however", "whatever", "whatsoever", "forever", "either", "neither", "after", "\\bnever", "\\bher", "differ", "number", "tower", "dinner", "matter", "trouser", "mister", "minister", "amber", "customer", "harbinger", "monger", "\\banger", "manger", "ganger", "\\bother", "another", "paper", "(head)?quarter", "helicopter", "over", "member", "water", "fiber", "wonder", "ancester", "cloister", "confer", "corner", "enter", "per", "luster", "neuter", "scepter", "order", "deliver", "prefer", "defer", "foster", "cluster", "murder", "chamber", "september", "october", "november", "december", "register", "weather", "together", "letter", "newsletter", "chapter", "better", "poker", "further", "farther", "remember", "river", "silver", "rather", "summer", "winter", "super", "cancer", "answer", "transfer", "filter", "consider", "partner", "character", "father", "mother", "brother", "sister", "daughter", "leather", "upper", "lower", "laser", "theater", "gender", "soccer", "proper", "refer", "master", "meter", "rubber", "monster", "mester", "prefer", "latter", "tiger", "finger", "danger", "powder", "integer", "pepper", "cover", "spider", "cyber", "shelter", "suffer", "beaver", "trigger", "fever", "butler", "timber", "gather", "roster", "encounter", "hammer", "cylinder", "boulder", "thunder", "ester", "render", "after", "monomer", "dimer", "trimer", "tetramer", "polymer", "bitter", "usher", "ginger", "carpenter", "clever", "alzheimer", "lavender", "eager", "surrender", "lumber", "diaper", "jupiter", "sweater", "minister", "litter", "panther", "pewter", "clutter", "bladder", "lever", "feather", "burger", "ledger", "lobster", "slaughter", "glitter", "garner", "oyster", "clover", "power", "conquer", "badger", "butcher", "register", "kosher", "viper", "whisper", "flower", "utter", "cater", "doppler", "snooker", "juniper", "cucumber", "deter", "infer", "ether", "caliber", "center", "hooker", "cider", "splinter", "chapter", "batter", "sober", "sinister", "otter", "slender", English_eer_keepers, English_ier_keepers)
English_iest_keepers <- c("priest")
English_est_keepers <- c("\\bbest", "digest", "earnest", "(?:\\b|gab|love|slug|song)fest", "harvest", "honest", "\\bjest", "\\blest", "manifest", "\\bnest", "\\bpest", "(?:\\b|arm|head)rest", "\\btest", "\\bvest", "(?:\\b|mid|north|south)west", "\\bzest", "arbalest", "arrest", "attest", "\\bchest", "contest", "crest", "forest", "(?:\\b|house)guest", "infest", "invest", "interest", "protest", "(?:\\b|ac|be|con|in|re)quest", "suggest", "tempest", English_iest_keepers)
English_ed_keepers <- c("\\bbed", "bred", "\\bfed", "hundred", "infrared", "kindred", "naked", "need", "\\bred", "sacred", "\\bshed", "watershed", "\\bwed", "\\bzed")
English_ing_keepers <- c("bring", "ceiling", "\\bcling", "darling", "\\bding", "\\bduring", "evening", "\\bfling", "\\bking", "lightning", "morning", "\\bpending", "\\bping", "\\bring", "\\bsing", "(?:\\b|un|war)sling", "spring", "sterling", "\\bsting", "string", "swing", "(?:\\b|any|every|no|some)?thing", "(?:\\b|hind)wing", "\\bwring", "\\bzing")

English_s_keepers <- c("always", "perhaps", "whereas", "has", "is", "was")

# rules for what kinds of word endings require an ultimate "e"
general_e_rules <- non_capturing_group("(?:\\b|[^aieou]|ll)[aeiouy][bcfgkmsvz]")
ce_rules <- non_capturing_group("[lnrs]c")
de_rules <- non_capturing_group("(?:ui|[^aeiou][aeiou])d")
ge_rules <- non_capturing_group("(?:[dlr]|(?:(?:r|ch|str)a|(?:ll|v)e|(?:b|h|cr)i|(?:c|sp)o|(?:l|p|pl|scro)u)n)g")
le_rules <- non_capturing_group("(?:(?:imp|wholes|sc|wh)a|(?:(?:\\b|de)f|p|\\b[prt]|rev|sm)i|(?:cond|h|par|\\bp|recons|\\bt)o|(?:r|sched)u|y|[bcdfgkpstz])l")
ne_rules <- non_capturing_group("(?:[^aeiou][aiu]|(?:\\b([bchtz]|cl|dr)|chaper|(?:de|im|post|pro)p|ph|thr|[as]t)o)n")
oe_rules <- non_capturing_group("(?:\\bh|(?:\\b|tip(py)?)t|sh)o")
pe_rules <- non_capturing_group("(?:[^aeiou][aeiuy]|(?:\\b([cdhmr]|el)|gr|sc)o)p")
re_rules <- non_capturing_group("(?:[^aeiou][aiu]|(?:\\b(?:[bcgps]|ad|ch|depl|enc|expl|ign|impl|rest|sh|sp|st|wh)|sc|sn)o|qui)r")
se_rules <- non_capturing_group("(?:(?:ai|au|ea|ee|oi|oo|(?:(?:\\b|[^l])[^l]|\\bl)ou)|ui|[lnrw])s")
te_rules <- non_capturing_group("(?:(?:[^eo]|cre|ide)a|(?:comp|compl|del|excr)e|(?:(?:\\b|[^abeiou])b|(?:\\b|[^i])c|ign|ind|inv|sm|sp|qu|un|wh|wr|xc)i|(?:\\b[cdntv]|m|qu|[^i]v)o|(?:[^aeiou]|\\bro)u|[bhptw]as)t")
ue_rules <- non_capturing_group("u")
ve_rules <- non_capturing_group("(?:ai|ea|ee|ei|i|[lr])v")
ye_rules <- non_capturing_group("(?:\\b|cross|hawk)ey")
ze_rules <- non_capturing_group("[^tz]z")
che_rules <- non_capturing_group("(?:(?:\\b|back|belly|head|stomach|tooth)a|ca)ch")
e_rules <- non_capturing_group(any_of(c(general_e_rules, ce_rules, de_rules, ge_rules, le_rules, ne_rules, oe_rules, pe_rules, re_rules, se_rules, te_rules, ue_rules, ve_rules, ye_rules, ze_rules, che_rules)))

digest_words <- function(words) {
  # description:  Used for "stemming" or "lemmatizing" words for Natural Language Processing. 
  # description:  Works by removing prefixes and suffixes in the appropriate order. 
  # description:  
  # description:  It's more accurate than typical stemming approaches: 
  # description:    (fewer confabulated results and more correctly connected results, 
  # description:    because it tests for and handles many special cases). 
  # description:  
  # description:  It's more user-friendly than typical lemmatizing approaches:
  # description:    (you don't need to worry about parts of speech, 
  # description:    and it automatically goes to the most basic form). 
  # 
  # details:  Uses the companion digested_word_dictionary(words) function to create a dictionary
  # details:    of the unique input words (as the element name/key) 
  # details:    and their digested outputs (as the element value). 
  # details:  Read the comments in digested_word_dictionary(words) for more information. 
  # details:  It relies on rules when there are rules (so it often works on made-up words), 
  # details:    but the rest is hard-coded (and there are admittedly still plenty of 
  # details:    gaps in coverage for special cases). 
  # details:  Uses the companion make_singular(words), is_plural(words), and singularize(words)
  # details:    functions for handling plural (especially foreign/Greek/Latin/unusual plural) forms. 
  # details:  See the documentation of these functions for more information. 
  # 
  # input:  a character vector of lower-case English words to "digest" into their core lemmas (most meaningful lexical components)
  # 
  # input specs:  NAs and "" elements are acceptable and do not cause error or warning. 
  # input specs:  Empty inputs are acceptable and do not cause error or warning. 
  # input specs:  Words containing contractions are acceptable and handled properly. 
  # input specs:  It also properly handles the last components of hyphenated words, 
  # input specs:    ignoring preceding compents (unless they're prefixes, in which case they're removed). 
  # input specs:  Proper nouns are currently *NOT* masked or handled properly, 
  # input specs:    so don't expect them to be returned unchanged. 
  # 
  # output:  a character vector of the "digested" words
  # 
  # output specs:  NAs elements returned as NA; "" elements returned as "". 
  # output specs:  Elements are returned in the same order (in a vector of the same length). 
  # output specs:  Nouns are returned in singular (non-plural) form. 
  # output specs:  Verbs are returned in infinitive form. 
  # output specs:  All negations (non-/un-/in-/dis-/anti-) are dropped. 
  # output specs:  Stopwords are returned unchanged--handle them on your own. 
  # 
  # example input: digest_words("antidisestablishmentarianismesquely")
  # example output:                    "establish"
  # 
  # example input: digest_words("supercalifragilisticexpialidocious")
  # example output:                  "califragilisticexpialidocious")
  # 
  # example input: digest_words("shouldn't've")
  # example output:             "shall"
  # 
  # example input: digest_words("can't-believe-it's-not-butterific")
  # example output:             "can't-believe-it's-not-butter"
  # 
  # example input: digest_words("re-doing")
  # example output:                "do"
  # 
  # notes:  This could be used in the future for grammatical approaches, 
  # notes:    as it breaks down words by part of speech-related suffixes. 
  # notes:  In future may separate contractions into component words. 
  # notes:  In future may handle co-, en-, inter-, intra-, re-, semi- prefixes. 
  # notes:  In future should handle encyclopaedia-like spellings. 
  # 
  results <- character(length(words))
  contains_words <- words != "" & !is.na(words)
  results[contains_words] <- digested_word_dictionary(words[contains_words])[words[contains_words]]
  return (results)
}

digested_word_dictionary <- function(words) {
  ### process only unique non-blank and non-NA values
  # (avoids redundant computation)
  original_text <- unique(words[words != "" & !is.na(words)])
  words <- original_text
  
  ### simplify number (singular/plural) to singular case
  # (obviates checking optional s on some suffixes--avoids unecessary computation)
  # has desired side effect of simplifying number (plurals and singulars alike all end up as singular)
  # has desired side effect of taking "ies" verb forms to "y" as well, further simplifying things
  can_be_made_singular <- words %!like% ending_with_word(any_of(English_s_keepers))
  words[can_be_made_singular] <- make_singular(words[can_be_made_singular])
  
  ### handle contractions
  # contractions block the ending of words (hiding endings in endsWith() checks), so they must be removed
  subset_scope <- words %exactlylike% "'"
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    subset <- gsub(subset, pattern = "\\bwon't\\b", replacement = "will")
    subset <- gsub(subset, pattern = "\\bmight've\\b", replacement = "might")
    subset <- grem(subset, "(?:n't|'ve|'ll|'re|')+$")
    words[subset_scope] <- subset
  }
  
  ### handle irregular words
  
  # irregular past participles ending in "dden"
  subset_scope <- endsWith(words, "dden")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "downtrodden" -> "downtread"
    odden_to_ead <- endsWith(subset, "trodden")
    subset[odden_to_ead] <- replace_last_n_chars_with(subset[odden_to_ead], 5L, "ead")
    # e.g. "forbidden" -> "forbid"
    delete_den <- endsWithAny(subset, c("adden", "bidden", "edden"))
    subset[delete_den] <- remove_last_n_chars(subset[delete_den], 3L)
    # e.g. "hidden" -> "hide"
    idden_to_ide <- endsWith(subset, "idden") & !endsWithAny(subset, c("midden", "swidden"))
    subset[idden_to_ide] <- replace_last_n_chars_with(subset[idden_to_ide], 3L, "e")
    words[subset_scope] <- subset
  }
  
  # irregular past participles ending in "tten"
  subset_scope <- endsWith(words, "tten")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "written" -> "write"
    itten_to_ite <- endsWith(subset, "itten") & !endsWithAny(subset, c("kitten", "mitten"))
    subset[itten_to_ite] <- replace_last_n_chars_with(subset[itten_to_ite], 3L, "e")
    # e.g. "rotten" -> "rot"
    delete_ten <- endsWithAny(subset, c("atten", "otten"))
    subset[delete_ten] <- remove_last_n_chars(subset[delete_ten], 3L)
    words[subset_scope] <- subset
  }
  
  # irregular past participles ending in "en" (and a few adjectives)
  subset_scope <- endsWith(words, "en")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "eaten" -> "eat" (also "been" to "be")
    delete_en <- endsWithAny(subset, c("ashen", "been", "drunken", "earthen", "eaten", "fallen", "olden", "silken", "swollen", "wooden", "woolen"))
    subset[delete_en] <- remove_last_n_chars(subset[delete_en], 2L)
    # e.g. "broken" -> "broke" (later to "break")
    delete_n <- endsWithAny(subset, c("aken", "chosen", "iven", "oken", "olen", "oven", "risen", "rozen", "seen")) & !(endsWith(subset, "kraken") | subset %like% "\\boven$")
    subset[delete_n] <- remove_last_n_chars(subset[delete_n], 1L)
    words[subset_scope] <- subset
  }
  
  # irregular past participles ending in "n"
  subset_scope <- endsWith(words, "n")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "worn" -> "wore" (later to "wear")
    n_to_e <- endsWithAny(subset, c("born", "torn", "worn")) & !endsWithAny(subset, c("stubborn", "attorn"))
    subset[n_to_e] <- replace_last_n_chars_with(subset[n_to_e], 1L, "e")
    # e.g. "lain" -> "lie"
    ain_to_ay <- endsWith(subset, "lain")
    subset[ain_to_ay] <- replace_last_n_chars_with(subset[ain_to_ay], 3L, "ie")
    # e.g. "shorn" -> "shear"
    orn_to_ear <- endsWith(subset, "shorn")
    subset[orn_to_ear] <- replace_last_n_chars_with(subset[orn_to_ear], 3L, "ear")
    # e.g. "drawn" -> "draw"
    delete_n <- subset %like% ending_with_word(any_of(c("blown", "drawn", "grown", "known", "sewn", "shaken", "shown", "sown", "thrown")))
    subset[delete_n] <- remove_last_n_chars(subset[delete_n], 1L)
    words[subset_scope] <- subset
  }
  
  # irregular past participles ending in "t"
  subset_scope <- endsWith(words, "t")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "burnt" -> "burn"
    delete_t <- endsWithAny(subset, c("burnt", "dealt", "dreamt", "learnt", "meant"))
    subset[delete_t] <- remove_last_n_chars(subset[delete_t], 1L)
    # e.g. "built" -> "build"
    t_to_d <- endsWithAny(subset, c("built", "spent")) | subset %like% ending_with_word(any_of(c("bent", "lent", "sent")))
    subset[t_to_d] <- replace_last_n_chars_with(subset[t_to_d], 1L, "d")
    # e.g. "lost" -> "lose"
    t_to_e <- endsWith(subset, "lost")
    subset[t_to_e] <- replace_last_n_chars_with(subset[t_to_e], 1L, "e")
    # e.g. "left" -> "leave"
    eft_to_eave <- endsWithAny(subset, c("bereft", "left"))
    subset[eft_to_eave] <- replace_last_n_chars_with(subset[eft_to_eave], 2L, "ave")
    words[subset_scope] <- subset
  }
  
  #*** prevents spurious edits later on
  #*** make common irregular words get fixed even if not at end of word phrase
  
  # common irregular words
  reasonable_slice <- words %like% "\\ban$"
  words[reasonable_slice] <- remove_last_n_chars(words[reasonable_slice], 1L)
  
  reasonable_slice <- endsWithAny(words, c("am", "are", "is", "was", "were"))
  words[reasonable_slice] <- gsub(words[reasonable_slice], pattern = ending_with_word(any_of(c("am", "are", "been", "is", "was", "were"))), replacement = "be")
  
  reasonable_slice <- endsWithAny(words, c("did", "done"))
  words[reasonable_slice] <- gsub(words[reasonable_slice], pattern = ending_with_word(any_of(c("did", "done"))), replacement = "do")
  
  reasonable_slice <- words %like% "\\bha[ds]$"
  words[reasonable_slice] <- replace_last_n_chars_with(words[reasonable_slice], 1L, "ve")
  
  reasonable_slice <- endsWithAny(words, c("went", "gone"))
  words[reasonable_slice] <- gsub(words[reasonable_slice], pattern = ending_with_word(any_of(c("went", "gone"))), replacement = "go")
  
  reasonable_slice <- endsWithAny(words, c("ate", "edible", "edibly"))
  words[reasonable_slice] <- gsub(words[reasonable_slice], pattern = ending_with_word(any_of(c("eats", "ate", "eaten", "eating", "edible", "edibly"))), replacement = "eat")
  
  reasonable_slice <- endsWithAny(words, c("cannot", "could"))
  words[reasonable_slice] <- gsub(words[reasonable_slice], pattern = ending_with_word(any_of(c("cannot", "could"))), replacement = "can")
  
  reasonable_slice <- endsWith(words, "should")
  words[reasonable_slice] <- replace_last_n_chars_with(words[reasonable_slice], 4L, "all")
  
  reasonable_slice <- endsWith(words, "might")
  words[reasonable_slice] <- replace_last_n_chars_with(words[reasonable_slice], 4L, "ay")
  
  reasonable_slice <- endsWithAny(words, c("bore", "borne"))
  words[reasonable_slice] <- gsub(words[reasonable_slice], pattern = ending_with_word(any_of(c("bore", "born", "borne"))), replacement = "bear")
  
  reasonable_slice <- endsWithAny(words, c("better", "best"))
  words[reasonable_slice] <- gsub(words[reasonable_slice], pattern = ending_with_word(any_of(c("better", "best"))), replacement = "good")
  
  reasonable_slice <- endsWithAny(words, c("worse", "worst"))
  words[reasonable_slice] <- gsub(words[reasonable_slice], pattern = ending_with_word(any_of(c("worse", "worst"))), replacement = "bad")
  
  reasonable_slice <- endsWith(words, "these")
  words[reasonable_slice] <- replace_last_n_chars_with(words[reasonable_slice], 3L, "is")
  
  reasonable_slice <- endsWith(words, "those")
  words[reasonable_slice] <- replace_last_n_chars_with(words[reasonable_slice], 3L, "at")
  
  # irregular verbs without much pattern
  
  # handle irregulars ending in "d"
  subset_scope <- endsWith(words, "d")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    
    subsubset_scope <- endsWith(subset, "ed")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      ed_to_ead <- subsubset %like% "\\bled$"
      subsubset[ed_to_ead] <- replace_last_n_chars_with(subsubset[ed_to_ead], 1L, "ad")
      ed_to_ee <- subsubset %like% "\\bfled$"
      subsubset[ed_to_ee] <- replace_last_n_chars_with(subsubset[ed_to_ee], 1L, "e")
      ed_to_eed <- subsubset %like% ending_with_word(any_of(c("bled", "bred", "fed", "sped")))
      subsubset[ed_to_eed] <- replace_last_n_chars_with(subsubset[ed_to_eed], 1L, "ed")
      subset[subsubset_scope] <- subsubset
    }
    
    subsubset_scope <- endsWith(subset, "id")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      id_to_ide <- endsWith(subsubset, "slid") | subsubset %like% "\\bhid$"
      subsubset[id_to_ide] <- paste0(subsubset[id_to_ide], "e")
      aid_to_ay <- endsWithAny(subsubset, c("laid", "paid", "said")) & !endsWith(subsubset, "plaid")
      subsubset[aid_to_ay] <- replace_last_n_chars_with(subsubset[aid_to_ay], 2L, "y")
      subset[subsubset_scope] <- subsubset
    }
    
    subsubset_scope <- endsWith(subset, "ld")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      eld_to_old <- endsWith(subsubset, "held")
      subsubset[eld_to_old] <- replace_last_n_chars_with(subsubset[eld_to_old], 3L, "old")
      old_to_ell <- endsWithAny(subsubset, c("sold", "told"))
      subsubset[old_to_ell] <- replace_last_n_chars_with(subsubset[old_to_ell], 3L, "ell")
      subset[subsubset_scope] <- subsubset
    }
    
    ound_to_ind <- endsWithAny(subset, c("bound", "found")) # "ground", "wound" (these are also unrelated nouns)
    subset[ound_to_ind] <- replace_last_n_chars_with(subset[ound_to_ind], 4L, "ind")
    
    subsubset_scope <- endsWith(subset, "od")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      od_to_ead <- endsWith(subsubset, "trod")
      subsubset[od_to_ead] <- replace_last_n_chars_with(subsubset[od_to_ead], 2L, "ead")
      ood_to_and <- endsWith(subsubset, "stood")
      subsubset[ood_to_and] <- replace_last_n_chars_with(subsubset[ood_to_and], 3L, "and")
      subset[subsubset_scope] <- subsubset
    }
    
    eard_to_ear <- endsWith(subset, "heard")
    subset[eard_to_ear] <- remove_last_n_chars(subset[eard_to_ear], 1L)
    
    words[subset_scope] <- subset
  }
  
  # handle irregulars ending in "e"
  subset_scope <- endsWith(words, "e")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    
    subsubset_scope <- endsWith(subset, "de")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      ade_to_ake <- endsWith(subsubset, "made") & !endsWithAny(subsubset, c("amade", "omade"))
      subsubset[ade_to_ake] <- replace_last_n_chars_with(subsubset[ade_to_ake], 2L, "ke")
      ade_to_id <- endsWith(subsubset, "forbade") | subsubset %like% "\\bbade$"
      subsubset[ade_to_id] <- replace_last_n_chars_with(subsubset[ade_to_id], 3L, "id")
      ode_to_ide <- endsWithAny(subsubset, c("joyrode", "outrode", "overrode", "strode")) | subsubset %like% "\\brode$"
      subsubset[ode_to_ide] <- replace_last_n_chars_with(subsubset[ode_to_ide], 3L, "ide")
      subset[subsubset_scope] <- subsubset
    }
    
    subsubset_scope <- endsWith(subset, "ke")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      oke_to_ake <- endsWith(subsubset, "woke")
      subsubset[oke_to_ake] <- replace_last_n_chars_with(subsubset[oke_to_ake], 3L, "ake")
      oke_to_eak <- endsWithAny(subsubset, c("broke", "spoke"))
      subsubset[oke_to_eak] <- replace_last_n_chars_with(subsubset[oke_to_eak], 3L, "eak")
      subset[subsubset_scope] <- subsubset
    }
    
    ole_to_eal <- endsWith(subset, "stole")
    subset[ole_to_eal] <- replace_last_n_chars_with(subset[ole_to_eal], 3L, "eal")
    
    ame_to_ome <- endsWith(subset, "came")
    subset[ame_to_ome] <- replace_last_n_chars_with(subset[ame_to_ome], 3L, "ome")
    
    one_to_ine <- endsWith(subset, "shone")
    subset[one_to_ine] <- replace_last_n_chars_with(subset[one_to_ine], 3L, "ine")
    
    ore_to_ear <- endsWithAny(subset, c("tore", "wore")) & !endsWithAny(subset, c("atore", "store"))
    subset[ore_to_ear] <- replace_last_n_chars_with(subset[ore_to_ear], 3L, "ear")
    
    subsubset_scope <- endsWith(subset, "se")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      ose_to_ise <- subsubset %like% "\\brose$"
      subsubset[ose_to_ise] <- replace_last_n_chars_with(subsubset[ose_to_ise], 3L, "ise")
      ose_to_oose <- endsWith(subsubset, "chose")
      subsubset[ose_to_oose] <- replace_last_n_chars_with(subsubset[ose_to_oose], 2L, "ose")
      subset[subsubset_scope] <- subsubset
    }
    
    ote_to_ite <- endsWithAny(subset, c("smote", "wrote"))
    subset[ote_to_ite] <- replace_last_n_chars_with(subset[ote_to_ite], 3L, "ite")
    
    subsubset_scope <- endsWith(subset, "ve")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      ave_to_ive <- endsWith(subsubset, "gave") & !endsWith(subsubset, "agave")
      subsubset[ave_to_ive] <- replace_last_n_chars_with(subsubset[ave_to_ive], 3L, "ive")
      ove_to_eave <- endsWith(subsubset, "wove")
      subsubset[ove_to_eave] <- replace_last_n_chars_with(subsubset[ove_to_eave], 3L, "eave")
      ove_to_ive <- endsWithAny(subsubset, c("drove", "strove", "throve")) | subsubset %like% "\\bdove$"
      subsubset[ove_to_ive] <- replace_last_n_chars_with(subsubset[ove_to_ive], 3L, "ive")
      subset[subsubset_scope] <- subsubset
    }
    
    oze_to_eeze <- endsWith(subset, "froze")
    subset[oze_to_eeze] <- replace_last_n_chars_with(subset[oze_to_eeze], 3L, "eeze")
    
    words[subset_scope] <- subset
  }
  
  # handle irregulars ending in "g"
  subset_scope <- endsWith(words, "g")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    aong_to_ing <- endsWithAny(subset, c("rang", "sang", "song", "sprang", "strang", "swang", "wrang"))
    subset[aong_to_ing] <- replace_last_n_chars_with(subset[aong_to_ing], 3L, "ing")
    
    # handle "ung" irregulars
    subsubset_scope <- endsWith(subset, "ung")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      ung_to_ang <- endsWith(subsubset, "hung")
      subsubset[ung_to_ang] <- replace_last_n_chars_with(subsubset[ung_to_ang], 3L, "ang")
      ung_to_ing <- endsWithAny(subsubset, c("clung", "flung", "rung", "slung", "sprung", "strung", "stung", "sung", "swung", "wrung"))
      subsubset[ung_to_ing] <- replace_last_n_chars_with(subsubset[ung_to_ing], 3L, "ing")
      subset[subsubset_scope] <- subsubset
    }
    
    ug_to_ig <- endsWith(subset, "dug")
    subset[ug_to_ig] <- replace_last_n_chars_with(subset[ug_to_ig], 2L, "ig")
    
    words[subset_scope] <- subset
  }
  
  # handle irregulars ending in "k"
  subset_scope <- endsWith(words, "k")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    
    subsubset_scope <- endsWith(subset, "ve")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      uck_to_ick <- endsWith(subsubset, "stuck")
      subsubset[uck_to_ick] <- replace_last_n_chars_with(subsubset[uck_to_ick], 3L, "ick")
      uck_to_ike <- endsWith(subsubset, "struck")
      subsubset[uck_to_ike] <- replace_last_n_chars_with(subsubset[uck_to_ike], 3L, "ike")
      subset[subsubset_scope] <- subsubset
    }
    
    aunk_to_ink <- endsWithAny(subset, c("drank", "drunk", "sank", "sunk", "slank", "slunk", "stank", "stunk"))
    subset[aunk_to_ink] <- replace_last_n_chars_with(subset[aunk_to_ink], 3L, "ink")
    
    ook_to_ake <- endsWithAny(subset, c("forsook", "shook", "took"))
    subset[ook_to_ake] <- replace_last_n_chars_with(subset[ook_to_ake], 3L, "ake")
    
    words[subset_scope] <- subset
  }
  
  # handle irregulars ending in "ll"
  subset_scope <- endsWith(words, "ll")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    ell_to_all <- endsWith(subset, "fell")
    subset[ell_to_all] <- replace_last_n_chars_with(subset[ell_to_all], 3L, "all")
    oll_to_ell <- endsWith(subset, "swoll")
    subset[oll_to_ell] <- replace_last_n_chars_with(subset[oll_to_ell], 3L, "ell")
    words[subset_scope] <- subset
  }
  
  aum_to_im <- endsWithAny(words, c("swam", "swum"))
  words[aum_to_im] <- replace_last_n_chars_with(words[aum_to_im], 2L, "im")
  
  # handle irregulars ending in "n"
  subset_scope <- endsWith(words, "n")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    an_to_un <- endsWithAny(subset, c("foreran", "reran", "outran", "overran")) | subset %like% "\\bran$"
    subset[an_to_un] <- replace_last_n_chars_with(subset[an_to_un], 2L, "un")
    on_to_in <- endsWith(subset, "won")
    subset[on_to_in] <- replace_last_n_chars_with(subset[on_to_in], 2L, "in")
    aun_to_in <- endsWithAny(subset, c("began", "begun", "spun"))
    subset[aun_to_in] <- replace_last_n_chars_with(subset[aun_to_in], 2L, "in")
    own_to_y <- endsWith(subset, "flown")
    subset[own_to_y] <- replace_last_n_chars_with(subset[own_to_y], 3L, "y")
    words[subset_scope] <- subset
  }
  
  # handle irregulars ending in "t"
  subset_scope <- endsWith(words, "t")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    at_to_it <- endsWithAny(subset, c("sat", "spat"))
    subset[at_to_it] <- replace_last_n_chars_with(subset[at_to_it], 2L, "it")
    
    et_to_eet <- subset %like% "\\bmet$"
    subset[et_to_eet] <- replace_last_n_chars_with(subset[et_to_eet], 1L, "et")
    
    # irregular verbs ending in "aught" or "ought"
    subsubset_scope <- endsWith(subset, "ught")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      ought_to_ing <- endsWith(subsubset, "brought")
      subsubset[ought_to_ing] <- replace_last_n_chars_with(subsubset[ought_to_ing], 5L, "ing")
      ought_to_uy <- endsWith(subsubset, "bought")
      subsubset[ought_to_uy] <- replace_last_n_chars_with(subsubset[ought_to_uy], 5L, "uy")
      ought_to_eek <- endsWith(subsubset, "sought")
      subsubset[ought_to_eek] <- replace_last_n_chars_with(subsubset[ought_to_eek], 5L, "eek")
      ought_to_ight <- endsWith(subsubset, "fought")
      subsubset[ought_to_ight] <- replace_last_n_chars_with(subsubset[ought_to_ight], 5L, "ight")
      ought_to_ink <- endsWith(subsubset, "thought")
      subsubset[ought_to_ink] <- replace_last_n_chars_with(subsubset[ought_to_ink], 5L, "ink")
      aught_to_atch <- endsWith(subsubset, "caught")
      subsubset[aught_to_atch] <- replace_last_n_chars_with(subsubset[aught_to_atch], 5L, "atch")
      aught_to_each <- endsWith(subsubset, "taught")
      subsubset[aught_to_each] <- replace_last_n_chars_with(subsubset[aught_to_each], 5L, "each")
      subset[subsubset_scope] <- subsubset
    }
    
    it_to_ight <- endsWith(subset, "lit") & !endsWithAny(subset, c("llit", "slit", "split"))
    subset[it_to_ight] <- replace_last_n_chars_with(subset[it_to_ight], 1L, "ght")
    it_to_ite <- endsWithAny(subset, c("frostbit", "snakebit")) | subset %like% "\\bbit$"
    subset[it_to_ite] <- replace_last_n_chars_with(subset[it_to_ite], 2L, "ite")
    
    elt_to_eel <- endsWithAny(subset, c("felt", "knelt"))
    subset[elt_to_eel] <- replace_last_n_chars_with(subset[elt_to_eel], 2L, "el")
    
    ept_to_eep <- endsWithAny(subset, c("crept", "kept", "slept", "swept", "wept"))
    subset[ept_to_eep] <- replace_last_n_chars_with(subset[ept_to_eep], 2L, "ep")
    
    ot_to_et <- endsWithAny(subset, c("begot", "forgot")) | subset %like% "\\bgot$"
    subset[ot_to_et] <- replace_last_n_chars_with(subset[ot_to_et], 2L, "et")
    ot_to_oot <- endsWithAny(subset, c("countershot", "outshot", "overshot", "reshot", "upshot", "troubleshot")) | subset %like% "\\bshot$"
    subset[ot_to_oot] <- replace_last_n_chars_with(subset[ot_to_oot], 1L, "ot")
    
    words[subset_scope] <- subset
  }
  
  # handle irregulars ending in "w"
  subset_scope <- endsWith(words, "w")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    aw_to_ee <- endsWithAny(subset, c("foresaw", "oversaw", "resaw", "sightsaw")) | subset %like% "\\bsaw$"
    subset[aw_to_ee] <- replace_last_n_chars_with(subset[aw_to_ee], 2L, "ee")
    
    # irregular verbs ending in "ew"
    subsubset_scope <- endsWith(subset, "ew")
    subsubset <- subset[subsubset_scope]
    if (isnt_empty(subsubset)) {
      ew_to_aw <- endsWith(subsubset, "drew")
      subsubset[ew_to_aw] <- replace_last_n_chars_with(subsubset[ew_to_aw], 2L, "aw")
      ew_to_y <- endsWith(subsubset, "flew")
      subsubset[ew_to_y] <- replace_last_n_chars_with(subsubset[ew_to_y], 2L, "y")
      ew_to_ay <- endsWith(subsubset, "slew")
      subsubset[ew_to_ay] <- replace_last_n_chars_with(subsubset[ew_to_ay], 2L, "ay")
      ew_to_ow <- endsWithAny(subsubset, c("blew", "grew", "knew", "threw"))
      subsubset[ew_to_ow] <- replace_last_n_chars_with(subsubset[ew_to_ow], 2L, "ow")
      subset[subsubset_scope] <- subsubset
    }
    
    words[subset_scope] <- subset
  }
  
  # ay_to_ie <- words %like% "\\blay$"
  # words[ay_to_ie] <- replace_last_n_chars_with(words[ay_to_ie], 2L, "ie")
  
  ### handle prefixes
  
  # decelerate/devolve ~ accelerate/evolve
  # handled before most prefixes because otherwise "de" would be handled incorrectly
  de_to_ac <- words %like% "\\bdecel"
  words[de_to_ac] <- gsub(words[de_to_ac], pattern = "\\bde", replacement = "ac")
  de_to_e <- words %like% "\\bdevol"
  words[de_to_e] <- grem(words[de_to_e], "\\bd")
  
  # prevent removal of prefix-like forms that actually aren't acting as prefixes
  has_keepable_prefix <- words %like% beginning_with(any_of(English_prefix_keepers))
  # removes multiple (nested) prefixes
  # excludes a few difficult cases for further processing below
  delete_prefix <- !has_keepable_prefix & words %like% "\\b(?:(?:(?:a|de|ex|post|pre|re|semi|un|well)-)|((anti|dis|im[bmp]|hyper|hypo|in|mis|non|over|sub|super|under|un)-?))" & words %!like% "\\b(?:none($|theless)|im(?:migra|pov|prop))"
  words[delete_prefix] <- grem(words[delete_prefix], "\\b(?:(?:a|de|ex|post|pre|re|semi|un|well)-|(?:anti|dis|im[bmp]|hyper|hypo|in|mis|non|over|sub|super|under|un)-?)(?:(?:a|de|ex|post|pre|re|semi|un|well)-|(?:anti|dis|im[bmp]|hyper|hypo|in|mis|non|over|sub|super|under|un)-?)*")
  
  # needs to be separate because the above rule would have taken immigrate/improper to igrate/roper
  delete_im_prefix <- words %like% "\\bim(?:migra|pov|prop)"
  words[delete_im_prefix] <- grem(words[delete_im_prefix], "\\bim")
  
  #* could add "ir" to normal prefix set above if a list of English_ir_keepers is made and used
  delete_ir_prefix <- words %like% "\\birr" & words %!like% "\\birrigat"
  words[delete_ir_prefix] <- grem(words[delete_ir_prefix], "\\bir")
  
  #* could add "ab" to normal prefix set above if a list of English_ab_keepers is made and used
  delete_ab_prefix <- words %like% "\\babnormal"
  words[delete_ab_prefix] <- grem(words[delete_ab_prefix], "\\bab")
  
  #* could add "mal" to normal prefix set above if a list of English_mal_keepers is made and used
  delete_mal_prefix <- words %like% "\\bmal" & words %!like% "\\bmal(?:ady|ari|ark|e(?:$|s|ness)|efa|efi|evo|ici|ign|ing|l(?:$|[aeiou])|m|(?:$|t[aeiou]))"
  words[delete_mal_prefix] <- grem(words[delete_mal_prefix], "\\bmal")
  
  ### handle first batch of generic noun and adjective suffixes
  
  # handle "ly" prefix
  subset_scope <- endsWith(words, "ly")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # sometimes "lly" -> "ly"
    ly_to_l <- endsWithAny(subset, c("billy", "hilly", "frilly")) | subset %like% "\\bfully$"
    subset[ly_to_l] <- remove_last_n_chars(subset[ly_to_l], 1L)
    # e.g. "ably" -> "able"
    bly_to_ble <- endsWith(subset, "bly")
    subset[bly_to_ble] <- replace_last_n_chars_with(subset[bly_to_ble], 1L, "e")
    ly_keeper_mask <- subset %!like% ending_with_word(any_of(English_ly_keepers))
    # e.g. "happily" -> "happy"
    ily_to_y <- endsWith(subset, "ily") & ly_keeper_mask
    subset[ily_to_y] <- replace_last_n_chars_with(subset[ily_to_y], 3L, "y")
    # e.g. "subtly" -> "subtle"
    ly_to_le <- endsWithAny(subset, English_ly_to_le_words) & ly_keeper_mask
    subset[ly_to_le] <- replace_last_n_chars_with(subset[ly_to_le], 1L, "e")
    # e.g. "truly" -> "true"
    ly_to_e <- endsWithAny(subset, c("uly", "wholly"))
    subset[ly_to_e] <- replace_last_n_chars_with(subset[ly_to_e], 2L, "e")
    # general rule--remove suffix
    delete_ly <- endsWith(subset, "ly") & ly_keeper_mask
    subset[delete_ly] <- remove_last_n_chars(subset[delete_ly], 2L)
    words[subset_scope] <- subset
  }
  
  # ("especially" ->) "especial" -> "special"
  is_especial <- endsWith(words, "especial")
  words[is_especial] <- gsub(words[is_especial], pattern = "\\bespecial$", replacement = "special")
  
  # handle "ness" suffix
  subset_scope <- endsWith(words, "ness")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "cleanliness" -> "clean"
    delete_liness <- endsWith(subset, "liness")
    subset[delete_liness] <- remove_last_n_chars(subset[delete_liness], 6L)
    # e.g. "happiness" -> "happy"
    iness_to_y <- endsWith(subset, "iness") & !endsWith(subset, "business")
    subset[iness_to_y] <- replace_last_n_chars_with(subset[iness_to_y], 5L, "y")
    # general rule--remove suffix
    delete_ness <- endsWith(subset, "ness") & !endsWith(subset, "business")
    subset[delete_ness] <- remove_last_n_chars(subset[delete_ness], 4L)
    words[subset_scope] <- subset
  }
  
  # handle "ity" suffix
  subset_scope <- endsWith(words, "ity")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "mobility" -> "mobile"
    bility_to_bile <- subset %like% "(?:\\bla|mo|nu)bility$"
    subset[bility_to_bile] <- replace_last_n_chars_with(subset[bility_to_bile], 3L, "e")
    # e.g. "ability" -> "able"
    bility_to_ble <- endsWith(subset, "bility")
    subset[bility_to_ble] <- replace_last_n_chars_with(subset[bility_to_ble], 5L, "le")
    # e.g. "activity" -> "active"
    ity_to_e <- endsWithAny(subset, c("antiquity", "purity", "ivity"))
    subset[ity_to_e] <- replace_last_n_chars_with(subset[ity_to_e], 3L, "e")
    # e.g. "credulity" -> "credulous"
    ulity_to_ulous <- endsWith(subset, "ulity")
    subset[ulity_to_ulous] <- replace_last_n_chars_with(subset[ulity_to_ulous], 3L, "ous")
    # e.g. "hilarity" -> "hilarious"
    arity_to_arious <- endsWith(subset, "hilarity")
    subset[arity_to_arious] <- replace_last_n_chars_with(subset[arity_to_arious], 2L, "ous")
    # e.g. "clarity" -> "clear"
    arity_to_ear <- endsWith(subset, "clarity")
    subset[arity_to_ear] <- replace_last_n_chars_with(subset[arity_to_ear], 5L, "ear")
    # general rule--leave suffix unless ends with "al", "ic", or "lar"
    delete_ity <- (endsWithAny(subset, c("ality", "icity", "larity")) & !endsWithAny(subset, c("complicity", "felicity", "quality"))) | subset %like% ending_with(any_of(paste0(English_al_keepers, "ity")))
    subset[delete_ity] <- remove_last_n_chars(subset[delete_ity], 3L)
    words[subset_scope] <- subset
  }
  
  # remove other "ty" suffixes
  delete_ty <- endsWithAny(words, c("certainty", "nicety"))
  words[delete_ty] <- remove_last_n_chars(words[delete_ty], 2L)
  
  # handle "esque" suffix
  subset_scope <- endsWith(words, "esque")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "statuesque" -> "statue"
    esque_to_e <- endsWithAny(subset, c("uesque", "uresque"))
    subset[esque_to_e] <- remove_last_n_chars(subset[esque_to_e], 4L)
    # general rule--remove suffix
    delete_esque <- endsWith(subset, "esque") & !endsWithAny(subset, c("burlesque", "grotesque"))
    subset[delete_esque] <- remove_last_n_chars(subset[delete_esque], 5L)
    words[subset_scope] <- subset
  }
  
  # handle "ish" suffix
  subset_scope <- endsWith(words, "ish")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "(im)poverish" -> "poverty"
    ish_to_ty <- endsWith(subset, "poverish")
    subset[ish_to_ty] <- replace_last_n_chars_with(subset[ish_to_ty], 3L, "ty")
    # e.g. "piggish" -> "pig"
    delete_ish_letter <- endsWithAny(subset, c("cattish", "doggish", "hottish", "piggish"))
    subset[delete_ish_letter] <- remove_last_n_chars(subset[delete_ish_letter], 4L)
    # e.g. "brutish" -> "brute"
    ish_to_e <- endsWithAny(subset, c("vampirish", "vulturish", "brutish", "ttish", "dovish", "voguish", "purplish", "ylish"))
    subset[ish_to_e] <- replace_last_n_chars_with(subset[ish_to_e], 3L, "e")
    # general rule--remove suffix
    delete_ish <- endsWith(subset, "ish") & (!endsWithAny(subset, English_ish_keepers) | endsWith(subset, "oafish"))
    subset[delete_ish] <- remove_last_n_chars(subset[delete_ish], 3L)
    words[subset_scope] <- subset
  }
  
  # handle "able" suffixes
  subset_scope <- endsWith(words, "able")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    able_keeper_mask <- subset %!like% ending_with_word(any_of(English_able_keepers))
    # e.g. "reliable" -> "rely"
    iable_to_y <- endsWith(subset, "iable") & subset %!like% ending_with_word(any_of(English_iable_keepers))
    subset[iable_to_y] <- replace_last_n_chars_with(subset[iable_to_y], 5L, "y")
    # e.g. "despicable" -> "despise"
    icable_to_ise <- endsWith(subset, "spicable")
    subset[icable_to_ise] <- replace_last_n_chars_with(subset[icable_to_ise], 5L, "se")
    # e.g. "irritable" -> "irritate"
    able_to_ate <- endsWithAny(subset, c("approximable", "culable", "gulable", "irritable", "operable", "icable")) & able_keeper_mask
    subset[able_to_ate] <- replace_last_n_chars_with(subset[able_to_ate], 3L, "te")
    # e.g. "(inde)fatigable" -> "fatigue"
    able_to_ue <- endsWith(subset, "fatigable")
    subset[able_to_ue] <- replace_last_n_chars_with(subset[able_to_ue], 4L, "ue")
    # e.g. "memorable" -> "memory"
    able_to_y <- endsWithAny(subset, c("charitable", "memorable"))
    subset[able_to_y] <- replace_last_n_chars_with(subset[able_to_y], 4L, "y")
    # e.g. "flammable" -> "flame
    able_letter_to_e <- endsWith(subset, "flammable")
    subset[able_letter_to_e] <- replace_last_n_chars_with(subset[able_letter_to_e], 5L, "e")
    # e.g. "transferrable" -> "transfer"
    delete_able_letter <- endsWithAny(subset, English_doubled_consonants_able) & able_keeper_mask
    subset[delete_able_letter] <- remove_last_n_chars(subset[delete_able_letter], 5L)
    # e.g. "sharable" -> "share"
    able_to_e <- subset %like% paste0(e_rules, "able$") & able_keeper_mask
    subset[able_to_e] <- replace_last_n_chars_with(subset[able_to_e], 4L, "e")
    # general rule--remove suffix
    delete_able <- endsWith(subset, "able") & able_keeper_mask
    subset[delete_able] <- remove_last_n_chars(subset[delete_able], 4L)
    words[subset_scope] <- subset
  }
  
  # handle "ible" suffixes
  subset_scope <- endsWith(words, "ible")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    ible_keeper_mask <- subset %!like% ending_with_word(any_of(English_ible_keepers))
    # e.g. "(in)visible" -> "vision"
    ible_to_ion <- endsWith(subset, "visible")
    subset[ible_to_ion] <- replace_last_n_chars_with(subset[ible_to_ion], 3L, "on")
    # e.g. "(in)credible" -> "credit"
    ible_to_ent <- endsWith(subset, "credible")
    subset[ible_to_ent] <- replace_last_n_chars_with(subset[ible_to_ent], 4L, "ent")
    # e.g. "sensible" -> "sense"
    ible_to_e <- subset %like% paste0(e_rules, "ible$") & ible_keeper_mask
    subset[ible_to_e] <- replace_last_n_chars_with(subset[ible_to_e], 4L, "e")
    # general rule--remove suffix
    delete_ible <- endsWith(subset, "ible") & ible_keeper_mask
    subset[delete_ible] <- remove_last_n_chars(subset[delete_ible], 4L)
    words[subset_scope] <- subset
  }
  
  # handle "hood" suffix
  subset_scope <- endsWith(words, "hood")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "livelihood" -> "live"
    delete_lihood <- endsWith(subset, "livelihood")
    subset[delete_lihood] <- remove_last_n_chars(subset[delete_lihood], 6L)
    # e.g. "likelihood" -> "likely"
    ihood_to_y <- endsWith(subset, "ihood")
    subset[ihood_to_y] <- replace_last_n_chars_with(subset[ihood_to_y], 5L, "y")
    # general rule--remove suffix
    delete_hood <- endsWith(subset, "hood")
    subset[delete_hood] <- remove_last_n_chars(subset[delete_hood], 4L)
    words[subset_scope] <- subset
  }
  
  # handle "ship" suffix
  subset_scope <- endsWith(words, "ship")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    delete_ship <- !(endsWithAny(subset, c("airship", "battleship", "fireship", "gunship", "longship", "mediumship", "midship", "motorship", "relationship", "spaceship", "steamship", "tankship", "tranship", "transship", "warship", "worship")) | subset %like% "\\bship$")
    subset[delete_ship] <- remove_last_n_chars(subset[delete_ship], 4L)
    words[subset_scope] <- subset
  }
  
  ### handle other oddities
  
  # e.g. "unison" -> "unity"
  ison_to_ity <- endsWith(words, "unison")
  words[ison_to_ity] <- replace_last_n_chars_with(words[ison_to_ity], 3L, "ty")
  # e.g. "comparison" -> "compare"
  ison_to_e <- endsWith(words, "comparison")
  words[ison_to_e] <- replace_last_n_chars_with(words[ison_to_e], 4L, "e")
  
  # e.g. "legalese" -> "legal"
  delete_ese <- endsWith(words, "ese") & !endsWithAny(words, c("diocese", "eese", "manganese", "obese", "these"))
  words[delete_ese] <- remove_last_n_chars(words[delete_ese], 3L)
  
  # e.g. "programme" -> "program"
  amme_to_am <- endsWith(words, "amme")
  words[amme_to_am] <- remove_last_n_chars(words[amme_to_am], 2L)
  # e.g. "theatre" -> "theater"
  re_to_er <- endsWithAny(words, c("bre", "tre"))
  words[re_to_er] <- replace_last_n_chars_with(words[re_to_er], 2L, "er")
  
  # e.g. "wowser" -> "wow"
  delete_ser <- endsWith(words, "wowser")
  words[delete_ser] <- remove_last_n_chars(words[delete_ser], 3L)
  # e.g. "lawyer" -> "law"
  delete_yer <- endsWithAny(words, c("bowyer", "lawyer", "sawyer"))
  words[delete_yer] <- remove_last_n_chars(words[delete_yer], 3L)
  
  # e.g. "western" -> "west"
  delete_ern <- endsWithAny(words, c("eastern", "northern", "southern", "western"))
  words[delete_ern] <- remove_last_n_chars(words[delete_ern], 3L)
  
  # e.g. "cowardice" -> "coward"
  delete_ice <- endsWith(words, "cowardice")
  words[delete_ice] <- remove_last_n_chars(words[delete_ice], 3L)
  
  # e.g. "hatred" -> "hate"
  red_to_e <- endsWith(words, "hatred")
  words[red_to_e] <- replace_last_n_chars_with(words[red_to_e], 3L, "e")
  
  # e.g. "elder" -> "old"
  eld_to_old <- words %like% "\\beld(?:er|est)?$"
  words[eld_to_old] <- gsub(words[eld_to_old], pattern = "\\beld(?:er|est)?$", replacement = "old")
  
  # handle "estry" and "istry" suffixes
  subset_scope <- endsWith(words, "stry")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "ancestry" -> "ancester"
    estry_to_est <- endsWithAny(subset, c("ancestry", "forestry"))
    subset[estry_to_est] <- remove_last_n_chars(subset[estry_to_est], 2L)
    # e.g. "registry" -> "register"
    istry_to_ter <- endsWithAny(subset, c("ministry", "registry"))
    subset[istry_to_ter] <- replace_last_n_chars_with(subset[istry_to_ter], 2L, "er")
    # e.g. "artistry" -> "artist"
    istry_to_ist <- endsWithAny(subset, c("artistry", "baptistry", "chemistry", "dentistry", "sophistry"))
    subset[istry_to_ist] <- remove_last_n_chars(subset[istry_to_ist], 2L)
    words[subset_scope] <- subset
  }
  
  ### fix final set of generic noun and adjective suffixes
  
  # e.g. "opportunistic" -> "opportunism"
  istic_to_ism <- endsWith(words, "istic") & words %!like% "\\bstatistic"
  words[istic_to_ism] <- replace_last_n_chars_with(words[istic_to_ism], 3L, "m")
  # e.g. "opportunist" -> "opportunism"
  # some words are changed knowing they'll be handled below (e.g. "therapist" -> "therapism" -> "therapy")
  ist_to_ism <- endsWith(words, "ist") & words %!like% ending_with_word(any_of(English_ist_keepers))
  words[ist_to_ism] <- replace_last_n_chars_with(words[ist_to_ism], 1L, "m")
  
  # handle "ism" suffix
  subset_scope <- endsWith(words, "ism")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "conservatism" -> "conservative"
    ism_to_ive <- endsWith(subset, "rvatism")
    subset[ism_to_ive] <- replace_last_n_chars_with(subset[ism_to_ive], 3L, "ive")
    # e.g. "scientism" -> "science"
    tism_to_ce <- endsWith(subset, "scientism")
    subset[tism_to_ce] <- replace_last_n_chars_with(subset[tism_to_ce], 4L, "ce")
    # e.g. "cosmopolitism" -> "cosmopolitan"
    ism_to_an <- endsWith(subset, "cosmopolitism")
    subset[ism_to_an] <- replace_last_n_chars_with(subset[ism_to_an], 3L, "an")
    # e.g. "(bi)linguism" (or "linguist") -> "lingual"
    ism_to_al <- endsWith(subset, "linguism")
    subset[ism_to_al] <- replace_last_n_chars_with(subset[ism_to_al], 3L, "al")
    # e.g. "metabolism" -> "metabolic"
    ism_to_ic <- endsWithAny(subset, c("abolism", "barism", "mechanism", "ntrism"))
    subset[ism_to_ic] <- replace_last_n_chars_with(subset[ism_to_ic], 2L, "c")
    # e.g. "therapism" (or "therapist") -> "therapy"
    ism_to_y <- endsWithAny(subset, c("economism", "jurism", "pharmacism", "quism", "rgism", "therapism"))
    subset[ism_to_y] <- replace_last_n_chars_with(subset[ism_to_y], 3L, "y")
    # e.g. "activism" -> "active"
    ism_to_e <- endsWithAny(subset, c("activism", "biblism", "chromism", "chronism", "communism", "cubism", "elitism", "flutism", "imagism", "itism", "nudism", "nudism", "oboism", "purism", "racism", "rapism", "titlism", "tropism", "typism", "vism"))
    subset[ism_to_e] <- replace_last_n_chars_with(subset[ism_to_e], 3L, "e")
    # e.g. "snobbism" -> "snob"
    delete_ism_letter <- endsWithAny(subset, English_doubled_consonants_ism)
    subset[delete_ism_letter] <- remove_last_n_chars(subset[delete_ism_letter], 4L)
    # general rule--remove suffix
    delete_ism <- endsWith(subset, "ism") & subset %!like% ending_with_word(any_of(English_ism_keepers))
    subset[delete_ism] <- remove_last_n_chars(subset[delete_ism], 3L)
    words[subset_scope] <- subset
  }
  
  # handle "al" suffix
  subset_scope <- endsWith(words, "al")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # handle "ial" suffix
    
    # e.g. "serial" -> "series"
    is_serial <- endsWith(subset, "serial")
    subset[is_serial] <- replace_last_n_chars_with(subset[is_serial], 2L, "es")
    # e.g. "imperial" -> "empire"
    is_imperial <- endsWith(subset, "imperial")
    subset[is_imperial] <- replace_last_n_chars_with(subset[is_imperial], 8L, "empire")
    # e.g. "beneficial" -> "benefit"
    cial_to_t <- endsWith(subset, "beneficial")
    subset[cial_to_t] <- replace_last_n_chars_with(subset[cial_to_t], 4L, "t")
    # e.g. "ceremonial" -> "ceremony"
    ial_to_y <- endsWithAny(subset, c("ceremonial", "colonial", "custodial", "memorial", "mercurial", "monial", "territorial", "trial", "versial"))
    subset[ial_to_y] <- replace_last_n_chars_with(subset[ial_to_y], 3L, "y")
    # e.g. "bacterial" -> "bacterium"
    ial_to_ium <- endsWithAny(subset, c("bacterial", "cranial", "ennial", "fluvial", "sporial", "stadial"))
    subset[ial_to_ium] <- replace_last_n_chars_with(subset[ial_to_ium], 2L, "um")
    # e.g. "essential" -> "essence"
    tial_to_ce <- endsWithAny(subset, c("essential", "influential", "spatial"))
    subset[tial_to_ce] <- replace_last_n_chars_with(subset[tial_to_ce], 4L, "ce")
    # e.g. "financial" -> "finance"
    ial_to_e <- subset %like% "(?:[aeiou][bcs]|[nr]c)ial$" & subset %!like% ending_with(any_of(English_ial_keepers))
    subset[ial_to_e] <- replace_last_n_chars_with(subset[ial_to_e], 3L, "e")
    # general "ial" rule--remove suffix
    delete_ial <- endsWith(subset, "ial") & subset %!like% ending_with(any_of(English_ial_keepers))
    subset[delete_ial] <- remove_last_n_chars(subset[delete_ial], 3L)
    
    # handle "ical" suffix
    
    # e.g. "cyclical" -> "cycle"
    lical_to_le <- endsWithAny(subset, c("blical", "clical"))
    subset[lical_to_le] <- replace_last_n_chars_with(subset[lical_to_le], 4L, "e")
    # e.g. "surgical" -> "surgery"
    ical_to_ery <- endsWith(subset, "surgical")
    subset[ical_to_ery] <- replace_last_n_chars_with(subset[ical_to_ery], 4L, "ery")
    # e.g. "identical" -> "identity"
    ical_to_ity <- endsWith(subset, "identical")
    subset[ical_to_ity] <- replace_last_n_chars_with(subset[ical_to_ity], 3L, "ty")
    # e.g. "chemical" -> "chemist"
    ical_to_ist <- endsWith(subset, "chemical")
    subset[ical_to_ist] <- replace_last_n_chars_with(subset[ical_to_ist], 3L, "st")
    # general "ical" rule is to follow general "al" rule (remove "al")
    
    # handle "ual" suffix
    
    # e.g. "annual" -> "annum"
    ual_to_um <- endsWith(subset, "annual")
    subset[ual_to_um] <- replace_last_n_chars_with(subset[ual_to_um], 2L, "m")
    # e.g. "sensual" -> "sense"
    ual_to_e <- endsWithAny(subset, c("gradual", "sensual"))
    subset[ual_to_e] <- replace_last_n_chars_with(subset[ual_to_e], 3L, "e")
    # e.g. "continual" -> "continue"
    ual_to_ue <- endsWithAny(subset, c("accrual", "continual", "residual", "tissual", "virtual"))
    subset[ual_to_ue] <- replace_last_n_chars_with(subset[ual_to_ue], 2L, "e")
    # e.g. "central" -> "center"
    tral_to_ter <- endsWithAny(subset, c("ancestral", "central", "cloistral", "lustral", "neutral", "sceptral"))
    subset[tral_to_ter] <- replace_last_n_chars_with(subset[tral_to_ter], 3L, "er")
    # general "ual" rule--remove suffix
    delete_ual <- endsWith(subset, "ual") & subset %!like% ending_with(any_of(English_ual_keepers))
    subset[delete_ual] <- remove_last_n_chars(subset[delete_ual], 3L)
    
    # handle "inal" suffix
    
    # e.g. "longitudinal" -> "longitude"
    tudinal_to_tude <- endsWith(subset, "tudinal")
    subset[tudinal_to_tude] <- replace_last_n_chars_with(subset[tudinal_to_tude], 4L, "e")
    # e.g. "criminal" -> "crime"
    inal_to_e <- endsWith(subset, "criminal")
    subset[inal_to_e] <- replace_last_n_chars_with(subset[inal_to_e], 4L, "e")
    # e.g. "maternal" -> "mater"
    #* could change this to "mother"/"father" later
    delete_nal <- endsWithAny(subset, c("maternal", "paternal"))
    subset[delete_nal] <- remove_last_n_chars(subset[delete_nal], 3L)
    # general "inal" rule is to follow general "al" rule (remove "al")
    
    # handle "tal" suffix
    
    # e.g. "horizontal" -> "horizon"
    delete_tal <- endsWith(subset, "horizontal")
    subset[delete_tal] <- remove_last_n_chars(subset[delete_tal], 3L)
    # general "tal" rule is to follow general "al" rule (remove "al")
    
    # handle plain "al" suffix
    
    # e.g. "referral" -> "refer"
    delete_al_letter <- endsWith(subset, "referral")
    subset[delete_al_letter] <- remove_last_n_chars(subset[delete_al_letter], 3L)
    # e.g. "larval" -> "larva"
    delete_l <- endsWithAny(subset, c("caval", "gingival", "larval", "orchestral", "vaginal"))
    subset[delete_l] <- remove_last_n_chars(subset[delete_l], 1L)
    # e.g. "peripheral" -> "periphery"
    al_to_y <- endsWithAny(subset, c("peripheral", "societal"))
    subset[al_to_y] <- replace_last_n_chars_with(subset[al_to_y], 2L, "y")
    # e.g. "neural" -> "neuron"
    al_to_on <- endsWith(subset, "neural")
    subset[al_to_on] <- replace_last_n_chars_with(subset[al_to_on], 2L, "on")
    # e.g. "spectral" -> "spectrum"
    al_to_um <- endsWithAny(subset, c("poreal", "spectral", "minimal", "maximal", "optimal", "cerebral"))
    subset[al_to_um] <- replace_last_n_chars_with(subset[al_to_um], 2L, "um")
    # e.g. "viral" -> "virus"
    al_to_us <- endsWithAny(subset, c("colossal", "focal", "terminal", "viral"))
    subset[al_to_us] <- replace_last_n_chars_with(subset[al_to_us], 2L, "us")
    # e.g. "global" -> "globe"
    al_to_e <- endsWithAny(subset, c("communal", "global", "tribal", "practical", "bridal", "tribunal", "brutal", "ral", "sal", "val")) & !(endsWithAny(subset, c("behavioral", "doctoral", "electoral", "medieval", "naval", "floral", "primeval")) | subset %like% "\\b(?:o|ri)val$") & subset %!like% ending_with(any_of(English_al_keepers))
    subset[al_to_e] <- replace_last_n_chars_with(subset[al_to_e], 2L, "e")
    # e.g. "reciprocal" -> "reciprocate"
    al_to_ate <- endsWith(subset, "reciprocal")
    subset[al_to_ate] <- replace_last_n_chars_with(subset[al_to_ate], 1L, "te")
    # general rule--remove suffix
    delete_al <- endsWith(subset, "al") & subset %!like% ending_with(any_of(English_al_keepers))
    subset[delete_al] <- remove_last_n_chars(subset[delete_al], 2L)
    words[subset_scope] <- subset
  }
  
  # handle "ian" suffix
  subset_scope <- endsWith(words, "ian")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "libertarian" -> "liberty"
    arian_to_y <- endsWithAny(subset, c("ilarian", "itarian", "rtarian", "stinarian"))
    subset[arian_to_y] <- replace_last_n_chars_with(subset[arian_to_y], 5L, "y")
    # e.g. "sectarian" -> "sect"
    delete_arian <- endsWithAny(subset, c("fruitarian", "sectarian"))
    subset[delete_arian] <- remove_last_n_chars(subset[delete_arian], 5L)
    # e.g. "civilian" -> "civil"
    ian_to_e <- endsWithAny(subset, c("avian", "esian", "ilian")) & !endsWith(subset, "civilian")
    subset[ian_to_e] <- replace_last_n_chars_with(subset[ian_to_e], 3L, "e")
    # e.g. "comedian" -> "comedy"
    ian_to_y <- endsWithAny(subset, c("arian", "comedian", "custodian", "torian", "tregedian", "ovarian"))
    subset[ian_to_y] <- replace_last_n_chars_with(subset[ian_to_y], 3L, "y")
    # general rule--remove suffix
    delete_ian <- endsWith(subset, "ian") & !endsWithAny(subset, English_ian_keepers)
    subset[delete_ian] <- remove_last_n_chars(subset[delete_ian], 3L)
    words[subset_scope] <- subset
  }
  
  # handle "ary" suffix
  subset_scope <- endsWith(words, "ary")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "disciplinary" -> "discipline"
    ary_to_e <- endsWithAny(subset, c("antiquary", "disciplinary", "primary"))
    subset[ary_to_e] <- replace_last_n_chars_with(subset[ary_to_e], 3L, "e")
    # e.g. "legendary" -> "legend"
    delete_ary <- endsWithAny(subset, c("dietary", "legendary", "ionary", "mentary", "parliamentary", "secondary"))
    subset[delete_ary] <- remove_last_n_chars(subset[delete_ary], 3L)
    words[subset_scope] <- subset
  }
  
  # handle "ment" suffix
  subset_scope <- endsWith(words, "ment")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "judgment" -> "judge"
    ment_to_e <- endsWith(subset, "dgment")
    subset[ment_to_e] <- replace_last_n_chars_with(subset[ment_to_e], 4L, "e")
    # e.g. "merriment" -> "merry"
    iment_to_y <- endsWith(subset, "iment") & !endsWithAny(subset, English_ment_keepers)
    subset[iment_to_y] <- replace_last_n_chars_with(subset[iment_to_y], 5L, "y")
    # general rule--remove suffix
    delete_ment <- endsWith(subset, "ment") & !endsWithAny(subset, English_ment_keepers)
    subset[delete_ment] <- remove_last_n_chars(subset[delete_ment], 4L)
    words[subset_scope] <- subset
  }
  
  # handle "ic" suffix
  subset_scope <- endsWith(words, "ic")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "technic" -> "technique"
    ic_to_ique <- endsWith(subset, "technic")
    subset[ic_to_ique] <- replace_last_n_chars_with(subset[ic_to_ique], 1L, "que")
    # e.g. "cortic(al)" -> "cortex"
    ic_to_ex <- endsWithAny(subset, c("cortic", "vortic"))
    subset[ic_to_ex] <- replace_last_n_chars_with(subset[ic_to_ex], 2L, "ex")
    
    # handle "ific" suffix
    
    # e.g. "scientific" -> "science"
    tific_to_ce <- endsWith(subset, "scientific")
    subset[tific_to_ce] <- replace_last_n_chars_with(subset[tific_to_ce], 5L, "ce")
    # e.g. "specific" -> "specify"
    ific_to_ify <- endsWithAny(subset, c("cific", "rific"))
    subset[ific_to_ify] <- replace_last_n_chars_with(subset[ific_to_ify], 3L, "fy")
    
    # handle "tic" suffixes
    
    # e.g. # "hypnotic" -> "hypnosis"
    tic_to_sis <- endsWithAny(subset, c("hypnotic", "hypothetic"))
    subset[tic_to_sis] <- replace_last_n_chars_with(subset[tic_to_sis], 3L, "sis")
    atic_to_e <- endsWith(subset, "chromatic")
    subset[atic_to_e] <- replace_last_n_chars_with(subset[atic_to_e], 4L, "e")
    delete_atic <- endsWithAny(subset, c("informatic", "symptomatic"))
    subset[delete_atic] <- remove_last_n_chars(subset[delete_atic], 4L)
    
    # handle "ric" suffix
    
    # e.g. "cylindric" -> "cylinder"
    ric_to_er <- endsWithAny(subset, c("ndric", "ntric", "theatric"))
    subset[ric_to_er] <- replace_last_n_chars_with(subset[ric_to_er], 3L, "er")
    
    # handle general "ic" suffix
    
    # e.g. "spheric" -> "sphere"
    ic_to_e <- endsWithAny(subset, c("spheric", "typic"))
    subset[ic_to_e] <- replace_last_n_chars_with(subset[ic_to_e], 2L, "e")
    # e.g. "toxic" -> "toxin"
    ic_to_in <- endsWith(subset, "toxic")
    subset[ic_to_in] <- replace_last_n_chars_with(subset[ic_to_in], 1L, "n")
    # e.g. "euphoric" -> "euphoria"
    ic_to_ia <- endsWithAny(subset, c("dysphoric", "euphoric"))
    subset[ic_to_ia] <- replace_last_n_chars_with(subset[ic_to_ia], 1L, "a")
    # e.g. "graphic" -> "graph"
    delete_ic <- endsWithAny(subset, c("alphabetic", "graphic", "gymnastic", "istic", "phoric", "xic")) & subset %!like% "\\bstatistic"
    subset[delete_ic] <- remove_last_n_chars(subset[delete_ic], 2L)
    # e.g. "botanic" -> "botany"
    ic_to_y <- endsWithAny(subset, c("archic", "botanic", "categoric", "metric", "nomic", "ologic", "pacific", "phic", "storic"))
    subset[ic_to_y] <- replace_last_n_chars_with(subset[ic_to_y], 2L, "y")
    # general "ic" rule is to leave it
    words[subset_scope] <- subset
  }
  
  # handle "ous" suffix
  subset_scope <- endsWith(words, "ous")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "multitudinous" -> "multitude"
    inous_to_e <- endsWithAny(subset, c("multitudinous", "vicissitudinous"))
    subset[inous_to_e] <- replace_last_n_chars_with(subset[inous_to_e], 5L, "e")
    # e.g. "extraneous" -> "extra"
    delete_ous_letters <- endsWith(subset, "extraneous")
    subset[delete_ous_letters] <- remove_last_n_chars(subset[delete_ous_letters], 5L)
    # e.g. "incestuous" -> "incest"
    delete_ous_letter <- endsWithAny(subset, c("censorious", "incestuous", "tortious"))
    subset[delete_ous_letter] <- remove_last_n_chars(subset[delete_ous_letter], 4L)
    # e.g. "famous" -> "fame"
    ous_to_e <- endsWithAny(subset, c("famous", "nervous", "porous", "prestigious", "rapturous"))
    subset[ous_to_e] <- gsub(subset[ous_to_e], pattern = "(i|in)?ous$", replacement = "e")
    # e.g. "monstrous" -> "monster"
    trous_to_ter <- endsWithAny(subset, c("brous", "strous", "xtrous"))
    subset[trous_to_ter] <- replace_last_n_chars_with(subset[trous_to_ter], 4L, "er")
    # e.g. "anxious" -> "anxiety"
    ous_to_ety <- endsWith(subset, "anxious")
    subset[ous_to_ety] <- replace_last_n_chars_with(subset[ous_to_ety], 3L, "ety")
    
    # # e.g. "credulous" -> ""
    # ulous_to_ <- endsWith(subset, "credulous")
    # subset[ulous_to_] <- replace_last_n_chars_with(subset[ulous_to_], L, "")
    
    # e.g. "tenacious" -> "tenacity"
    ous_to_ty <- endsWithAny(subset, c("atrocious", "capacious", "ferocious", "loquacious", "rapacious", "salacious", "tenacious"))
    subset[ous_to_ty] <- replace_last_n_chars_with(subset[ous_to_ty], 3L, "ty")
    # e.g. "rebellious" -> "rebellion"
    ous_to_on <- endsWithAny(subset, c("rebellious", "gious", "tious")) & !endsWithAny(subset, c("facetious", "litigious", "prodigious"))
    subset[ous_to_on] <- replace_last_n_chars_with(subset[ous_to_on], 2L, "n")
    # e.g. "decorous" -> "decorum"
    ous_to_um <- endsWithAny(subset, c("decorous", "delirious", "tedious", "vacuous"))
    subset[ous_to_um] <- replace_last_n_chars_with(subset[ous_to_um], 3L, "um")
    # e.g. "envious" -> "envy"
    ious_to_y <- endsWithAny(subset, c("efficacious", "envious", "fallacious", "furious", "glorious", "luxurious", "melodious", "onious", "prodigious", "various"))
    subset[ious_to_y] <- replace_last_n_chars_with(subset[ious_to_y], 4L, "y")
    # e.g. "gracious" -> "grace"
    cious_to_ce <- endsWith(subset, "cious")
    subset[cious_to_ce] <- replace_last_n_chars_with(subset[cious_to_ce], 4L, "e")
    # e.g. "felonous" -> "felony"
    ous_to_y <- endsWithAny(subset, c("adulterous", "felonous", "gamous", "lecherous", "usurous"))
    subset[ous_to_y] <- replace_last_n_chars_with(subset[ous_to_y], 3L, "y")
    # e.g. "hazardous" -> "hazard"
    delete_ous <- endsWithAny(subset, c("advantageous", "amorous", "circuitous", "courageous", "feverous", "hazardous", "joyous", "nymous", "ponderous", "solicitous", "sulfurous", "tuberous", "ulcerous", "valorous", "vaporous", "verminous", "viperous", "vomitous", "zealous")) #*** assume delete all but use ous keepers instead
    subset[delete_ous] <- remove_last_n_chars(subset[delete_ous], 3L)
    # general "ous" rule is to leave it
    words[subset_scope] <- subset
  }
  
  # handle "ful" suffix
  subset_scope <- endsWith(words, "ful")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "beautiful" -> "beauty"
    iful_to_y <- endsWith(subset, "iful")
    subset[iful_to_y] <- replace_last_n_chars_with(subset[iful_to_y], 4L, "y")
    # general rule--remove suffix
    delete_ful <- endsWith(subset, "ful") & subset %!like% "\\b(?:aw|grate)ful$"
    subset[delete_ful] <- remove_last_n_chars(subset[delete_ful], 3L)
    words[subset_scope] <- subset
  }
  
  # handle "less" suffix
  subset_scope <- endsWith(words, "less")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "penniless" -> "penny"
    iless_to_y <- endsWith(subset, "iless")
    subset[iless_to_y] <- replace_last_n_chars_with(subset[iless_to_y], 5L, "y")
    # general rule--remove suffix
    delete_less <- endsWith(subset, "less") & subset %!like% "\\b(?:b|hap|(?:never|none)the|un)?less$"
    subset[delete_less] <- remove_last_n_chars(subset[delete_less], 4L)
    words[subset_scope] <- subset
  }
  
  # handle "ar" suffix
  subset_scope <- endsWith(words, "ar")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # handle "ular" suffix
    
    # e.g. "angular" -> "angle"
    ular_to_le <- endsWithAny(subset, c("angular", "circular", "clavicular", "corpuscular", "cuticular", "follicular", "miracular", "ocular", "oracular", "singular", "spectacular", "tabular", "tabernacular", "tentacular", "vehicular", "ventricular"))
    subset[ular_to_le] <- replace_last_n_chars_with(subset[ular_to_le], 4L, "le")
    # e.g. "cellular" -> "cell"
    delete_ular <- endsWithAny(subset, c("glandular", "cellular"))
    subset[delete_ular] <- remove_last_n_chars(subset[delete_ular], 4L)
    # general "ular" rule--remove suffix
    ular_to_ule <- endsWith(subset, "ular") & !endsWithAny(subset, c("particular", "popular", "regular"))
    subset[ular_to_ule] <- replace_last_n_chars_with(subset[ular_to_ule], 2L, "e")
    
    # handle "iar" suffix
    
    # e.g. "liar" -> "lie"
    iar_to_ie <- subset %like% "\\bliar$"
    subset[iar_to_ie] <- replace_last_n_chars_with(subset[iar_to_ie], 2L, "e")
    # e.g. "familiar" -> "family"
    iar_to_y <- endsWith(subset, "familiar")
    subset[iar_to_y] <- replace_last_n_chars_with(subset[iar_to_y], 3L, "y")
    
    # handle general "ar" suffix
    
    # e.g. "scholar" -> "school"
    delete_ar_school <- endsWith(subset, "scholar")
    subset[delete_ar_school] <- replace_last_n_chars_with(subset[delete_ar_school], 3L, "ol")
    # general "ar" rule is to leave it
    words[subset_scope] <- subset
  }
  
  # e.g. "congruence" -> "congruent"
  ence_to_ent <- endsWith(words, "ence") & !(endsWithAny(words, c("audience", "essence", "experience", "influence", "license", "sentence")) | words %like% "\\b(?:[fhp]|sci|sp|th|wh)ence$") # endsWithAny(words, c("abhorrence", "absence", "accidence", "congruence", "diligence", "evidence", "immanence", "indolence", "inherence", "insistence", "nascence", "opulence", "patience", "permanence", "potence", "presence", "prudence", "quence", "residence", "reticence", "reverence", "salience", "tangence", "transcience", "valence", "violence"))
  words[ence_to_ent] <- replace_last_n_chars_with(words[ence_to_ent], 2L, "t")
  # e.g. "abundance" -> "abundant"
  ance_to_ant <- endsWithAny(words, c("abundance", "clairvoyance", "distance", "ificance", "malignance", "norance", "performance", "pursuance", "resistance"))
  words[ance_to_ant] <- replace_last_n_chars_with(words[ance_to_ant], 2L, "t")
  
  # handle "ant" suffix
  subset_scope <- endsWith(words, "ant")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "significant" -> "signify"
    ificant_to_y <- endsWith(subset, "ificant")
    subset[ificant_to_y] <- replace_last_n_chars_with(subset[ificant_to_y], 5L, "y")
    # e.g. "reductant" -> "reduce"
    ctant_to_ce <- endsWith(subset, "reductant")
    subset[ctant_to_ce] <- replace_last_n_chars_with(subset[ctant_to_ce], 4L, "e")
    # e.g. "oxidant" -> "oxide"
    ant_to_e <- endsWithAny(subset, c("ignorant", "oxidant", "piquant", "pleasant", "pursuant"))
    subset[ant_to_e] <- replace_last_n_chars_with(subset[ant_to_e], 3L, "e")
    # e.g. "reactant" -> "react"
    delete_ant <- endsWithAny(subset, c("colorant", "formant", "infestant", "inhabitant", "malignant", "reactant", "relaxant", "resistant", "toxicant"))
    subset[delete_ant] <- remove_last_n_chars(subset[delete_ant], 3L)
    # e.g. "participant" -> "participate"
    #*** or make this general rule?
    ant_to_ate <- endsWithAny(subset, c("administrant", "participant", "supplicant"))
    subset[ant_to_ate] <- replace_last_n_chars_with(subset[ant_to_ate], 2L, "te")
    # general "ant" rule is to leave it
    words[subset_scope] <- subset
  }
  
  # handle "ent" suffix
  subset_scope <- endsWith(words, "ent")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "student" -> "study"
    ent_to_y <- endsWith(subset, "student")
    subset[ent_to_y] <- replace_last_n_chars_with(subset[ent_to_y], 3L, "y")
    # e.g. "emergent" -> "emerge"
    ent_to_e <- endsWith(subset, "ergent")
    subset[ent_to_e] <- remove_last_n_chars(subset[ent_to_e], 2L)
    # e.g. "credent" (from "credence") -> "credit"
    ent_to_it <- endsWith(subset, "credent")
    subset[ent_to_it] <- replace_last_n_chars_with(subset[ent_to_it], 3L, "it")
    # e.g. "recurrent" -> "recur"
    delete_ent_letter <- endsWithAny(subset, c("deterrent", "incurrent", "occurrent", "recurrent"))
    subset[delete_ent_letter] <- remove_last_n_chars(subset[delete_ent_letter], 4L)
    # e.g. "different" -> "differ"
    delete_ent <- endsWithAny(subset, c("different", "conferent", "existent", "insistent", "preferent", "referent"))
    subset[delete_ent] <- remove_last_n_chars(subset[delete_ent], 3L)
    # general "ent" rule is to leave it
    words[subset_scope] <- subset
  }
  
  # handle "ive" suffix
  subset_scope <- endsWith(words, "ive")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # handle "ative" suffix
    
    # e.g. "affirmative" -> "affirm"
    delete_ative <- endsWithAny(subset, c("ulcerative", "ntative", "rmative", "rtative"))
    subset[delete_ative] <- remove_last_n_chars(subset[delete_ative], 5L)
    # e.g. "comparative" -> "compare"
    ative_to_e <- endsWithAny(subset, c("comparative", "curative")) 
    subset[ative_to_e] <- replace_last_n_chars_with(subset[ative_to_e], 5L, "e")
    # e.g. "relative" -> "relate"
    ative_to_ate <- endsWith(subset, "ative") & !(endsWith(subset, "putative") | subset %like% "\\b[dn]ative$")
    subset[ative_to_ate] <- replace_last_n_chars_with(subset[ative_to_ate], 3L, "e")
    
    # handle "itive" suffix
    
    # e.g. "sensitive" -> "sensate" (-> "sense" later on)
    itive_to_ate <- endsWith(subset, "sensitive")
    subset[itive_to_ate] <- replace_last_n_chars_with(subset[itive_to_ate], 5L, "ate")
    
    # handle "ctive" suffix
    
    # e.g. "deductive" -> "deduce"
    ctive_to_ce <- endsWith(subset, "ductive")
    subset[ctive_to_ce] <- replace_last_n_chars_with(subset[ctive_to_ce], 4L, "e")
    # general "ctive" rule--remove suffix (e.g. "detective" -> "detect")
    delete_ive <- endsWith(subset, "ctive") & !endsWithAny(subset, c("adjective", "objective", "subjective"))
    subset[delete_ive] <- remove_last_n_chars(subset[delete_ive], 3L)
    
    # handle "ptive" suffix
    
    # e.g. "captive" -> "capture"
    ptive_to_pture <- endsWith(subset, "captive")
    subset[ptive_to_pture] <- replace_last_n_chars_with(subset[ptive_to_pture], 3L, "ure")
    # e.g. "presumptive" -> "presume"
    mptive_to_me <- endsWith(subset, "mptive")
    subset[mptive_to_me] <- replace_last_n_chars_with(subset[mptive_to_me], 5L, "e")
    # e.g. "absorptive" -> "absorb"
    rptive_to_b <- endsWith(subset, "rptive")
    subset[rptive_to_b] <- replace_last_n_chars_with(subset[rptive_to_b], 5L, "b")
    # e.g. "prescriptive" -> "prescribe"
    ptive_to_be <- endsWith(subset, "scriptive")
    subset[ptive_to_be] <- replace_last_n_chars_with(subset[ptive_to_be], 5L, "be")
    # e.g. "adaptive" -> "adapt"
    ptive_to_pt <- endsWithAny(subset, c("acceptive", "adaptive", "adoptive", "ruptive"))
    subset[ptive_to_pt] <- remove_last_n_chars(subset[ptive_to_pt], 3L)
    # e.g. "interruptive" -> "interrupt"
    delete_ptive <- endsWith(subset, "interruptive")
    subset[delete_ptive] <- remove_last_n_chars(subset[delete_ptive], 5L)
    # general "ptive" rule--remove suffix (e.g. "receptive" -> "receive")
    ptive_to_ive <- endsWith(subset, "ptive")
    subset[ptive_to_ive] <- replace_last_n_chars_with(subset[ptive_to_ive], 5L, "ive")
    
    # handle general "ive" suffix
    
    # e.g. "iterative" -> "iterate"
    ive_to_e <- endsWithAny(subset, c("decorative", "defensive", "iterative", "locative", "offensive"))
    subset[ive_to_e] <- replace_last_n_chars_with(subset[ive_to_e], 3L, "e")
    # e.g. "assertive" -> "assert"
    delete_ive <- endsWithAny(subset, c("adoptive", "adventive", "appointive", "assertive", "attractive", "detective", "ejective", "erective", "eruptive", "excessive", "exeptive", "exertive", "preventive", "reactive", "reflective", "selective", "transitive", "vomitive"))
    subset[delete_ive] <- remove_last_n_chars(subset[delete_ive], 3L)
    # general "ive" rule is to leave it
    words[subset_scope] <- subset
  }
  
  # e.g. "celebratory" -> "celebrate"
  atory_to_ate <- endsWith(words, "atory") & !endsWith(words, "oratory")
  words[atory_to_ate] <- replace_last_n_chars_with(words[atory_to_ate], 3L, "e")
  
  # e.g. "messenger" -> "message"
  enger_to_age <- endsWithAny(words, c("messenger", "passenger"))
  words[enger_to_age] <- replace_last_n_chars_with(words[enger_to_age], 5L, "age")
  
  # handle "age" suffix
  subset_scope <- endsWith(words, "age")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "verbiage" -> "verb"
    delete_iage <- endsWith(subset, "verbiage")
    subset[delete_iage] <- remove_last_n_chars(subset[delete_iage], 4L)
    # e.g. "marriage" -> "marry"
    iage_to_y <- endsWith(subset, "rriage")
    subset[iage_to_y] <- replace_last_n_chars_with(subset[iage_to_y], 4L, "y")
    # e.g. "assemblage" -> "assembly"
    age_to_y <- endsWith(subset, "blage")
    subset[age_to_y] <- replace_last_n_chars_with(subset[age_to_y], 3L, "y")
    # e.g. "dosage" -> "dose"
    age_to_e <- endsWithAny(subset, c("chaperonage", "cleavage", "dosage", "pipage", "storage", "usage"))
    subset[age_to_e] <- replace_last_n_chars_with(subset[age_to_e], 3L, "e")
    # remove suffix if example in list (e.g. "wattage" -> "watt")
    delete_age <- endsWithAny(subset, English_age_removers)
    subset[delete_age] <- remove_last_n_chars(subset[delete_age], 3L)
    # general "age" rule is to leave it
    words[subset_scope] <- subset
  }
  
  # handle "tion" suffix
  subset_scope <- endsWith(words, "tion")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # handle "ication" suffix
    
    # e.g. "amplification" -> "amplify"
    ication_to_y <- endsWith(subset, "ification")
    subset[ication_to_y] <- replace_last_n_chars_with(subset[ication_to_y], 7L, "y")
    # e.g. "publication" -> "publish"
    ication_to_ish <- endsWith(subset, "publication")
    subset[ication_to_ish] <- replace_last_n_chars_with(subset[ication_to_ish], 6L, "sh")
    
    # handle "faction" suffix
    
    # e.g. "satisfaction" -> "satisfy"
    faction_to_fy <- endsWith(subset, "faction")
    subset[faction_to_fy] <- replace_last_n_chars_with(subset[faction_to_fy], 6L, "y")
    
    # handle "ation" suffix
    
    # e.g. "pronunciation" -> "pronounce"
    nunciation_to_nounce <- endsWith(subset, "nunciation")
    subset[nunciation_to_nounce] <- replace_last_n_chars_with(subset[nunciation_to_nounce], 9L, "ounce")
    # e.g. "filtration" -> "filter"
    tration_to_ter <- endsWithAny(subset, c("filtration", "istration"))
    subset[tration_to_ter] <- replace_last_n_chars_with(subset[tration_to_ter], 6L, "er")
    # e.g. "cancellation" -> "cancel"
    delete_lation <- endsWith(subset, "cancellation")
    subset[delete_lation] <- remove_last_n_chars(subset[delete_lation], 6L)
    # e.g. "invitation" -> "invite"
    ation_to_e <- endsWithAny(subset, c("compilation", "invitation"))
    subset[ation_to_e] <- replace_last_n_chars_with(subset[ation_to_e], 5L, "e")
    # e.g. "consideration" -> "consider"
    delete_ation <- endsWithAny(subset, c("accreditation", "adaptation", "consideration", "distillation", "installation", "instillation", "ntation", "recommendation", "transformation"))
    subset[delete_ation] <- remove_last_n_chars(subset[delete_ation], 5L)
    # e.g. "colonization" -> "colonize"
    iszation_to_ize <- endsWithAny(subset, c("isation", "ization"))
    subset[iszation_to_ize] <- replace_last_n_chars_with(subset[iszation_to_ize], 6L, "ze")
    # e.g. "expectation" -> "expect"
    delete_ation <- endsWithAny(subset, c("expectation", "formation", "foundation", "information", "transportation"))
    subset[delete_ation] <- remove_last_n_chars(subset[delete_ation], 5L)
    # e.g. "sanitation" -> "sanitary"
    ation_to_ary <- endsWith(subset, "sanitation")
    subset[ation_to_ary] <- replace_last_n_chars_with(subset[ation_to_ary], 4L, "ry")
    # e.g. "celebration" -> "celebrate" (general "ation" rule)
    ation_to_ate <- endsWith(subset, "ation") & !endsWithAny(subset, c("nation", "occupation", "ration", "station", "vocation"))
    subset[ation_to_ate] <- replace_last_n_chars_with(subset[ation_to_ate], 3L, "e")
    
    # handle "ition" and "ution" suffixes
    
    # e.g. "practicioner" -> "practice"
    ition_to_ice <- endsWithAny(subset, c("practition", "practitioner"))
    subset[ition_to_ice] <- gsub(subset[ition_to_ice], pattern = "tion(er)?$", replacement = "ce")
    # e.g. "solution" -> "solve"
    ution_to_ve <- endsWith(subset, "olution")
    subset[ution_to_ve] <- replace_last_n_chars_with(subset[ution_to_ve], 5L, "ve")
    
    # handle "ption" suffix
    
    # e.g. "redemption" -> "redeem"
    mption_to_em <- endsWith(subset, "redemption")
    subset[mption_to_em] <- replace_last_n_chars_with(subset[mption_to_em], 6L, "em")
    # e.g. "consumption" -> "consume"
    mption_to_me <- endsWith(subset, "mption") & !endsWith(subset, "exemption")
    subset[mption_to_me] <- replace_last_n_chars_with(subset[mption_to_me], 6L, "me")
    # e.g. "conception" -> "conceive"
    eption_to_eive <- endsWith(subset, "eption") & !endsWith(subset, "exception")
    subset[eption_to_eive] <- replace_last_n_chars_with(subset[eption_to_eive], 5L, "ive")
    # e.g. "transcription" -> "transcribe"
    iption_to_ibe <- endsWith(subset, "iption")
    subset[iption_to_ibe] <- replace_last_n_chars_with(subset[iption_to_ibe], 5L, "be")
    # e.g. "absorption" -> "absorb"
    orption_to_orb <- endsWith(subset, "orption")
    subset[orption_to_orb] <- replace_last_n_chars_with(subset[orption_to_orb], 5L, "b")
    
    # handle "ction" suffix
    
    # e.g. "destruction" -> "destroy"
    uction_to_oy <- endsWith(subset, "destruction")
    subset[uction_to_oy] <- replace_last_n_chars_with(subset[uction_to_oy], 6L, "oy")
    # e.g. "introduction" -> "introduce"
    ction_to_ce <- endsWithAny(subset, c("introduction", "reduction", "reproduction", "seduction"))
    subset[ction_to_ce] <- replace_last_n_chars_with(subset[ction_to_ce], 4L, "e")
    
    # handle general "ion" suffix
    # e.g. "depiction" -> "depict"
    delete_ion <- endsWithAny(subset, c("ction", "ption")) & !endsWithAny(subset, c("caption", "duration", "auction", "diction", "fiction", "fraction", "function", "junction", "sanction", "surrection"))
    subset[delete_ion] <- remove_last_n_chars(subset[delete_ion], 3L)
    # general "ion" rule is to leave it
    words[subset_scope] <- subset
  }
  
  # e.g. "compression" -> "compress"
  delete_ion <- endsWith(words, "ession") & !endsWithAny(words, c("cession", "session"))
  words[delete_ion] <- remove_last_n_chars(words[delete_ion], 3L)
  
  # handle "ery" suffix
  subset_scope <- endsWith(words, "ery")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "thuggery" -> "thug"
    delete_ery_letter <- endsWithAny(subset, c("blubbery", "buggery", "cutlery", "doggery", "gunnery", "jewellery", "nunnery", "piggery", "pottery", "robbery", "shrubbery", "snobbery", "thuggery"))
    subset[delete_ery_letter] <- remove_last_n_chars(subset[delete_ery_letter], 4L)
    # e.g. "brewery" -> "brew"
    delete_ery <- endsWithAny(subset, c("bitchery", "brewery", "butchery", "cookery", "creamery", "crockery", "crookery", "deanery", "demagoguery", "distillery", "eatery", "fishery", "foolery", "fuckery", "greenery", "joinery", "mockery", "monkery", "printery", "quackery", "rookery", "smithery", "trickery"))
    subset[delete_ery] <- remove_last_n_chars(subset[delete_ery], 3L)
    # e.g. "bribery" -> "bribe"
    delete_ry <- endsWithAny(subset, c("bribery", "bakery", "bravery", "cyclery", "drapery", "fakery", "finery", "forgery", "grotesquery", "imagery", "machinery", "missilery", "mopery", "nursery", "pedlery", "perfumery", "refinery", "rocketry", "roguery", "savagery", "scenery", "slavery", "winery"))
    subset[delete_ry] <- remove_last_n_chars(subset[delete_ry], 2L)
    # e.g. "watery" -> "water"
    delete_y <- endsWithAny(subset, c("beery", "butlery", "buttery", "cheery", "delivery", "discovery", "flowery", "grocery", "jittery", "leathery", "leery", "mastery", "mothery", "papery", "quivery", "recovery", "rubbery", "silvery", "sneery", "spidery", "watery", "wintery"))
    subset[delete_y] <- remove_last_n_chars(subset[delete_y], 1L)
    words[subset_scope] <- subset
  }
  
  # handle "y" suffix
  subset_scope <- endsWith(words, "y")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "(in)finity" -> "finite"
    y_to_e <- endsWithAny(subset, c("finity", "injury")) & !endsWith(subset, "affinity")
    subset[y_to_e] <- replace_last_n_chars_with(subset[y_to_e], 1L, "e")
    # e.g. "advisory" -> "advisor"
    delete_y <- endsWithAny(subset, c("archy", "complicity", "visory"))
    subset[delete_y] <- remove_last_n_chars(subset[delete_y], 1L)
    words[subset_scope] <- subset
  }
  
  # handle "it" suffix
  subset_scope <- endsWith(words, "it")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # "(in)evit(able)" -> "evade"
    evit_to_evade <- subset %like% "\\bevit$"
    subset[evit_to_evade] <- replace_last_n_chars_with(subset[evit_to_evade], 2L, "ade")
    # "implicit" -> "imply"
    mplicit_to_mply <- endsWith(subset, "mplicit")
    subset[mplicit_to_mply] <- replace_last_n_chars_with(subset[mplicit_to_mply], 4L, "y")
    words[subset_scope] <- subset
  }
  
  # handle "itude" suffix
  subset_scope <- endsWith(words, "itude")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "amplitude" -> "amplify"
    itude_to_ify <- endsWithAny(subset, c("amplitude", "certitude", "fortitude", "gratitude", "magnitude"))
    subset[itude_to_ify] <- replace_last_n_chars_with(subset[itude_to_ify], 4L, "fy")
    # e.g. "similitude" -> "similar"
    itude_to_ar <- endsWith(subset, "similitude")
    subset[itude_to_ar] <- replace_last_n_chars_with(subset[itude_to_ar], 5L, "ar")
    # e.g. "servitude" -> "serve"
    itude_to_e <- endsWith(subset, "servitude")
    subset[itude_to_e] <- replace_last_n_chars_with(subset[itude_to_e], 5L, "e")
    # e.g. "plentitude" -> "plenty"
    itude_to_y <- endsWith(subset, "plentitude")
    subset[itude_to_y] <- replace_last_n_chars_with(subset[itude_to_y], 5L, "y")
    # e.g. "decrepitude" -> "decrepit"
    itude_to_it <- endsWithAny(subset, c("decrepitude", "solicitude"))
    subset[itude_to_it] <- remove_last_n_chars(subset[itude_to_it], 3L)
    # e.g. "(in)finitude" -> "finite"
    itude_to_ite <- endsWith(subset, "finitude")
    subset[itude_to_ite] <- replace_last_n_chars_with(subset[itude_to_ite], 3L, "e")
    # e.g. "exactitude" -> "exact"
    delete_itude <- endsWithAny(subset, c("aptitude", "correctitude", "crassitude", "eptitude", "exactitude", "vastitude"))
    subset[delete_itude] <- remove_last_n_chars(subset[delete_itude], 5L)
    words[subset_scope] <- subset
  }
  
  # handle "ysis" suffix
  subset_scope <- endsWith(words, "ysis")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "lysis" -> "lyse"
    ysis_to_yse <- subset %like% "\\blysis$"
    subset[ysis_to_yse] <- replace_last_n_chars_with(subset[ysis_to_yse], 3L, "se")
    # e.g. "hydrolysis" -> "hydrolyze"
    ysis_to_yze <- endsWith(subset, "ysis")
    subset[ysis_to_yze] <- replace_last_n_chars_with(subset[ysis_to_yze], 3L, "ze")
    words[subset_scope] <- subset
  }
  
  ### handle comparative/doer ("er"), superlative ("est"), past tense ("ed"), and progressive tense ("ing") endings
  
  #* nested back-references don't work in R regex
  
  # handle "er" suffix
  subset_scope <- endsWith(words, "er")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    er_keeper_mask <- subset %!like% ending_with(any_of(English_er_keepers))
    # e.g. "controller" -> "control"
    delete_er_letter <- ((endsWithAny(subset, c(English_doubled_consonants_er, "awfuller", "compeller", "controller", "traveller", "quizzer", "frolicker", "mimicker", "mosaicker", "panicker", "picnicker", "politicker", "trafficker", "laughter", "remainder")) & subset %!like% "((([aiu]|\\b([fhjstwy]|bests|dw|kn|kv|qu|sh|sm|sp|sw)e|((\\b|en)r)o)ll)|(\\bodd))er$") | (endsWith(subset, "eer") & subset %!like% ending_with(any_of(English_eer_keepers)) & !endsWithAny(subset, c("decreer", "fleer", "freer", "seer")))) & er_keeper_mask
    subset[delete_er_letter] <- remove_last_n_chars(subset[delete_er_letter], 3L)
    # e.g. "carrier" -> "carry"
    ier_to_y <- endsWith(subset, "ier") & subset %!like% ending_with(any_of(English_ier_keepers)) & !(endsWithAny(subset, c("taxier", "waterskier")) | subset %like% "\\bskier$")
    subset[ier_to_y] <- replace_last_n_chars_with(subset[ier_to_y], 3L, "y")
    er_keeper_mask <- !delete_er_letter & subset %!like% ending_with(any_of(English_er_keepers))
    # e.g. "(over)seer" -> "see"
    delete_r <- (subset %like% paste0(e_rules, "er$") | endsWithAny(subset, c("decreer", "fleer", "freer", "seer"))) & er_keeper_mask
    subset[delete_r] <- remove_last_n_chars(subset[delete_r], 1L)
    # general rule--remove suffix (e.g. "talker" -> "talk")
    delete_er <- endsWith(subset, "er") & er_keeper_mask
    subset[delete_er] <- remove_last_n_chars(subset[delete_er], 2L)
    words[subset_scope] <- subset
  }
  
  # handle "est" suffix
  subset_scope <- endsWith(words, "est")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    est_keeper_mask <- subset %!like% ending_with(any_of(English_est_keepers))
    # e.g. "biggest" -> "big"
    delete_est_letter <- endsWithAny(subset, c(English_doubled_consonants_est, "awfullest")) & subset %!like% "(?:[aiu]ll|\\bodd)est$" & est_keeper_mask
    subset[delete_est_letter] <- remove_last_n_chars(subset[delete_est_letter], 4L)
    # e.g. "earliest" -> "early"
    iest_to_y <- endsWith(subset, "iest") & subset %!like% ending_with(any_of(English_iest_keepers))
    subset[iest_to_y] <- replace_last_n_chars_with(subset[iest_to_y], 4L, "y")
    est_keeper_mask <- !delete_est_letter & subset %!like% ending_with(any_of(English_est_keepers))
    # e.g. "latest" -> "late"
    delete_st <- subset %like% paste0(e_rules, "est$") & est_keeper_mask
    subset[delete_st] <- remove_last_n_chars(subset[delete_st], 2L)
    # general rule--remove suffix (e.g. "smallest" -> "small")
    delete_est <- endsWith(subset, "est") & est_keeper_mask
    subset[delete_est] <- remove_last_n_chars(subset[delete_est], 3L)
    words[subset_scope] <- subset
  }
  
  # handle "ed" suffix
  subset_scope <- endsWith(words, "ed")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    ed_keeper_mask <- subset %!like% ending_with(any_of(English_ed_keepers))
    # e.g. "centred" -> "center"
    tred_to_ter <- endsWith(subset, "tred") & !endsWith(subset, "hatred")
    subset[tred_to_ter] <- replace_last_n_chars_with(subset[tred_to_ter], 3L, "er")
    # e.g. "travelled" -> "travel"
    delete_ed_letter <- endsWithAny(subset, c(English_doubled_consonants_ed, "compelled", "controlled", "travelled", "quizzed", "frolicked", "mimicked", "mosaicked", "panicked", "picnicked", "politicked", "trafficked")) & subset %!like% "(?:[aiu]|\\b(?:[fhjtwy]|dw|kn|kv|qu|sh|sm|sp|sw)e|(?:\\b|en)ro)lled$" & ed_keeper_mask
    subset[delete_ed_letter] <- remove_last_n_chars(subset[delete_ed_letter], 3L)
    # e.g. "tied" -> "tie"
    delete_d <- subset %like% ending_with_word(any_of(c("died", "lied", "tied", "hogtied")))
    subset[delete_d] <- remove_last_n_chars(subset[delete_d], 1L)
    # e.g. "carried" -> "carry"
    ied_to_y <- endsWith(subset, "ied") & !(endsWithAny(subset, c("taxied", "waterskied")) | subset %like% "\\bskied$")
    subset[ied_to_y] <- replace_last_n_chars_with(subset[ied_to_y], 3L, "y")
    ed_keeper_mask <- !delete_ed_letter & subset %!like% ending_with(any_of(English_ed_keepers))
    # e.g. "wasted" -> "waste"
    delete_d <- subset %like% paste0(e_rules, "ed$") & !endsWithAny(subset, c("synced", "focused")) & ed_keeper_mask
    subset[delete_d] <- remove_last_n_chars(subset[delete_d], 1L)
    # general rule--remove suffix (e.g. "walked" -> "walk")
    delete_ed <- endsWith(subset, "ed") & !endsWith(subset, "eed") & ed_keeper_mask
    subset[delete_ed] <- remove_last_n_chars(subset[delete_ed], 2L)
    words[subset_scope] <- subset
  }
  
  # handle "ing" suffix
  subset_scope <- endsWith(words, "ing")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    ing_keeper_mask <- subset %!like% ending_with(any_of(English_ing_keepers))
    # e.g. "centring" -> "center"
    tring_to_ter <- endsWith(subset, "tring") & (!endsWith(subset, "string") | endsWith(subset, "lustring"))
    subset[tring_to_ter] <- replace_last_n_chars_with(subset[tring_to_ter], 4L, "er")
    # e.g. "trafficking" -> "traffic"
    delete_ing_letter <- endsWithAny(subset, c(English_doubled_consonants_ing, "compelling", "controlling", "travelling", "quizzing", "frolicking", "mimick", "mosaicking", "panicking", "picnicking", "politicking", "trafficking")) & subset %!like% "(?:[aiu]|\\b(?:[fhjstwy]|bests|dw|kn|kv|qu|sh|sm|sp|sw)e|(?:\\b|en)ro)lling$" & ing_keeper_mask
    subset[delete_ing_letter] <- remove_last_n_chars(subset[delete_ing_letter], 4L)
    ing_keeper_mask <- !delete_ing_letter & subset %!like% ending_with(any_of(English_ing_keepers))
    # e.g. "waving" -> "wave"
    ing_to_e <- subset %like% paste0(e_rules, "ing$") & !endsWithAny(subset, c("syncing", "focusing")) & ing_keeper_mask
    subset[ing_to_e] <- replace_last_n_chars_with(subset[ing_to_e], 3L, "e")
    # general rule--remove suffix (e.g. "singing" -> "sing")
    delete_ing <- endsWith(subset, "ing") & ing_keeper_mask
    subset[delete_ing] <- remove_last_n_chars(subset[delete_ing], 3L)
    words[subset_scope] <- subset
  }
  
  ### handle generic verb suffixes
  
  # handle "ify" suffix
  subset_scope <- endsWith(words, "ify")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "exemplify" -> "example"
    exemplify_to_example <- endsWith(subset, "exemplify")
    subset[exemplify_to_example] <- replace_last_n_chars_with(subset[exemplify_to_example], 7L, "ample")
    # e.g. "certify" -> "certain"
    ify_to_ain <- endsWith(subset, "certify")
    subset[ify_to_ain] <- replace_last_n_chars_with(subset[ify_to_ain], 3L, "ain")
    # e.g. "simplify" -> "simple"
    plify_to_ple <- endsWith(subset, "plify")
    subset[plify_to_ple] <- replace_last_n_chars_with(subset[plify_to_ple], 3L, "e")
    # e.g. "terrify" -> "terror"
    ify_to_or <- endsWithAny(subset, c("horrify", "terrify"))
    subset[ify_to_or] <- replace_last_n_chars_with(subset[ify_to_or], 3L, "or")
    # e.g. "commodify" -> "commodity"
    ify_to_ity <- endsWithAny(subset, c("deify", "commodify", "entify", "qualify", "unify", "verify"))
    subset[ify_to_ity] <- replace_last_n_chars_with(subset[ify_to_ity], 2L, "ty")
    # e.g. "stratify" -> "stratum"
    ify_to_um <- endsWith(subset, "stratify")
    subset[ify_to_um] <- replace_last_n_chars_with(subset[ify_to_um], 3L, "um")
    # e.g. "calcify" -> "calcium"
    ify_to_ium <- endsWith(subset, "calcify")
    subset[ify_to_ium] <- replace_last_n_chars_with(subset[ify_to_ium], 2L, "um")
    # e.g. "syllabify" -> "syllabus"
    ify_to_us <- endsWith(subset, "syllabify")
    subset[ify_to_us] <- replace_last_n_chars_with(subset[ify_to_us], 2L, "us")
    # e.g. "glorify" -> "glory"
    ify_to_y <- endsWithAny(subset, c("citify", "gentrify", "glorify", "glify", "llify", "ncify", "ndify", "prettify", "rsify"))
    subset[ify_to_y] <- replace_last_n_chars_with(subset[ify_to_y], 3L, "y")
    # e.g. "notify" -> "note"
    ify_to_e <- endsWithAny(subset, c("arify", "asify", "atify", "codify", "ilify", "ivify", "lsify", "notify", "nsify", "orify", "plify", "urify", "utify", "ypify")) & !endsWithAny(subset, c("amplify", "gasify"))
    subset[ify_to_e] <- replace_last_n_chars_with(subset[ify_to_e], 3L, "e")
    # general rule--remove suffix (e.g. "solidify" -> "solid")
    delete_ify <- endsWith(subset, "ify") & !(endsWithAny(subset, c("certify", "gnify", "modify", "mystify", "ratify", "specify", "testify")) | subset %like% "\\bedify$")
    subset[delete_ify] <- remove_last_n_chars(subset[delete_ify], 3L)
    words[subset_scope] <- subset
  }
  
  ity_to_ite <- words %like% "\\bunity$"
  words[ity_to_ite] <- replace_last_n_chars_with(words[ity_to_ite], 2L, "te")
  
  # handle "en" suffix
  subset_scope <- endsWith(words, "en")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "flatten" -> "flat"
    delete_en_letter <- endsWithAny(subset, c("flatten", "gladden"))
    subset[delete_en_letter] <- remove_last_n_chars(subset[delete_en_letter], 3L)
    # e.g. "broaden" -> "broad"
    delete_en <- endsWithAny(subset, c("blacken", "brighten", "broaden", "cheapen", "deepen", "freshen", "frighten", "harden", "harken", "hearten", "heighten", "lengthen", "lessen", "moisten", "roughen", "sharpen", "shorten", "slacken", "slicken", "smarten", "smoothen", "soften", "steepen", "stiffen", "sweeten", "thicken", "threaten", "tighten", "toughen", "weaken"))
    subset[delete_en] <- remove_last_n_chars(subset[delete_en], 2L)
    # e.g. "whiten" -> "white"
    delete_n <- endsWithAny(subset, c("hasten", "whiten"))
    subset[delete_n] <- remove_last_n_chars(subset[delete_n], 1L)
    words[subset_scope] <- subset
  }
  
  # handle "ize" suffix
  subset_scope <- endsWith(words, "ize")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "stabilize" -> "stable"
    bilize_to_ble <- endsWith(subset, "bilize") & !endsWith(subset, "mobilize")
    subset[bilize_to_ble] <- replace_last_n_chars_with(subset[bilize_to_ble], 5L, "le")
    # e.g. "systematize" -> "system"
    delete_tize_letter <- endsWith(subset, "systematize")
    subset[delete_tize_letter] <- remove_last_n_chars(subset[delete_tize_letter], 5L)
    # e.g. "traumatize" -> "trauma"
    delete_tize <- endsWith(subset, "matize")
    subset[delete_tize] <- remove_last_n_chars(subset[delete_tize], 4L)
    # e.g. "emphasize" -> "emphasis"
    size_to_sis <- endsWithAny(subset, c("emphasize", "hypothesize", "metastasize", "parenthesize", "synthesize"))
    subset[size_to_sis] <- replace_last_n_chars_with(subset[size_to_sis], 2L, "s")
    # e.g. "categorize" -> "category"
    ize_to_y <- endsWithAny(subset, c("anatomize", "apologize", "categorize", "chronize", "colonize", "ectomize", "eulogize", "fantasize", "otomize", "prioritize", "summarize"))
    subset[ize_to_y] <- replace_last_n_chars_with(subset[ize_to_y], 3L, "y")
    # e.g. "iodize" -> "iodine"
    ize_to_ine <- endsWith(subset, "iodize")
    subset[ize_to_ine] <- replace_last_n_chars_with(subset[ize_to_ine], 2L, "ne")
    # e.g. "sanitize" -> "sanitary"
    ize_to_ary <- endsWith(subset, "sanitize")
    subset[ize_to_ary] <- replace_last_n_chars_with(subset[ize_to_ary], 3L, "ary")
    # e.g. "metabolize" -> "metabolic"
    ize_to_ic <- endsWith(subset, "metabolize")
    subset[ize_to_ic] <- replace_last_n_chars_with(subset[ize_to_ic], 2L, "c")
    # e.g. "optimize" -> "optimum"
    ize_to_um <- endsWithAny(subset, c("maximize", "minimize", "optimize"))
    subset[ize_to_um] <- replace_last_n_chars_with(subset[ize_to_um], 3L, "um")
    # e.g. "crystallize" -> "crystal"
    delete_ize_letter <- endsWithAny(subset, c("crystallize", "tranquillize"))
    subset[delete_ize_letter] <- remove_last_n_chars(subset[delete_ize_letter], 4L)
    # e.g. "cyclize" -> "cycle"
    ize_to_e <- (subset %like% paste0(e_rules, "ize$") | endsWith(subset, "mobilize")) & (!endsWith(subset, "tomize") | endsWith(subset, "epitomize")) & !endsWithAny(subset, English_ize_keepers)
    subset[ize_to_e] <- replace_last_n_chars_with(subset[ize_to_e], 3L, "e")
    # e.g. general rule--remove suffix (e.g. "randomize" -> "random")
    delete_ize <- endsWith(subset, "ize") & !endsWithAny(subset, English_ize_keepers)
    subset[delete_ize] <- remove_last_n_chars(subset[delete_ize], 3L)
    words[subset_scope] <- subset
  }
  
  # e.g. "aviator" -> "aviate"
  ator_to_ate <- endsWith(words, "ator") & !endsWithAny(words, c("alligator", "equator"))
  words[ator_to_ate] <- replace_last_n_chars_with(words[ator_to_ate], 2L, "e")
  
  # handle "ate" suffix
  subset_scope <- endsWith(words, "ate")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "activate" -> "act"
    delete_ivate <- endsWith(subset, "activate")
    subset[delete_ivate] <- remove_last_n_chars(subset[delete_ivate], 5L)
    # e.g. "captivate" -> "capture"
    ivate_to_ure <- endsWithAny(subset, c("activate", "cultivate"))
    subset[ivate_to_ure] <- replace_last_n_chars_with(subset[ivate_to_ure], 5L, "ure")
    # e.g. "administrate" -> "administer"
    strate_to_ster <- endsWithAny(subset, c("administrate", "sequestrate"))
    subset[strate_to_ster] <- replace_last_n_chars_with(subset[strate_to_ster], 4L, "er")
    # e.g. "paginate" -> "page"
    inate_to_e <- endsWith(subset, "paginate")
    subset[inate_to_e] <- replace_last_n_chars_with(subset[inate_to_e], 5L, "e")
    # e.g. "(in)furiate" -> "fury"
    iate_to_y <- endsWithAny(subset, c("furiate", "variate"))
    subset[iate_to_y] <- replace_last_n_chars_with(subset[iate_to_y], 4L, "y")
    # e.g. "quantitate" -> "quantity"
    tate_to_ty <- endsWithAny(subset, c("qualitate", "quantitate"))
    subset[tate_to_ty] <- replace_last_n_chars_with(subset[tate_to_ty], 3L, "y")
    # e.g. "differentiate" -> "different"
    delete_iate <- endsWithAny(subset, c("differentiate", "instantiate", "potentiate", "substantiate"))
    subset[delete_iate] <- remove_last_n_chars(subset[delete_iate], 4L)
    # e.g. "circumstantiate" -> "circumstance"
    tiate_to_ce <- endsWithAny(subset, c("circumstantiate", "licentiate"))
    subset[tiate_to_ce] <- replace_last_n_chars_with(subset[tiate_to_ce], 5L, "ce")
    # e.g. "reconciliate" -> "reconcile"
    liate_to_le <- endsWith(subset, "nciliate")
    subset[liate_to_le] <- replace_last_n_chars_with(subset[liate_to_le], 4L, "e")
    # e.g. "(tri)angulate" -> "triangle"
    ulate_to_le <- endsWithAny(subset, c("articulate", "angulate", "circulate"))
    subset[ulate_to_le] <- replace_last_n_chars_with(subset[ulate_to_le], 5L, "le")
    # e.g. "(in)flammate" -> "flame"
    mmate_to_me <- endsWith(subset, "flammate")
    subset[mmate_to_me] <- replace_last_n_chars_with(subset[mmate_to_me], 4L, "e")
    # e.g. "applicate" -> "apply"
    icate_to_y <- endsWithAny(subset, c("applicate", "duplicate", "multiplicate", "quadruplicate", "triplicate"))
    subset[icate_to_y] <- replace_last_n_chars_with(subset[icate_to_y], 5L, "y")
    # e.g. "authenticate" -> "authentic"
    delete_ate <- endsWithAny(subset, c("archate", "assassinate", "authenticate", "exploitate", "fractionate", "interpretate", "limitate", "passionate", "relaxate", "solicitate", "taxate", "vigorate", "ltate", "rmate"))
    subset[delete_ate] <- remove_last_n_chars(subset[delete_ate], 3L)
    # e.g. "stimulate" -> "stimulus"
    ate_to_us <- endsWith(subset, "stimulate")
    subset[ate_to_us] <- replace_last_n_chars_with(subset[ate_to_us], 3L, "us")
    # e.g. "salivate" -> "saliva"
    ate_to_a <- endsWith(subset, "salivate")
    subset[ate_to_a] <- remove_last_n_chars(subset[ate_to_a], 2L)
    # e.g. "activate" -> "active"
    ate_to_e <- endsWithAny(subset, c("activate", "brominate", "causate", "chlorinate", "citate", "combinate", "computate", "condensate", "continuate", "conversate", "degradate", "derivate", "destinate", "determinate", "divinate", "durate", "electorate", "examinate", "excitate", "explorate", "figurate", "fluorinate", "imaginate", "iodinate", "limitate", "notate", "oxidate", "preparate", "pristinate", "quotate", "reputate", "respirate", "restorate", "sensate", "vaccinate", "rvate"))
    subset[ate_to_e] <- replace_last_n_chars_with(subset[ate_to_e], 3L, "e")
    words[subset_scope] <- subset
  }
  
  ### connect a few verb forms to their noun forms (and vice versa)
  
  # handle "duct" suffix
  subset_scope <- endsWith(words, "duct")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "product" -> "produce"
    duct_to_duce <- endsWithAny(subset, c("educt", "nduct", "oduct"))
    subset[duct_to_duce] <- replace_last_n_chars_with(subset[duct_to_duce], 1L, "e")
    words[subset_scope] <- subset
  }
  
  # handle "y" suffix
  subset_scope <- endsWith(words, "y")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "multiply" -> "multiple"
    y_to_e <- endsWithAny(subset, c("dubly", "tiply", "tuply"))
    subset[y_to_e] <- replace_last_n_chars_with(subset[y_to_e], 1L, "e")
    words[subset_scope] <- subset
  }
  
  # handle "ve" suffix
  subset_scope <- endsWith(words, "ve")
  subset <- words[subset_scope]
  if (isnt_empty(subset)) {
    # e.g. "live" -> "life"
    ive_to_ife <- subset %like% "\\blive$"
    subset[ive_to_ife] <- replace_last_n_chars_with(subset[ive_to_ife], 2L, "fe")
    # e.g. "grieve" -> "grief"
    eve_to_ef <- endsWithAny(subset, c("grieve", "thieve"))
    subset[eve_to_ef] <- replace_last_n_chars_with(subset[eve_to_ef], 2L, "f")
    words[subset_scope] <- subset
  }
  
  # returns a named vector (like a dictionary) 
  # so the original word can be used to lookup its simplified form
  return (re_name(words, original_text))
}
