# @author Scott Dobbins
# @version 0.5
# @date 2018-01-09 18:00

### ACID
# contains, pluralizer, singularizer, and lemmatizer
# the lemmatizer "digests" words down into their 
# simplest root form automatically, without any need 
# to supply part of speech information
# lemmatizer also available in Python version

import functools
import re
import numpy as np


def non_empty_string(string):
    return type(string) in (str, unicode) and string != ""

def reduce_concat(stuff):
    return functools.reduce(lambda x, y: x + y, stuff)

def collapse_bar(strings):
    return functools.reduce(lambda x, y: str(x) + '|' + str(y), strings)

def paste0(*lists):
    return list(map(lambda x: reduce_concat(x), zip(*lists)))

def any_of(strings):
    return "(" + collapse_bar(strings) + ")"

def flat_concat(ls):
    return reduce_concat([subls if type(subls) is list else [subls] for subls in ls])


### Constants ---------------------------------------------------------------

English_invariant_words = ["bison", "buffalo", "cannon", "carp", "cod", "deer", "fish", "hi", "moose", "pike", "salmon", "sheep", "shrimp", "squid", "swine", "trout"]
English_uncountable_words = ["chaos", "chassis", "molasses", "news", "precis", "rendezvous", "series", "species"]
English_ie_singulars = ["anomie", "baddie", "beastie", "biggie", "birdie", "boogie", "bootie", "brownie", "calorie", "camaraderie", "charcuterie", "collie", "commie", "cookie", "cootie", "cowrie", "dearie", "doggie", "dougie", "foodie", "genie", "goalie", "goodie", "groupie", "hippie", "hoodie", "hottie", "junkie", "kiddie", "kittie", "magpie", "meanie", "movie", "newbie", "potpie", "sweetiepie", "sweetypie", "patisserie", "pixie", "prarie", "premie", "quickie", "reverie", "rookie", "roomie", "rotisserie", "smoothie", "softie", "sweetie", "hogtie", "necktie", "talkie", "toughie", "townie", "veggie", "wheelie", "yuppie", "zombie"]
English_ie_singulars_plurals = [string + "s" for string in English_ie_singulars]
English_oe_singulars_string = "\\b(?:al|ob|r|sh|t|w)oes$"
English_zz_singulars = ["buzz", "fizz", "frizz", "fuzz", "jazz", "razz"]
English_zz_singulars_plurals = [string + "es" for string in English_zz_singulars]
English_s_singulars = ["alias", "apparatus", "asbestos", "atlas", "bias", "bonus", "campus", "canvas", "caucus", "citrus", "loris", "mucus", "octopus", "oops", "pancreas", "pelvis", "porticullis", "ruckus", "status", "trellis", "tuckus", "virus"]
English_s_singulars_plurals = [string + "es" for string in English_s_singulars]
English_f_to_ves_singulars = ["calf", "dwarf", "elf", "half", "hoof", "leaf", "loaf", "scarf", "self", "shelf", "thief", "wolf"]
English_f_to_ves_plurals = [re.sub(string = string, pattern = "f$", repl = "ves") for string in English_f_to_ves_singulars]
English_fe_to_ves_singulars = ["knife", "life", "wife"]
English_fe_to_ves_plurals = [re.sub(string = string, pattern = "fe$", repl = "ves") for string in English_fe_to_ves_singulars]
English_us_plurals = ["bayous", "caribous", "emus", "gnus", "menus", "tiramisus", "tutus"]
English_is_plurals = ["khakis", "skis", "taxis"]
English_normal_oses_plurals = ["brownnoses", "bullnoses", "hardnoses", "hooknoses", "shovelnoses", "arabinoses", "flavinoses", "furanoses", "manoses", "pyranoses", "heptoses", "lactoses", "maltoses", "pentoses"]
English_es_to_e_plurals = ["backaches", "bellyaches", "headaches", "stomachaches", "toothaches", "caches", "moustaches", "panaches", "pistaches", "bastes", "castes", "gestes", "hastes", "mostes", "pastes", "pistes", "tastes", "wastes", "chastes", "trystes", "artistes", "batistes", "ripostes", "langoustes"]

plain_plural_singulars = ["canto", "hereto", "kimono", "photo", "piano", "portico", "pro", "quarto", "zero"]
plain_plural_plurals = [string + "s" for string in plain_plural_singulars]

Latin_us_to_i_singulars = ["alumnus", "cactus", "focus", "fungus", "succubus", "syllabus", "terminus", "uterus"]
Latin_us_to_i_plurals = ["alumni", "cacti", "foci", "fungi", "succubi", "syllabi", "termini", "uteri"]
Latin_us_to_a_plurals = ["addenda", "auditoria", "collisea", "compendia", "media", "memoranda", "millennia", "ova", "referenda", "spectra", "stadia", "strata", "symposia"]
Latin_a_to_ae_singulars = ["alga", "alumna", "antenna", "fauna", "fistula", "flora", "formula", "fovea", "hernia", "larva", "trachea"]
Latin_is_to_es_singulars = ["crisis", "genesis", "kinesis", "nemesis", "nosis", "oasis", "testis", "thesis", "tosis"] #* bases could be base or basis (asbestoses could be asbestosis but more likely asbestos)
Latin_is_to_es_plurals = [re.sub(string = string, pattern = "is$", repl = "es") for string in Latin_is_to_es_singulars]

English_ses_to_s_plurals = flat_concat([English_s_singulars_plurals, [string + "es" for string in Latin_us_to_i_singulars]])
Japanese_words_in_English = ["bento", "katana", "kimono", "ninja", "otaku", "samurai", "sushi", "tsunami"]
Maori_words_in_English = ["kakapo", "kiwi", "waka"]
other_foreign_is_plurals = [string for string in [string + "s" for string in flat_concat([Japanese_words_in_English, Maori_words_in_English])] if string.endswith("is")]
all_is_plurals = flat_concat([English_is_plurals, other_foreign_is_plurals])


### Singulars and Plurals ---------------------------------------------------

def singularize(words):
    # invariants
    is_invariant = np.any([words.endswith(string) for string in English_invariant_words], axis = 0) | \
        np.any([words.endswith(string) for string in English_uncountable_words], axis = 0) | \
        np.any([words.endswith(string) for string in Japanese_words_in_English], axis = 0) | \
        np.any([words.endswith(string) for string in Maori_words_in_English], axis = 0) | \
        words.endswith("nese")
  
    # Anglo-Saxon oddities
    is_person = words.endswith("people")
    remove_last3 = words.endswith("children")
    is_brother = words.endswith("brethren")
    is_man = words.endswith("men") & ~(np.any([words.endswith(string) for string in ("abdomen", "acumen", "albumen", "bitumen", "foramen", "hymen", "lumen", "ramen", "regimen", "rumen", "semen", "specimen", "stamen")], axis = 0) | np.array([bool(re.search("\\b[ao]men$", word)) for word in words]))
    is_oo = np.any([words.endswith(string) for string in ("teeth", "feet", "geese")], axis = 0)
    is_ouse = is_ouse = np.any([words.endswith(string) for string in ("booklice", "headlice", "dormice", "fieldmice", "shrewmice", "titmice")], axis = 0) | np.array([bool(re.search("\\b[lm]ice$", word)) for word in words])
    remove_last2 = np.array([bool(re.search("\\boxen$", word)) for word in words])
    is_die = np.array([bool(re.search("\\bdice$", word)) for word in words])
  
    rule_not_found = np.logical_not(np.any((is_invariant, is_person, remove_last3, is_brother, is_man, is_oo, is_ouse, remove_last2, is_die), axis = 0))
  
    # foreign language rules
    remove_last = np.any([words.endswith(string) for string in ("kobzari", "oblasti", "eaux", "ae")], axis = 0) & rule_not_found
    need_o = np.any([words.endswith(string) for string in ("kniazhestva", "celli")], axis = 0) & rule_not_found
    rule_not_found = rule_not_found & np.logical_not(np.any((remove_last, need_o), axis = 0))
  
    need_itis = words.endswith("itides") & rule_not_found
    rule_not_found = rule_not_found & ~need_itis
  
    need_on = np.any([words.endswith(string) for string in ("automata", "criteria", "hedra", "mena")], axis = 0) & rule_not_found
    rule_not_found = rule_not_found & ~need_on
  
    remove_last2 = remove_last2 | (np.any([words.endswith(string) for string in ("im", "mata")], axis = 0) & rule_not_found)
    need_ah = words.endswith("ot") & rule_not_found
    rule_not_found = rule_not_found & np.logical_not(np.any((remove_last2, need_ah), axis = 0))
  
    need_ma = words.endswith("mata") & rule_not_found
    need_us = words.endswith("i") & rule_not_found
    need_us_special = np.any([words.endswith(string) for string in ("corpora", "genera", "viscera")], axis = 0) & rule_not_found
    rule_not_found = rule_not_found & np.logical_not(np.any((need_ma, need_us, need_us_special), axis = 0))
  
    need_um = words.endswith("a") & rule_not_found
    rule_not_found = rule_not_found & ~need_um
  
    need_is_latin = (np.any([words.endswith(string) for string in Latin_is_to_es_plurals], axis = 0) | np.array([bool(re.search("\\baxes$", word)) for word in words])) & (np.any([words.endswith(string) for string in English_normal_oses_plurals], axis = 0) | np.array([bool(re.search("\\bnoses$", word)) for word in words])) & rule_not_found
    rule_not_found = rule_not_found & ~need_is_latin
  
    need_ex = np.any([words.endswith(string) for string in ("codices", "cortices", "indices", "vortices")], axis = 0) & rule_not_found
    need_ix = np.any([words.endswith(string) for string in ("radices", "trices")], axis = 0) & rule_not_found
    need_is_greek = words.endswith("eis") & ~(words.endswith("senseis") | np.array([bool(re.search("\\bleis$", word)) for word in words])) & rule_not_found
    rule_not_found = rule_not_found & np.logical_not(np.any((need_ex, need_ix, need_is_greek), axis = 0))
  
    need_f = np.any([words.endswith(string) for string in English_f_to_ves_plurals], axis = 0) & rule_not_found
    need_fe = np.any([words.endswith(string) for string in English_fe_to_ves_plurals], axis = 0) & rule_not_found
    need_y = words.endswith("ies") & ~(np.any([words.endswith(string) for string in English_ie_singulars_plurals], axis = 0) | np.array([bool(re.search("\\b[lpt]ies$", word)) for word in words])) & rule_not_found
    rule_not_found = rule_not_found & np.logical_not(np.any((need_f, need_fe, need_y), axis = 0))
  
    remove_last3 = remove_last3 | ((np.any([words.endswith(string) for string in ("busses", "gasses")], axis = 0) | \
                                     (words.endswith("zzes") & \
                                                                                ~np.any([words.endswith(string) for string in English_zz_singulars_plurals], axis = 0))) & \
                                                                        rule_not_found)
    rule_not_found = rule_not_found & ~remove_last3
  
    remove_last = remove_last | (((np.any([words.endswith(string) for string in English_ie_singulars_plurals], axis = 0) | np.array([bool(re.search("\\b[lpt]ies$", word)) for word in words])) | \
                                   np.array([bool(re.search(English_oe_singulars_string, word)) for word in words]) | \
                                   np.array([bool(re.search("[aeiouy][^aeioux]es$", word)) for word in words]) | \
                                   words.endswith("mmes") | \
                                   np.any([words.endswith(string) for string in English_es_to_e_plurals], axis = 0) | \
                                   np.array([bool(re.search("(?:[bcdfglprstz][glr]|l[csv]|n[cgrs]|p[s]|r[cgsv]|s[c]|tt|u|\\bach)es$", word)) for word in words])) & \
                                                                    ~np.any([words.endswith(string) for string in English_ses_to_s_plurals], axis = 0) & \
                                                                    rule_not_found)
    rule_not_found = rule_not_found & ~remove_last
  
    remove_last2 = remove_last2 | (np.array([bool(re.search("[^e]es$", word)) for word in words]) & rule_not_found)
    rule_not_found = rule_not_found & ~remove_last2
  
    remove_last = remove_last | (words.endswith("s") & rule_not_found)
  
    # fix English rules
    words[is_person] = [string[:-4] + "rson" for string in words[is_person]]
    words[is_brother] = [string[:-6] + "other" for string in words[is_brother]]
    words[is_man] = [string[:-2] + "an" for string in words[is_man]]
    words[is_oo] = [re.sub(string = string, pattern = "ee([a-z]:1,2)$", repl = "oo\\1") for string in words[is_oo]]
    words[is_ouse] = [string[:-3] + "ouse" for string in words[is_ouse]]
    words[is_die] = [string[:-3] + "ie" for string in words[is_die]]
    words[need_f] = [string[:-3] + "f" for string in words[need_f]]
    words[need_fe] = [string[:-3] + "fe" for string in words[need_fe]]
    words[need_y] = [string[:-3] + "y" for string in words[need_y]]
  
    # fix foreign rules
    words[need_o] = [string[:-1] + "o" for string in words[need_o]]
    words[need_itis] = [string[:-6] + "itis" for string in words[need_itis]]
    words[need_ah] = [string[:-2] + "ah" for string in words[need_ah]]
    words[need_ma] = [string[:-2] + "ma" for string in words[need_ma]]
    words[need_on] = [string[:-1] + "on" for string in words[need_on]]
    words[need_us] = [string[:-1] + "us" for string in words[need_us]]
    words[need_us_special] = [string[:-3] + "us" for string in words[need_us_special]]
    words[need_um] = [string[:-1] + "um" for string in words[need_um]]
    words[need_ex] = [string[:-4] + "ex" for string in words[need_ex]]
    words[need_ix] = [string[:-4] + "ix" for string in words[need_ix]]
    words[need_is_greek] = [string[:-3] + "is" for string in words[need_is_greek]]
    words[need_is_latin] = [string[:-2] + "is" for string in words[need_is_latin]]
  
    # fix generic rules
    words[remove_last3] = [string[:-3] for string in words[remove_last3]]
    words[remove_last2] = [string[:-2] for string in words[remove_last2]]
    words[remove_last] = [string[:-1] for string in words[remove_last]]
  
    return words

def make_singular(words):
    can_be_made_singular = ~is_singular(words)
    if any(can_be_made_singular):
        words[can_be_made_singular] = singularize(words[can_be_made_singular])
    return words


### Number Testers ----------------------------------------------------------

def is_singular(words):
    is_singular_with_s = (np.any([words.endswith(string) for string in English_s_singulars], axis = 0) | np.array([bool(re.search("\\b(?:bu|ga|ibi|len|ye)s$", word)) for word in words])) | \
        (np.array([bool(re.search("[^e]iu?s$", word)) for word in words]) & ~np.any([words.endswith(string) for string in all_is_plurals], axis = 0)) | \
        np.any([words.endswith(string) for string in Latin_us_to_i_singulars], axis = 0) | \
        np.any([words.endswith(string) for string in ("corpus", "genus", "viscus")], axis = 0) | \
        (np.any([words.endswith(string) for string in Latin_is_to_es_singulars], axis = 0) | np.array([bool(re.search("\\baxis$", word)) for word in words])) | \
        words.endswith("itis") | \
        words.endswith("ss") | \
        (words.endswith("us") & ~np.any([words.endswith(string) for string in English_us_plurals], axis = 0) & ~words.endswith("eaus"))
  
    is_plural_without_s = words.endswith("people") | \
        np.any([words.endswith(string) for string in ("brethren", "children")], axis = 0) | \
        (words.endswith("men") & ~(np.any([words.endswith(string) for string in ("abdomen", "acumen", "albumen", "bitumen", "foramen", "hymen", "lumen", "ramen", "regimen", "rumen", "semen", "specimen", "stamen")], axis = 0) | np.array([bool(re.search("\\b[ao]men$", word)) for word in words]))) | \
        np.any([words.endswith(string) for string in ("teeth", "feet", "geese")], axis = 0) | \
        (np.any([words.endswith(string) for string in ("booklice", "headlice", "dormice", "fieldmice", "shrewmice", "titmice")], axis = 0) | np.array([bool(re.search("\\b[lm]ice$", word)) for word in words])) | \
        np.array([bool(re.search("\\boxen$", word)) for word in words]) | \
        np.array([bool(re.search("\\bdice$", word)) for word in words]) | \
        np.any([words.endswith(string) for string in ("kobzari", "oblasti")], axis = 0) | \
        words.endswith("eaux") | \
        words.endswith("ae") | \
        words.endswith("kniazhestva") | \
        words.endswith("celli") | \
        np.any([words.endswith(string) for string in ("cherubim", "kibbutz", "seraph")], axis = 0) | \
        words.endswith("matzot") | \
        np.any([words.endswith(string) for string in ("hedra", "mata", "mena", "ria")], axis = 0) | \
        np.any([words.endswith(string) for string in ("genera", "viscera", "corpora")], axis = 0) | \
        np.any([words.endswith(string) for string in Latin_us_to_i_plurals], axis = 0) | \
        np.any([words.endswith(string) for string in Latin_us_to_a_plurals], axis = 0)
  
    is_indeterminate = np.any([words.endswith(string) for string in English_invariant_words], axis = 0) | \
        np.any([words.endswith(string) for string in English_uncountable_words], axis = 0) | \
        np.any([words.endswith(string) for string in Japanese_words_in_English], axis = 0) | \
        np.any([words.endswith(string) for string in Maori_words_in_English], axis = 0) | \
        words.endswith("nese")
    
    is_singular = is_indeterminate | \
        is_singular_with_s | \
        ~(words.endswith("s") | is_plural_without_s)
    
    return is_singular


### Lemmatizer --------------------------------------------------------------

English_ly_nouns = ["ally", "anomaly", "assembly", "belly", "bully", "butterfly", "contumely", "doily", "dragonfly", "gadfly", "family", "filly", "firefly", "fly", "folly", "gully", "holly", "homily", "horsefly", "housefly", "jelly", "lily", "melancholy", "monopoly", "oligopoly", "panoply", "rally", "sandfly", "tally"]
English_ly_verbs = ["apply", "bely", "bully", "comply", "dally", "dilly-dally", "imply", "multiply", "ply", "rally", "rely", "reply", "sally", "shilly-shally", "supply", "tally"]
English_ly_adjectives = ["billy", "dilly", "early", "filly", "holy", "likely", "nilly", "only", "silly", "smily", "willy"]
English_ly_keepers = list(set(flat_concat([English_ly_nouns, English_ly_verbs, English_ly_adjectives])))
English_ly_to_le_words = ["doubly", "cycly", "muscly", "crackly", "crinkly", "fickly", "knuckly", "sparkly", "tinkly", "wrinkly", "crumply", "dimply", "druply", "riply", "rumply", "simply", "triply", "tuply", "bristly", "gently", "gristly", "rattly", "subtly", "thistly"]

English_anti_keepers = ["anticipat", "antidote", "antilog", "antimony", "anting", "antiquari", "antiquary", "antiquat", "antique", "antiqui", "antiquit", "antistrophe"]
English_dis_keepers = ["discreet", "discret(?:e|ion)", "discrepan", "discriminat", "disk", "dish", "display", "dismay", "dismal", "dismiss", "dispel", "discern", "discipl", "dispute", "distribu", "disrupt", "disturb", "discus", "diss", "dispose", "disgust", "dismiss", "distill", "disdain", "distort", "disease", "disco$", "discograph", "discover", "district", "distinct", "distinguish", "distan", "disten", "distress"]
English_imbmp_keepers = ["imbib", "imbitter", "imbolden", "imbecil", "imblaz", "imbroglio", "imbue", "immediat", "imp$", "impair", "impal", "impeach", "imped", "imperitive", "impertinent", "import", "implement", "imply", "implic", "impregnat", "improp", "impuls", "impresario", "impose", "imposit", "impetuous", "imperil", "imperial", "impact", "implod", "implos", "impress", "imprint", "imput", "impel", "impromptu", "implant", "impish", "impound", "impunit", "improv", "implor", "impuls", "imping", "immanenc", "immigrat", "immun", "immur", "immers", "immanent", "immens"]
English_in_keepers = ["in$", "inside$", "into$", "inane", "inanit", "inaug", "inbound", "inbre", "inch", "incas", "incens", "incentiv", "incept", "incid", "incis", "incit", "inclin", "inclos", "includ", "inclus", "incom[ei]", "increas", "increment", "incub", "inculca", "incur", "indeed", "indemn", "indent", "index", "india", "indic", "indie", "indig", "individual", "induc", "indulg", "industr", "indy", "inert", "infant", "inertia", "infatua", "infect", "infer", "infest", "infix", "inflat", "inflect", "inflict", "influen", "info", "infra", "infring", "infus", "ingest", "ingot", "ingrain", "ingrati", "ingredient", "ingroup", "inhabit", "inhal", "inherent", "inherit", "inhibit", "initia", "inject", "injure", "ink", "inlay", "inmate", "inn", "inositol", "input", "inquir", "insert", "insid", "insinuat", "insip", "insist", "insinuat", "inspect", "inspir", "install", "instan", "instat", "instead", "instigat", "instill", "instruct", "instrum", "institut", "insul", "insur", "intact", "integ", "intell", "inten", "inter", "intestin", "intimat", "intomb", "intro", "intru", "intubat", "intuit", "inundat", "inur", "invad", "invas", "invent", "invers", "invert", "invest", "invit", "invok", "invoc", "involv", "inward"]
English_mis_keepers = ["missile", "mission", "miser", "mischiev", "miscible", "misceg", "miscell", "misses", "miss$", "missed", "missing", "mishap", "mist", "miso", "mississippi"]
English_sub_keepers = ["sub$", "submit", "submar", "subtl", "subb(?:ed|ing)", "subject", "suburb", "subdu(?:e|ing)", "subway", "subsequent", "subvene", "subpena", "subduce", "subvert", "subsidy", "subside", "subsist", "sublime", "subtend", "submer[gs]e", "subtract", "substan[ct]", "subscri[bp]", "substitut", "subsidiar", "substrate"]#***
English_super_keepers = ["super$", "superfluous", "superior", "superlativ"]
English_un_keepers = ["uncle", "union", "unif", "univer", "unilat", "uniloc", "unifol", "uniform", "unit", "unival", "univar", "univoc", "unicycl", "uniling", "unilin", "unicam", "uniplan", "unipot", "unicol", "unitar", "unicorn", "uniax", "unique", "unison", "uniface", "unisex", "unless", "until"]
English_under_keepers = ["under$", "underneath$", "understand", "understood"]
English_other_keepers = ["anti$", "hyper$", "hypo$", "hypothe", "over$", "overly$", "under$", "underwh"]
English_prefix_keepers = flat_concat([English_anti_keepers, English_dis_keepers, English_imbmp_keepers, English_in_keepers, English_mis_keepers, English_sub_keepers, English_super_keepers, English_un_keepers, English_under_keepers, English_other_keepers])

English_iable_keepers = ["amiable", "liable", "viable"]
English_able_keepers = flat_concat(["able", "available", "cable", "fable", "gable", "horrible", "parable", "probable", "reliable", "stable", "table", "timetable", "vegetable", "vulnerable", English_iable_keepers])
English_ible_keepers = ["bible", "compatible", "eligible", "feasible", "horrible", "possible", "responsible", "terrible"]
English_eal_keepers = ["anneal", "appeal", "conceal", "congeal", "deal", "\\bmeal", "ordeal", "\\breal", "repeal", "reveal", "seal", "squeal", "steal"]
English_ial_keepers = ["artificial", "axial", "colloquial", "congenial", "cordial", "crucial", "jovial", "judicial", "material", "nubial", "social", "special", "superficial", "trial", "trivial", "venial", "vivial"]
English_ual_keepers = ["actual", "casual", "dual", "equal", "eventual", "individual", "lingual", "manual", "menstrual", "mutual", "ritual", "usual", "victual", "visual"]
English_al_keepers = flat_concat(["aboriginal", "animal", "arsenal", "capital", "cardinal", "carnival", "cathedral", "charcoal", "chemical", "coal", "crystal", "decimal", "\\bdent", "eternal", "federal", "final", "fiscal", "funeral", "general", "hospital", "integral", "international", "interval", "journal", "lateral", "legal", "liberal", "literal", "local", "loyal", "mammal", "marital", "medieval", "mental", "mineral", "moral", "municipal", "naval", "normal", "numeral", "\\boval", "primeval", "principal", "radical", "rival", "rural", "scandal", "secular", "several", "spectrum", "spiral", "temporal", "thermal", "total", "vassal", "vertical", "virtual", "vital", English_eal_keepers, English_ial_keepers, English_ual_keepers]) #*** integral to integrate?
English_ist_keepers = ["assist", "artist", "checklist", "chemist", "cist", "consist", "dentist", "enlist", "exist", "feist", "fist", "foist", "gist", "heist", "hoist", "insist", "list", "joist", "mist", "moist", "persist", "playlist", "protist", "resist", "schist", "shist", "twist", "wishlist", "wrist"] #, "florist"
English_ism_keepers = ["animism", "atheism", "autism", "baptism", "catechism", "deism", "fascism", "sadism", "sophism", "theism"]
English_ian_keepers = ["lesbian", "thespian"]
English_age_removers = ["acreage", "anchorage", "appendage", "baronage", "binage", "bondage", "breakage", "cellarage", "coinage", "corkage", "cousinage", "coverage", "creepage", "drainage", "factorage", "flowerage", "footage", "frontage", "fruitage", "gallonage", "graftage", "harborage", "herbage", "hermitage", "innage", "layerage", "leafage", "leakage", "layerage", "lighterage", "linkage", "meltage", "meterage", "mileage", "moorage", "orphanage", "package", "parentage", "passage", "patronage", "percentage", "pilotage", "portage", "porterage", "postage", "poundage", "pressage", "quarterage", "reportage", "roughage", "seepage", "sewerage", "shortage", "shrinkage", "signage", "siphonage", "spillage", "soilage", "steerage", "stowage", "surplusage", "tankage", "tillage", "tinage", "towage", "tutorage", "voltage", "wagonage", "wattage", "wharfage", "yardage"]
English_ish_keepers = ["abolish", "blish", "blemish", "burnish", "dish", "fish", "fetish", "finish", "flourish", "foolish", "garish", "guish", "hashish", "lavish", "monish", "parish", "perish", "plish", "plenish", "polish", "publish", "quish", "ravish", "relish", "wish"]
English_ment_keepers = ["parliament", "tournament", "testament", "ornament", "torment", "armament", "garment", "element", "plement", "department", "environment", "segment", "aliment", "moment", "comment", "condiment", "experiment", "ndiment", "pliment", "regiment", "sediment", "sentiment", "triment", "argument", "document", "instrument", "monument"]
English_ize_keepers = ["baptize", "braize", "maize", "ognize", "organize", "ostracize", "prize", "seize", "size"]

English_able_double_consonants = [thing * 2 for thing in ["b", "d", "g", "m", "n", "p", "r", "t"]]
English_doubled_consonants_able = [string + "able" for string in English_able_double_consonants]
English_ism_double_consonants = [thing * 2 for thing in ["b", "d", "g", "l", "n", "p", "t", "z"]]
English_doubled_consonants_ism = [string + "ism" for string in English_ism_double_consonants]
English_er_double_consonants = [thing * 2 for thing in ["b", "d", "g", "m", "n", "p", "t"]]
English_doubled_consonants_er = [string + "er" for string in English_er_double_consonants]
English_est_double_consonants = [thing * 2 for thing in ["b", "d", "g", "m", "n", "p", "t"]]
English_doubled_consonants_est = [string + "est" for string in English_est_double_consonants]
English_ed_double_consonants = [thing * 2 for thing in ["b", "d", "g", "l", "m", "n", "p", "r", "t", "v", "z"]]
English_doubled_consonants_ed = [string + "ed" for string in English_ed_double_consonants]
English_ing_double_consonants = [thing * 2 for thing in ["b", "d", "g", "l", "m", "n", "p", "r", "t", "v", "z"]]
English_doubled_consonants_ing = [string + "ing" for string in English_ing_double_consonants]

English_eer_keepers = ["beer", "career", "cheer", "deer", "domineer", "engineer", "killdeer", "jeer", "leer", "peer", "pioneer", "queer", "reindeer", "schmeer", "sheer", "sneer", "steer", "veer", "veneer", "volunteer"]
English_ier_keepers = ["brier", "cashier", "cavalier", "chandelier", "courier", "frontier", "glacier", "\\bpier", "premier", "soldier", "\\bspier", "\\btier"]
English_er_keepers = flat_concat(["under", "whether", "\\bever", "whenever", "wherever", "whichever", "whoever", "whomever", "however", "whatever", "whatsoever", "forever", "either", "neither", "after", "\\bnever", "\\bher", "differ", "number", "tower", "dinner", "matter", "trouser", "mister", "minister", "amber", "customer", "harbinger", "monger", "\\banger", "manger", "ganger", "\\bother", "another", "paper", "(?:head)?quarter", "helicopter", "over", "member", "water", "fiber", "wonder", "ancester", "cloister", "confer", "corner", "enter", "per", "luster", "neuter", "scepter", "order", "deliver", "prefer", "defer", "foster", "cluster", "murder", "chamber", "september", "october", "november", "december", "register", "weather", "together", "letter", "newsletter", "chapter", "better", "poker", "further", "farther", "remember", "river", "silver", "rather", "summer", "winter", "super", "cancer", "answer", "transfer", "filter", "consider", "partner", "character", "father", "mother", "brother", "sister", "daughter", "leather", "upper", "lower", "laser", "theater", "gender", "soccer", "proper", "refer", "master", "meter", "rubber", "monster", "mester", "prefer", "latter", "tiger", "finger", "danger", "powder", "integer", "pepper", "cover", "spider", "cyber", "shelter", "suffer", "beaver", "trigger", "fever", "butler", "timber", "gather", "roster", "encounter", "hammer", "cylinder", "boulder", "thunder", "ester", "render", "after", "monomer", "dimer", "trimer", "tetramer", "polymer", "bitter", "usher", "ginger", "carpenter", "clever", "alzheimer", "lavender", "eager", "surrender", "lumber", "diaper", "jupiter", "sweater", "minister", "litter", "panther", "pewter", "clutter", "bladder", "lever", "feather", "burger", "ledger", "lobster", "slaughter", "glitter", "garner", "oyster", "clover", "power", "conquer", "badger", "butcher", "register", "kosher", "viper", "whisper", "flower", "utter", "cater", "doppler", "snooker", "juniper", "cucumber", "deter", "infer", "ether", "caliber", "center", "hooker", "cider", "splinter", "chapter", "batter", "sober", "sinister", "otter", "slender", English_eer_keepers, English_ier_keepers])
English_iest_keepers = ["priest"]
English_est_keepers = flat_concat(["\\bbest", "digest", "earnest", "(?:\\b|gab|love|slug|song)fest", "harvest", "honest", "\\bjest", "\\blest", "manifest", "\\bnest", "\\bpest", "(?:\\b|arm|head)rest", "\\btest", "\\bvest", "(?:\\b|mid|north|south)west", "\\bzest", "arbalest", "arrest", "attest", "\\bchest", "contest", "crest", "forest", "(?:\\b|house)guest", "infest", "invest", "interest", "protest", "(?:\\b|ac|be|con|in|re)quest", "suggest", "tempest", English_iest_keepers])
English_ed_keepers = ["\\bbed", "bred", "\\bfed", "hundred", "infrared", "naked", "need", "\\bred", "sacred", "\\bshed", "watershed", "\\bwed", "\\bzed"]
English_ing_keepers = ["bring", "ceiling", "\\bcling", "darling", "\\bding", "\\bduring", "evening", "\\bfling", "\\bking", "lightning", "morning", "\\bpending", "\\bping", "\\bring", "\\bsing", "(?:\\b|un|war)sling", "spring", "sterling", "\\bsting", "string", "swing", "(?:\\b|any|every|no|some)?thing", "\\bwing", "\\bwring", "\\bzing"]

English_s_keepers = ["always", "perhaps", "whereas", "has", "is", "was"]

# rules for what kinds of word endings require an ultimate "e"
general_e_rules = "(?:(?:\\b|[^aieou]|ll)[aeiouy][bcfgkmsvz])"
ce_rules = "(?:[lnrs]c)"
de_rules = "(?:(?:[^aeiou][aeiou]|ui)d)"
ge_rules = "(?:(?:[dlr]|(?:(?:r|ch|str)a|(?:ll|v)e|(?:b|h|cr)i|(?:c|sp)o|(?:l|p|pl|scro)u)n)g)"
le_rules = "(?:(?:(?:imp|wholes|sc|wh)a|(?:(?:\\b|de)f|p|\\b[prt]|rev|sm)i|(?:cond|h|par|\\bp|recons|\\bt)o|(?:r|sched)u|y|[bcdfgkpstz])l)"
ne_rules = "(?:(?:[^aeiou][aiu]|(?:\\b(?:[bchtz]|cl|dr)|chaper|(?:de|im|post|pro)p|ph|thr|[as]t)o)n)"
oe_rules = "(?:(?:\\bh|(?:\\b|tip(py)?)t|sh)o)"
pe_rules = "(?:(?:[^aeiou][aeiuy]|(?:\\b(?:[cdhmr]|el)|gr|sc)o)p)"
re_rules = "(?:(?:[^aeiou][aiu]|(?:\\b(?:[bcgps]|ad|ch|depl|enc|expl|ign|impl|rest|sh|sp|st|wh)|sc|sn)o|qui)r)"
se_rules = "(?:(?:(?:ai|au|ea|ee|oi|oo|(?:(?:\\b|[^l])[^l]|\\bl)ou)|ui|[lnrw])s)"
te_rules = "(?:(?:(?:[^eo]|cre|ide)a|(?:comp|compl|del|excr)e|(?:(?:\\b|[^abeiou])b|(?:\\b|[^i])c|ign|ind|inv|sm|sp|qu|un|wh|wr|xc)i|(?:\\b[cdntv]|m|qu|[^i]v)o|(?:[^aeiou]|\\bro)u|[bhptw]as)t)"
ue_rules = "(?:u)"
ve_rules = "(?:(?:ai|ea|ee|ei|ie|[lr])v)"
ye_rules = "(?:(?:\\b|cross|hawk)ey)"
ze_rules = "(?:[^tz]z)"
che_rules = "(?:(?:(?:\\b|back|belly|head|stomach|tooth)a|ca)ch)"
e_rules = "(?:" + any_of(flat_concat([general_e_rules, ce_rules, de_rules, ge_rules, le_rules, ne_rules, oe_rules, pe_rules, re_rules, se_rules, te_rules, ue_rules, ve_rules, ye_rules, ze_rules, che_rules])) + ")"

def digest_words(words):
    # description: Used for "stemming" or "lemmatizing" words for Natural Language Processing. 
    # description: Works by removing prefixes and suffixes in the appropriate order. 
    # description:  
    # description: It's more accurate than typical stemming approaches: 
    # description: (fewer confabulated results and more correctly connected results, 
    # description: because it tests for and handles many special cases). 
    # description:  
    # description: It's more user-friendly than typical lemmatizing approaches:
    # description: (you don't need to worry about parts of speech, 
    # description: and it automatically goes to the most basic form). 
    # 
    # details: Uses the companion digested_word_dictionary(words) function to create a dictionary
    # details: of the unique input words (as the element name/key) 
    # details: and their digested outputs (as the element value). 
    # details: Read the comments in digested_word_dictionary(words) for more information. 
    # details: It relies on rules when there are rules (so it often works on made-up words), 
    # details: but the rest is hard-coded (and there are admittedly still plenty of 
    # details: gaps in coverage for special cases). 
    # details: Uses the companion make_singular(words), is_plural(words), and singularize(words)
    # details: functions for handling plural (especially foreign/Greek/Latin/unusual plural) forms. 
    # details: See the documentation of these functions for more information. 
    # 
    # input: a character vector of lower-case English words to "digest" into their core lemmas (most meaningful lexical components)
    # 
    # input specs: Nones and "" elements are acceptable and do not cause error or warning. 
    # input specs: Empty inputs are acceptable and do not cause error or warning. 
    # input specs: Words containing contractions are acceptable and handled properly. 
    # input specs: It also properly handles the last components of hyphenated words, 
    # input specs: ignoring preceding compents (unless they're prefixes, in which case they're removed). 
    # input specs: Proper nouns are currently *NOT* masked or handled properly, 
    # input specs: so don't expect them to be returned unchanged. 
    # 
    # output: a character vector of the "digested" words
    # 
    # output specs: Nones elements returned as None; "" elements returned as "". 
    # output specs: Elements are returned in the same order (in a vector of the same length). 
    # output specs: Nouns are returned in singular (non-plural) form. 
    # output specs: Verbs are returned in infinitive form. 
    # output specs: All negations (non-/un-/in-/dis-/anti-) are dropped. 
    # output specs: Stopwords are returned unchanged--handle them on your own. 
    # 
    # example input: digest_words("antidisestablishmentarianismesquely")
    # example output: "establish"
    # 
    # example input: digest_words("supercalifragilisticexpialidocious")
    # example output: "califragilisticexpialidocious")
    # 
    # example input: digest_words("shouldn't've")
    # example output: "shall"
    # 
    # example input: digest_words("can't-believe-it's-not-butterific")
    # example output: "can't-believe-it's-not-butter"
    # 
    # example input: digest_words("re-doing")
    # example output: "do"
    # 
    # notes: This could be used in the future for grammatical approaches, 
    # notes: as it breaks down words by part of speech-related suffixes. 
    # notes: In future may separate contractions into component words. 
    # notes: In future may handle co-, en-, inter-, intra-, semi- prefixes. 
    # 
    if type(words) is list:
        words = [word if type(word) is str else "" for word in words]
    results = np.char.array(words, itemsize = (len(max(words, key = len))) + 2)
    digest_dict = digested_word_dictionary(words)
    valid = results.nonzero()
    results[valid] = [digest_dict[word] for word in results[valid]]
    if type(words) is list:
        return [result for result in results]
    else:
        return results

def digested_word_dictionary(words):
    ### process only unique non-blank and non-NA values
    # (avoids redundant computation)
    original_words = np.unique(np.char.array(filter(non_empty_string, words)))
    words = np.char.array(original_words, itemsize = (original_words.itemsize + 2))
  
    ### simplify number (singular/plural) to singular case
    # (obviates checking optional s on some suffixes--avoids unecessary computation)
    # has desired side effect of simplifying number (plurals and singulars alike all end up as singular)
    # has desired side effect of taking "ies" verb forms to "y" as well, further simplifying things
    can_be_made_singular = np.array([not(bool(re.search("\\b" + any_of(English_s_keepers) + "$", word))) for word in words])
    if any(can_be_made_singular):
        words[can_be_made_singular] = make_singular(words[can_be_made_singular])
  
    ### handle contractions
    # contractions block the ending of words (hiding endings in endsWith() checks), so they must be removed
    subset_scope = np.array([bool(re.search("'", word)) for word in words])
    subset = words[subset_scope]
    if len(subset) != 0:
        subset = [re.sub(string = string, pattern = "\\bwon't\\b", repl = "will") for string in subset]
        subset = [re.sub(string = string, pattern = "\\bmight've\\b", repl = "might") for string in subset]
        subset = [re.sub(string = string, pattern = "(?:n't|'ve|'ll|'re|')+$", repl = "") for string in subset]
        words[subset_scope] = subset
  
    ### handle irregular words
  
    # irregular past participles ending in "dden"
    subset_scope = words.endswith("dden")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "downtrodden" -> "downtread"
        odden_to_ead = subset.endswith("trodden")
        subset[odden_to_ead] = [string[:-5] + "ead" for string in subset[odden_to_ead]]
        # e.g. "forbidden" -> "forbid"
        delete_den = np.any([subset.endswith(string) for string in ("adden", "bidden", "edden")], axis = 0)
        subset[delete_den] = [string[:-3] for string in subset[delete_den]]
        # e.g. "hidden" -> "hide"
        idden_to_ide = subset.endswith("idden") & ~np.any([subset.endswith(string) for string in ("midden", "swidden")], axis = 0)
        subset[idden_to_ide] = [string[:-3] + "e" for string in subset[idden_to_ide]]
        words[subset_scope] = subset
  
    # irregular past participles ending in "tten"
    subset_scope = words.endswith("tten")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "written" -> "write"
        itten_to_ite = subset.endswith("itten") & ~np.any([subset.endswith(string) for string in ("kitten", "mitten")], axis = 0)
        subset[itten_to_ite] = [string[:-3] + "e" for string in subset[itten_to_ite]]
        # e.g. "rotten" -> "rot"
        delete_ten = np.any([subset.endswith(string) for string in ("atten", "otten")], axis = 0)
        subset[delete_ten] = [string[:-3] for string in subset[delete_ten]]
        words[subset_scope] = subset
  
    # irregular past participles ending in "en" (and a few adjectives)
    subset_scope = words.endswith("en")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "eaten" -> "eat" (also "been" to "be")
        delete_en = np.any([subset.endswith(string) for string in ("ashen", "been", "drunken", "earthen", "eaten", "fallen", "olden", "silken", "swollen", "wooden", "woolen")], axis = 0)
        subset[delete_en] = [string[:-2] for string in subset[delete_en]]
        # e.g. "broken" -> "broke" (later to "break")
        delete_n = np.any([subset.endswith(string) for string in ("aken", "chosen", "iven", "oken", "olen", "oven", "risen", "rozen", "seen")], axis = 0) & ~(subset.endswith("kraken") | np.array([bool(re.search("\\boven$", word)) for word in subset]))
        subset[delete_n] = [string[:-1] for string in subset[delete_n]]
        words[subset_scope] = subset
  
    # irregular past participles ending in "n"
    subset_scope = words.endswith("n")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "worn" -> "wore" (later to "wear")
        n_to_e = np.any([subset.endswith(string) for string in ("born", "torn", "worn")], axis = 0) & ~np.any([subset.endswith(string) for string in ("stubborn", "attorn")], axis = 0)
        subset[n_to_e] = [string[:-1] + "e" for string in subset[n_to_e]]
        # e.g. "lain" -> "lie"
        ain_to_ay = subset.endswith("lain")
        subset[ain_to_ay] = [string[:-3] + "ie" for string in subset[ain_to_ay]]
        # e.g. "shorn" -> "shear"
        orn_to_ear = subset.endswith("shorn")
        subset[orn_to_ear] = [string[:-3] + "ear" for string in subset[orn_to_ear]]
        # e.g. "drawn" -> "draw"
        delete_n = np.array([bool(re.search("\\b" + any_of(["blown", "drawn", "grown", "known", "sewn", "shaken", "shown", "sown", "thrown"]) + "$", word)) for word in subset])
        subset[delete_n] = [string[:-1] for string in subset[delete_n]]
        words[subset_scope] = subset
  
    # irregular past participles ending in "t"
    subset_scope = words.endswith("t")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "burnt" -> "burn"
        delete_t = np.any([subset.endswith(string) for string in ("burnt", "dealt", "dreamt", "learnt", "meant")], axis = 0)
        subset[delete_t] = [string[:-1] for string in subset[delete_t]]
        # e.g. "built" -> "build"
        t_to_d = np.any([subset.endswith(string) for string in ("built", "spent")], axis = 0) | np.array([bool(re.search("\\b" + any_of(["bent", "lent", "sent"]) + "$", word)) for word in subset])
        subset[t_to_d] = [string[:-1] + "d" for string in subset[t_to_d]]
        # e.g. "lost" -> "lose"
        t_to_e = subset.endswith("lost")
        subset[t_to_e] = [string[:-1] + "e" for string in subset[t_to_e]]
        # e.g. "left" -> "leave"
        eft_to_eave = np.any([subset.endswith(string) for string in ("bereft", "left")], axis = 0)
        subset[eft_to_eave] = [string[:-2] + "ave" for string in subset[eft_to_eave]]
        words[subset_scope] = subset
  
    #*** prevents spurious edits later on
    #*** make common irregular words get fixed even if not at end of word phrase
  
    # common irregular words
    reasonable_slice = np.array([bool(re.search("\\ban$", word)) for word in words])
    words[reasonable_slice] = [string[:-1] for string in words[reasonable_slice]]
  
    reasonable_slice = np.any([words.endswith(string) for string in ("am", "are", "is", "was", "were")], axis = 0)
    words[reasonable_slice] = [re.sub(string = string, pattern = "\\b" + any_of(["am", "are", "been", "is", "was", "were"]) + "$", repl = "be") for string in words[reasonable_slice]]
  
    reasonable_slice = np.any([words.endswith(string) for string in ("did", "done")], axis = 0)
    words[reasonable_slice] = [re.sub(string = string, pattern = "\\b" + any_of(["did", "done"]) + "$", repl = "do") for string in words[reasonable_slice]]
  
    reasonable_slice = np.array([bool(re.search("\\bha[ds]$", word)) for word in words])
    words[reasonable_slice] = [string[:-1] + "ve" for string in words[reasonable_slice]]
  
    reasonable_slice = np.any([words.endswith(string) for string in ("went", "gone")], axis = 0)
    words[reasonable_slice] = [re.sub(string = string, pattern = "\\b" + any_of(["went", "gone"]) + "$", repl = "go") for string in words[reasonable_slice]]
  
    reasonable_slice = np.any([words.endswith(string) for string in ("ate", "edible", "edibly")], axis = 0)
    words[reasonable_slice] = [re.sub(string = string, pattern = "\\b" + any_of(["eats", "ate", "eaten", "eating", "edible", "edibly"]) + "$", repl = "eat") for string in words[reasonable_slice]]
  
    reasonable_slice = np.any([words.endswith(string) for string in ("cannot", "could")], axis = 0)
    words[reasonable_slice] = [re.sub(string = string, pattern = "\\b" + any_of(["cannot", "could"]) + "$", repl = "can") for string in words[reasonable_slice]]
  
    reasonable_slice = words.endswith("should")
    words[reasonable_slice] = [string[:-4] + "all" for string in words[reasonable_slice]]
  
    reasonable_slice = words.endswith("might")
    words[reasonable_slice] = [string[:-4] + "ay" for string in words[reasonable_slice]]
  
    reasonable_slice = np.any([words.endswith(string) for string in ("bore", "borne")], axis = 0)
    words[reasonable_slice] = [re.sub(string = string, pattern = "\\b" + any_of(["bore", "born", "borne"]) + "$", repl = "bear") for string in words[reasonable_slice]]
  
    reasonable_slice = np.any([words.endswith(string) for string in ("better", "best")], axis = 0)
    words[reasonable_slice] = [re.sub(string = string, pattern = "\\b" + any_of(["better", "best"]) + "$", repl = "good") for string in words[reasonable_slice]]
  
    reasonable_slice = np.any([words.endswith(string) for string in ("worse", "worst")], axis = 0)
    words[reasonable_slice] = [re.sub(string = string, pattern = "\\b" + any_of(["worse", "worst"]) + "$", repl = "bad") for string in words[reasonable_slice]]
  
    reasonable_slice = words.endswith("these")
    words[reasonable_slice] = [string[:-3] + "is" for string in words[reasonable_slice]]
  
    reasonable_slice = words.endswith("those")
    words[reasonable_slice] = [string[:-3] + "at" for string in words[reasonable_slice]]
  
    # irregular verbs without much pattern
  
    # handle irregulars ending in "d"
    subset_scope = words.endswith("d")
    subset = words[subset_scope]
    if len(subset) != 0:
    
        subsubset_scope = subset.endswith("ed")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            ed_to_ead = np.array([bool(re.search("\\bled$", word)) for word in subsubset])
            subsubset[ed_to_ead] = [string[:-1] + "ad" for string in subsubset[ed_to_ead]]
            ed_to_ee = np.array([bool(re.search("\\bfled$", word)) for word in subsubset])
            subsubset[ed_to_ee] = [string[:-1] + "e" for string in subsubset[ed_to_ee]]
            ed_to_eed = np.array([bool(re.search("\\b" + any_of(["bled", "bred", "fed", "sped"]) + "$", word)) for word in subsubset])
            subsubset[ed_to_eed] = [string[:-1] + "ed" for string in subsubset[ed_to_eed]]
            subset[subsubset_scope] = subsubset
    
        subsubset_scope = subset.endswith("id")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            id_to_ide = subsubset.endswith("slid") | np.array([bool(re.search("\\bhid$", word)) for word in subsubset])
            subsubset[id_to_ide] = [string + "e" for string in subsubset[id_to_ide]]
            aid_to_ay = np.any([subsubset.endswith(string) for string in ("laid", "paid", "said")], axis = 0) & ~subsubset.endswith("plaid")
            subsubset[aid_to_ay] = [string[:-2] + "y" for string in subsubset[aid_to_ay]]
            subset[subsubset_scope] = subsubset
    
        subsubset_scope = subset.endswith("ld")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            eld_to_old = subsubset.endswith("held")
            subsubset[eld_to_old] = [string[:-3] + "old" for string in subsubset[eld_to_old]]
            old_to_ell = np.any([subsubset.endswith(string) for string in ("sold", "told")], axis = 0)
            subsubset[old_to_ell] = [string[:-3] + "ell" for string in subsubset[old_to_ell]]
            subset[subsubset_scope] = subsubset
    
        ound_to_ind = np.any([subset.endswith(string) for string in ("bound", "found")], axis = 0) # "ground", "wound" (these are also unrelated nouns)
        subset[ound_to_ind] = [string[:-4] + "ind" for string in subset[ound_to_ind]]
    
        subsubset_scope = subset.endswith("od")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            od_to_ead = subsubset.endswith("trod")
            subsubset[od_to_ead] = [string[:-2] + "ead" for string in subsubset[od_to_ead]]
            ood_to_and = subsubset.endswith("stood")
            subsubset[ood_to_and] = [string[:-3] + "and" for string in subsubset[ood_to_and]]
            subset[subsubset_scope] = subsubset
    
        eard_to_ear = subset.endswith("heard")
        subset[eard_to_ear] = [string[:-1] for string in subset[eard_to_ear]]
    
        words[subset_scope] = subset
  
    # handle irregulars ending in "e"
    subset_scope = words.endswith("e")
    subset = words[subset_scope]
    if len(subset) != 0:
    
        subsubset_scope = subset.endswith("de")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            ade_to_ake = subsubset.endswith("made") & ~np.any([subsubset.endswith(string) for string in ("amade", "omade")], axis = 0)
            subsubset[ade_to_ake] = [string[:-2] + "ke" for string in subsubset[ade_to_ake]]
            ade_to_id = subsubset.endswith("forbade") | np.array([bool(re.search("\\bbade$", word)) for word in subsubset])
            subsubset[ade_to_id] = [string[:-3] + "id" for string in subsubset[ade_to_id]]
            ode_to_ide = np.any([subsubset.endswith(string) for string in ("joyrode", "outrode", "overrode", "strode")], axis = 0) | np.array([bool(re.search("\\brode$", word)) for word in subsubset])
            subsubset[ode_to_ide] = [string[:-3] + "ide" for string in subsubset[ode_to_ide]]
            subset[subsubset_scope] = subsubset
    
        subsubset_scope = subset.endswith("ke")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            oke_to_ake = subsubset.endswith("woke")
            subsubset[oke_to_ake] = [string[:-3] + "ake" for string in subsubset[oke_to_ake]]
            oke_to_eak = np.any([subsubset.endswith(string) for string in ("broke", "spoke")], axis = 0)
            subsubset[oke_to_eak] = [string[:-3] + "eak" for string in subsubset[oke_to_eak]]
            subset[subsubset_scope] = subsubset
    
        ole_to_eal = subset.endswith("stole")
        subset[ole_to_eal] = [string[:-3] + "eal" for string in subset[ole_to_eal]]
    
        ame_to_ome = subset.endswith("came")
        subset[ame_to_ome] = [string[:-3] + "ome" for string in subset[ame_to_ome]]
    
        one_to_ine = subset.endswith("shone")
        subset[one_to_ine] = [string[:-3] + "ine" for string in subset[one_to_ine]]
    
        ore_to_ear = np.any([subset.endswith(string) for string in ("tore", "wore")], axis = 0) & ~np.any([subset.endswith(string) for string in ("atore", "store")], axis = 0)
        subset[ore_to_ear] = [string[:-3] + "ear" for string in subset[ore_to_ear]]
    
        subsubset_scope = subset.endswith("se")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            ose_to_ise = np.array([bool(re.search("\\brose$", word)) for word in subsubset])
            subsubset[ose_to_ise] = [string[:-3] + "ise" for string in subsubset[ose_to_ise]]
            ose_to_oose = subsubset.endswith("chose")
            subsubset[ose_to_oose] = [string[:-2] + "ose" for string in subsubset[ose_to_oose]]
            subset[subsubset_scope] = subsubset
    
        ote_to_ite = np.any([subset.endswith(string) for string in ("smote", "wrote")], axis = 0)
        subset[ote_to_ite] = [string[:-3] + "ite" for string in subset[ote_to_ite]]
    
        subsubset_scope = subset.endswith("ve")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            ave_to_ive = subsubset.endswith("gave") & ~subsubset.endswith("agave")
            subsubset[ave_to_ive] = [string[:-3] + "ive" for string in subsubset[ave_to_ive]]
            ove_to_eave = subsubset.endswith("wove")
            subsubset[ove_to_eave] = [string[:-3] + "eave" for string in subsubset[ove_to_eave]]
            ove_to_ive = np.any([subsubset.endswith(string) for string in ("drove", "strove", "throve")], axis = 0) | np.array([bool(re.search("\\bdove$", word)) for word in subsubset])
            subsubset[ove_to_ive] = [string[:-3] + "ive" for string in subsubset[ove_to_ive]]
            subset[subsubset_scope] = subsubset
    
        oze_to_eeze = subset.endswith("froze")
        subset[oze_to_eeze] = [string[:-3] + "eeze" for string in subset[oze_to_eeze]]
    
        words[subset_scope] = subset
  
    # handle irregulars ending in "g"
    subset_scope = words.endswith("g")
    subset = words[subset_scope]
    if len(subset) != 0:
        aong_to_ing = np.any([subset.endswith(string) for string in ("rang", "sang", "song", "sprang", "strang", "swang", "wrang")], axis = 0)
        subset[aong_to_ing] = [string[:-3] + "ing" for string in subset[aong_to_ing]]
    
        # handle "ung" irregulars
        subsubset_scope = subset.endswith("ung")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            ung_to_ang = subsubset.endswith("hung")
            subsubset[ung_to_ang] = [string[:-3] + "ang" for string in subsubset[ung_to_ang]]
            ung_to_ing = np.any([subsubset.endswith(string) for string in ("clung", "flung", "rung", "slung", "sprung", "strung", "stung", "sung", "swung", "wrung")], axis = 0)
            subsubset[ung_to_ing] = [string[:-3] + "ing" for string in subsubset[ung_to_ing]]
            subset[subsubset_scope] = subsubset
    
        ug_to_ig = subset.endswith("dug")
        subset[ug_to_ig] = [string[:-2] + "ig" for string in subset[ug_to_ig]]
    
        words[subset_scope] = subset
  
    # handle irregulars ending in "k"
    subset_scope = words.endswith("k")
    subset = words[subset_scope]
    if len(subset) != 0:
    
        subsubset_scope = subset.endswith("ve")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            uck_to_ick = subsubset.endswith("stuck")
            subsubset[uck_to_ick] = [string[:-3] + "ick" for string in subsubset[uck_to_ick]]
            uck_to_ike = subsubset.endswith("struck")
            subsubset[uck_to_ike] = [string[:-3] + "ike" for string in subsubset[uck_to_ike]]
            subset[subsubset_scope] = subsubset
    
        aunk_to_ink = np.any([subset.endswith(string) for string in ("drank", "drunk", "sank", "sunk", "slank", "slunk", "stank", "stunk")], axis = 0)
        subset[aunk_to_ink] = [string[:-3] + "ink" for string in subset[aunk_to_ink]]
    
        ook_to_ake = np.any([subset.endswith(string) for string in ("forsook", "shook", "took")], axis = 0)
        subset[ook_to_ake] = [string[:-3] + "ake" for string in subset[ook_to_ake]]
    
        words[subset_scope] = subset
  
    # handle irregulars ending in "ll"
    subset_scope = words.endswith("ll")
    subset = words[subset_scope]
    if len(subset) != 0:
        ell_to_all = subset.endswith("fell")
        subset[ell_to_all] = [string[:-3] + "all" for string in subset[ell_to_all]]
        oll_to_ell = subset.endswith("swoll")
        subset[oll_to_ell] = [string[:-3] + "ell" for string in subset[oll_to_ell]]
        words[subset_scope] = subset
  
    aum_to_im = np.any([words.endswith(string) for string in ("swam", "swum")], axis = 0)
    words[aum_to_im] = [string[:-2] + "im" for string in words[aum_to_im]]
  
    # handle irregulars ending in "n"
    subset_scope = words.endswith("n")
    subset = words[subset_scope]
    if len(subset) != 0:
        an_to_un = np.any([subset.endswith(string) for string in ("foreran", "reran", "outran", "overran")], axis = 0) | np.array([bool(re.search("\\bran$", word)) for word in subset])
        subset[an_to_un] = [string[:-2] + "un" for string in subset[an_to_un]]
        on_to_in = subset.endswith("won")
        subset[on_to_in] = [string[:-2] + "in" for string in subset[on_to_in]]
        aun_to_in = np.any([subset.endswith(string) for string in ("began", "begun", "spun")], axis = 0)
        subset[aun_to_in] = [string[:-2] + "in" for string in subset[aun_to_in]]
        own_to_y = subset.endswith("flown")
        subset[own_to_y] = [string[:-3] + "y" for string in subset[own_to_y]]
        words[subset_scope] = subset
  
    # handle irregulars ending in "t"
    subset_scope = words.endswith("t")
    subset = words[subset_scope]
    if len(subset) != 0:
        at_to_it = np.any([subset.endswith(string) for string in ("sat", "spat")], axis = 0)
        subset[at_to_it] = [string[:-2] + "it" for string in subset[at_to_it]]
    
        et_to_eet = np.array([bool(re.search("\\bmet$", word)) for word in subset])
        subset[et_to_eet] = [string[:-1] + "et" for string in subset[et_to_eet]]
    
        # irregular verbs ending in "aught" or "ought"
        subsubset_scope = subset.endswith("ught")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            ought_to_ing = subsubset.endswith("brought")
            subsubset[ought_to_ing] = [string[:-5] + "ing" for string in subsubset[ought_to_ing]]
            ought_to_uy = subsubset.endswith("bought")
            subsubset[ought_to_uy] = [string[:-5] + "uy" for string in subsubset[ought_to_uy]]
            ought_to_eek = subsubset.endswith("sought")
            subsubset[ought_to_eek] = [string[:-5] + "eek" for string in subsubset[ought_to_eek]]
            ought_to_ight = subsubset.endswith("fought")
            subsubset[ought_to_ight] = [string[:-5] + "ight" for string in subsubset[ought_to_ight]]
            ought_to_ink = subsubset.endswith("thought")
            subsubset[ought_to_ink] = [string[:-5] + "ink" for string in subsubset[ought_to_ink]]
            aught_to_atch = subsubset.endswith("caught")
            subsubset[aught_to_atch] = [string[:-5] + "atch" for string in subsubset[aught_to_atch]]
            aught_to_each = subsubset.endswith("taught")
            subsubset[aught_to_each] = [string[:-5] + "each" for string in subsubset[aught_to_each]]
            subset[subsubset_scope] = subsubset
    
        it_to_ight = subset.endswith("lit") & ~np.any([subset.endswith(string) for string in ("llit", "slit", "split")], axis = 0)
        subset[it_to_ight] = [string[:-1] + "ght" for string in subset[it_to_ight]]
        it_to_ite = np.any([subset.endswith(string) for string in ("frostbit", "snakebit")], axis = 0) | np.array([bool(re.search("\\bbit$", word)) for word in subset])
        subset[it_to_ite] = [string[:-2] + "ite" for string in subset[it_to_ite]]
    
        elt_to_eel = np.any([subset.endswith(string) for string in ("felt", "knelt")], axis = 0)
        subset[elt_to_eel] = [string[:-2] + "el" for string in subset[elt_to_eel]]
    
        ept_to_eep = np.any([subset.endswith(string) for string in ("crept", "kept", "slept", "swept", "wept")], axis = 0)
        subset[ept_to_eep] = [string[:-2] + "ep" for string in subset[ept_to_eep]]
    
        ot_to_et = np.any([subset.endswith(string) for string in ("begot", "forgot")], axis = 0) | np.array([bool(re.search("\\bgot$", word)) for word in subset])
        subset[ot_to_et] = [string[:-2] + "et" for string in subset[ot_to_et]]
        ot_to_oot = np.any([subset.endswith(string) for string in ("countershot", "outshot", "overshot", "reshot", "upshot", "troubleshot")], axis = 0) | np.array([bool(re.search("\\bshot$", word)) for word in subset])
        subset[ot_to_oot] = [string[:-1] + "ot" for string in subset[ot_to_oot]]
    
        words[subset_scope] = subset
  
    # handle irregulars ending in "w"
    subset_scope = words.endswith("w")
    subset = words[subset_scope]
    if len(subset) != 0:
        aw_to_ee = np.any([subset.endswith(string) for string in ("foresaw", "oversaw", "resaw", "sightsaw")], axis = 0) | np.array([bool(re.search("\\bsaw$", word)) for word in subset])
        subset[aw_to_ee] = [string[:-2] + "ee" for string in subset[aw_to_ee]]
    
        # irregular verbs ending in "ew"
        subsubset_scope = subset.endswith("ew")
        subsubset = subset[subsubset_scope]
        if len(subsubset) != 0:
            ew_to_aw = subsubset.endswith("drew")
            subsubset[ew_to_aw] = [string[:-2] + "aw" for string in subsubset[ew_to_aw]]
            ew_to_y = subsubset.endswith("flew")
            subsubset[ew_to_y] = [string[:-2] + "y" for string in subsubset[ew_to_y]]
            ew_to_ay = subsubset.endswith("slew")
            subsubset[ew_to_ay] = [string[:-2] + "ay" for string in subsubset[ew_to_ay]]
            ew_to_ow = np.any([subsubset.endswith(string) for string in ("blew", "grew", "knew", "threw")], axis = 0)
            subsubset[ew_to_ow] = [string[:-2] + "ow" for string in subsubset[ew_to_ow]]
            subset[subsubset_scope] = subsubset
    
        words[subset_scope] = subset
  
    # ay_to_ie <- words %like% "\\blay$"
    # words[ay_to_ie] <- replace_last_n_chars_with(words[ay_to_ie], 2, "ie")
  
    ### handle prefixes
  
    # decelerate/devolve ~ accelerate/evolve
    # handled before most prefixes because otherwise "de" would be handled incorrectly
    de_to_ac = np.array([bool(re.search("\\bdecel", word)) for word in words])
    words[de_to_ac] = [re.sub(string = string, pattern = "\\bde", repl = "ac") for string in words[de_to_ac]]
    de_to_e = np.array([bool(re.search("\\bdevol", word)) for word in words])
    words[de_to_e] = [re.sub(string = string, pattern = "\\bd", repl = "") for string in words[de_to_e]]
  
    # prevent removal of prefix-like forms that actually aren't acting as prefixes
    has_keepable_prefix = np.array([bool(re.search("^" + any_of(English_prefix_keepers), word)) for word in words])
    # removes multiple (nested) prefixes
    # excludes a few difficult cases for further processing below
    delete_prefix = ~has_keepable_prefix & np.array([bool(re.search("\\b(?:(?:(?:a|de|ex|post|pre|re|semi|un|well)-)|((anti|dis|im[bmp]|hyper|hypo|in|mis|non|over|sub|super|under|un)-?))", word)) for word in words]) & np.array([not(bool(re.search("\\b(?:none($|theless)|im(?:migra|pov|prop))", word))) for word in words])
    words[delete_prefix] = [re.sub(string = string, pattern = "\\b(?:(?:a|de|ex|post|pre|re|semi|un|well)-|(?:anti|dis|im[bmp]|hyper|hypo|in|mis|non|over|sub|super|under|un)-?)(?:(?:a|de|ex|post|pre|re|semi|un|well)-|(?:anti|dis|im[bmp]|hyper|hypo|in|mis|non|over|sub|super|under|un)-?)*", repl = "") for string in words[delete_prefix]]
  
    # needs to be separate because the above rule would have taken immigrate/improper to igrate/roper
    delete_im_prefix = np.array([bool(re.search("\\bim(?:migra|pov|prop)", word)) for word in words])
    words[delete_im_prefix] = [re.sub(string = string, pattern = "\\bim", repl = "") for string in words[delete_im_prefix]]
  
    #* could add "ir" to normal prefix set above if a list of English_ir_keepers is made and used
    delete_ir_prefix = np.array([bool(re.search("\\birr", word)) for word in words]) & np.array([not(bool(re.search("\\birrigat", word))) for word in words])
    words[delete_ir_prefix] = [re.sub(string = string, pattern = "\\bir", repl = "") for string in words[delete_ir_prefix]]
  
    #* could add "ab" to normal prefix set above if a list of English_ab_keepers is made and used
    delete_ab_prefix = np.array([bool(re.search("\\babnormal", word)) for word in words])
    words[delete_ab_prefix] = [re.sub(string = string, pattern = "\\bab", repl = "") for string in words[delete_ab_prefix]]
  
    #* could add "mal" to normal prefix set above if a list of English_mal_keepers is made and used
    delete_mal_prefix = np.array([bool(re.search("\\bmal", word)) for word in words]) & np.array([not(bool(re.search("\\bmal(?:ady|ari|ark|e(?:$|s|ness)|efa|efi|evo|ici|ign|ing|l(?:$|[aeiou])|m|(?:$|t[aeiou]))", word))) for word in words])
    words[delete_mal_prefix] = [re.sub(string = string, pattern = "\\bmal", repl = "") for string in words[delete_mal_prefix]]
  
    ### handle first batch of generic noun and adjective suffixes
  
    # handle "ly" prefix
    subset_scope = words.endswith("ly")
    subset = words[subset_scope]
    if len(subset) != 0:
        # sometimes "lly" -> "ly"
        ly_to_l = np.any([subset.endswith(string) for string in ("billy", "hilly", "frilly")], axis = 0) | np.array([bool(re.search("\\bfully$", word)) for word in subset])
        subset[ly_to_l] = [string[:-1] for string in subset[ly_to_l]]
        # e.g. "ably" -> "able"
        bly_to_ble = subset.endswith("bly")
        subset[bly_to_ble] = [string[:-1] + "e" for string in subset[bly_to_ble]]
        ly_keeper_mask = np.array([not(bool(re.search("\\b" + any_of(English_ly_keepers) + "$", word))) for word in subset])
        # e.g. "happily" -> "happy"
        ily_to_y = subset.endswith("ily") & ly_keeper_mask
        subset[ily_to_y] = [string[:-3] + "y" for string in subset[ily_to_y]]
        # e.g. "subtly" -> "subtle"
        ly_to_le = np.any([subset.endswith(string) for string in English_ly_to_le_words], axis = 0) & ly_keeper_mask
        subset[ly_to_le] = [string[:-1] + "e" for string in subset[ly_to_le]]
        # e.g. "truly" -> "true"
        ly_to_e = np.any([subset.endswith(string) for string in ("uly", "wholly")], axis = 0)
        subset[ly_to_e] = [string[:-2] + "e" for string in subset[ly_to_e]]
        # general rule--remove suffix
        delete_ly = subset.endswith("ly") & ly_keeper_mask
        subset[delete_ly] = [string[:-2] for string in subset[delete_ly]]
        words[subset_scope] = subset
  
    # ("especially" ->) "especial" -> "special"
    is_especial = words.endswith("especial")
    words[is_especial] = [re.sub(string = string, pattern = "\\bespecial$", repl = "special") for string in words[is_especial]]
  
    # handle "ness" suffix
    subset_scope = words.endswith("ness")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "cleanliness" -> "clean"
        delete_liness = subset.endswith("liness")
        subset[delete_liness] = [string[:-6] for string in subset[delete_liness]]
        # e.g. "happiness" -> "happy"
        iness_to_y = subset.endswith("iness") & ~subset.endswith("business")
        subset[iness_to_y] = [string[:-5] + "y" for string in subset[iness_to_y]]
        # general rule--remove suffix
        delete_ness = subset.endswith("ness") & ~subset.endswith("business")
        subset[delete_ness] = [string[:-4] for string in subset[delete_ness]]
        words[subset_scope] = subset
  
    # handle "ity" suffix
    subset_scope = words.endswith("ity")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "mobility" -> "mobile"
        bility_to_bile = np.array([bool(re.search("(?:\\bla|mo|nu)bility$", word)) for word in subset])
        subset[bility_to_bile] = [string[:-3] + "e" for string in subset[bility_to_bile]]
        # e.g. "ability" -> "able"
        bility_to_ble = subset.endswith("bility")
        subset[bility_to_ble] = [string[:-5] + "le" for string in subset[bility_to_ble]]
        # e.g. "activity" -> "active"
        ity_to_e = np.any([subset.endswith(string) for string in ("antiquity", "purity", "ivity")], axis = 0)
        subset[ity_to_e] = [string[:-3] + "e" for string in subset[ity_to_e]]
        # e.g. "credulity" -> "credulous"
        ulity_to_ulous = subset.endswith("ulity")
        subset[ulity_to_ulous] = [string[:-3] + "ous" for string in subset[ulity_to_ulous]]
        # e.g. "hilarity" -> "hilarious"
        arity_to_arious = subset.endswith("hilarity")
        subset[arity_to_arious] = [string[:-2] + "ous" for string in subset[arity_to_arious]]
        # e.g. "clarity" -> "clear"
        arity_to_ear = subset.endswith("clarity")
        subset[arity_to_ear] = [string[:-5] + "ear" for string in subset[arity_to_ear]]
        # general rule--leave suffix unless ends with "al", "ic", or "lar"
        delete_ity = (np.any([subset.endswith(string) for string in ("ality", "icity", "larity")], axis = 0) & ~np.any([subset.endswith(string) for string in ("complicity", "felicity", "quality")], axis = 0)) | np.array([bool(re.search(any_of([string + "ity" for string in English_al_keepers]) + "$", word)) for word in subset])
        subset[delete_ity] = [string[:-3] for string in subset[delete_ity]]
        words[subset_scope] = subset
  
    # remove other "ty" suffixes
    delete_ty = np.any([words.endswith(string) for string in ("certainty", "nicety")], axis = 0)
    words[delete_ty] = [string[:-2] for string in words[delete_ty]]
  
    # handle "esque" suffix
    subset_scope = words.endswith("esque")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "statuesque" -> "statue"
        esque_to_e = np.any([subset.endswith(string) for string in ("uesque", "uresque")], axis = 0)
        subset[esque_to_e] = [string[:-4] for string in subset[esque_to_e]]
        # general rule--remove suffix
        delete_esque = subset.endswith("esque") & ~np.any([subset.endswith(string) for string in ("burlesque", "grotesque")], axis = 0)
        subset[delete_esque] = [string[:-5] for string in subset[delete_esque]]
        words[subset_scope] = subset
  
    # handle "ish" suffix
    subset_scope = words.endswith("ish")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "(im)poverish" -> "poverty"
        ish_to_ty = subset.endswith("poverish")
        subset[ish_to_ty] = [string[:-3] + "ty" for string in subset[ish_to_ty]]
        # e.g. "piggish" -> "pig"
        delete_ish_letter = np.any([subset.endswith(string) for string in ("cattish", "doggish", "hottish", "piggish")], axis = 0)
        subset[delete_ish_letter] = [string[:-4] for string in subset[delete_ish_letter]]
        # e.g. "brutish" -> "brute"
        ish_to_e = np.any([subset.endswith(string) for string in ("vampirish", "vulturish", "brutish", "ttish", "dovish", "voguish", "purplish", "ylish")], axis = 0)
        subset[ish_to_e] = [string[:-3] + "e" for string in subset[ish_to_e]]
        # general rule--remove suffix
        delete_ish = subset.endswith("ish") & (~np.any([subset.endswith(string) for string in English_ish_keepers], axis = 0) | subset.endswith("oafish"))
        subset[delete_ish] = [string[:-3] for string in subset[delete_ish]]
        words[subset_scope] = subset
  
    # handle "able" suffixes
    subset_scope = words.endswith("able")
    subset = words[subset_scope]
    if len(subset) != 0:
        able_keeper_mask = np.array([not(bool(re.search("\\b" + any_of(English_able_keepers) + "$", word))) for word in subset])
        # e.g. "reliable" -> "rely"
        iable_to_y = subset.endswith("iable") & np.array([not(bool(re.search("\\b" + any_of(English_iable_keepers) + "$", word))) for word in subset])
        subset[iable_to_y] = [string[:-5] + "y" for string in subset[iable_to_y]]
        # e.g. "despicable" -> "despise"
        icable_to_ise = subset.endswith("spicable")
        subset[icable_to_ise] = [string[:-5] + "se" for string in subset[icable_to_ise]]
        # e.g. "irritable" -> "irritate"
        able_to_ate = np.any([subset.endswith(string) for string in ("approximable", "culable", "gulable", "irritable", "operable", "icable")], axis = 0) & able_keeper_mask
        subset[able_to_ate] = [string[:-3] + "te" for string in subset[able_to_ate]]
        # e.g. "(inde)fatigable" -> "fatigue"
        able_to_ue = subset.endswith("fatigable")
        subset[able_to_ue] = [string[:-4] + "ue" for string in subset[able_to_ue]]
        # e.g. "memorable" -> "memory"
        able_to_y = np.any([subset.endswith(string) for string in ("charitable", "memorable")], axis = 0)
        subset[able_to_y] = [string[:-4] + "y" for string in subset[able_to_y]]
        # e.g. "flammable" -> "flame
        able_letter_to_e = subset.endswith("flammable")
        subset[able_letter_to_e] = [string[:-5] + "e" for string in subset[able_letter_to_e]]
        # e.g. "transferrable" -> "transfer"
        delete_able_letter = np.any([subset.endswith(string) for string in English_doubled_consonants_able], axis = 0) & able_keeper_mask
        subset[delete_able_letter] = [string[:-5] for string in subset[delete_able_letter]]
        # e.g. "sharable" -> "share"
        able_to_e = np.array([bool(re.search((e_rules + "able$"), word)) for word in subset]) & able_keeper_mask
        subset[able_to_e] = [string[:-4] + "e" for string in subset[able_to_e]]
        # general rule--remove suffix
        delete_able = subset.endswith("able") & able_keeper_mask
        subset[delete_able] = [string[:-4] for string in subset[delete_able]]
        words[subset_scope] = subset
  
    # handle "ible" suffixes
    subset_scope = words.endswith("ible")
    subset = words[subset_scope]
    if len(subset) != 0:
        ible_keeper_mask = np.array([not(bool(re.search("\\b" + any_of(English_ible_keepers) + "$", word))) for word in subset])
        # e.g. "(in)visible" -> "vision"
        ible_to_ion = subset.endswith("visible")
        subset[ible_to_ion] = [string[:-3] + "on" for string in subset[ible_to_ion]]
        # e.g. "(in)credible" -> "credit"
        ible_to_ent = subset.endswith("credible")
        subset[ible_to_ent] = [string[:-4] + "ent" for string in subset[ible_to_ent]]
        # e.g. "sensible" -> "sense"
        ible_to_e = np.array([bool(re.search((e_rules + "ible$"), word)) for word in subset]) & ible_keeper_mask
        subset[ible_to_e] = [string[:-4] + "e" for string in subset[ible_to_e]]
        # general rule--remove suffix
        delete_ible = subset.endswith("ible") & ible_keeper_mask
        subset[delete_ible] = [string[:-4] for string in subset[delete_ible]]
        words[subset_scope] = subset
  
    # handle "hood" suffix
    subset_scope = words.endswith("hood")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "livelihood" -> "live"
        delete_lihood = subset.endswith("livelihood")
        subset[delete_lihood] = [string[:-6] for string in subset[delete_lihood]]
        # e.g. "likelihood" -> "likely"
        ihood_to_y = subset.endswith("ihood")
        subset[ihood_to_y] = [string[:-5] + "y" for string in subset[ihood_to_y]]
        # general rule--remove suffix
        delete_hood = subset.endswith("hood")
        subset[delete_hood] = [string[:-4] for string in subset[delete_hood]]
        words[subset_scope] = subset
  
    # handle "ship" suffix
    subset_scope = words.endswith("ship")
    subset = words[subset_scope]
    if len(subset) != 0:
        delete_ship = ~(np.any([subset.endswith(string) for string in ("airship", "battleship", "fireship", "gunship", "longship", "mediumship", "midship", "motorship", "relationship", "spaceship", "steamship", "tankship", "tranship", "transship", "warship", "worship")], axis = 0) | np.array([bool(re.search("\\bship$", word)) for word in subset]))
        subset[delete_ship] = [string[:-4] for string in subset[delete_ship]]
        words[subset_scope] = subset
  
    ### handle other oddities
  
    # e.g. "unison" -> "unity"
    ison_to_ity = words.endswith("unison")
    words[ison_to_ity] = [string[:-3] + "ty" for string in words[ison_to_ity]]
    # e.g. "comparison" -> "compare"
    ison_to_e = words.endswith("comparison")
    words[ison_to_e] = [string[:-4] + "e" for string in words[ison_to_e]]
  
    # e.g. "legalese" -> "legal"
    delete_ese = words.endswith("ese") & ~np.any([words.endswith(string) for string in ("diocese", "eese", "manganese", "obese", "these")], axis = 0)
    words[delete_ese] = [string[:-3] for string in words[delete_ese]]
  
    # e.g. "programme" -> "program"
    amme_to_am = words.endswith("amme")
    words[amme_to_am] = [string[:-2] for string in words[amme_to_am]]
    # e.g. "theatre" -> "theater"
    re_to_er = np.any([words.endswith(string) for string in ("bre", "tre")], axis = 0)
    words[re_to_er] = [string[:-2] + "er" for string in words[re_to_er]]
  
    # e.g. "wowser" -> "wow"
    delete_ser = words.endswith("wowser")
    words[delete_ser] = [string[:-3] for string in words[delete_ser]]
    # e.g. "lawyer" -> "law"
    delete_yer = np.any([words.endswith(string) for string in ("bowyer", "lawyer", "sawyer")], axis = 0)
    words[delete_yer] = [string[:-3] for string in words[delete_yer]]
  
    # e.g. "western" -> "west"
    delete_ern = np.any([words.endswith(string) for string in ("eastern", "northern", "southern", "western")], axis = 0)
    words[delete_ern] = [string[:-3] for string in words[delete_ern]]
  
    # e.g. "cowardice" -> "coward"
    delete_ice = words.endswith("cowardice")
    words[delete_ice] = [string[:-3] for string in words[delete_ice]]
  
    # e.g. "hatred" -> "hate"
    red_to_e = words.endswith("hatred")
    words[red_to_e] = [string[:-3] + "e" for string in words[red_to_e]]
  
    # e.g. "elder" -> "old"
    eld_to_old = np.array([bool(re.search("\\beld(?:er|est)?$", word)) for word in words])
    words[eld_to_old] = [re.sub(string = string, pattern = "\\beld(?:er|est)?$", repl = "old") for string in words[eld_to_old]]
  
    # handle "estry" and "istry" suffixes
    subset_scope = words.endswith("stry")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "ancestry" -> "ancester"
        estry_to_est = np.any([subset.endswith(string) for string in ("ancestry", "forestry")], axis = 0)
        subset[estry_to_est] = [string[:-2] for string in subset[estry_to_est]]
        # e.g. "registry" -> "register"
        istry_to_ter = np.any([subset.endswith(string) for string in ("ministry", "registry")], axis = 0)
        subset[istry_to_ter] = [string[:-2] + "er" for string in subset[istry_to_ter]]
        # e.g. "artistry" -> "artist"
        istry_to_ist = np.any([subset.endswith(string) for string in ("artistry", "baptistry", "chemistry", "dentistry", "sophistry")], axis = 0)
        subset[istry_to_ist] = [string[:-2] for string in subset[istry_to_ist]]
        words[subset_scope] = subset
  
    ### fix final set of generic noun and adjective suffixes
  
    # e.g. "opportunistic" -> "opportunism"
    istic_to_ism = words.endswith("istic") & np.array([not(bool(re.search("\\bstatistic", word))) for word in words])
    words[istic_to_ism] = [string[:-3] + "m" for string in words[istic_to_ism]]
    # e.g. "opportunist" -> "opportunism"
    # some words are changed knowing they'll be handled below (e.g. "therapist" -> "therapism" -> "therapy")
    ist_to_ism = words.endswith("ist") & np.array([not(bool(re.search("\\b" + any_of(English_ist_keepers) + "$", word))) for word in words])
    words[ist_to_ism] = [string[:-1] + "m" for string in words[ist_to_ism]]
  
    # handle "ism" suffix
    subset_scope = words.endswith("ism")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "conservatism" -> "conservative"
        ism_to_ive = subset.endswith("rvatism")
        subset[ism_to_ive] = [string[:-3] + "ive" for string in subset[ism_to_ive]]
        # e.g. "scientism" -> "science"
        tism_to_ce = subset.endswith("scientism")
        subset[tism_to_ce] = [string[:-4] + "ce" for string in subset[tism_to_ce]]
        # e.g. "cosmopolitism" -> "cosmopolitan"
        ism_to_an = subset.endswith("cosmopolitism")
        subset[ism_to_an] = [string[:-3] + "an" for string in subset[ism_to_an]]
        # e.g. "(bi)linguism" (or "linguist") -> "lingual"
        ism_to_al = subset.endswith("linguism")
        subset[ism_to_al] = [string[:-3] + "al" for string in subset[ism_to_al]]
        # e.g. "metabolism" -> "metabolic"
        ism_to_ic = np.any([subset.endswith(string) for string in ("abolism", "barism", "mechanism", "ntrism")], axis = 0)
        subset[ism_to_ic] = [string[:-2] + "c" for string in subset[ism_to_ic]]
        # e.g. "therapism" (or "therapist") -> "therapy"
        ism_to_y = np.any([subset.endswith(string) for string in ("economism", "jurism", "pharmacism", "quism", "rgism", "therapism")], axis = 0)
        subset[ism_to_y] = [string[:-3] + "y" for string in subset[ism_to_y]]
        # e.g. "activism" -> "active"
        ism_to_e = np.any([subset.endswith(string) for string in ("activism", "biblism", "chromism", "chronism", "communism", "cubism", "elitism", "flutism", "imagism", "itism", "nudism", "nudism", "oboism", "purism", "racism", "rapism", "titlism", "tropism", "typism", "vism")], axis = 0)
        subset[ism_to_e] = [string[:-3] + "e" for string in subset[ism_to_e]]
        # e.g. "snobbism" -> "snob"
        delete_ism_letter = np.any([subset.endswith(string) for string in English_doubled_consonants_ism], axis = 0)
        subset[delete_ism_letter] = [string[:-4] for string in subset[delete_ism_letter]]
        # general rule--remove suffix
        delete_ism = subset.endswith("ism") & np.array([not(bool(re.search("\\b" + any_of(English_ism_keepers) + "$", word))) for word in subset])
        subset[delete_ism] = [string[:-3] for string in subset[delete_ism]]
        words[subset_scope] = subset
  
    # handle "al" suffix
    subset_scope = words.endswith("al")
    subset = words[subset_scope]
    if len(subset) != 0:
        # handle "ial" suffix
    
        # e.g. "serial" -> "series"
        is_serial = subset.endswith("serial")
        subset[is_serial] = [string[:-2] + "es" for string in subset[is_serial]]
        # e.g. "imperial" -> "empire"
        is_imperial = subset.endswith("imperial")
        subset[is_imperial] = [string[:-8] + "empire" for string in subset[is_imperial]]
        # e.g. "beneficial" -> "benefit"
        cial_to_t = subset.endswith("beneficial")
        subset[cial_to_t] = [string[:-4] + "t" for string in subset[cial_to_t]]
        # e.g. "ceremonial" -> "ceremony"
        ial_to_y = np.any([subset.endswith(string) for string in ("ceremonial", "colonial", "custodial", "memorial", "mercurial", "monial", "territorial", "trial", "versial")], axis = 0)
        subset[ial_to_y] = [string[:-3] + "y" for string in subset[ial_to_y]]
        # e.g. "bacterial" -> "bacterium"
        ial_to_ium = np.any([subset.endswith(string) for string in ("bacterial", "cranial", "ennial", "fluvial", "sporial", "stadial")], axis = 0)
        subset[ial_to_ium] = [string[:-2] + "um" for string in subset[ial_to_ium]]
        # e.g. "essential" -> "essence"
        tial_to_ce = np.any([subset.endswith(string) for string in ("essential", "influential", "spatial")], axis = 0)
        subset[tial_to_ce] = [string[:-4] + "ce" for string in subset[tial_to_ce]]
        # e.g. "financial" -> "finance"
        ial_to_e = np.array([bool(re.search("(?:[aeiou][bcs]|[nr]c)ial$", word)) for word in subset]) & np.array([not(bool(re.search(any_of(English_ial_keepers) + "$", word))) for word in subset])
        subset[ial_to_e] = [string[:-3] + "e" for string in subset[ial_to_e]]
        # general "ial" rule--remove suffix
        delete_ial = subset.endswith("ial") & np.array([not(bool(re.search(any_of(English_ial_keepers) + "$", word))) for word in subset])
        subset[delete_ial] = [string[:-3] for string in subset[delete_ial]]
    
        # handle "ical" suffix
    
        # e.g. "cyclical" -> "cycle"
        lical_to_le = np.any([subset.endswith(string) for string in ("blical", "clical")], axis = 0)
        subset[lical_to_le] = [string[:-4] + "e" for string in subset[lical_to_le]]
        # e.g. "surgical" -> "surgery"
        ical_to_ery = subset.endswith("surgical")
        subset[ical_to_ery] = [string[:-4] + "ery" for string in subset[ical_to_ery]]
        # e.g. "identical" -> "identity"
        ical_to_ity = subset.endswith("identical")
        subset[ical_to_ity] = [string[:-3] + "ty" for string in subset[ical_to_ity]]
        # e.g. "chemical" -> "chemist"
        ical_to_ist = subset.endswith("chemical")
        subset[ical_to_ist] = [string[:-3] + "st" for string in subset[ical_to_ist]]
        # general "ical" rule is to follow general "al" rule (remove "al")
    
        # handle "ual" suffix
    
        # e.g. "annual" -> "annum"
        ual_to_um = subset.endswith("annual")
        subset[ual_to_um] = [string[:-2] + "m" for string in subset[ual_to_um]]
        # e.g. "sensual" -> "sense"
        ual_to_e = np.any([subset.endswith(string) for string in ("gradual", "sensual")], axis = 0)
        subset[ual_to_e] = [string[:-3] + "e" for string in subset[ual_to_e]]
        # e.g. "continual" -> "continue"
        ual_to_ue = np.any([subset.endswith(string) for string in ("accrual", "continual", "residual", "tissual", "virtual")], axis = 0)
        subset[ual_to_ue] = [string[:-2] + "e" for string in subset[ual_to_ue]]
        # e.g. "central" -> "center"
        tral_to_ter = np.any([subset.endswith(string) for string in ("ancestral", "central", "cloistral", "lustral", "neutral", "sceptral")], axis = 0)
        subset[tral_to_ter] = [string[:-3] + "er" for string in subset[tral_to_ter]]
        # general "ual" rule--remove suffix
        delete_ual = subset.endswith("ual") & np.array([not(bool(re.search(any_of(English_ual_keepers) + "$", word))) for word in subset])
        subset[delete_ual] = [string[:-3] for string in subset[delete_ual]]
    
        # handle "inal" suffix
    
        # e.g. "longitudinal" -> "longitude"
        tudinal_to_tude = subset.endswith("tudinal")
        subset[tudinal_to_tude] = [string[:-4] + "e" for string in subset[tudinal_to_tude]]
        # e.g. "criminal" -> "crime"
        inal_to_e = subset.endswith("criminal")
        subset[inal_to_e] = [string[:-4] + "e" for string in subset[inal_to_e]]
        # e.g. "maternal" -> "mater"
        #* could change this to "mother"/"father" later
        delete_nal = np.any([subset.endswith(string) for string in ("maternal", "paternal")], axis = 0)
        subset[delete_nal] = [string[:-3] for string in subset[delete_nal]]
        # general "inal" rule is to follow general "al" rule (remove "al")
    
        # handle "tal" suffix
    
        # e.g. "horizontal" -> "horizon"
        delete_tal = subset.endswith("horizontal")
        subset[delete_tal] = [string[:-3] for string in subset[delete_tal]]
        # general "tal" rule is to follow general "al" rule (remove "al")
    
        # handle plain "al" suffix
    
        # e.g. "referral" -> "refer"
        delete_al_letter = subset.endswith("referral")
        subset[delete_al_letter] = [string[:-3] for string in subset[delete_al_letter]]
        # e.g. "larval" -> "larva"
        delete_l = np.any([subset.endswith(string) for string in ("caval", "gingival", "larval", "orchestral", "vaginal")], axis = 0)
        subset[delete_l] = [string[:-1] for string in subset[delete_l]]
        # e.g. "peripheral" -> "periphery"
        al_to_y = np.any([subset.endswith(string) for string in ("peripheral", "societal")], axis = 0)
        subset[al_to_y] = [string[:-2] + "y" for string in subset[al_to_y]]
        # e.g. "neural" -> "neuron"
        al_to_on = subset.endswith("neural")
        subset[al_to_on] = [string[:-2] + "on" for string in subset[al_to_on]]
        # e.g. "spectral" -> "spectrum"
        al_to_um = np.any([subset.endswith(string) for string in ("poreal", "spectral", "minimal", "maximal", "optimal", "cerebral")], axis = 0)
        subset[al_to_um] = [string[:-2] + "um" for string in subset[al_to_um]]
        # e.g. "viral" -> "virus"
        al_to_us = np.any([subset.endswith(string) for string in ("colossal", "focal", "terminal", "viral")], axis = 0)
        subset[al_to_us] = [string[:-2] + "us" for string in subset[al_to_us]]
        # e.g. "global" -> "globe"
        al_to_e = np.any([subset.endswith(string) for string in ("communal", "global", "tribal", "practical", "bridal", "tribunal", "brutal", "ral", "sal", "val")], axis = 0) & ~(np.any([subset.endswith(string) for string in ("behavioral", "doctoral", "electoral", "medieval", "naval", "floral", "primeval")], axis = 0) | np.array([bool(re.search("\\b(?:o|ri)val$", word)) for word in subset])) & np.array([not(bool(re.search(any_of(English_al_keepers) + "$", word))) for word in subset])
        subset[al_to_e] = [string[:-2] + "e" for string in subset[al_to_e]]
        # e.g. "reciprocal" -> "reciprocate"
        al_to_ate = subset.endswith("reciprocal")
        subset[al_to_ate] = [string[:-1] + "te" for string in subset[al_to_ate]]
        # general rule--remove suffix
        delete_al = subset.endswith("al") & np.array([not(bool(re.search(any_of(English_al_keepers) + "$", word))) for word in subset])
        subset[delete_al] = [string[:-2] for string in subset[delete_al]]
        words[subset_scope] = subset
  
    # handle "ian" suffix
    subset_scope = words.endswith("ian")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "libertarian" -> "liberty"
        arian_to_y = np.any([subset.endswith(string) for string in ("ilarian", "itarian", "rtarian", "stinarian")], axis = 0)
        subset[arian_to_y] = [string[:-5] + "y" for string in subset[arian_to_y]]
        # e.g. "sectarian" -> "sect"
        delete_arian = np.any([subset.endswith(string) for string in ("fruitarian", "sectarian")], axis = 0)
        subset[delete_arian] = [string[:-5] for string in subset[delete_arian]]
        # e.g. "civilian" -> "civil"
        ian_to_e = np.any([subset.endswith(string) for string in ("avian", "esian", "ilian")], axis = 0) & ~subset.endswith("civilian")
        subset[ian_to_e] = [string[:-3] + "e" for string in subset[ian_to_e]]
        # e.g. "comedian" -> "comedy"
        ian_to_y = np.any([subset.endswith(string) for string in ("arian", "comedian", "custodian", "torian", "tregedian", "ovarian")], axis = 0)
        subset[ian_to_y] = [string[:-3] + "y" for string in subset[ian_to_y]]
        # general rule--remove suffix
        delete_ian = subset.endswith("ian") & ~np.any([subset.endswith(string) for string in English_ian_keepers], axis = 0)
        subset[delete_ian] = [string[:-3] for string in subset[delete_ian]]
        words[subset_scope] = subset
  
    # handle "ary" suffix
    subset_scope = words.endswith("ary")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "disciplinary" -> "discipline"
        ary_to_e = np.any([subset.endswith(string) for string in ("antiquary", "disciplinary", "primary")], axis = 0)
        subset[ary_to_e] = [string[:-3] + "e" for string in subset[ary_to_e]]
        # e.g. "legendary" -> "legend"
        delete_ary = np.any([subset.endswith(string) for string in ("dietary", "legendary", "ionary", "mentary", "parliamentary", "secondary")], axis = 0)
        subset[delete_ary] = [string[:-3] for string in subset[delete_ary]]
        words[subset_scope] = subset
  
    # handle "ment" suffix
    subset_scope = words.endswith("ment")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "judgment" -> "judge"
        ment_to_e = subset.endswith("dgment")
        subset[ment_to_e] = [string[:-4] + "e" for string in subset[ment_to_e]]
        # e.g. "merriment" -> "merry"
        iment_to_y = subset.endswith("iment") & ~np.any([subset.endswith(string) for string in English_ment_keepers], axis = 0)
        subset[iment_to_y] = [string[:-5] + "y" for string in subset[iment_to_y]]
        # general rule--remove suffix
        delete_ment = subset.endswith("ment") & ~np.any([subset.endswith(string) for string in English_ment_keepers], axis = 0)
        subset[delete_ment] = [string[:-4] for string in subset[delete_ment]]
        words[subset_scope] = subset
  
    # handle "ic" suffix
    subset_scope = words.endswith("ic")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "technic" -> "technique"
        ic_to_ique = subset.endswith("technic")
        subset[ic_to_ique] = [string[:-1] + "que" for string in subset[ic_to_ique]]
        # e.g. "cortic(al)" -> "cortex"
        ic_to_ex = np.any([subset.endswith(string) for string in ("cortic", "vortic")], axis = 0)
        subset[ic_to_ex] = [string[:-2] + "ex" for string in subset[ic_to_ex]]
    
        # handle "ific" suffix
    
        # e.g. "scientific" -> "science"
        tific_to_ce = subset.endswith("scientific")
        subset[tific_to_ce] = [string[:-5] + "ce" for string in subset[tific_to_ce]]
        # e.g. "specific" -> "specify"
        ific_to_ify = np.any([subset.endswith(string) for string in ("cific", "rific")], axis = 0)
        subset[ific_to_ify] = [string[:-3] + "fy" for string in subset[ific_to_ify]]
    
        # handle "tic" suffixes
    
        # e.g. # "hypnotic" -> "hypnosis"
        tic_to_sis = np.any([subset.endswith(string) for string in ("hypnotic", "hypothetic")], axis = 0)
        subset[tic_to_sis] = [string[:-3] + "sis" for string in subset[tic_to_sis]]
        atic_to_e = subset.endswith("chromatic")
        subset[atic_to_e] = [string[:-4] + "e" for string in subset[atic_to_e]]
        delete_atic = np.any([subset.endswith(string) for string in ("informatic", "symptomatic")], axis = 0)
        subset[delete_atic] = [string[:-4] for string in subset[delete_atic]]
    
        # handle "ric" suffix
    
        # e.g. "cylindric" -> "cylinder"
        ric_to_er = np.any([subset.endswith(string) for string in ("ndric", "ntric", "theatric")], axis = 0)
        subset[ric_to_er] = [string[:-3] + "er" for string in subset[ric_to_er]]
    
        # handle general "ic" suffix
    
        # e.g. "spheric" -> "sphere"
        ic_to_e = np.any([subset.endswith(string) for string in ("spheric", "typic")], axis = 0)
        subset[ic_to_e] = [string[:-2] + "e" for string in subset[ic_to_e]]
        # e.g. "toxic" -> "toxin"
        ic_to_in = subset.endswith("toxic")
        subset[ic_to_in] = [string[:-1] + "n" for string in subset[ic_to_in]]
        # e.g. "euphoric" -> "euphoria"
        ic_to_ia = np.any([subset.endswith(string) for string in ("dysphoric", "euphoric")], axis = 0)
        subset[ic_to_ia] = [string[:-1] + "a" for string in subset[ic_to_ia]]
        # e.g. "graphic" -> "graph"
        delete_ic = np.any([subset.endswith(string) for string in ("alphabetic", "graphic", "gymnastic", "istic", "phoric", "xic")], axis = 0) & np.array([not(bool(re.search("\\bstatistic", word))) for word in subset])
        subset[delete_ic] = [string[:-2] for string in subset[delete_ic]]
        # e.g. "botanic" -> "botany"
        ic_to_y = np.any([subset.endswith(string) for string in ("archic", "botanic", "categoric", "metric", "nomic", "ologic", "pacific", "phic", "storic")], axis = 0)
        subset[ic_to_y] = [string[:-2] + "y" for string in subset[ic_to_y]]
        # general "ic" rule is to leave it
        words[subset_scope] = subset
  
    # handle "ous" suffix
    subset_scope = words.endswith("ous")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "multitudinous" -> "multitude"
        inous_to_e = np.any([subset.endswith(string) for string in ("multitudinous", "vicissitudinous")], axis = 0)
        subset[inous_to_e] = [string[:-5] + "e" for string in subset[inous_to_e]]
        # e.g. "extraneous" -> "extra"
        delete_ous_letters = subset.endswith("extraneous")
        subset[delete_ous_letters] = [string[:-5] for string in subset[delete_ous_letters]]
        # e.g. "incestuous" -> "incest"
        delete_ous_letter = np.any([subset.endswith(string) for string in ("censorious", "incestuous", "tortious")], axis = 0)
        subset[delete_ous_letter] = [string[:-4] for string in subset[delete_ous_letter]]
        # e.g. "famous" -> "fame"
        ous_to_e = np.any([subset.endswith(string) for string in ("famous", "nervous", "porous", "prestigious", "rapturous")], axis = 0)
        subset[ous_to_e] = [re.sub(string = string, pattern = "(i|in)?ous$", repl = "e") for string in subset[ous_to_e]]
        # e.g. "monstrous" -> "monster"
        trous_to_ter = np.any([subset.endswith(string) for string in ("brous", "strous", "xtrous")], axis = 0)
        subset[trous_to_ter] = [string[:-4] + "er" for string in subset[trous_to_ter]]
        # e.g. "anxious" -> "anxiety"
        ous_to_ety = subset.endswith("anxious")
        subset[ous_to_ety] = [string[:-3] + "ety" for string in subset[ous_to_ety]]
    
        # # e.g. "credulous" -> ""
        # ulous_to_ <- endsWith(subset, "credulous")
        # subset[ulous_to_] <- replace_last_n_chars_with(subset[ulous_to_], L, "")
    
        # e.g. "tenacious" -> "tenacity"
        ous_to_ty = np.any([subset.endswith(string) for string in ("atrocious", "capacious", "ferocious", "loquacious", "rapacious", "salacious", "tenacious")], axis = 0)
        subset[ous_to_ty] = [string[:-3] + "ty" for string in subset[ous_to_ty]]
        # e.g. "rebellious" -> "rebellion"
        ous_to_on = np.any([subset.endswith(string) for string in ("rebellious", "gious", "tious")], axis = 0) & ~np.any([subset.endswith(string) for string in ("facetious", "litigious", "prodigious")], axis = 0)
        subset[ous_to_on] = [string[:-2] + "n" for string in subset[ous_to_on]]
        # e.g. "decorous" -> "decorum"
        ous_to_um = np.any([subset.endswith(string) for string in ("decorous", "delirious", "tedious", "vacuous")], axis = 0)
        subset[ous_to_um] = [string[:-3] + "um" for string in subset[ous_to_um]]
        # e.g. "envious" -> "envy"
        ious_to_y = np.any([subset.endswith(string) for string in ("efficacious", "envious", "fallacious", "furious", "glorious", "luxurious", "melodious", "onious", "prodigious", "various")], axis = 0)
        subset[ious_to_y] = [string[:-4] + "y" for string in subset[ious_to_y]]
        # e.g. "gracious" -> "grace"
        cious_to_ce = subset.endswith("cious")
        subset[cious_to_ce] = [string[:-4] + "e" for string in subset[cious_to_ce]]
        # e.g. "felonous" -> "felony"
        ous_to_y = np.any([subset.endswith(string) for string in ("adulterous", "felonous", "gamous", "lecherous", "usurous")], axis = 0)
        subset[ous_to_y] = [string[:-3] + "y" for string in subset[ous_to_y]]
        # e.g. "hazardous" -> "hazard"
        delete_ous = np.any([subset.endswith(string) for string in ("advantageous", "amorous", "circuitous", "courageous", "feverous", "hazardous", "joyous", "nymous", "ponderous", "solicitous", "sulfurous", "tuberous", "ulcerous", "valorous", "vaporous", "verminous", "viperous", "vomitous", "zealous")], axis = 0) #*** assume delete all but use ous keepers instead
        subset[delete_ous] = [string[:-3] for string in subset[delete_ous]]
        # general "ous" rule is to leave it
        words[subset_scope] = subset
  
    # handle "ful" suffix
    subset_scope = words.endswith("ful")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "beautiful" -> "beauty"
        iful_to_y = subset.endswith("iful")
        subset[iful_to_y] = [string[:-4] + "y" for string in subset[iful_to_y]]
        # general rule--remove suffix
        delete_ful = subset.endswith("ful") & np.array([not(bool(re.search("\\b(?:aw|grate)ful$", word))) for word in subset])
        subset[delete_ful] = [string[:-3] for string in subset[delete_ful]]
        words[subset_scope] = subset
  
    # handle "less" suffix
    subset_scope = words.endswith("less")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "penniless" -> "penny"
        iless_to_y = subset.endswith("iless")
        subset[iless_to_y] = [string[:-5] + "y" for string in subset[iless_to_y]]
        # general rule--remove suffix
        delete_less = subset.endswith("less") & np.array([not(bool(re.search("\\b(?:b|hap|(?:never|none)the|un)?less$", word))) for word in subset])
        subset[delete_less] = [string[:-4] for string in subset[delete_less]]
        words[subset_scope] = subset
  
    # handle "ar" suffix
    subset_scope = words.endswith("ar")
    subset = words[subset_scope]
    if len(subset) != 0:
        # handle "ular" suffix
    
        # e.g. "angular" -> "angle"
        ular_to_le = np.any([subset.endswith(string) for string in ("angular", "circular", "clavicular", "corpuscular", "cuticular", "follicular", "miracular", "ocular", "oracular", "singular", "spectacular", "tabular", "tabernacular", "tentacular", "vehicular", "ventricular")], axis = 0)
        subset[ular_to_le] = [string[:-4] + "le" for string in subset[ular_to_le]]
        # e.g. "cellular" -> "cell"
        delete_ular = np.any([subset.endswith(string) for string in ("glandular", "cellular")], axis = 0)
        subset[delete_ular] = [string[:-4] for string in subset[delete_ular]]
        # general "ular" rule--remove suffix
        ular_to_ule = subset.endswith("ular") & ~np.any([subset.endswith(string) for string in ("particular", "popular", "regular")], axis = 0)
        subset[ular_to_ule] = [string[:-2] + "e" for string in subset[ular_to_ule]]
    
        # handle "iar" suffix
    
        # e.g. "liar" -> "lie"
        iar_to_ie = np.array([bool(re.search("\\bliar$", word)) for word in subset])
        subset[iar_to_ie] = [string[:-2] + "e" for string in subset[iar_to_ie]]
        # e.g. "familiar" -> "family"
        iar_to_y = subset.endswith("familiar")
        subset[iar_to_y] = [string[:-3] + "y" for string in subset[iar_to_y]]
    
        # handle general "ar" suffix
    
        # e.g. "scholar" -> "school"
        delete_ar_school = subset.endswith("scholar")
        subset[delete_ar_school] = [string[:-3] + "ol" for string in subset[delete_ar_school]]
        # general "ar" rule is to leave it
        words[subset_scope] = subset
  
    # e.g. "congruence" -> "congruent"
    ence_to_ent = words.endswith("ence") & ~(np.any([words.endswith(string) for string in ("audience", "essence", "experience", "influence", "license", "sentence")], axis = 0) | np.array([bool(re.search("\\b(?:[fhp]|sci|sp|th|wh)ence$", word)) for word in words])) # np.any([words.endswith(string) for string in ("abhorrence", "absence", "accidence", "congruence", "diligence", "evidence", "immanence", "indolence", "inherence", "insistence", "nascence", "opulence", "patience", "permanence", "potence", "presence", "prudence", "quence", "residence", "reticence", "reverence", "salience", "tangence", "transcience", "valence", "violence")], axis = 0)
    words[ence_to_ent] = [string[:-2] + "t" for string in words[ence_to_ent]]
    # e.g. "abundance" -> "abundant"
    ance_to_ant = np.any([words.endswith(string) for string in ("abundance", "clairvoyance", "distance", "ificance", "malignance", "norance", "performance", "pursuance", "resistance")], axis = 0)
    words[ance_to_ant] = [string[:-2] + "t" for string in words[ance_to_ant]]
  
    # handle "ant" suffix
    subset_scope = words.endswith("ant")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "significant" -> "signify"
        ificant_to_y = subset.endswith("ificant")
        subset[ificant_to_y] = [string[:-5] + "y" for string in subset[ificant_to_y]]
        # e.g. "reductant" -> "reduce"
        ctant_to_ce = subset.endswith("reductant")
        subset[ctant_to_ce] = [string[:-4] + "e" for string in subset[ctant_to_ce]]
        # e.g. "oxidant" -> "oxide"
        ant_to_e = np.any([subset.endswith(string) for string in ("ignorant", "oxidant", "piquant", "pleasant", "pursuant")], axis = 0)
        subset[ant_to_e] = [string[:-3] + "e" for string in subset[ant_to_e]]
        # e.g. "reactant" -> "react"
        delete_ant = np.any([subset.endswith(string) for string in ("colorant", "formant", "infestant", "inhabitant", "malignant", "reactant", "relaxant", "resistant", "toxicant")], axis = 0)
        subset[delete_ant] = [string[:-3] for string in subset[delete_ant]]
        # e.g. "participant" -> "participate"
        #*** or make this general rule?
        ant_to_ate = np.any([subset.endswith(string) for string in ("administrant", "participant", "supplicant")], axis = 0)
        subset[ant_to_ate] = [string[:-2] + "te" for string in subset[ant_to_ate]]
        # general "ant" rule is to leave it
        words[subset_scope] = subset
  
    # handle "ent" suffix
    subset_scope = words.endswith("ent")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "student" -> "study"
        ent_to_y = subset.endswith("student")
        subset[ent_to_y] = [string[:-3] + "y" for string in subset[ent_to_y]]
        # e.g. "emergent" -> "emerge"
        ent_to_e = subset.endswith("ergent")
        subset[ent_to_e] = [string[:-2] for string in subset[ent_to_e]]
        # e.g. "credent" (from "credence") -> "credit"
        ent_to_it = subset.endswith("credent")
        subset[ent_to_it] = [string[:-3] + "it" for string in subset[ent_to_it]]
        # e.g. "recurrent" -> "recur"
        delete_ent_letter = np.any([subset.endswith(string) for string in ("deterrent", "incurrent", "occurrent", "recurrent")], axis = 0)
        subset[delete_ent_letter] = [string[:-4] for string in subset[delete_ent_letter]]
        # e.g. "different" -> "differ"
        delete_ent = np.any([subset.endswith(string) for string in ("different", "conferent", "existent", "insistent", "preferent", "referent")], axis = 0)
        subset[delete_ent] = [string[:-3] for string in subset[delete_ent]]
        # general "ent" rule is to leave it
        words[subset_scope] = subset
  
    # handle "ive" suffix
    subset_scope = words.endswith("ive")
    subset = words[subset_scope]
    if len(subset) != 0:
        # handle "ative" suffix
    
        # e.g. "affirmative" -> "affirm"
        delete_ative = np.any([subset.endswith(string) for string in ("ulcerative", "ntative", "rmative", "rtative")], axis = 0)
        subset[delete_ative] = [string[:-5] for string in subset[delete_ative]]
        # e.g. "comparative" -> "compare"
        ative_to_e = np.any([subset.endswith(string) for string in ("comparative", "curative")], axis = 0) 
        subset[ative_to_e] = [string[:-5] + "e" for string in subset[ative_to_e]]
        # e.g. "relative" -> "relate"
        ative_to_ate = subset.endswith("ative") & ~(subset.endswith("putative") | np.array([bool(re.search("\\b[dn]ative$", word)) for word in subset]))
        subset[ative_to_ate] = [string[:-3] + "e" for string in subset[ative_to_ate]]
    
        # handle "itive" suffix
    
        # e.g. "sensitive" -> "sensate" (-> "sense" later on)
        itive_to_ate = subset.endswith("sensitive")
        subset[itive_to_ate] = [string[:-5] + "ate" for string in subset[itive_to_ate]]
    
        # handle "ctive" suffix
    
        # e.g. "deductive" -> "deduce"
        ctive_to_ce = subset.endswith("ductive")
        subset[ctive_to_ce] = [string[:-4] + "e" for string in subset[ctive_to_ce]]
        # general "ctive" rule--remove suffix (e.g. "detective" -> "detect")
        delete_ive = subset.endswith("ctive") & ~np.any([subset.endswith(string) for string in ("adjective", "objective", "subjective")], axis = 0)
        subset[delete_ive] = [string[:-3] for string in subset[delete_ive]]
    
        # handle "ptive" suffix
    
        # e.g. "captive" -> "capture"
        ptive_to_pture = subset.endswith("captive")
        subset[ptive_to_pture] = [string[:-3] + "ure" for string in subset[ptive_to_pture]]
        # e.g. "presumptive" -> "presume"
        mptive_to_me = subset.endswith("mptive")
        subset[mptive_to_me] = [string[:-5] + "e" for string in subset[mptive_to_me]]
        # e.g. "absorptive" -> "absorb"
        rptive_to_b = subset.endswith("rptive")
        subset[rptive_to_b] = [string[:-5] + "b" for string in subset[rptive_to_b]]
        # e.g. "prescriptive" -> "prescribe"
        ptive_to_be = subset.endswith("scriptive")
        subset[ptive_to_be] = [string[:-5] + "be" for string in subset[ptive_to_be]]
        # e.g. "adaptive" -> "adapt"
        ptive_to_pt = np.any([subset.endswith(string) for string in ("acceptive", "adaptive", "adoptive", "ruptive")], axis = 0)
        subset[ptive_to_pt] = [string[:-3] for string in subset[ptive_to_pt]]
        # e.g. "interruptive" -> "interrupt"
        delete_ptive = subset.endswith("interruptive")
        subset[delete_ptive] = [string[:-5] for string in subset[delete_ptive]]
        # general "ptive" rule--remove suffix (e.g. "receptive" -> "receive")
        ptive_to_ive = subset.endswith("ptive")
        subset[ptive_to_ive] = [string[:-5] + "ive" for string in subset[ptive_to_ive]]
    
        # handle general "ive" suffix
    
        # e.g. "iterative" -> "iterate"
        ive_to_e = np.any([subset.endswith(string) for string in ("decorative", "defensive", "iterative", "locative", "offensive")], axis = 0)
        subset[ive_to_e] = [string[:-3] + "e" for string in subset[ive_to_e]]
        # e.g. "assertive" -> "assert"
        delete_ive = np.any([subset.endswith(string) for string in ("adoptive", "adventive", "appointive", "assertive", "attractive", "detective", "ejective", "erective", "eruptive", "excessive", "exeptive", "exertive", "preventive", "reactive", "reflective", "selective", "transitive", "vomitive")], axis = 0)
        subset[delete_ive] = [string[:-3] for string in subset[delete_ive]]
        # general "ive" rule is to leave it
        words[subset_scope] = subset
  
    # e.g. "celebratory" -> "celebrate"
    atory_to_ate = words.endswith("atory") & ~words.endswith("oratory")
    words[atory_to_ate] = [string[:-3] + "e" for string in words[atory_to_ate]]
  
    # e.g. "messenger" -> "message"
    enger_to_age = np.any([words.endswith(string) for string in ("messenger", "passenger")], axis = 0)
    words[enger_to_age] = [string[:-5] + "age" for string in words[enger_to_age]]
  
    # handle "age" suffix
    subset_scope = words.endswith("age")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "verbiage" -> "verb"
        delete_iage = subset.endswith("verbiage")
        subset[delete_iage] = [string[:-4] for string in subset[delete_iage]]
        # e.g. "marriage" -> "marry"
        iage_to_y = subset.endswith("rriage")
        subset[iage_to_y] = [string[:-4] + "y" for string in subset[iage_to_y]]
        # e.g. "assemblage" -> "assembly"
        age_to_y = subset.endswith("blage")
        subset[age_to_y] = [string[:-3] + "y" for string in subset[age_to_y]]
        # e.g. "dosage" -> "dose"
        age_to_e = np.any([subset.endswith(string) for string in ("chaperonage", "cleavage", "dosage", "pipage", "storage", "usage")], axis = 0)
        subset[age_to_e] = [string[:-3] + "e" for string in subset[age_to_e]]
        # remove suffix if example in list (e.g. "wattage" -> "watt")
        delete_age = np.any([subset.endswith(string) for string in English_age_removers], axis = 0)
        subset[delete_age] = [string[:-3] for string in subset[delete_age]]
        # general "age" rule is to leave it
        words[subset_scope] = subset
  
    # handle "tion" suffix
    subset_scope = words.endswith("tion")
    subset = words[subset_scope]
    if len(subset) != 0:
        # handle "ication" suffix
    
        # e.g. "amplification" -> "amplify"
        ication_to_y = subset.endswith("ification")
        subset[ication_to_y] = [string[:-7] + "y" for string in subset[ication_to_y]]
        # e.g. "publication" -> "publish"
        ication_to_ish = subset.endswith("publication")
        subset[ication_to_ish] = [string[:-6] + "sh" for string in subset[ication_to_ish]]
    
        # handle "faction" suffix
    
        # e.g. "satisfaction" -> "satisfy"
        faction_to_fy = subset.endswith("faction")
        subset[faction_to_fy] = [string[:-6] + "y" for string in subset[faction_to_fy]]
    
        # handle "ation" suffix
    
        # e.g. "pronunciation" -> "pronounce"
        nunciation_to_nounce = subset.endswith("nunciation")
        subset[nunciation_to_nounce] = [string[:-9] + "ounce" for string in subset[nunciation_to_nounce]]
        # e.g. "filtration" -> "filter"
        tration_to_ter = np.any([subset.endswith(string) for string in ("filtration", "istration")], axis = 0)
        subset[tration_to_ter] = [string[:-6] + "er" for string in subset[tration_to_ter]]
        # e.g. "cancellation" -> "cancel"
        delete_lation = subset.endswith("cancellation")
        subset[delete_lation] = [string[:-6] for string in subset[delete_lation]]
        # e.g. "invitation" -> "invite"
        ation_to_e = np.any([subset.endswith(string) for string in ("compilation", "invitation")], axis = 0)
        subset[ation_to_e] = [string[:-5] + "e" for string in subset[ation_to_e]]
        # e.g. "consideration" -> "consider"
        delete_ation = np.any([subset.endswith(string) for string in ("accreditation", "adaptation", "consideration", "distillation", "installation", "instillation", "ntation", "recommendation", "transformation")], axis = 0)
        subset[delete_ation] = [string[:-5] for string in subset[delete_ation]]
        # e.g. "colonization" -> "colonize"
        iszation_to_ize = np.any([subset.endswith(string) for string in ("isation", "ization")], axis = 0)
        subset[iszation_to_ize] = [string[:-6] + "ze" for string in subset[iszation_to_ize]]
        # e.g. "expectation" -> "expect"
        delete_ation = np.any([subset.endswith(string) for string in ("expectation", "formation", "foundation", "information", "transportation")], axis = 0)
        subset[delete_ation] = [string[:-5] for string in subset[delete_ation]]
        # e.g. "sanitation" -> "sanitary"
        ation_to_ary = subset.endswith("sanitation")
        subset[ation_to_ary] = [string[:-4] + "ry" for string in subset[ation_to_ary]]
        # e.g. "celebration" -> "celebrate" (general "ation" rule)
        ation_to_ate = subset.endswith("ation") & ~np.any([subset.endswith(string) for string in ("nation", "occupation", "ration", "station", "vocation")], axis = 0)
        subset[ation_to_ate] = [string[:-3] + "e" for string in subset[ation_to_ate]]
    
        # handle "ition" and "ution" suffixes
    
        # e.g. "practicioner" -> "practice"
        ition_to_ice = np.any([subset.endswith(string) for string in ("practition", "practitioner")], axis = 0)
        subset[ition_to_ice] = [re.sub(string = string, pattern = "tion(er)?$", repl = "ce") for string in subset[ition_to_ice]]
        # e.g. "solution" -> "solve"
        ution_to_ve = subset.endswith("olution")
        subset[ution_to_ve] = [string[:-5] + "ve" for string in subset[ution_to_ve]]
    
        # handle "ption" suffix
    
        # e.g. "redemption" -> "redeem"
        mption_to_em = subset.endswith("redemption")
        subset[mption_to_em] = [string[:-6] + "em" for string in subset[mption_to_em]]
        # e.g. "consumption" -> "consume"
        mption_to_me = subset.endswith("mption") & ~subset.endswith("exemption")
        subset[mption_to_me] = [string[:-6] + "me" for string in subset[mption_to_me]]
        # e.g. "conception" -> "conceive"
        eption_to_eive = subset.endswith("eption") & ~subset.endswith("exception")
        subset[eption_to_eive] = [string[:-5] + "ive" for string in subset[eption_to_eive]]
        # e.g. "transcription" -> "transcribe"
        iption_to_ibe = subset.endswith("iption")
        subset[iption_to_ibe] = [string[:-5] + "be" for string in subset[iption_to_ibe]]
        # e.g. "absorption" -> "absorb"
        orption_to_orb = subset.endswith("orption")
        subset[orption_to_orb] = [string[:-5] + "b" for string in subset[orption_to_orb]]
    
        # handle "ction" suffix
    
        # e.g. "destruction" -> "destroy"
        uction_to_oy = subset.endswith("destruction")
        subset[uction_to_oy] = [string[:-6] + "oy" for string in subset[uction_to_oy]]
        # e.g. "introduction" -> "introduce"
        ction_to_ce = np.any([subset.endswith(string) for string in ("introduction", "reduction", "reproduction", "seduction")], axis = 0)
        subset[ction_to_ce] = [string[:-4] + "e" for string in subset[ction_to_ce]]
    
        # handle general "ion" suffix
        # e.g. "depiction" -> "depict"
        delete_ion = np.any([subset.endswith(string) for string in ("ction", "ption")], axis = 0) & ~np.any([subset.endswith(string) for string in ("caption", "duration", "auction", "diction", "fiction", "fraction", "function", "junction", "sanction", "surrection")], axis = 0)
        subset[delete_ion] = [string[:-3] for string in subset[delete_ion]]
        # general "ion" rule is to leave it
        words[subset_scope] = subset
  
    # e.g. "compression" -> "compress"
    delete_ion = words.endswith("ession") & ~np.any([words.endswith(string) for string in ("cession", "session")], axis = 0)
    words[delete_ion] = [string[:-3] for string in words[delete_ion]]
  
    # handle "ery" suffix
    subset_scope = words.endswith("ery")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "thuggery" -> "thug"
        delete_ery_letter = np.any([subset.endswith(string) for string in ("blubbery", "buggery", "cutlery", "doggery", "gunnery", "jewellery", "nunnery", "piggery", "pottery", "robbery", "shrubbery", "snobbery", "thuggery")], axis = 0)
        subset[delete_ery_letter] = [string[:-4] for string in subset[delete_ery_letter]]
        # e.g. "brewery" -> "brew"
        delete_ery = np.any([subset.endswith(string) for string in ("bitchery", "brewery", "butchery", "cookery", "creamery", "crockery", "crookery", "deanery", "demagoguery", "distillery", "eatery", "fishery", "foolery", "fuckery", "greenery", "joinery", "mockery", "monkery", "printery", "quackery", "rookery", "smithery", "trickery")], axis = 0)
        subset[delete_ery] = [string[:-3] for string in subset[delete_ery]]
        # e.g. "bribery" -> "bribe"
        delete_ry = np.any([subset.endswith(string) for string in ("bribery", "bakery", "bravery", "cyclery", "drapery", "fakery", "finery", "forgery", "grotesquery", "imagery", "machinery", "missilery", "mopery", "nursery", "pedlery", "perfumery", "refinery", "rocketry", "roguery", "savagery", "scenery", "slavery", "winery")], axis = 0)
        subset[delete_ry] = [string[:-2] for string in subset[delete_ry]]
        # e.g. "watery" -> "water"
        delete_y = np.any([subset.endswith(string) for string in ("beery", "butlery", "buttery", "cheery", "delivery", "discovery", "flowery", "grocery", "jittery", "leathery", "leery", "mastery", "mothery", "papery", "quivery", "recovery", "rubbery", "silvery", "sneery", "spidery", "watery", "wintery")], axis = 0)
        subset[delete_y] = [string[:-1] for string in subset[delete_y]]
        words[subset_scope] = subset
  
    # handle "y" suffix
    subset_scope = words.endswith("y")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "(in)finity" -> "finite"
        y_to_e = np.any([subset.endswith(string) for string in ("finity", "injury")], axis = 0) & ~subset.endswith("affinity")
        subset[y_to_e] = [string[:-1] + "e" for string in subset[y_to_e]]
        # e.g. "advisory" -> "advisor"
        delete_y = np.any([subset.endswith(string) for string in ("archy", "complicity", "visory")], axis = 0)
        subset[delete_y] = [string[:-1] for string in subset[delete_y]]
        words[subset_scope] = subset
  
    # handle "it" suffix
    subset_scope = words.endswith("it")
    subset = words[subset_scope]
    if len(subset) != 0:
        # "(in)evit(able)" -> "evade"
        evit_to_evade = np.array([bool(re.search("\\bevit$", word)) for word in subset])
        subset[evit_to_evade] = [string[:-2] + "ade" for string in subset[evit_to_evade]]
        # "implicit" -> "imply"
        mplicit_to_mply = subset.endswith("mplicit")
        subset[mplicit_to_mply] = [string[:-4] + "y" for string in subset[mplicit_to_mply]]
        words[subset_scope] = subset
  
    # handle "itude" suffix
    subset_scope = words.endswith("itude")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "amplitude" -> "amplify"
        itude_to_ify = np.any([subset.endswith(string) for string in ("amplitude", "certitude", "fortitude", "gratitude", "magnitude")], axis = 0)
        subset[itude_to_ify] = [string[:-4] + "fy" for string in subset[itude_to_ify]]
        # e.g. "similitude" -> "similar"
        itude_to_ar = subset.endswith("similitude")
        subset[itude_to_ar] = [string[:-5] + "ar" for string in subset[itude_to_ar]]
        # e.g. "servitude" -> "serve"
        itude_to_e = subset.endswith("servitude")
        subset[itude_to_e] = [string[:-5] + "e" for string in subset[itude_to_e]]
        # e.g. "plentitude" -> "plenty"
        itude_to_y = subset.endswith("plentitude")
        subset[itude_to_y] = [string[:-5] + "y" for string in subset[itude_to_y]]
        # e.g. "decrepitude" -> "decrepit"
        itude_to_it = np.any([subset.endswith(string) for string in ("decrepitude", "solicitude")], axis = 0)
        subset[itude_to_it] = [string[:-3] for string in subset[itude_to_it]]
        # e.g. "(in)finitude" -> "finite"
        itude_to_ite = subset.endswith("finitude")
        subset[itude_to_ite] = [string[:-3] + "e" for string in subset[itude_to_ite]]
        # e.g. "exactitude" -> "exact"
        delete_itude = np.any([subset.endswith(string) for string in ("aptitude", "correctitude", "crassitude", "eptitude", "exactitude", "vastitude")], axis = 0)
        subset[delete_itude] = [string[:-5] for string in subset[delete_itude]]
        words[subset_scope] = subset
  
    # handle "ysis" suffix
    subset_scope = words.endswith("ysis")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "lysis" -> "lyse"
        ysis_to_yse = np.array([bool(re.search("\\blysis$", word)) for word in subset])
        subset[ysis_to_yse] = [string[:-3] + "se" for string in subset[ysis_to_yse]]
        # e.g. "hydrolysis" -> "hydrolyze"
        ysis_to_yze = subset.endswith("ysis")
        subset[ysis_to_yze] = [string[:-3] + "ze" for string in subset[ysis_to_yze]]
        words[subset_scope] = subset
  
    ### handle comparative/doer ("er"), superlative ("est"), past tense ("ed"), and progressive tense ("ing") endings
  
    #* nested back-references don't work in R regex
  
    # handle "er" suffix
    subset_scope = words.endswith("er")
    subset = words[subset_scope]
    if len(subset) != 0:
        er_keeper_mask = np.array([not(bool(re.search(any_of(English_er_keepers) + "$", word))) for word in subset])
        # e.g. "controller" -> "control"
        delete_er_letter = ((np.any([subset.endswith(string) for string in flat_concat([English_doubled_consonants_er, "awfuller", "compeller", "controller", "traveller", "quizzer", "frolicker", "mimicker", "mosaicker", "panicker", "picnicker", "politicker", "trafficker", "laughter", "remainder"])], axis = 0) & np.array([not(bool(re.search("((([aiu]|\\b([fhjstwy]|bests|dw|kn|kv|qu|sh|sm|sp|sw)e|((\\b|en)r)o)ll)|(\\bodd))er$", word))) for word in subset])) | (subset.endswith("eer") & np.array([not(bool(re.search(any_of(English_eer_keepers) + "$", word))) for word in subset]) & ~np.any([subset.endswith(string) for string in ("decreer", "fleer", "freer", "seer")], axis = 0))) & er_keeper_mask
        subset[delete_er_letter] = [string[:-3] for string in subset[delete_er_letter]]
        # e.g. "carrier" -> "carry"
        ier_to_y = subset.endswith("ier") & np.array([not(bool(re.search(any_of(English_ier_keepers) + "$", word))) for word in subset]) & ~(np.any([subset.endswith(string) for string in ("taxier", "waterskier")], axis = 0) | np.array([bool(re.search("\\bskier$", word)) for word in subset]))
        subset[ier_to_y] = [string[:-3] + "y" for string in subset[ier_to_y]]
        er_keeper_mask = ~delete_er_letter & np.array([not(bool(re.search(any_of(English_er_keepers) + "$", word))) for word in subset])
        # e.g. "(over)seer" -> "see"
        delete_r = (np.array([bool(re.search((e_rules + "er$"), word)) for word in subset]) | np.any([subset.endswith(string) for string in ("decreer", "fleer", "freer", "seer")], axis = 0)) & er_keeper_mask
        subset[delete_r] = [string[:-1] for string in subset[delete_r]]
        # general rule--remove suffix (e.g. "talker" -> "talk")
        delete_er = subset.endswith("er") & er_keeper_mask
        subset[delete_er] = [string[:-2] for string in subset[delete_er]]
        words[subset_scope] = subset
  
    # handle "est" suffix
    subset_scope = words.endswith("est")
    subset = words[subset_scope]
    if len(subset) != 0:
        est_keeper_mask = np.array([not(bool(re.search(any_of(English_est_keepers) + "$", word))) for word in subset])
        # e.g. "biggest" -> "big"
        delete_est_letter = np.any([subset.endswith(string) for string in flat_concat([English_doubled_consonants_est, "awfullest"])], axis = 0) & np.array([not(bool(re.search("(?:[aiu]ll|\\bodd)est$", word))) for word in subset]) & est_keeper_mask
        subset[delete_est_letter] = [string[:-4] for string in subset[delete_est_letter]]
        # e.g. "earliest" -> "early"
        iest_to_y = subset.endswith("iest") & np.array([not(bool(re.search(any_of(English_iest_keepers) + "$", word))) for word in subset])
        subset[iest_to_y] = [string[:-4] + "y" for string in subset[iest_to_y]]
        est_keeper_mask = ~delete_est_letter & np.array([not(bool(re.search(any_of(English_est_keepers) + "$", word))) for word in subset])
        # e.g. "latest" -> "late"
        delete_st = np.array([bool(re.search((e_rules + "est$"), word)) for word in subset]) & est_keeper_mask
        subset[delete_st] = [string[:-2] for string in subset[delete_st]]
        # general rule--remove suffix (e.g. "smallest" -> "small")
        delete_est = subset.endswith("est") & est_keeper_mask
        subset[delete_est] = [string[:-3] for string in subset[delete_est]]
        words[subset_scope] = subset
  
    # handle "ed" suffix
    subset_scope = words.endswith("ed")
    subset = words[subset_scope]
    if len(subset) != 0:
        ed_keeper_mask = np.array([not(bool(re.search(any_of(English_ed_keepers) + "$", word))) for word in subset])
        # e.g. "centred" -> "center"
        tred_to_ter = subset.endswith("tred") & ~subset.endswith("hatred")
        subset[tred_to_ter] = [string[:-3] + "er" for string in subset[tred_to_ter]]
        # e.g. "travelled" -> "travel"
        delete_ed_letter = np.any([subset.endswith(string) for string in flat_concat([English_doubled_consonants_ed, "compelled", "controlled", "travelled", "quizzed", "frolicked", "mimicked", "mosaicked", "panicked", "picnicked", "politicked", "trafficked"])], axis = 0) & np.array([not(bool(re.search("(?:[aiu]|\\b(?:[fhjtwy]|dw|kn|kv|qu|sh|sm|sp|sw)e|(?:\\b|en)ro)lled$", word))) for word in subset]) & ed_keeper_mask
        subset[delete_ed_letter] = [string[:-3] for string in subset[delete_ed_letter]]
        # e.g. "tied" -> "tie"
        delete_d = np.array([bool(re.search("\\b" + any_of(["died", "lied", "tied", "hogtied"]) + "$", word)) for word in subset])
        subset[delete_d] = [string[:-1] for string in subset[delete_d]]
        # e.g. "carried" -> "carry"
        ied_to_y = subset.endswith("ied") & ~(np.any([subset.endswith(string) for string in ("taxied", "waterskied")], axis = 0) | np.array([bool(re.search("\\bskied$", word)) for word in subset]))
        subset[ied_to_y] = [string[:-3] + "y" for string in subset[ied_to_y]]
        ed_keeper_mask = ~delete_ed_letter & np.array([not(bool(re.search(any_of(English_ed_keepers) + "$", word))) for word in subset])
        # e.g. "wasted" -> "waste"
        delete_d = np.array([bool(re.search((e_rules + "ed$"), word)) for word in subset]) & ~np.any([subset.endswith(string) for string in ("synced", "focused")], axis = 0) & ed_keeper_mask
        subset[delete_d] = [string[:-1] for string in subset[delete_d]]
        # general rule--remove suffix (e.g. "walked" -> "walk")
        delete_ed = subset.endswith("ed") & ~subset.endswith("eed") & ed_keeper_mask
        subset[delete_ed] = [string[:-2] for string in subset[delete_ed]]
        words[subset_scope] = subset
  
    # handle "ing" suffix
    subset_scope = words.endswith("ing")
    subset = words[subset_scope]
    if len(subset) != 0:
        ing_keeper_mask = np.array([not(bool(re.search(any_of(English_ing_keepers) + "$", word))) for word in subset])
        # e.g. "centring" -> "center"
        tring_to_ter = subset.endswith("tring") & (~subset.endswith("string") | subset.endswith("lustring"))
        subset[tring_to_ter] = [string[:-4] + "er" for string in subset[tring_to_ter]]
        # e.g. "trafficking" -> "traffic"
        delete_ing_letter = np.any([subset.endswith(string) for string in flat_concat([English_doubled_consonants_ing, "compelling", "controlling", "travelling", "quizzing", "frolicking", "mimick", "mosaicking", "panicking", "picnicking", "politicking", "trafficking"])], axis = 0) & np.array([not(bool(re.search("(?:[aiu]|\\b(?:[fhjstwy]|bests|dw|kn|kv|qu|sh|sm|sp|sw)e|(?:\\b|en)ro)lling$", word))) for word in subset]) & ing_keeper_mask
        subset[delete_ing_letter] = [string[:-4] for string in subset[delete_ing_letter]]
        ing_keeper_mask = ~delete_ing_letter & np.array([not(bool(re.search(any_of(English_ing_keepers) + "$", word))) for word in subset])
        # e.g. "waving" -> "wave"
        ing_to_e = np.array([bool(re.search((e_rules + "ing$"), word)) for word in subset]) & ~np.any([subset.endswith(string) for string in ("syncing", "focusing")], axis = 0) & ing_keeper_mask
        subset[ing_to_e] = [string[:-3] + "e" for string in subset[ing_to_e]]
        # general rule--remove suffix (e.g. "singing" -> "sing")
        delete_ing = subset.endswith("ing") & ing_keeper_mask
        subset[delete_ing] = [string[:-3] for string in subset[delete_ing]]
        words[subset_scope] = subset
  
    ### handle generic verb suffixes
  
    # handle "ify" suffix
    subset_scope = words.endswith("ify")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "exemplify" -> "example"
        exemplify_to_example = subset.endswith("exemplify")
        subset[exemplify_to_example] = [string[:-7] + "ample" for string in subset[exemplify_to_example]]
        # e.g. "certify" -> "certain"
        ify_to_ain = subset.endswith("certify")
        subset[ify_to_ain] = [string[:-3] + "ain" for string in subset[ify_to_ain]]
        # e.g. "simplify" -> "simple"
        plify_to_ple = subset.endswith("plify")
        subset[plify_to_ple] = [string[:-3] + "e" for string in subset[plify_to_ple]]
        # e.g. "terrify" -> "terror"
        ify_to_or = np.any([subset.endswith(string) for string in ("horrify", "terrify")], axis = 0)
        subset[ify_to_or] = [string[:-3] + "or" for string in subset[ify_to_or]]
        # e.g. "commodify" -> "commodity"
        ify_to_ity = np.any([subset.endswith(string) for string in ("deify", "commodify", "entify", "qualify", "unify", "verify")], axis = 0)
        subset[ify_to_ity] = [string[:-2] + "ty" for string in subset[ify_to_ity]]
        # e.g. "stratify" -> "stratum"
        ify_to_um = subset.endswith("stratify")
        subset[ify_to_um] = [string[:-3] + "um" for string in subset[ify_to_um]]
        # e.g. "calcify" -> "calcium"
        ify_to_ium = subset.endswith("calcify")
        subset[ify_to_ium] = [string[:-2] + "um" for string in subset[ify_to_ium]]
        # e.g. "syllabify" -> "syllabus"
        ify_to_us = subset.endswith("syllabify")
        subset[ify_to_us] = [string[:-2] + "us" for string in subset[ify_to_us]]
        # e.g. "glorify" -> "glory"
        ify_to_y = np.any([subset.endswith(string) for string in ("citify", "gentrify", "glorify", "glify", "llify", "ncify", "ndify", "prettify", "rsify")], axis = 0)
        subset[ify_to_y] = [string[:-3] + "y" for string in subset[ify_to_y]]
        # e.g. "notify" -> "note"
        ify_to_e = np.any([subset.endswith(string) for string in ("arify", "asify", "atify", "codify", "ilify", "ivify", "lsify", "notify", "nsify", "orify", "plify", "urify", "utify", "ypify")], axis = 0) & ~np.any([subset.endswith(string) for string in ("amplify", "gasify")], axis = 0)
        subset[ify_to_e] = [string[:-3] + "e" for string in subset[ify_to_e]]
        # general rule--remove suffix (e.g. "solidify" -> "solid")
        delete_ify = subset.endswith("ify") & ~(np.any([subset.endswith(string) for string in ("certify", "gnify", "modify", "mystify", "ratify", "specify", "testify")], axis = 0) | np.array([bool(re.search("\\bedify$", word)) for word in subset]))
        subset[delete_ify] = [string[:-3] for string in subset[delete_ify]]
        words[subset_scope] = subset
  
    ity_to_ite = np.array([bool(re.search("\\bunity$", word)) for word in words])
    words[ity_to_ite] = [string[:-2] + "te" for string in words[ity_to_ite]]
  
    # handle "en" suffix
    subset_scope = words.endswith("en")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "flatten" -> "flat"
        delete_en_letter = np.any([subset.endswith(string) for string in ("flatten", "gladden")], axis = 0)
        subset[delete_en_letter] = [string[:-3] for string in subset[delete_en_letter]]
        # e.g. "broaden" -> "broad"
        delete_en = np.any([subset.endswith(string) for string in ("blacken", "brighten", "broaden", "cheapen", "deepen", "freshen", "frighten", "harden", "harken", "hearten", "heighten", "lengthen", "lessen", "moisten", "roughen", "sharpen", "shorten", "slacken", "slicken", "smarten", "smoothen", "soften", "steepen", "stiffen", "sweeten", "thicken", "threaten", "tighten", "toughen", "weaken")], axis = 0)
        subset[delete_en] = [string[:-2] for string in subset[delete_en]]
        # e.g. "whiten" -> "white"
        delete_n = np.any([subset.endswith(string) for string in ("hasten", "whiten")], axis = 0)
        subset[delete_n] = [string[:-1] for string in subset[delete_n]]
        words[subset_scope] = subset
  
    # handle "ize" suffix
    subset_scope = words.endswith("ize")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "stabilize" -> "stable"
        bilize_to_ble = subset.endswith("bilize") & ~subset.endswith("mobilize")
        subset[bilize_to_ble] = [string[:-5] + "le" for string in subset[bilize_to_ble]]
        # e.g. "systematize" -> "system"
        delete_tize_letter = subset.endswith("systematize")
        subset[delete_tize_letter] = [string[:-5] for string in subset[delete_tize_letter]]
        # e.g. "traumatize" -> "trauma"
        delete_tize = subset.endswith("matize")
        subset[delete_tize] = [string[:-4] for string in subset[delete_tize]]
        # e.g. "emphasize" -> "emphasis"
        size_to_sis = np.any([subset.endswith(string) for string in ("emphasize", "hypothesize", "metastasize", "parenthesize", "synthesize")], axis = 0)
        subset[size_to_sis] = [string[:-2] + "s" for string in subset[size_to_sis]]
        # e.g. "categorize" -> "category"
        ize_to_y = np.any([subset.endswith(string) for string in ("anatomize", "apologize", "categorize", "chronize", "colonize", "ectomize", "eulogize", "fantasize", "otomize", "prioritize", "summarize")], axis = 0)
        subset[ize_to_y] = [string[:-3] + "y" for string in subset[ize_to_y]]
        # e.g. "iodize" -> "iodine"
        ize_to_ine = subset.endswith("iodize")
        subset[ize_to_ine] = [string[:-2] + "ne" for string in subset[ize_to_ine]]
        # e.g. "sanitize" -> "sanitary"
        ize_to_ary = subset.endswith("sanitize")
        subset[ize_to_ary] = [string[:-3] + "ary" for string in subset[ize_to_ary]]
        # e.g. "metabolize" -> "metabolic"
        ize_to_ic = subset.endswith("metabolize")
        subset[ize_to_ic] = [string[:-2] + "c" for string in subset[ize_to_ic]]
        # e.g. "optimize" -> "optimum"
        ize_to_um = np.any([subset.endswith(string) for string in ("maximize", "minimize", "optimize")], axis = 0)
        subset[ize_to_um] = [string[:-3] + "um" for string in subset[ize_to_um]]
        # e.g. "crystallize" -> "crystal"
        delete_ize_letter = np.any([subset.endswith(string) for string in ("crystallize", "tranquillize")], axis = 0)
        subset[delete_ize_letter] = [string[:-4] for string in subset[delete_ize_letter]]
        # e.g. "cyclize" -> "cycle"
        ize_to_e = (np.array([bool(re.search((e_rules + "ize$"), word)) for word in subset]) | subset.endswith("mobilize")) & (~subset.endswith("tomize") | subset.endswith("epitomize")) & ~np.any([subset.endswith(string) for string in English_ize_keepers], axis = 0)
        subset[ize_to_e] = [string[:-3] + "e" for string in subset[ize_to_e]]
        # e.g. general rule--remove suffix (e.g. "randomize" -> "random")
        delete_ize = subset.endswith("ize") & ~np.any([subset.endswith(string) for string in English_ize_keepers], axis = 0)
        subset[delete_ize] = [string[:-3] for string in subset[delete_ize]]
        words[subset_scope] = subset
  
    # e.g. "aviator" -> "aviate"
    ator_to_ate = words.endswith("ator") & ~np.any([words.endswith(string) for string in ("alligator", "equator")], axis = 0)
    words[ator_to_ate] = [string[:-2] + "e" for string in words[ator_to_ate]]
  
    # handle "ate" suffix
    subset_scope = words.endswith("ate")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "activate" -> "act"
        delete_ivate = subset.endswith("activate")
        subset[delete_ivate] = [string[:-5] for string in subset[delete_ivate]]
        # e.g. "captivate" -> "capture"
        ivate_to_ure = np.any([subset.endswith(string) for string in ("activate", "cultivate")], axis = 0)
        subset[ivate_to_ure] = [string[:-5] + "ure" for string in subset[ivate_to_ure]]
        # e.g. "administrate" -> "administer"
        strate_to_ster = np.any([subset.endswith(string) for string in ("administrate", "sequestrate")], axis = 0)
        subset[strate_to_ster] = [string[:-4] + "er" for string in subset[strate_to_ster]]
        # e.g. "paginate" -> "page"
        inate_to_e = subset.endswith("paginate")
        subset[inate_to_e] = [string[:-5] + "e" for string in subset[inate_to_e]]
        # e.g. "(in)furiate" -> "fury"
        iate_to_y = np.any([subset.endswith(string) for string in ("furiate", "variate")], axis = 0)
        subset[iate_to_y] = [string[:-4] + "y" for string in subset[iate_to_y]]
        # e.g. "quantitate" -> "quantity"
        tate_to_ty = np.any([subset.endswith(string) for string in ("qualitate", "quantitate")], axis = 0)
        subset[tate_to_ty] = [string[:-3] + "y" for string in subset[tate_to_ty]]
        # e.g. "differentiate" -> "different"
        delete_iate = np.any([subset.endswith(string) for string in ("differentiate", "instantiate", "potentiate", "substantiate")], axis = 0)
        subset[delete_iate] = [string[:-4] for string in subset[delete_iate]]
        # e.g. "circumstantiate" -> "circumstance"
        tiate_to_ce = np.any([subset.endswith(string) for string in ("circumstantiate", "licentiate")], axis = 0)
        subset[tiate_to_ce] = [string[:-5] + "ce" for string in subset[tiate_to_ce]]
        # e.g. "reconciliate" -> "reconcile"
        liate_to_le = subset.endswith("nciliate")
        subset[liate_to_le] = [string[:-4] + "e" for string in subset[liate_to_le]]
        # e.g. "(tri)angulate" -> "triangle"
        ulate_to_le = np.any([subset.endswith(string) for string in ("articulate", "angulate", "circulate")], axis = 0)
        subset[ulate_to_le] = [string[:-5] + "le" for string in subset[ulate_to_le]]
        # e.g. "(in)flammate" -> "flame"
        mmate_to_me = subset.endswith("flammate")
        subset[mmate_to_me] = [string[:-4] + "e" for string in subset[mmate_to_me]]
        # e.g. "applicate" -> "apply"
        icate_to_y = np.any([subset.endswith(string) for string in ("applicate", "duplicate", "multiplicate", "quadruplicate", "triplicate")], axis = 0)
        subset[icate_to_y] = [string[:-5] + "y" for string in subset[icate_to_y]]
        # e.g. "authenticate" -> "authentic"
        delete_ate = np.any([subset.endswith(string) for string in ("archate", "assassinate", "authenticate", "exploitate", "fractionate", "interpretate", "limitate", "passionate", "relaxate", "solicitate", "taxate", "vigorate", "ltate", "rmate")], axis = 0)
        subset[delete_ate] = [string[:-3] for string in subset[delete_ate]]
        # e.g. "stimulate" -> "stimulus"
        ate_to_us = subset.endswith("stimulate")
        subset[ate_to_us] = [string[:-3] + "us" for string in subset[ate_to_us]]
        # e.g. "salivate" -> "saliva"
        ate_to_a = subset.endswith("salivate")
        subset[ate_to_a] = [string[:-2] for string in subset[ate_to_a]]
        # e.g. "activate" -> "active"
        ate_to_e = np.any([subset.endswith(string) for string in ("activate", "brominate", "causate", "chlorinate", "citate", "combinate", "computate", "condensate", "continuate", "conversate", "degradate", "derivate", "destinate", "determinate", "divinate", "durate", "electorate", "examinate", "excitate", "explorate", "figurate", "fluorinate", "imaginate", "iodinate", "limitate", "notate", "oxidate", "preparate", "pristinate", "quotate", "reputate", "respirate", "restorate", "sensate", "vaccinate", "rvate")], axis = 0)
        subset[ate_to_e] = [string[:-3] + "e" for string in subset[ate_to_e]]
        words[subset_scope] = subset
  
    ### connect a few verb forms to their noun forms (and vice versa)
  
    # handle "duct" suffix
    subset_scope = words.endswith("duct")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "product" -> "produce"
        duct_to_duce = np.any([subset.endswith(string) for string in ("educt", "nduct", "oduct")], axis = 0)
        subset[duct_to_duce] = [string[:-1] + "e" for string in subset[duct_to_duce]]
        words[subset_scope] = subset
  
    # handle "y" suffix
    subset_scope = words.endswith("y")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "multiply" -> "multiple"
        y_to_e = np.any([subset.endswith(string) for string in ("dubly", "tiply", "tuply")], axis = 0)
        subset[y_to_e] = [string[:-1] + "e" for string in subset[y_to_e]]
        words[subset_scope] = subset
  
    # handle "ve" suffix
    subset_scope = words.endswith("ve")
    subset = words[subset_scope]
    if len(subset) != 0:
        # e.g. "live" -> "life"
        ive_to_ife = np.array([bool(re.search("\\blive$", word)) for word in subset])
        subset[ive_to_ife] = [string[:-2] + "fe" for string in subset[ive_to_ife]]
        # e.g. "grieve" -> "grief"
        eve_to_ef = np.any([subset.endswith(string) for string in ("grieve", "thieve")], axis = 0)
        subset[eve_to_ef] = [string[:-2] + "f" for string in subset[eve_to_ef]]
        words[subset_scope] = subset
  
    # returns a named vector (like a dictionary) 
    # so the original word can be used to lookup its simplified form
    return {original_words[i]:words[i] for i in xrange(len(words))}
