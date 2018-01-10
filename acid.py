#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sat Jan  6 22:31:57 2018

@author: scottdobbins
"""

import re
import numpy as np

other_foreign_is_plurals = [string for string in [string + "s" for string in flat_concat([Japanese_words_in_English, Maori_words_in_English])] if string.endswith("is")]

English_able_double_consonants = ["b", "d", "g", "m", "n", "p", "r", "t"]
English_doubled_consonants_able = [string + "able" for string in paste0(English_able_double_consonants, English_able_double_consonants)]
English_ism_double_consonants = ["b", "d", "g", "l", "n", "p", "t", "z"]
English_doubled_consonants_ism = [string + "ism" for string in paste0(English_ism_double_consonants, English_ism_double_consonants)]
English_er_double_consonants = ["b", "d", "g", "m", "n", "p", "t"]
English_doubled_consonants_er = [string + "er" for string in paste0(English_er_double_consonants, English_er_double_consonants)]
English_est_double_consonants = ["b", "d", "g", "m", "n", "p", "t"]
English_doubled_consonants_est = [string + "est" for string in paste0(English_est_double_consonants, English_est_double_consonants)]
English_ed_double_consonants = ["b", "d", "g", "l", "m", "n", "p", "r", "t", "v", "z"]
English_doubled_consonants_ed = [string + "ed" for string in paste0(English_ed_double_consonants, English_ed_double_consonants)]
English_ing_double_consonants = ["b", "d", "g", "l", "m", "n", "p", "r", "t", "v", "z"]
English_doubled_consonants_ing = [string + "ing" for string in paste0(English_ing_double_consonants, English_ing_double_consonants)]

def non_empty_string(string):
    return type(string) is str and string != ""

def digest_words(words):
    if type(words) is list:
        words = [word if type(word) is str else "" for word in words]
    results = np.char.array(words)
    digest_dict = digested_word_dictionary(words)
    valid = results.nonzero()
    results[valid] = [digest_dict[word] for word in results[valid]]
    return results
    
def digested_word_dictionary(words):
    original_words = np.unique(np.char.array(filter(non_empty_string, words)))
    words = np.char.array(original_words)
    
    return {original_words[i]:words[i] for i in xrange(len(words))}

def is_singular(words):
    is_singular_with_s = (words.find(English_s_singulars_string) > 0) | \
        ((words.find("[^e]iu?s$") > 0) & ~words.endswith(tuple(all_is_plurals))) | \
        (words.find(any_of(Latin_us_to_i_singulars) + "$") > 0) | \
        words.endswith(("corpus", "genus", "viscus")) | \
        (words.find(Latin_is_singulars_string) > 0) | \
        words.endswith("itis") | \
        words.endswith("ss") | \
        (words.endswith("us") & (words.find(any_of(English_us_plurals) + "$") < 0) & ~words.endswith("eaus"))
  
    is_plural_without_s = words.endswith("people") | \
        words.endswith(("brethren", "children")) | \
        (words.endswith("men") & (words.find("(\\b[ao]|abdo|acu|albu|bitu|fora|hy|lu|ra|regi|ru|se|speci|sta)men$") < 0)) | \
        words.endswith(("teeth", "feet", "geese")) | \
        (words.find("((\\b|book|head)l|(\\b|dor|field|shrew|tit)m)ice$") > 0) | \
        (words.find("\\boxen$") > 0) | \
        (words.find("\\bdice$") > 0) | \
        words.endswith(("kobzari", "oblasti")) | \
        words.endswith("eaux") | \
        words.endswith("ae") | \
        words.endswith("kniazhestva") | \
        words.endswith("celli") | \
        words.endswith(("cherubim", "kibbutz", "seraph")) | \
        words.endswith("matzot") | \
        words.endswith(("hedra", "mata", "mena", "ria")) | \
        words.endswith(("genera", "viscera", "corpora")) | \
        (words.find(any_of(Latin_us_to_i_plurals) + "$") > 0) | \
        (words.find(any_of(Latin_us_to_a_plurals) + "$") > 0)
  
    is_indeterminate = (words.find(any_of(English_invariant_words) + "$") > 0) | \
        (words.find(English_invariant_words_s_string) > 0) | \
        (words.find(any_of(Japanese_words_in_English) + "$") > 0) | \
        (words.find(any_of(Maori_words_in_English) + "$") > 0) | \
        words.endswith("nese")
        
    is_singular = is_indeterminate | \
        is_singular_with_s | \
        ~(words.endswith("s") | is_plural_without_s)
        
    return is_singular

def make_singular(words):
    can_be_made_singular = ~is_singular(words)
    words[can_be_made_singular] = singularize(words[can_be_made_singular])
    return words

#remove_last = np.any((remove_last, np.all((np.any((words.find(English_ie_singulars_string) > 0, words.find(English_oe_singulars_string) > 0, words.find("[aeiouy][^aeioux]es$") > 0, words.endswith("mmes"), words.find("(([bcdfglprstz][glr])|(l[csv])|(n[cgrs])|(r[cgsv])|(s[c])|(u)|(((\\b|back|belly|head|stomach|tooth)a|ca|mousta|pana|pista)ch)|(rr)|(tt)|(\\b(ba|ca|ge|ha|mo|pa|pi|ta|wa|cha|try|arti|bati|ripo|langou)st))es$") > 0), axis = 0), 
#                                               words.find(English_s_singulars_plurals_string) < 0, 
#                                               words.find(ending_with_word(any_of(paste0(Latin_us_to_i_singulars, "es")))) < 0, 
#                                               rule_not_found), axis = 0)), axis = 0)
