# acid
Word digester: an esay, accurate lemmatizer that also handles words outside dictionaries

The R version has no external dependencies, though it runs slightly faster if you first load the "stringi" package. The R version also contains code for pluralizing words, which is not available in Python yet (though singularizing words, which is required for the lemmatizer/digester, is available through the Python version). 

The Python version requires numpy, re, and functools. 

Both can lemmatize 20,000 unique words in roughly 1-3 seconds on a typical system. Feel free to report errors or suggestions, especially as I have developed this only very recently and haven't yet exhausted the special cases. 

The code I wrote to transcribe the R code to Python is also available here. It only requires some ~10 manual changes at the end to produce the functioning Python code you see here. 

Documentation for both is located within the digest_words(words) function, and reproduced below:

Used for "stemming" or "lemmatizing" words for Natural Language Processing. 
Works by removing prefixes and suffixes in the appropriate order. 

It's more accurate than typical stemming approaches: 
  (fewer confabulated results and more correctly connected results, 
  because it tests for and handles many special cases). 

It's more user-friendly than typical lemmatizing approaches:
  (you don't need to worry about parts of speech, 
  and it automatically goes to the most basic form). 

Uses the companion digested_word_dictionary(words) function to create a dictionary
  of the unique input words (as the element name/key) 
  and their digested outputs (as the element value). 
Read the comments in digested_word_dictionary(words) for more information. 
It relies on rules when there are rules (so it often works on made-up words), 
  but the rest is hard-coded (and there are admittedly still plenty of 
  gaps in coverage for special cases). 
Uses the companion make_singular(words), is_plural(words), and singularize(words)
  functions for handling plural (especially foreign/Greek/Latin/unusual plural) forms. 
See the documentation of these functions for more information. 

Input:
a character vector of lower-case English words to "digest" 
into their core lemmas (most meaningful lexical components)

NAs and "" elements are acceptable and do not cause error or warning. 
Empty inputs are acceptable and do not cause error or warning. 
Words containing contractions are acceptable and handled properly. 
It also properly handles the last components of hyphenated words, 
  ignoring preceding compents (unless they're prefixes, in which case they're removed). 
Proper nouns are currently *NOT* masked or handled properly, 
  so don't expect them to be returned unchanged. 

Output:
a character vector of the "digested" words

NAs elements returned as NA; "" elements returned as "". 
Elements are returned in the same order (in a vector of the same length). 
Nouns are returned in singular (non-plural) form. 
Verbs are returned in infinitive form. 
All negations (non-/un-/in-/dis-/anti-) are dropped. 
Stopwords are returned unchanged--handle them on your own. 

input: digest_words("antidisestablishmentarianismesquely")
output:                    "establish"

input: digest_words("supercalifragilisticexpialidocious")
output:                  "califragilisticexpialidocious")

input: digest_words("shouldn't've")
output:             "shall"

input: digest_words("can't-believe-it's-not-butterific")
output:             "can't-believe-it's-not-butter"

input: digest_words("re-doing")
output:                "do"

Notes:
This could be used in the future for grammatical approaches, 
  as it breaks down words by part of speech-related suffixes. 
In future may separate contractions into component words. 
In future may handle co-, en-, inter-, intra-, semi- prefixes. 

