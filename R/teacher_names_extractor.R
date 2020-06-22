# ---------- Capturing patterns and handy function ----------
allowable_sets <- list(
  '1' = c('*', '*', '<AND>', '*', '*'), # Olga Marina <AND> Andreas Olson
  '2' = c('*', '<AND>', '*'), # Olga <AND> Andreas
  '3' = c('*', '<AND>', '*', '*'), # Bernard <AND> Anne-Helene Cavasa
  '4' = c('*', '*', '*', '<AND>', '*', '*'), # Solen Le Bihan <AND> Laurent Wallet
  '5' = c('*', '*', '<AND>', '*', '*', '*'), # Laurent Wallet <AND> Solen Le Bihan
  '6' = c('*', '*', '*', '<AND>', '*', '*', '*'), # Laurent V. Wallet <AND> Solen Le Bihan
  '7' = c('*', '*'), # Dexter Santos
  '8' = c('*', '*', '*'), # Jean Charles Joyss
  '9' = c('*'), # Olga
  'STOP' = c('<(DANCE|LEVEL|EXTRA|MORE|BRACKETS|ORIGIN|SEP)>')
)

make_pattern_from_allowable_sets <- function(allowable_sets){
  switcher <- function(s){
    switch(s,
           '*' = stringr::regex('^[^<>].*[^<>]$'),
           stringr::regex(s)
    )
  }
  lapply(allowable_sets, function(l) sapply(l, switcher, USE.NAMES = FALSE))
}

capturing_patterns <- make_pattern_from_allowable_sets(allowable_sets)

# ---------- Transformation patterns and function ----------
word_pattern <- stringr::regex('[\\p{Letter}Ã±.-]+', ignore_case = TRUE)

bracketed_pattern <- stringr::regex(
  '\\(.+?\\)',
  ignore_case = TRUE)

origin_pattern <- stringr::regex(
  'from [\\p{Letter}Ã±0-9/-]+',
  ignore_case = TRUE)

more_pattern <- stringr::regex(
  '(and|&)?( +)?(others?| more|bands?|teachers?|instructors?)( +).{0,10}( +)?(TBC|TBA|.*?(announced|come|coming|confirme?d?)?( +)?(very)?( +)?(soon)?)',
  ignore_case = TRUE
)

dance_pattern <- stringr::regex(
  '(lindy( +hop)?|(slow +)?balboa|(collegiate|st.? +louis)?( +)?shag|tap|solo( +jazz)?|blues|competitions?)( *)?(:|tracks?|lanes?)?',
  ignore_case = TRUE
)
level_pattern <- stringr::regex(
  '(beginners?|intermediates?|advanceds?|improvers?)',
  ignore_case = TRUE
)
extraneous_pattern <- stringr::regex(
  '(the +)?(classe?s?|hot engines? band|swing shouters?|hot sugar bands?|naomi (and|&) her handsome devils?|gordon webster|stefano ronchi|jelly rolls? sweet band|\\.\\.\\.|\\|\\||teachers?)',
  ignore_case = TRUE
) 
link_pattern <- stringr::regex(
  '(&| +and +| +et +| +y +)',
  ignore_case = TRUE
)
sep_pattern <- stringr::regex(
  '(,|;|\n| - )',
  ignore_case = TRUE
)


process_teacher_description <- function(s){
  o <- s %>%
    stringr::str_replace_all(bracketed_pattern, ' <BRACKETS> ') %>%
    stringr::str_replace_all(origin_pattern, ' <ORIGIN>') %>%
    stringr::str_replace_all(more_pattern, ' <MORE> ') %>%
    stringr::str_replace_all(dance_pattern, ' <DANCE> ') %>%
    stringr::str_replace_all(level_pattern, ' <LEVEL> ') %>%
    stringr::str_replace_all(extraneous_pattern, ' <EXTRA> ') %>%
    stringr::str_replace_all(link_pattern, ' <AND> ') %>%
    stringr::str_replace_all(sep_pattern, ' <SEP> ') %>%
    stringr::str_squish()
  return (o)
}

# ---------- Recognizing and splitting functions ----------
recognize_pattern <- function(pattern, tokens){
  sliced_tokens <- tokens[1:length(pattern)]
  if (NA %in% sliced_tokens){
    return (FALSE)
  }
  return (all(stringr::str_detect(sliced_tokens, pattern)))
}

extract_set <- function(tokens){
  stringr::str_c(tokens, collapse = ' ') %>%
    stringr::str_split(' <.*?> ')
}
  
find_sets <- function(tokens, pattern_vectors, tokenize = TRUE){
  if (tokenize){
    tokens <- stringr::str_split(tokens, ' ', simplify = TRUE) %>%
      as.vector()
  }
  
  found <- FALSE
  for (p in pattern_vectors){
    if(recognize_pattern(p, tokens)){
      x <- length(p)
      captured_tokens <- tokens[1:x]
      extracted <- extract_set(captured_tokens)
      next_tokens <- tokens[-(1:x)]
      found <- TRUE
      break()
    }
  }
  if (found){
    o <- c(extracted, find_sets(next_tokens, pattern_vectors, FALSE))
    return (o[! grepl('^<.*>$', o) & ! is.na(o)])
  } else {
    return (NA)
  }
}
