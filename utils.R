# library('tidyverse')
# library('rvest')
# library('jsonlite')
# library('httr')
# library('xml2')

build_placenames_list <- function(data_file = 'data/geonames.jsonlines'){
  json_data <- jsonlite::stream_in(file(data_file))
  placenames <- list(subset(json_data, ascii_name != '')$ascii_name)

  return (placenames)
}
# placenames <- build_placenames_list()

call_geonames <- function(location, country=NULL, continent=NULL) {
  
  do_request <- function(location, country, continent){
    code_continent <- switch(continent,
      'europe'        = 'EU',
      'Europe'        = 'EU',
      'North America' = 'NA',
      'north-america' = 'NA',
      'South America' = 'SA',
      'south-america' = 'SA',
      'Africa'        = 'AF',
      'africa'        = 'AF',
      'Asia'          = 'AS',
      'asia'          = 'AS',
      'Australasia'  = 'OC',
      'australasia'  = 'OC',
      NULL
    )

    query_url <- paste0('http://www.geonames.org/advanced-search.html?',
                        'q=',
                        location,
                        '+',
                        country,
                        '&featureClass=P&continentCode=',
                        code_continent)
    
    req <- httr::POST(url = query_url)
    response <- xml2::read_html(req)
  
    return (response)
  }
  
  parse_response <- function(response){
    geo <- response %>%
      rvest::html_node(xpath = '//span[@class = "geo"]')
    lon <- geo %>%
      rvest::html_node('span.longitude') %>%
      rvest::html_text(trim = TRUE) %>%
      as.double()
    lat <- geo %>%
      rvest::html_node('span.latitude') %>%
      rvest::html_text(trim = TRUE) %>%
      as.double()
    
    return (list('longitude' = lon, 'latitude' = lat))
  }
    
  response <- do_request(location, country, continent)
  result <- parse_response(response)
  
  if (purrrr::has_element(result, NA)){
    els <- strsplit(location, split = '(,| )')[[1]]
    best_guess <- els[els %in% placenames[[1]]][1]
    response <- geonames_call(location = els, country = '', code_continent = '')
    result <- parse_response(response)
  }
  
  return (result)
}


word_pattern <- regex('[\\p{Letter}Ã±.-]+', ignore_case = TRUE)

bracketed_pattern <- regex(
  '\\([\\p{Letter}Ã± 0-9/-]+\\)', ignore_case = TRUE)
origin_pattern <- regex(
  'from [\\p{Letter}Ã±0-9/-]+', ignore_case = TRUE)

extract_clean_sets <- function(s){
  if (remove_cruft(s) == ''){
    return (NA)
  }
  clean_sets <- list()
  r1 <- split_on_set_separators(s)
  r2 <- setNames(vapply(r1, remove_cruft, FUN.VALUE = ''), NULL)
  r3 <- lapply(r2, split_on_partnership_linkers)
  for (v in r3){
    if (length(v) > 1){
      directions <- c('lhs', rep('rhs', length(v) - 1))
      rt <- setNames(mapply(extract_names, v, direction = directions), NULL)
      
      clean_sets <- append(clean_sets, relink_partnerships(rt))
    } else {
      rt <- setNames(extract_names(v), NULL)
      clean_sets <- append(clean_sets, relink_partnerships(rt, 'from_names'))
    }
  }
  return (clean_sets)
}


split_on_set_separators <- function(s){
  if (s == ''){
    return ('')
  }
  return (unlist(stringr::str_split(s, '\\s*(,|\n| - )\\s*')))
}

split_on_partnership_linkers <- function(s){
  if (s == ''){
    return ('')
  }
  return (unlist(stringr::str_split(s,  '\\s?(&| and )\\s?')))
}

remove_cruft <- function(s){
  if (s == ''){
    return ('')
  }
  s <- s %>%
    str_remove_all(pattern = bracketed_pattern) %>%
    str_remove_all(pattern = origin_pattern) %>%
    str_trim(side = 'both') %>%
    str_replace_all(pattern = '  +', replacement = ' ')
  return (s)
}


extract_names <- function(s, direction = 'lhs'){
  
  if (s == ''){
    return ('')
  }
  
  n_words <- s %>% str_count(pattern = word_pattern)
  if (n_words <= 3){
    return (s)
  }
  
  words <- s %>% str_extract_all(pattern = word_pattern, simplify = TRUE)
  if (n_words %% 2 == 0){
    splits <- setNames(split(words, ceiling(seq_along(words)/2)), NULL)
  } else {
    if (direction == 'lhs'){
      cutoff <- n_words - 3
      cutoff_2 <- cutoff + 1
      trunc_words <- words[1:cutoff]
      t1 <- setNames(split(trunc_words, ceiling(seq_along(trunc_words)/2)), NULL)
      t2 <- list(words[cutoff_2:n_words])
    } else if (direction == 'rhs'){
      trunc_words <- words[4:n_words]
      t1 <- list(words[1:3])
      t2 <- setNames(split(trunc_words, ceiling(seq_along(trunc_words)/2)), NULL)
    }
    splits <- append(t1, t2)
  }
  splits <- unlist(lapply(splits,
                          paste,
                          collapse = ' ')
                   )
  return (splits) 
}

relink_partnerships <- function(l, method = 'from_split'){
  
  sets <- list()
  skip_next <- FALSE
  if (method == 'from_split'){
    # Expects a list of vectors of strings
    for (v_idx in seq_along(l)){
      v <- l[[v_idx]]
      for (n_idx in seq_along(v)){
        if (! skip_next){
          if (n_idx == length(v) & v_idx < length(l)){
            t <- c(v[n_idx], l[[v_idx + 1]][1])
            sets[[length(sets) + 1]] <- t
            skip_next <- TRUE
          } else {
            sets[[length(sets) + 1]] <- v[n_idx]
            skip_next <- FALSE
          }
        } else { skip_next <- FALSE}
      }
    }
  } else if (method == 'from_names'){
    # Expects a list or vector of strings
    for (s_idx in seq_along(l)){
      s1 <- l[[s_idx]]
      if (! skip_next){
        t <- s1
        if (s_idx < length(l)){
          s2 <- l[[s_idx + 1]]
          words_s1 <- s1 %>% str_extract_all(pattern = word_pattern, simplify = TRUE)
          words_s2 <- s2 %>% str_extract_all(pattern = word_pattern, simplify = TRUE)
          if (words_s1[length(words_s1)] == words_s2[length(words_s2)]){
            t <- c(s1, s2)
            skip_next <- TRUE
          }
        }
        sets[[length(sets) + 1]] <- t
      } else { skip_next <- FALSE}
    }
  }
  return (sets)
}


# get_splits <- function(l, indices){
#   splits <- c()
#   for (idx in seq_along(indices)){
#     offset_fix <- 1
#     if (idx == length(indices)){
#       return (splits)
#     } else if (idx == length(indices) - 1){
#       offset_fix <- 0
#     }
#     beginning <- indices[idx]
#     end <- indices[idx+1] - offset_fix
#     splits <- c(splits, paste(l[beginning:end], collapse = ' '))
#   }
#   return (splits)
# }
