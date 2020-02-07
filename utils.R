library('tidyverse')
library('rvest')
library('jsonlite')


build_placenames_list <- function(data_file = 'data/geonames.jsonlines'){
  json_data <- stream_in(file(data_file))
  placenames <- list(subset(json_data, ascii_name != '')$ascii_name)

  return (placenames)
}
placenames <- build_placenames_list()

call_geonames <- function(location, country, continent) {
  
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
    )

    query_url <- paste0('http://www.geonames.org/advanced-search.html?',
                        'q=',
                        location,
                        '+',
                        country,
                        '&featureClass=P&continentCode=',
                        code_continent)
    
    req <- POST(url = query_url)
    response <- read_html(req)
  
    return (response)
  }
  
  parse_response <- function(response){
    geo <- response %>% html_node(xpath = '//span[@class = "geo"]')
    lon <- geo %>% html_node('span.longitude') %>% html_text(trim = TRUE) %>% as.double()
    lat <- geo %>% html_node('span.latitude') %>% html_text(trim = TRUE) %>% as.double()
    
    return (list('longitude' = lon, 'latitude' = lat))
  }
    
  response <- do_request(location, country, continent)
  result <- parse_response(response)
  
  if (has_element(result, NA)){
    els <- strsplit(location, split = '(,| )')[[1]]
    best_guess <- els[els %in% placenames[[1]]][1]
    response <- geonames_call(location = els, country = '', code_continent = '')
    result <- parse_response(response)
  }
  
  return (result)
}