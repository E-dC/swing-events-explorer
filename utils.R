library('tidyverse')
library('rvest')

call.geonames <- function(location, country, continent) {
  
  do.request <- function(location, country, continent){
    code.continent <- switch(continent,
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

    query.url <- paste0('http://www.geonames.org/advanced-search.html?',
                        'q=',
                        location,
                        '+',
                        country,
                        '&featureClass=P&continentCode=',
                        code.continent)
    
    req <- POST(url = query.url)
    response <- read_html(req)
  
    return (response)
  }
  
  parse.response <- function(response){
    geo <- response %>% html_node(xpath = '//span[@class = "geo"]')
    lon <- geo %>% html_node('span.longitude') %>% html_text(trim = TRUE) %>% as.double()
    lat <- geo %>% html_node('span.latitude') %>% html_text(trim = TRUE) %>% as.double()
    
    return (list('longitude' = lon, 'latitude' = lat))
  }
    
  response <- do.request(location, country, continent)
  result <- parse.response(response)
  
  if (has_element(result, NA)){
    els <- strsplit(location, split = '(,| )')[[1]]
    # best_guess <- els[els %in% placenames[[1]]][1]
    response <- geonames_call(location = els, country = '', code.continent = '')
    result <- parse.response(response)
  }
  
  return (result)
}