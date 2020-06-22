load(file = 'data/geo_structures.rds')
# ---------- Construct handy useful geo structures ----------
get_placenames <- function(data_file = 'data/geonames.jsonlines', pop_cutoff = 100){
  json_data <- jsonlite::stream_in(file(data_file), verbose = FALSE)
  tibble::tibble(json_data) %>%
    dplyr::select(ascii_name, population) %>%
    dplyr::mutate(population = as.numeric(population)) %>%
    dplyr::filter(population > pop_cutoff | population == 0) %>%
    dplyr::arrange(dplyr::desc(population)) %>%
    dplyr::select(ascii_name) %>%
    do.call(paste, .)
}

make_country2continent <- function(){
  
  response <- xml2::read_html('http://www.geonames.org/countries/')
  
  countries <- response %>%
    rvest::html_nodes("table#countries > tr > td > a[href*='/countries/']") %>%
    rvest::html_text()
  
  continents <- response %>%
    rvest::html_nodes("table#countries > tr > td:last-child") %>%
    rvest::html_text() %>%
    sapply(., continent_switcher, USE.NAMES = FALSE)
  return(tibble::tibble(country = countries, continent = continents))
}

make_structures <- function(data_file = 'data/geonames.jsonlines', pop_cutoff = 100){
  placenames <- get_placenames(data_file, pop_cutoff)
  country2continent <- make_country2continent()
  save(placenames, country2continent, file = 'data/geo_structures.rds')
}

# ---------- Fallback function and utilities ----------
location_extra_pattern <- stringr::regex(
  '([0-9]|\\bkm\\b|ski resort|near|from|close|close to|@.*?\\.com|the one and only|little village|village)',
  ignore_case = TRUE)
loc2repl <- c('[Tt]win [Cc]ities' = 'Minneapolis',
              '[Ee]astliegh' = 'Eastleigh',
              '[Ee]lba [Ii]sland' = 'Elba')

continent_switcher <- function(input){
  switch(input,
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
         'Australasia'   = 'OC',
         'australasia'   = 'OC',
         'Antarctica'    = 'AN',
         'antarctica'    = 'AN',
         'AF'            = 'Africa',
         'AN'            = 'Antarctica',
         'AS'            = 'Asia',
         'EU'            = 'Europe',
         'OC'            = 'Australasia',
         'NA'            = 'North America',
         'SA'            = 'South America',
         NULL)
}

get_closest_match <- function(x, placenames, method = 'lcs'){
  
  dist_wrapper <- function(s, placenames, method){
    (tibble::tibble(name = placenames,
                    dist = stringdist::stringdist(s, placenames, method = method)) %>%
      dplyr::arrange(dist) %>%
      dplyr::select(name) %>%
      do.call(paste, .))
  }
  
  x <- x %>%
    stringr::str_remove_all('\\(.*\\)') %>%
    stringr::str_remove_all(location_extra_pattern) %>%
    stringr::str_replace_all(loc2repl)

  sapply(x, dist_wrapper, placenames = placenames, method = method)[1]
}

# ---------- Geonames API functions and parsing ----------

build_query <- function(location, country = '', continent = ''){
  code_continent <- continent_switcher(continent)
  
  query_url <- paste0('http://www.geonames.org/advanced-search.html?',
                      'q=',
                      URLencode(location),
                      '+',
                      URLencode(country),
                      '&featureClass=P&continentCode=',
                      code_continent)
  
  return (query_url)
}

request_page <- function(query_url){
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

  resp_country <- response %>%
    rvest::html_node(xpath = '//td/a[contains(@href, "countries")]') %>%
    rvest::html_text()
  continent <- country2continent %>%
    dplyr::filter(country == resp_country) %>%
    select(continent) %>%
    as.character()
  
  return (list('longitude' = lon, 'latitude' = lat, 'country' = resp_country, 'continent' = continent))
}


# ---------- Geonames call wrapper ----------

#' @title Get latitude and longitude from GeoNames
#' @description \code{call_geonames()} is a wrapper to call the Geonames database
#'   and retrieve latitude and longitude for a given location
#' @param location A placename.
#' @param country A country name, useful for those towns appearing in multiple
#'   countries, Default: ''
#' @param continent A continent name, useful for the same reason as the 
#'   \code{country} parameter, Default: ''
#' @param bypass_first_attempts Logical. Whether to go straight to the fallback
#'   Default: FALSE
#' @return A named list \code{latitude, longitude)}, where each value is either
#'   a \code{double} or \code{NA}.
#' @details \code{continent} parameter is ignored if a first attempt with it
#'   fails.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  call_geonames('Paris', 'France')
#'  call_geonames('Istanbul', 'Turkey', 'Asia')# Still works despite geonames placing it in Europe
#'  }
#' }
#' @seealso 
#'  \code{\link[httr]{POST}}
#'  \code{\link[xml2]{read_xml}}
#'  \code{\link[rvest]{html_nodes}},\code{\link[rvest]{html_text}}
#' @rdname call_geonames
#' @export 
#' @import magrittr
#' @importFrom httr POST
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_text
call_geonames <- function(location, country='', continent='', bypass_first_attempts = FALSE) {
  
  try_again <- TRUE
  if (! bypass_first_attempts){
    Sys.sleep(1)
    result <- tryCatch(
      expr = {r <- build_query(location, country, continent) %>% 
                        request_page() %>%
                        parse_response()
              try_again <- FALSE
              r$location <- location
              r},
      error = function(e) {warning(e)})
  
    if (try_again){
      Sys.sleep(1)
      result <- tryCatch(
        expr = {r <- build_query(location, country, '') %>% 
                        request_page() %>%
                        parse_response()
                try_again <- FALSE
                r$location <- location
                r},
        error = function(e) {warning(e)})
    }
    if (try_again){
      Sys.sleep(1)
      result <- tryCatch(
        expr = {r <- build_query(location, '', '') %>% 
                        request_page() %>%
                        parse_response()
                try_again <- FALSE
                r$location <- location
                r},
        error = function(e) {warning(e)})
    }
  }
  if (try_again){
    Sys.sleep(1)
    result <- tryCatch(
      expr = {location <- get_closest_match(location, placenames)
              r <- build_query(location, '', '') %>% 
                      request_page() %>%
                      parse_response()
              try_again <- FALSE
              r$location <- location
              r},
      error = function(e) {warning(e)})
  }
  
  if (try_again){
    result <- list(latitude = NA, longitude = NA, location = NA, country = NA, continent = NA)
  }
  return (result)
}
