# Extractors scrape the content for the target fields and perform some light
# cleaning (trimming whitespaces, simple capitalisation)

get_html_classes <- function(node){
  (node %>%
     rvest::html_attr(name = 'class', default = '') %>%
     strsplit('\\s+'))[[1]]
}
get_html_circles <- function(node){
  o <- node %>%
    rvest::html_nodes('a div.circledetails div') %>%
    rvest::html_attr(name = 'class')
  if (length(o) == 0){
    o <- node %>%
      rvest::html_nodes('div.circledetails div') %>%
      rvest::html_attr(name = 'class')
  }
  return (o)
}

get_event_name <- function(body){
  body %>%
    rvest::html_node('div.cardtitle h2') %>%
    rvest::html_text(trim = TRUE)
}
get_event_url <- function(body){
  body %>%
    rvest::html_node(xpath = '//div[@class="postcardleft"]/ul/li/a/@href') %>%
    rvest::html_text(trim = TRUE)
}
get_event_country <- function(body){
  body %>%
    rvest::html_node(xpath = '//div[@class="postcardleft"]/ul/li[./span/text() = "Country:"]/text()') %>%
    rvest::html_text(trim = TRUE)
}
get_event_location <- function(body){
  body %>%
    rvest::html_node(xpath = '//div[@class="postcardleft"]/ul/li[./span/text() = "Town:"]/text()') %>%
    rvest::html_text(trim = TRUE)
}
get_event_lat_lon <- function(location, country='', continent=''){
  call_geonames(location, country, continent)
}
get_event_start_end_dates <- function(body){
  body %>%
    rvest::html_node(xpath = '//div[@class="postcardleft"]/ul/li[./span/text() = "When?"]/text()') %>%
    rvest::html_text(trim = TRUE) %>%
    stringr::str_split('-', simplify = TRUE) %>%
    trimws(which = 'both') %>%
    lubridate::parse_date_time(orders = 'dmy')
}
get_event_styles <- function(body){
  (body %>% 
     rvest::html_node(xpath = '//div[@class="postcardleft"]/ul/li[./span/text() = "Styles:"]/text()') %>%
     rvest::html_text(trim = TRUE) %>%
     stringr::str_split(', ', simplify = TRUE))[1,]
}
get_event_description <- function(body){
  body %>%
    rvest::html_node(xpath = '//div[@class="postcardleft"]/div[@class="scroll-pane2"]/p/text()') %>%
    rvest::html_text(trim = FALSE)
}
get_event_teachers <- function(body){
  body %>%
    rvest::html_node(xpath = '//div[contains(@class, "postcardright")]/p/following-sibling::div/p/text()') %>%
    rvest::html_text(trim = FALSE) %>%
    extract_clean_sets()
}

get_event_newness <- function(node){
  any(get_html_classes(node) %in% c('munf12'))
}
get_event_competitions <- function(node){
  any(get_html_circles(node) %in% c('circcomp'))
}
get_event_teachers_status <- function(node){
  any(get_html_circles(node) %in% c('circconf'))
}
get_event_continent <- function(node){
  x <- get_html_classes(node)
  x <- x[x %in% c('europe', 'north-america', 'south-america', 'africa', 'asia', 'australasia')]
  if (length(x) == 0){
    return (NA_character_)
  }
  switch(x[1],
         'europe' = 'Europe',
         'africa' = 'Africa',
         'north-america' = 'North America',
         'south-america' = 'South America',
         'asia' = 'Asia',
         'australasia' = 'Australasia',
         NA
  )
}

setGeneric("get_event_format", function(x, ...) standardGeneric("get_event_format"))
setMethod("get_event_format", "xml_node", function(x){
  cl <- get_html_classes(x)
  cl <- cl[cl %in% c('exchange', 'weekender', 'dance-camp')]
  if (length(cl) == 0){
    return (NA_character_)
  }
  switch(cl[1],
         'circexc' = 'Exchange',
         'exchange' = 'Exchange',
         'weekender' = 'Weekender',
         'dance-camp' = 'Dance Camp',
         NA_character_
  )
})
setMethod("get_event_format", "character", function(x, has_teachers){
  cl <- x %>%
    stringr::str_to_lower() %>%
    stringr::str_extract(
      '(exchange|weekender|weekend|week-end|dance-camp|-camp([0-9]+)?$|^camp-)') %>%
    stringr::str_replace(
      '(dance-camp|-camp([0-9]+)?$|^camp-)',
      replacement = 'dance-camp') %>%
    stringr::str_replace(
      '(weekender|weekend|week-end)',
      replacement = 'weekender')
  
  if (is.na(cl)){
    if (!has_teachers){
      return ('Exchange')
    } else {
      return ('Weekender')
    }
  }
  switch(cl,
         'circexc' = 'Exchange',
         'exchange' = 'Exchange',
         'weekender' = 'Weekender',
         'dance-camp' = 'Dance Camp',
         NA_character_
  )
})
