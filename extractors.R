# Extractors scrape the content for the target fields and perform some light
# cleaning (trimming whitespaces, simple capitalisation)

parse_dance_event <- function(homepage_node, body){
  stopifnot(class(homepage_node) %in% c('xml_nodeset', 'xml_node'))
  stopifnot(class(body) %in% c('xml_node', 'xml_document', 'xml_nodeset'))
  
  tryCatch({
    slots <- list()
    slots$name <- get_event_name(body)
    slots$url <- get_event_url(body)
    slots$country <- get_event_country(body)
    slots$location <- get_event_location(body)
    slots$description <- get_event_description(body)
    slots$styles <- get_event_styles(body)
    slots$teachers <- get_event_teachers(body)
    slots$teachers_confirmed <- get_event_teachers_status(homepage_node)
    slots$competitions_held <- get_event_competitions(homepage_node)
    slots$is_new <- get_event_newness(homepage_node)
    slots$continent <- get_event_continent(homepage_node)
    slots$format <- get_event_format(homepage_node)
    
    slots$styles <- get_event_styles(body)
    
    lat_lon <- get_event_lat_lon(slots$location, slots$country, slots$continent)
    start_end_date <- get_event_start_end_dates(body)
    
    slots$latitude <- lat_lon$latitude
    slots$longitude <- lat_lon$longitude
    slots$start_date <- start_end_date[1]
    slots$end_date <- start_end_date[2]
    return (slots)
  },
  error = function (e){print(e); return (NA)})
}


get_html_classes <- function(node){
  (node %>%
     rvest::html_attr(name = 'class', default = '') %>%
     strsplit('\\s+'))[[1]]
}
get_html_circles <- function(node){
  node %>%
    rvest::html_nodes('a div.circledetails div') %>%
    rvest::html_attr(name = 'class')
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
    return (NA)
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
get_event_format <- function(node){
  x <- get_html_classes(node)
  x <- x[x %in% c('exchange', 'weekender', 'dance-camp')]
  if (length(x) == 0){
    return (NA)
  }
  switch(x[1],
         'circexc' = 'Exchange',
         'exchange' = 'Exchange',
         'weekender' = 'Weekender',
         'dance-camp' = 'Dance Camp',
         NA
  )
}