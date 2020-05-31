
setClass("Event",
         slots = c(
           event_code = 'character',
           name = 'character',
           url = 'character',
           event_format = 'factor',
           country = 'character',
           location = 'character',
           continent = 'character',
           description = 'character',
           styles = 'factor',
           teachers = 'list',
           teachers_confirmed = 'logical',
           competitions_held = 'logical',
           is_new_event = 'logical',
           latitude = 'numeric',
           longitude = 'numeric',
           start_date = 'Date',
           end_date = 'Date'
         ),
         prototype = list(
           event_code = NA_character_,
           name = NA_character_,
           url = NA_character_,
           event_format = factor(),
           country = NA_character_,
           location = NA_character_,
           continent = NA_character_,
           description = NA_character_,
           styles = factor(),
           teachers = list(),
           teachers_confirmed = NA,
           competitions_held = NA,
           is_new_event = NA,
           latitude = NA_real_,
           longitude = NA_real_,
           start_date = NA,
           end_date = NA
         )
)

# Event Accessors
setGeneric("event_code", function(x) standardGeneric("event_code"))
setMethod("event_code", "Event", function(x) x@event_code)
setMethod("event_code", "list", function(x) lapply(x, event_code))

setGeneric("name", function(x) standardGeneric("name"))
setMethod("name", "Event", function(x) x@name)
setMethod("name", "list", function(x) lapply(x, name))

setGeneric("url", function(x) standardGeneric("url"))
setMethod("url", "Event", function(x) x@url)
setMethod("url", "list", function(x) lapply(x, url))

setGeneric("event_format", function(x) standardGeneric("event_format"))
setMethod("event_format", "Event", function(x) x@event_format)
setMethod("event_format", "list", function(x) lapply(x, event_format))

setGeneric("country", function(x) standardGeneric("country"))
setMethod("country", "Event", function(x) x@country)
setMethod("country", "list", function(x) lapply(x, country))

setGeneric("location", function(x) standardGeneric("location"))
setMethod("location", "Event", function(x) x@location)
setMethod("location", "list", function(x) lapply(x, location))

setGeneric("continent", function(x) standardGeneric("continent"))
setMethod("continent", "Event", function(x) x@continent)
setMethod("continent", "list", function(x) lapply(x, continent))

setGeneric("description", function(x) standardGeneric("description"))
setMethod("description", "Event", function(x) x@description)
setMethod("description", "list", function(x) lapply(x, description))

setGeneric("styles", function(x) standardGeneric("styles"))
setMethod("styles", "Event", function(x) x@styles)
setMethod("styles", "list", function(x) lapply(x, styles))

setGeneric("teachers", function(x) standardGeneric("teachers"))
setMethod("teachers", "Event", function(x) x@teachers)
setMethod("teachers", "list", function(x) lapply(x, teachers))

setGeneric("teachers_confirmed", function(x) standardGeneric("teachers_confirmed"))
setMethod("teachers_confirmed", "Event", function(x) x@teachers_confirmed)
setMethod("teachers_confirmed", "list", function(x) lapply(x, teachers_confirmed))

setGeneric("competitions_held", function(x) standardGeneric("competitions_held"))
setMethod("competitions_held", "Event", function(x) x@competitions_held)
setMethod("competitions_held", "list", function(x) lapply(x, competitions_held))

setGeneric("is_new_event", function(x) standardGeneric("is_new_event"))
setMethod("is_new_event", "Event", function(x) x@is_new_event)
setMethod("is_new_event", "list", function(x) lapply(x, is_new_event))

setGeneric("latitude", function(x) standardGeneric("latitude"))
setMethod("latitude", "Event", function(x) x@latitude)
setMethod("latitude", "list", function(x) lapply(x, latitude))

setGeneric("longitude", function(x) standardGeneric("longitude"))
setMethod("longitude", "Event", function(x) x@longitude)
setMethod("longitude", "list", function(x) lapply(x, longitude))

setGeneric("start_date", function(x) standardGeneric("start_date"))
setMethod("start_date", "Event", function(x) x@start_date)
setMethod("start_date", "list", function(x) lapply(x, start_date))

setGeneric("end_date", function(x) standardGeneric("end_date"))
setMethod("end_date", "Event", function(x) x@end_date)
setMethod("end_date", "list", function(x) lapply(x, end_date))


setMethod("show", "Event", function(object){
  cat(is(object)[[1]], ': ', object@event_code, '\n',
      object@name, '  (', as.character(object@event_format), ', from ',
      as.character(object@start_date), ' to ',
      as.character(object@end_date),')\n\n',
      
      '  -> ', ifelse(object@teachers_confirmed, 'Teachers confirmed', 'Teachers unconfirmed'), '\n',
      '  -> ', ifelse(object@competitions_held, 'Competitions held', 'No competitions'), '\n',
      '  -> ', ifelse(object@is_new_event, 'New event!', 'Regular event'), '\n',
      
      '  ', object@location, ' (', object@country, ', ', object@continent, ')\n',
      '  Coordinates: (', object@latitude, ', ', object@longitude, ')\n\n',
      
      '  Styles: ', paste(object@styles, collapse = ', '), '\n',
      '  Description: ', object@description, '\n',
      '  Teachers: ', paste(object@teachers, collapse = ' ,'), '\n\n',
      '  More information at: ', object@url, '\n',
      sep = '')
  
  
})

setGeneric("make_event_row", function(x) standardGeneric("make_event_row"))
setMethod("make_event_row", "Event", function(x){
  list(
    event_code = event_code(x),
    name = name(x),
    url = url(x),
    event_format = event_format(x),
    location = location(x),
    country = country(x),
    continent = continent(x),
    latitude = latitude(x),
    longitude = longitude(x),
    start_date = start_date(x),
    end_date = end_date(x),
    teachers_confirmed = teachers_confirmed(x),
    is_new_event = is_new_event(x),
    competitions_held = competitions_held(x))
})

setGeneric("make_event_table", function(l) standardGeneric("make_event_table"))
setMethod("make_event_table", "list", function(l){
  dplyr::bind_rows(lapply(l, make_event_row))
})

setGeneric("make_style_rows", function(x) standardGeneric("make_style_rows"))
setMethod("make_style_rows", "Event", function(x){
  tibble::tibble(
    event_code = rep(event_code(x),
                     length(styles(x))),
    style = styles(e))
})

setGeneric("make_style_table", function(l) standardGeneric("make_style_table"))
setMethod("make_style_table", "list", function(l){
  dplyr::bind_rows(lapply(l, make_style_rows))
})

setGeneric("make_teacher_rows", function(x) standardGeneric("make_teacher_rows"))
setMethod("make_teacher_rows", "Event", function(x){

  dplyr::bind_rows(
    lapply(teachers(x),
           function(t) list(event_code = event_code(x), teacher_1 = t[1], teacher_2 = t[2])))
})

setGeneric("make_teacher_table", function(l) standardGeneric("make_teacher_table"))
setMethod("make_teacher_table", "list", function(l){
  dplyr::bind_rows(lapply(l, make_teacher_rows))
})

# Helper
Event <- function(event_page, homepage_node, event_page_url){

  stopifnot(is(event_page)[[1]] == 'xml_document')
  stopifnot(is(homepage_node)[[1]] == 'xml_node')
  stopifnot(is(event_page_url)[[1]] == 'character')
  
  slots <- list('Event')

  slots$event_code <- event_page_url %>%
    stringr::str_split('/') %>%
    purrr::as_vector() %>%
    tail(n = 1)
    
  slots$name <- get_event_name(event_page)
  slots$url <- get_event_url(event_page)

  start_end_date <- get_event_start_end_dates(event_page)
  slots$start_date <- lubridate::as_date(start_end_date[1])
  slots$end_date <- lubridate::as_date(start_end_date[2])
  
  slots$country <- get_event_country(event_page)
  slots$location <- get_event_location(event_page)
  slots$continent <- tryCatch({get_event_continent(homepage_node)}, error = function(e) NA)
  
  lat_lon <- get_event_lat_lon(slots$location, slots$country, slots$continent)
  slots$latitude <- as.double(lat_lon$latitude)
  slots$longitude <- as.double(lat_lon$longitude)

  slots$event_format <- as.factor(get_event_format(homepage_node))
  slots$description <- get_event_description(event_page)
  slots$styles <- as.factor(get_event_styles(event_page))
  
  slots$teachers_confirmed <- get_event_teachers_status(homepage_node)
  slots$competitions_held <- get_event_competitions(homepage_node)
  slots$is_new_event <- get_event_newness(homepage_node)
      
  slots$teachers <- get_event_teachers(event_page)

  tryCatch(
    expr = {do.call(methods::new, slots[!is.na(slots)])},
    error = function(e) {cat(paste(slots$name, ':\n', e)) ; NA})
}
