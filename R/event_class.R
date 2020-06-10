
#' @title A class to hold information about a dance event
#' @description A (likely non-optimal) way of holding all the information
#'   about a dance event
#' @slot event_code Character. A unique identifier for the event
#' @slot name Character.
#' @slot url Character. The organisers' page for the event
#' @slot event_format Factor
#' @slot location Character. Town/City name
#' @slot country Character.
#' @slot continent Character.
#' @slot description Character.
#' @slot teacher_description Character.
#' @slot styles Vector of factors. The dances present at the event.
#' @slot teachers List of character vectors. The sets of teachers at the event.
#' @slot teachers_confirmed Logical.
#' @slot competitions_held Logical.
#' @slot is_new_event Logical.
#' @slot latitude Numeric.
#' @slot longitude Numeric.
#' @slot start_date Date.
#' @slot end_date Date.
#' @return An \code{Event} class
#' @rdname new_Event
#' @export 
new_Event <- setClass("Event",
         slots = c(
           event_code = 'character',
           name = 'character',
           url = 'character',
           event_format = 'factor',
           country = 'character',
           location = 'character',
           continent = 'character',
           description = 'character',
           teacher_description = 'character',
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
           teacher_description = NA_character_,
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

validate_Event <- setValidity('Event', function(object){
  if (is.na(object@event_code)){
    "event_code must be provided"
  } else if (is.na(object@name)){
    "name must be provided"
  } else if (object@start_date > object@end_date & !is.na(object@end_date)){
    "start_date must be before end_date, or end_date must be NA"
  } else {
  return (TRUE)
  }
})

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

setGeneric("teacher_description", function(x) standardGeneric("teacher_description"))
setMethod("teacher_description", "Event", function(x) x@teacher_description)
setMethod("teacher_description", "list", function(x) lapply(x, teacher_description))

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

#' @export
setMethod("show", "Event", function(object){
  cat(is(object)[[1]], ': ', object@event_code, '\n',
      object@name, '  (', as.character(object@event_format), ', from ',
      as.character(object@start_date), ' to ',
      as.character(object@end_date),')\n\n',
      
      '  -> ', ifelse(object@teachers_confirmed, 'Teachers confirmed', 'Teachers unconfirmed'), '\n',
      '  -> ', ifelse(object@competitions_held, 'Competitions held', 'No competitions'), '\n',
      '  -> ', ifelse(object@is_new_event, 'New event', 'Regular event'), '\n',
      
      '  ', object@location, ' (', object@country, ', ', object@continent, ')\n',
      '  Coordinates: (', object@latitude, ', ', object@longitude, ')\n\n',
      
      '  Styles: ', paste(object@styles, collapse = ', '), '\n',
      '  Description: ', object@description, '\n',
      '  Teacher Description: ', object@teacher_description, '\n',
      '  Parse attempt of the teachers: ', paste(object@teachers, collapse = ' ,'), '\n\n',
      '  More information at: ', object@url, '\n',
      sep = '')
  
  
})


#' @title Construct a row for the database's 'event' table
#' @param x An Event object
#' @param sqlite_types Whether to adapt types for an SQLite database
#' @return A list with all relevant slots
#' @rdname make_event_row
setGeneric("make_event_row", function(x, ...) standardGeneric("make_event_row"))
setMethod("make_event_row", "Event", function(x, sqlite_types = FALSE){
  o <- list(
    event_code = event_code(x),
    name = name(x),
    url = url(x),
    description = description(x),
    teacher_description = teacher_description(x),
    event_format = event_format(x) %>% as.factor(),
    location = location(x),
    country = country(x) %>% as.factor(),
    continent = continent(x) %>% as.factor(),
    latitude = latitude(x),
    longitude = longitude(x),
    start_date = start_date(x),
    end_date = end_date(x),
    teachers_confirmed = teachers_confirmed(x),
    is_new_event = is_new_event(x),
    competitions_held = competitions_held(x))
  if (sqlite_types){
    o$start_date <- as.character(o$start_date)
    o$end_date <- as.character(o$end_date)
    o$event_format <- as.character(o$event_format)
    o$country <- as.character(o$country)
    o$continent <- as.character(o$continent)
  }
  return (o)
})

#' @export
setGeneric("make_event_table", function(l, ...) standardGeneric("make_event_table"))
#' @export
setMethod("make_event_table", "list", function(l, sqlite_types = FALSE){
  dplyr::bind_rows(lapply(l, make_event_row, sqlite_types = sqlite_types))
})

setGeneric("make_style_rows", function(x, ...) standardGeneric("make_style_rows"))
setMethod("make_style_rows", "Event", function(x, sqlite_types = FALSE){
  a <- rep(event_code(x), length(styles(x)))
  b <- styles(x)
  if (sqlite_types){
    t <- tibble::tibble(event_code = a,
                        style = b)
  } else {
    t <- tibble::tibble(event_code = a,
                        style = b  %>% as.factor())
  }
  return (t)
})

#' @export
setGeneric("make_style_table", function(l, ...) standardGeneric("make_style_table"))
#' @export
setMethod("make_style_table", "list", function(l, sqlite_types = FALSE){
  dplyr::bind_rows(lapply(l, make_style_rows, sqlite_types = sqlite_types))
})

setGeneric("make_teacher_rows", function(x) standardGeneric("make_teacher_rows"))
setMethod("make_teacher_rows", "Event", function(x){

  dplyr::bind_rows(
    lapply(teachers(x),
           function(t) list(event_code = event_code(x), teacher_1 = t[1], teacher_2 = t[2])))
})

#' @export
setGeneric("make_teacher_table", function(l) standardGeneric("make_teacher_table"))
#' @export
setMethod("make_teacher_table", "list", function(l){
  dplyr::bind_rows(lapply(l, make_teacher_rows))
})

# Helper
#' @export
Event <- function(event_page, homepage_node, event_code){

  node_na <- is.na(homepage_node)
  
  stopifnot(is(event_page)[[1]] == 'xml_document')
  stopifnot(is(homepage_node)[[1]] == 'xml_node' | node_na)
  stopifnot(is(event_code)[[1]] == 'character')
  
  slots <- list('Event')

  slots$event_code <- event_code
    
  slots$name <- get_event_name(event_page) %>% as.character()
  slots$url <- get_event_url(event_page) %>% as.character()

  start_end_date <- get_event_start_end_dates(event_page)
  slots$start_date <- start_end_date[1] %>% lubridate::as_date()
  slots$end_date <- start_end_date[2] %>% lubridate::as_date()
  
  slots$country <-  get_event_country(event_page) %>% as.character()
  slots$location <- get_event_location(event_page) %>% as.character()
  slots$continent <- ifelse(!node_na,
                            tryCatch({get_event_continent(homepage_node)}, error = function(e) NA), NA) %>% as.character()
  
  lat_lon <- get_event_lat_lon(slots$location, slots$country, slots$continent)
  slots$latitude <- lat_lon$latitude %>% as.double()
  slots$longitude <- lat_lon$longitude %>% as.double()
  
  slots$teachers_confirmed <- ifelse(!node_na,
                                     get_event_teachers_status(homepage_node), NA)
  slots$competitions_held <- ifelse(!node_na,
                                    get_event_competitions(homepage_node), NA)
  slots$is_new_event <- ifelse(!node_na,
                               get_event_newness(homepage_node), NA)
  
  slots$teacher_description <- get_teacher_description(event_page) %>% as.character()
  slots$teachers <- get_event_teachers(event_page) %>% as.list()

  slots$event_format <- ifelse(!node_na,
                               get_event_format(homepage_node),
                               get_event_format(slots$event_code,
                                                has_teachers = !is.na(slots$teachers))) %>% as.factor()
  slots$description <- get_event_description(event_page) %>% as.character()
  slots$styles <- get_event_styles(event_page) %>% as.factor()
  
  tryCatch(
    expr = {do.call(methods::new, slots)},
    error = function(e) {cat(paste(slots$name, ':\n', e)) ; NA})
}
