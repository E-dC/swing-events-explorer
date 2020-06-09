
#' @title Parse the Swing Plan-It past events page
#' @description Extract the html nodes and event codes (necessary to
#'   rebuild the past events urls) from the past events page
#' @param doc An \code{xml_document} of the past-events page
#' @return A named list with two values: \code{event_nodes} and \code{event_codes},
#'   where \code{event_nodes} is a list of \code{xml_node} and \code{event_codes}
#'   is a character vector.
#' @details There are some weirdness with events whose name starts with a number,
#'   so we get the \code{event_code} by manipulating the event's name.
#' @seealso 
#'  \code{\link[rvest]{html_nodes}},\code{\link[rvest]{html_text}}
#'  \code{\link[stringr]{str_match}},\code{\link[stringr]{case}},\code{\link[stringr]{str_replace}}
#' @rdname find_past_event_nodes_and_codes
#' @export 
#' @importFrom rvest html_nodes html_text
#' @importFrom stringr str_match str_to_lower str_replace_all
find_past_event_nodes_and_codes <- function(doc){
  
  # Find event nodes and codes
  event_nodes <- doc %>%
    rvest::html_nodes('ul li.color-shape')
  event_codes <- (event_nodes %>%
                    as.character() %>%
                    stringr::str_match('event="" (.*) data-fancybox'))[,2]
  
  # Separate missed nodes/codes from the rest
  matched <- !is.na(event_codes)
  missed_nodes <- event_nodes[!matched]
  event_nodes <- event_nodes[matched]
  event_codes <- event_codes[matched]
  
  # Find missed codes
  missed_codes <- missed_nodes %>%
    rvest::html_nodes('div.maintitle2') %>%
    rvest::html_text() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(pattern = ' ', replacement = '-') %>%
    stringr::str_replace_all(pattern = "[^a-zA-z0-9.-]", replacement = '') %>%
    stringr::str_replace_all(pattern = '\\.$', replacement = '') %>%
    stringr::str_replace_all(pattern = '--+', replacement = '-')
  
  # Concatenate
  event_nodes <- c(event_nodes, missed_nodes)
  event_codes <- c(event_codes, missed_codes)
  
  return (list(event_codes = event_codes, event_nodes = event_nodes))
}

#' @title Read the past-events page of Swing Plan-it using phantomjs
#' @description The \code{http://www.swingplanit.com/past-events} page has
#'   pretty weird HTML issues going on, so we read it using phantomjs
#'   in order to get all the information we need
#' @return An HTML page of class \code{xml_document}
#' @details The small JS script used to download the page is created on the fly
#'   in a temporary location, and so is the output download. No cleanup needed.  
#'   You NEED \code{phantomjs} installed on your machine, it is called using
#'   \code{system}
#' @export 
#' @rdname screen_scrape_homepage
#'  
#' @import xml2
screen_scrape_homepage <- function(){
  script_path <- tempfile(fileext = '.js')
  out_path <- tempfile(pattern = 'past-events', fileext = '.html')
  f_conn <- file(script_path)
  cat("
var webPage = require('webpage');
var page = webPage.create();
var fs = require('fs');
",
      paste0("var path = '", out_path, "'"), 
      "
page.open('http://www.swingplanit.com/past-events', function (status) {
var content = page.content;
fs.write(path,content,'w')
phantom.exit();
});
",
      sep = ' ',
      file = script_path)
  close(f_conn)
  
  cmd <- paste("phantomjs", script_path)
  system(cmd, ignore.stdout = TRUE, wait = TRUE)
  doc <- xml2::read_html(out_path)
  return (doc)
}

#' @title Guess past editions of an \code{event_code}
#' @description Swing Plan-It keeps past events on the website, but
#'   doesn't make them accessible through the homepage nor the past-events
#'   page. However they follow simple naming conventions: if you already have
#'   a url \code{https://www.swingplanit.com/event/hot-rhythm-holiday-6}, then
#'   you can try and access the urls for \code{hot-rhythm-holiday-5}, 
#'   \code{hot-rhythm-holiday-4}, etc, and there are good chances that those
#'   pages will still be up. This function recursively go through the likely
#'   candidates and record them.
#' @param s A character vector. The \code{event_code} to find past editions for 
#' @param idx An integer. The current index for which to create a "guessed"
#'   \code{event_code}. Used only in the recursion, don't put anything
#'   here, Default: NULL
#' @param pad Whether to pad \code{idx} with a \code{0} (in order to handle dates).
#'   Used only in the recursion, don't put anything here, Default: FALSE
#' @return A character vector of "guessed" events, or NA if nothing was found.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  recurse_past_editions('hot-rhythm-holiday7')
#'  recurse_past_editions('my-balboa-weekend-2020')
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_match}},
#'  \code{\link[swingscrapeit]{find_more_codes}}
#' @rdname recurse_past_editions
#' @export 
#' @importFrom stringr str_match
recurse_past_editions <- function(s, idx = NULL, pad = FALSE){
  if (is.null(idx)){
    m <- stringr::str_match(s, '(.*?(..))([0-9]{1,2})$')
    if (is.na(m[,4])){
      return (NA)
    }
    
    whole_code <- m[,1]
    s <- m[,2]
    penultimate_chars <- m[,3]
    last_chars <- m[,4]
    pad <- grepl('[0-9][0-9]', penultimate_chars)
    idx <- as.numeric(last_chars) - 1
    
    return (recurse_past_editions(s, idx, pad))
    
  } else if (idx == 0){
    return (s)
  } else {
    if (idx < 10 & pad){
      v <- paste0('0', idx)
    } else {
      v <- idx
    }
    current <- paste0(s, v)
    return (c(current, recurse_past_editions(s, idx - 1, pad)))
  }
}

#' @title Check for already seen events
#' @description Compare \code{event_codes} gathered with those already present in database.
#' @param event_codes A character vector
#' @param db_conn A connection to the database used. Should contain an "\code{event}" table 
#' @return A logical vector, where seen events are \code{TRUE}
#' @seealso 
#'  \code{\link[DBI]{dbReadTable}},
#'  \code{\link[dplyr]{select}},
#'  \code{\link[tibble]{as_tibble}}  
#' @rdname get_seen_event_logical_vector
#' @export 
#' @importFrom DBI dbReadTable
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
get_seen_event_logical_vector <- function(event_codes, db_conn){
  tryCatch({
    x <- DBI::dbReadTable(db_conn, 'event') %>%
      dplyr::select('event_code') %>%
      tibble::as_tibble()
    return(event_codes %in% x$event_code)},
    error = function(e) {
      warning('Is there an "event" table accessible through the database connection you provided?')
      rep(FALSE, length(event_codes))
    })
}

#' @title Convenience wrapper for \code{recurse_past_editions}
#' @description Apply \code{recurse_past_editions} for each \code{event_code}
#'   provided, make the results into a neat, NA-free character vector.
#' @param event_codes Event codes for which to guess past editions
#' @return A character vector of "guessed" event codes
#' @seealso \code{\link[swingscrapeit]{recurse_past_editions}}
#' @rdname find_more_codes
#' @export 
find_more_codes <- function(event_codes){
  o <- unlist(lapply(event_codes, recurse_past_editions))
  return(o[!is.na(o)])
}

#' @title Download a webpage
#' @description Download a webpage in a slightly more polite way. This function
#'   is meant to be lapplied.
#' @param url A character vector.
#' @param be_nice Whether to wait a bit after downloading, Default: TRUE
#' @param delay How long to wait, in seconds, Default: 1
#' @return A \code{xml_document}
#' @seealso 
#'  \code{\link[xml2]{read_xml}}
#' @rdname download_event
#' @export 
#' @importFrom xml2 read_html
download_event <- function(url, be_nice = TRUE, delay = 1){
  page <- tryCatch(
    xml2::read_html(url),
    error = function(e) NA
  )
  if (be_nice){ Sys.sleep(delay)}
  return (page)
}
