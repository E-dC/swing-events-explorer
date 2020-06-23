#! /usr/bin/Rscript
# ---------- Read in arguments, setup options ----------
require('docopt', quietly = TRUE)
"Scrape events data from SwingPlanIt. 
Use a dbname ending with .rds to load and dump data using R objects.

Usage:
  run_scraper.R  <dbname> [--nopast] [--noguess] [--nofilter] [--limit <LIMIT>]

Options:
-h --help         Show this
--nopast          Don't attempt to scrape past events page, but still try to guess.
--noguess         Don't attempt to guess past events codes
--nofilter        Don't filter out seen events
--limit=<LIMIT>   Limit to LIMIT downloads" -> doc

args <- docopt::docopt(doc)

library(swingscrapeit)
library(magrittr)

# ---------- Check the DB, set up tables if necessary ----------
cat('Prepping DB...\n')
if (endsWith(args$dbname, '.sqlite')){
  db_conn <- DBI::dbConnect(RSQLite::SQLite(), args$dbname)
  tables <- DBI::dbListTables(db_conn)
  if (!'event' %in% tables){
    DBI::dbCreateTable(db_conn, 'event',
                       c('event_code' = 'character', 'name' = 'character', 'url' = 'character', 'event_format' = 'character',
                       'description' = 'character', 'teacher_description' = 'character',
                       'location' = 'character', 'country' = 'character', 'continent' = 'character',
                       'latitude' = 'numeric', 'longitude' = 'numeric',
                       'start_date' = 'character', 'end_date' = 'character',
                       'teachers_confirmed' = 'logical', 'is_new_event' = 'logical', 'competitions_held' = 'logical'))
  }
  if (!'teacher' %in% tables){
    DBI::dbCreateTable(db_conn, 'teacher',
                       c('event_code' = 'character', 'teacher_1' = 'character', 'teacher_2' = 'character'))
  }
  if (!'style' %in% tables){
    DBI::dbCreateTable(db_conn, 'style',
                       c('event_code' = 'character', 'style' = 'character'))
  }
  if (!'tried' %in% tables){
    DBI::dbCreateTable(db_conn, 'tried',
                       c('event_code' = 'character', 'url' = 'character'))
  }
  
  seen_events <- db_conn %>%
    DBI::dbGetQuery('SELECT event_code FROM event;') %>%
    tibble::as_tibble()
  
  tried_events <- db_conn %>%
    DBI::dbGetQuery('SELECT event_code FROM tried;') %>%
    tibble::as_tibble()
} else {
  db <- tryCatch(
    {load(args$dbname); TRUE},
    error = function(e) FALSE)
  event_table <- tryCatch(event_table, error = function(x) tibble::as_tibble())
  style_table <- tryCatch(style_table, error = function(x) tibble::as_tibble())
  teacher_table <- tryCatch(teacher_table, error = function(x) tibble::as_tibble())
  tried_table <- tryCatch(tried_table, error = function(x) tibble::as_tibble())
  
  seen_events <- tryCatch({event_table %>% dplyr::select(event_code)},
                        error = function (e) list(event_code = c()))
  tried_events <- tryCatch({tried_table %>% dplyr::select(event_code)},
                           error = function (e) list(event_code = c()))
  
}
  
cat('Event already in DB: ', length(seen_events$event_code), '\n')
cat('URLs "tried" in the past: ', length(tried_events$event_code), '\n')

# ---------- Prep present events ----------
cat('Prepping future events...\n')
# Load the page
swingplanit <- xml2::read_html('https://www.swingplanit.com/')

# Find the events
event_nodes <- swingplanit %>%
  rvest::html_nodes('div.bodyright ul.homepagelist li.color-shape')

event_urls <- event_nodes %>%
  rvest::html_nodes('a') %>%
  rvest::html_attr(name = 'href')

event_codes <- (event_urls %>%
  stringr::str_match('event/(.*)$'))[,2]

# ---------- Prep past events ----------

if (! args$nopast){
  cat('Prepping past events...\n')
  doc <- screen_scrape_homepage()
  
  o <- find_past_event_nodes_and_codes(doc)
  past_event_nodes <- o$event_nodes
  past_event_codes <- o$event_codes
  past_event_urls <- vapply(past_event_codes,
                            function(x) paste0('https://www.swingplanit.com/event/', x),
                            FUN.VALUE = 'character',
                            USE.NAMES = FALSE)
  
  event_nodes <- c(event_nodes, past_event_nodes)
  event_codes <- c(event_codes, past_event_codes)
  event_urls <- c(event_urls, past_event_urls)
}

# ---------- Prep guessed events ----------

if (! args$noguess){
  cat('Prepping "guessed" events...\n')
  guessed_event_codes <- find_more_codes(event_codes)
  guessed_event_nodes <- rep(NA, times = length(guessed_event_codes))
  guessed_event_urls <- vapply(guessed_event_codes,
                               function(x) paste0('https://www.swingplanit.com/event/', x),
                               FUN.VALUE = 'character',
                               USE.NAMES = FALSE)

  event_nodes <- c(event_nodes, guessed_event_nodes)
  event_codes <- c(event_codes, guessed_event_codes)
  event_urls <- c(event_urls, guessed_event_urls)
}

# ---------- Filter out unwanted events ----------
if (! args$nofilter){
  cat('Filtering events already in DB, and previously tried URLs...\n')
  seen_lgl <- event_codes %in% seen_events$event_code
  tried_lgl <- event_codes %in% tried_events$event_code
  both_lgl <- seen_lgl | tried_lgl
  event_nodes <- event_nodes[!both_lgl]
  event_codes <- event_codes[!both_lgl]
  event_urls <- event_urls[!both_lgl]
}

# ---------- Limit the number of downloads ----------
if (!is.null(args$limit)){
  l <- as.numeric(args$limit)
  cat('Limiting to ', l, ' downloads...\n', sep = '')
  event_nodes <- event_nodes[1:l]
  event_codes <- event_codes[1:l]
  event_urls <- event_urls[1:l]
  
  
}

# ---------- Download events, and filter NAs ----------
cat('Attempting download of ', length(event_urls), ' events...\n', sep = '')
event_pages <- lapply(event_urls, download_event)
na_pages <- is.na(event_pages)

# So we can avoid trying to download failed urls again
cat('Filtering out NAs, and memoising them...\n')
tried_table_new <- tibble::tibble(
  event_code = event_codes[na_pages],
  url = event_urls[na_pages]
)

event_pages <- event_pages[!na_pages]
event_nodes <- event_nodes[!na_pages]
event_codes <- event_codes[!na_pages]

# ---------- Process scraped data ----------
cat('Processing downloaded pages...\n')
event_classes <- mapply(Event, event_pages, event_nodes, event_codes)
event_classes <- event_classes[!is.na(event_classes)]

sqlite_types <- ifelse(endsWith(args$dbname, '.sqlite'), TRUE, FALSE)

event_table_new <- make_event_table(event_classes, sqlite_types = sqlite_types)
teacher_table_new <- make_teacher_table(event_classes)
style_table_new <- make_style_table(event_classes)

# ---------- Debug information ----------
print(event_table_new)
print(teacher_table_new)
print(style_table_new)

# ---------- Add data to DB ----------
cat('Adding to DB...\n')
if (endsWith(args$dbname, '.sqlite')){
  event_table_new[is.na(event_table_new)] <- ''
  teacher_table_new[is.na(teacher_table_new)] <- ''
  style_table_new[is.na(style_table_new)] <- ''
  
  cat(DBI::dbAppendTable(db_conn, 'event', event_table_new), ' rows added to "event" table\n', sep = '')
  cat(DBI::dbAppendTable(db_conn, 'teacher', teacher_table_new), ' rows added to "teacher" table\n', sep = '')
  cat(DBI::dbAppendTable(db_conn, 'style', style_table_new), ' rows added to "style" table\n', sep = '')
  cat(DBI::dbAppendTable(db_conn, 'tried', tried_table_new), ' rows added to "tried" table\n', sep = '')
  
  DBI::dbDisconnect(db_conn)
} else {
  event_table <- dplyr::bind_rows(event_table, event_table_new)
  teacher_table <- dplyr::bind_rows(teacher_table, teacher_table_new)
  style_table <- dplyr::bind_rows(style_table, style_table_new)
  tried_table <- dplyr::bind_rows(tried_table, tried_table_new)
  save(event_table, teacher_table, style_table, tried_table, file = args$dbname)
}
cat('Events added to DB: ', nrow(event_table_new), '\n', sep = '')