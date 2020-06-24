# SwingPlanIt events explorer 

Event scraper and Shiny app for the SwingPlanIt website.  

The project consists of 2 parts:
* The `swingscrapeit` package contains utilities necessary to retrieve events information from the SwingPlanIt website.  
The scraper itself is run with the `run_scraper.R` script. It is slow on purpose, so as not to overwhelm their server with requests.
* The Shiny app `app.R` to show, filter, and search events in a map view. See an instance in action on [shinyapps.io](https://e-dc.shinyapps.io/swing-events-explorer/).

### Usage `run_scraper.R`

##### Basic usage 
To scrape events, make sure you have `Rscript` installed, and run `run_scraper.R` with it, or make `run_scraper.R` executable and run it directly. In its simplest form:

<pre><b>$ run_scraper.R  path/to/my/db.rda</b></pre>

Show more options and help:
<pre><b>$ run_scraper.R --help</b></pre>

```
Scrape events data from SwingPlanIt. 
Use a dbname ending with .rda to load and dump data using R objects.

Usage:
  run_scraper.R  <dbname> [--nopast] [--noguess] [--nofilter] [--limit <LIMIT>]

Options:
-h --help         Show this
--nopast          Don't attempt to scrape past events page, but still try to guess.
--noguess         Don't attempt to guess past events codes
--nofilter        Don't filter out seen events
--limit=<LIMIT>   Limit to LIMIT downloads
```


##### Database argument
The `<dbname>` argument is compulsory:
* If the file doesn't exist, the script creates one and store all found events in the db.
* If the file already exists, the script looks it up and downloads only those events not already present in the db.

The scraper stores the events either in a Rdata (`.rda`) file by default, or a SQLite database if the filename ends with `.sqlite`.
You should just provide the file extension no matter what. Beware, the Shiny app only works with a Rdata file for now.

##### `--nopast`, `--noguess`, `--nofilter`, `--limit` arguments

By default, the scraper attempts to grab past events from the SwingPlanIt archive page: `--nopast` prevents that.  

Based on events already grabbed, it also tries to guess past events urls (if you have `event-5`, then there's probably an `event-4`, `event-3`, etc): `--noguess` prevents it.  

`--nofilter` will override the default behaviour of the scraper and download events regardless of whether they are already in the DB or have been tried before (in the case of "guessed" urls).  

`--limit 5` will cap the number of event downloads to `5`. 

### Geonames
The scraper relies on Geonames to figure out the latitude and longitude of the location given by events organisers, and so this stage is slow as well, deliberately, to avoid hitting the Geonames server too much.

### Usage `app.R`
Just run `Rscript app.R my_db_name.rda` and open a browser tab with the URL given in the terminal, for example:
<pre><b>$ Rscript app.R data/my_db.rda</b></pre>

```
Attaching package: ‘dplyr’
... more R stuff ...
Listening on http://127.0.0.1:5445
```


