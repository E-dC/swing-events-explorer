testthat::context("Geonames calls")
testthat::test_that("Paris is at the correct latitude and longitude", {

  lat_lon <- list(latitude = 48.85341,
                  longitude = 2.3488,
                  country = 'France',
                  continent = 'Europe',
                  location = 'Paris')
  
  testthat::expect_mapequal(call_geonames('Paris', 'France', 'Europe'), lat_lon)
  testthat::expect_mapequal(call_geonames('Paris', 'France', 'Eurezfez'), lat_lon)
  testthat::expect_mapequal(call_geonames('Paris', 'France'), lat_lon)
  testthat::expect_mapequal(call_geonames('Paris'), lat_lon)
  testthat::expect_error(call_geonames('sdkj'))
})


testthat::context("Cleaning functions")
  raw_test_data <- c(
    "Meghan Gilmore - Yana Sanamyantz - Jon Tigert",
    "Shani Brown and Mel Calanglang, Anni Skoglund and Gasper Hrovat",
    "Dawa Jung and Youngdon Kwon from Seoul, Niclas Palmér & Sandra Thörn from Gothenburg, Anne Helene  Cavasa and Bernard Cavasa from Toulouse",
    "Noni Healy, Sibby Dillon, Shieva Norusian, Rebecca Maria",
    "Joel Plys & Irina Amzashvili\nAndreas Olsson & Olga Marina\nPhil & Aude\nCathy & Gilbert",
    "Moe Sakan & Vincenzo Fesi - Anna-Maria Bernhard Ralf Bernhard",
    "Arnas Razgunas & Egle Nemickaite (Lithuania) Abeth Farag & Martynas Stonys (Portugal/Lithuania)",
    "Nick Williams, Laura Keat,Jeremy Otth, Tise Chao, Kelly Arsenault & Mickey Fortenasce Matt Mitchell, Jennifer Reed, Jennifer Barnett, Shannon Butler & Bill Butler Annabelle Hale & Nial Bruce Tabitha Robinson & Joseph Robinson Andrew MuÃ±oz Amanda Pincock & Zach Lockett-Streiff")
  
  fully_split_test_data <- list(
    list(c('Meghan Gilmore'),
         c('Yana Sanamyantz'),
         c('Jon Tigert')),
    list(c('Shani Brown', 'Mel Calanglang'),
         c('Anni Skoglund', 'Gasper Hrovat')),
    list(c('Dawa Jung', 'Youngdon Kwon'),
         c('Niclas Palmér', 'Sandra Thörn'),
         c('Anne Helene Cavasa', 'Bernard Cavasa')),
    list(c('Noni Healy'),
         c('Sibby Dillon'),
         c('Shieva Norusian'),
         c('Rebecca Maria')),
    list(c('Joel Plys', 'Irina Amzashvili'),
         c('Andreas Olsson', 'Olga Marina'),
         c('Phil', 'Aude'),
         c('Cathy', 'Gilbert')),
    list(c('Moe Sakan', 'Vincenzo Fesi'),
         c('Anna-Maria Bernhard'),
         c('Ralf Bernhard')), # Ideally AM & R Bernhard could be matched
    list(c('Arnas Razgunas', 'Egle Nemickaite'),
         c('Abeth Farag', 'Martynas Stonys')),
    list(c('Nick Williams'),
         c('Laura Keat'),
         c('Jeremy Otth'),
         c('Tise Chao'),
         c('Kelly Arsenault', 'Mickey Fortenasce'),
         c('Matt Mitchell'),
         c('Jennifer Reed'),
         c('Jennifer Barnett'),
         c('Shannon Butler', 'Bill Butler'),
         c('Annabelle Hale', 'Nial Bruce'),
         c('Tabitha Robinson', 'Joseph Robinson'),
         c('Andrew MuÃ±oz'),
         c('Amanda Pincock', 'Zach Lockett-Streiff'))
  )

testthat::test_that("Extracting teachers names from string", {
  test_data <- setNames(fully_split_test_data, nm = raw_test_data)
  
  for (n in names(test_data)){
    o <- process_teacher_description(n) %>% find_sets(capturing_patterns)
    testthat::expect_equal(o, test_data[[n]])
  }
  testthat::expect_equal(find_sets('', capturing_patterns), NA)
  
})

