library(testthat)

root_dir <- '~/dev/shiny_swing'

source(file.path(root_dir, 'utils.R'))

test_dir(file.path(root_dir, 'tests', 'testthat'), reporter='summary')
