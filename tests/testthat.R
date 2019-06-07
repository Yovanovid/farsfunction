library(testthat)
library(farsfunction)

test_check("farsfunction")

expect_that(fars_map_state(00,2015), throws_error("invalid STATE number: 0"))

expect_that(fars_read("productions.csv"), throws_error("file 'productions.csv' does not exist"))
