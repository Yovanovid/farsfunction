context("test-farsfuntion")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

expect_that(fars_read("productions.csv"), throws_error("file 'productions.csv' does not exist"))

expect_that(fars_map_state(00,2015), throws_error("invalid STATE number: 0"))
