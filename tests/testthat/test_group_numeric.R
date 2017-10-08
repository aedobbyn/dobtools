library(tidyverse)
library(assertthat)
library(testthat)
library(dobtools)

context("Grouping")

mpg_cuts <- c(15, 20, 25, 30)
group_numeric(mtcars, "mpg", mpg_cuts, add_first_last = TRUE)

mtcars$some_dates <- (rnorm(n = 32) * 5) %>% as_date()
date_cuts <- c("1969-11-01", "1969-12-29", "1970-01-01", "1970-01-04", "1970-02-01")
group_numeric(mtcars, "some_dates", date_cuts, add_first_last = FALSE)


test_that("Group numeric vector is greater than 0", {
  expect_length(group_equal(mtcars, "mpg", n_groups = 5, add_first_last = FALSE), 5)
  expect_length(group_equal(mtcars, "mpg", n_groups = 5, add_first_last = TRUE), 7)
})
