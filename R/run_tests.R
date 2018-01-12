#' Run Tests
#'
#' @param files A vector of test files, with or without extensions
#' @param ext Optional extension, if does not exist in file name.
#' @param line_by_line Do you want to step through with prompts? If non-interactive, this is not an option
#'
#' @import stringr
#' @export
#'
#' @examples
#'
#' my_test_files %>% run_tests()
#'


# Return whether an individual test passed or not
test_it <- function(f, verbose = FALSE) {
  if (verbose == FALSE) {
    result <- suppressMessages(suppressWarnings(try(source(f), silent = TRUE)))
  } else {
    result <- try(source(f), silent = TRUE)
  }

  if (inherits(result, "try-error")) {
    message(paste0(" --- ", f, " FAILED --- "))
  } else {
    message(paste0(" --- ", f, " PASSED --- "))
  }
}


# Run individual tests interactively or not
run_tests <- function(files = NULL, ext = NULL, line_by_line = TRUE) {
  files <- str_c(files, ext)

  if (line_by_line == FALSE ) {  # | interactive() == FALSE
    for (i in seq_along(files)) {
      test_it(files[i])
    }

  } else {
    i <- 1
    while (i <= length(files)) {
      answer <- readline(paste0("Should we test ", files[i], " ? \n y/n:       "))

      if (answer == "y" | answer == "Y") {
        test_it(files[i])
        i <- i + 1

      } else if (answer == "n" | answer == "N") {
        message(paste0("Ok; not testing ", files[i], "."))
        i <- i + 1

      } else if (answer == "q" | answer == "Q") {
        message("Quitting tests.")
        break

      } else {
        message("Unrecognized choice submitted. Trying again.")
        i <- i    # Step back one
      }
    }
  }
}