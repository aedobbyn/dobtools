#' Run Tests
#'
#' @param dir A directory in which your test files live
#' @param files A vector of test files, with or without extensions
#' @param ext Optional extension, if does not exist in file name.
#' @param line_by_line Do you want to step through with prompts? If non-interactive, this is not an option
#' @param beep Should we beep when done?
#' @param beep_num If so, what beep number should we use?
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' my_test_files %>% run_tests()
#' }


# Run individual tests interactively or not
run_tests <- function(dir = NULL, files = NULL, ext = NULL, line_by_line = TRUE,
                      beep = TRUE, beep_num = 1) {
  files <- stringr::str_c(dir, "/", files, ext)

  if (line_by_line == FALSE ) {  # | interactive() == FALSE
    for (i in seq_along(files)) {
      test_it(files[i])
    }

  } else {
    i <- 1
    while (i <= length(files)) {
      answer <- readline(paste0("Should we test ", files[i], " ? \n y/n:       "))

      if (answer == "y" | answer == "Y") {
        test_it(files[i]) %>% beepr::beep(beep_num)
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
