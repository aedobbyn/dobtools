
#' Execute a function in batches
#'
#' In the vector method, attempts to apply fun to inp that result in an error will return NAs for that batch.
#' However, in the dataframe method, these will be returned as \code{NULL}s and not applied to the output.
#'
#' @param inp (vector, dataframe) A vector or dataframe of inputs.
#' @param fun (function) A function to apply to the input. This is what is returned if \code{grow_full_output} is true.
#' @param fun_2 (function) Optional secondary function to apply to the result of \code{fun(inp)}, conditional upon its successful execution. Good for e.g. sending results over a network or inserting them into a database. The result of \code{fun_2(fun(inp))} is not returned.
#' @param n_batches (integer) Number of batches to split inp into.
#' @param grow_full_output (boolean) Should the result of each batch be combined with the ones before it and the whole result returned at the end?
#' @param verbose (boolean) Should progress be messaged?
#' @param ... Further arguments.
#'
#' @return A dataframe or vector, depending on the type of inp.
#'
#'
#' @export
#'
#' @examples
#'
#' add_one <- function(tbl) {
#'   tbl %>% purrr::map_dbl(~.x + 1)
#' }
#' batch_it(mtcars$mpg, fun = add_one, n_batches = 3)
#'
#' print_it <- function(tbl) {
#'   tbl %>% purrr::map_dbl(~ print(.x))
#' }
#' # Print the result of each batch, but don't return the full output at the end
#' batch_it(mtcars$mpg, fun = add_one, fun_2 = print_it, n_batches = 8, grow_full_output = FALSE)
#'
#' add_one_tbl <- function(tbl) {
#'   tbl %>% purrr::map_dfr(~.x + 1)
#' }
#' batch_it(mtcars, add_one_tbl, n_batches = 5)
#'

batch_it <- function(inp, fun, fun_2 = NULL,
                     n_batches, grow_full_output = TRUE,
                     verbose = TRUE, ...) {
  UseMethod("batch_it", inp)
}

#' @export
batch_it.default <- function(inp, fun, fun_2 = NULL,
                             n_batches, grow_full_output = TRUE,
                             verbose = TRUE, ...) {
  fun <- match.fun(fun, ...)
  safe_fun <- purrr::safely(fun, ...)

  quiet <- !verbose

  if (!is.null(fun_2)) {
    fun_2 <- match.fun(fun_2)
    safe_fun_2 <- purrr::possibly(fun_2, otherwise = NA, quiet = quiet)
  }

  full_len <- length(inp)
  batch_len <- ceiling(full_len / n_batches)

  first_e <- 1
  last_e <- batch_len

  out <- NULL

  for (i in seq(n_batches)) {
    this_batch <- inp[first_e:last_e]
    if (verbose) {
      message(glue::glue("--- Applying fun to batch {i} of {n_batches}. ---"))
    }
    this_res_safe <- safe_fun(this_batch)

    if (!is.null(this_res_safe$error)) {
      message(glue::glue("    Error in batch {i}. Moving to batch {i + 1}. \n    {this_res_safe$error}"))
      if (!is.null(fun_2)) message("      Skipping application of fun_2.")

    } else {
      message(glue::glue("    Success in batch {i}."))
      this_res <- this_res_safe$result
      if (grow_full_output) {
        out <- out %>% c(this_res)
      }
      # fun_2 conditional on successful completion of fun
      if (!is.null(fun_2)) {
        message(glue::glue("      Applying fun_2 to batch {i}."))
        safe_fun_2(this_res)
      }
    }

    first_e <- first_e + batch_len
    last_e <- last_e + batch_len

  }
  if (grow_full_output == FALSE) {
    return(invisible(out))
  }
  out
}

#' @export
batch_it.data.frame <- function(inp, fun, fun_2 = NULL,
                         n_batches, grow_full_output = TRUE,
                         verbose = TRUE, ...) {
  fun <- match.fun(fun)
  safe_fun <- purrr::safely(fun)

  quiet <- !verbose

  if (!is.null(fun_2)) {
    fun_2 <- match.fun(fun_2)
    safe_fun_2 <- purrr::possibly(fun_2, otherwise = NULL, quiet = quiet)
  }

  full_len <- nrow(inp)
  batch_len <- ceiling(full_len / n_batches)

  first_e <- 1
  last_e <- batch_len

  out <- NULL

  for (i in seq(n_batches)) {
    this_batch <- inp[first_e:last_e, ]
    if (verbose) {
      message(glue::glue("--- Applying fun to batch {i} of {n_batches}. ---"))
    }
    this_res_safe <- safe_fun(this_batch)

    if (!is.null(this_res_safe$error)) {
      message(glue::glue("    Error in batch {i}. Moving to batch {i + 1}. \n    {this_res_safe$error}"))
      if (!is.null(fun_2)) message("      Skipping application of fun_2.")

    } else {
      message(glue::glue("    Success in batch {i}."))
      this_res <- this_res_safe$result
      if (grow_full_output) {
        out <- out %>% dplyr::bind_rows(this_res)
      }
      # fun_2 conditional on successful completion of fun
      if (!is.null(fun_2)) {
        message(glue::glue("      Applying fun_2 to batch {i}."))
        safe_fun_2(this_res)
      }
    }

    first_e <- first_e + batch_len
    last_e <- last_e + batch_len

  }
  if (grow_full_output == FALSE) {
    return(invisible(out))
  }
  out
}

