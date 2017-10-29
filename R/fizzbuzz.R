#' FizzBuzz
#'
#' Compare for loop time to vectorized time.
#'
#' FizzBuzz problem: Print 1 to 100; for multiples of three print "Fizz", for the multiples of five print "Buzz".
#' For numbers which are multiples of both three and five print "FizzBuzz".
#' @param len Length of vector
#' @param i Index, in fiz_vec
#' @keywords fizzbuzz
#' @import purrr
#' @export
#' @examples
#'
#'
#' system.time(fizzbuzz(1000))
#' system.time(fizzbuzz_2(1000))
#'
#' system.time(lapply(1:1000, fiz_vec))
#' system.time(purrr::map(1:1000, fiz_vec))
#'
#' system.time(sapply(1:1000, fiz_vec))
#' system.time(purrr::map_chr(1:1000, fiz_vec))


fizzbuzz <- function(len) {
  for (i in 1:len) {
    if (i %% 3 == 0) {
      print("Fizz")
    } else if (i %% 5 == 0) {
      print("Buzz")
    } else if ((i %% 3 & i %% 5) == 0) {
      print("FizzBuzz")
    } else {
      print(i)
    }
  }
}


fizzbuzz_2 <- function(len) {
  vec <- vector(length = len)
  for (i in 1:len) {
    if (i %% 3 == 0) {
      i <- "Fizz"
    } else if (i %% 5 == 0) {
      i <- "Buzz"
    } else if ((i %% 3 & i %% 5) == 0) {
      i <- "FizzBuzz"
    } else {
      i
    }
    vec <- c(vec, i)
  }
  return(vec)
}


fiz_vec <- function(i) {
  if (i %% 3 == 0) {
    i <- "Fizz"
  } else if (i %% 5 == 0) {
    i <- "Buzz"
  } else if ((i %% 3 & i %% 5) == 0) {
    i <- "FizzBuzz"
  } else {
    i
  }
}






