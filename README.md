# dobtools 

***

[![Build Status](https://travis-ci.org/aedobbyn/dobtools.svg?branch=master)](https://travis-ci.org/aedobbyn/dobtools)


### Installation Instructions

`if (!require("devtools")) install.packages("devtools")`

`devtools::install_github('aedobbyn/dobtools')`

<br>

### About

This is a personal package of utility functions -- feel free to use, but know that you do so at your own risk!

**What can I do with this stuff?**

* Run all tests in a directory from the console, test by test (an alternative to `testthat::test_dir`)
    * `run_tests`
* Do fuzzy string matching
    * `match_maker()`
* Trim out outliers above a certain z-score
    * `trim_outliers()`
* Capitalize names with underscores and periods in them 
    * `cap_it()`
* Specify types for columns in a dataframe
    * `set_col_types()`
* Style tidied regression outputs
    * `tidy_mod()`
* Convert all `NA`s in a dataframe to 0s
    * `na_to_zero()`
* k-means cluster a tidy dataframe
    * `do_cluster()`
* Run a random forest 
    * `run_rf()`
* Speak the truth
    * `nerdR()`

...and some other things too. 

<br>

Happy [dob]tooling! :hammer:
