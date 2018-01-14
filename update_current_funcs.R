
update_current_funcs <- function() {
  nominal_funcs <- read_lines("./current_funcitons.txt")

  namespace <- read_lines("./NAMESPACE")
  current_funcs <- namespace %>% str_extract_all("(export\\([a-z])\\w+\\)") %>%
    as_vector() %>%
    str_replace_all("export", "") %>%
    str_replace_all("\\(", "") %>%
    str_replace_all("\\)", "")

  if (length(setdiff(nominal_funcs, current_funcs)) > 1) {
    write_lines(funcs, "./current_funcitons")
  }
  return(current_funcs)
}



