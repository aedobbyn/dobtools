original_names <- c("Actions Cumsum", "Profit Cumsum", "Dealer Id", "Action Year", "Action Month")
replacement_names <- c("Sum of Actions: All Time", "Sum of Profit: All Time", "Dealer ID",
                       "Previous Action Year", "Previous Action Month")


name_df <- cbind(original_names, replacement_names) %>% as_tibble()

original_names_future <- c("Profit", "Last Action Profit", "Action Type", "Last Action Type",
                           "Action Typeservice", "Gold Box Boolean1", "Is Heavy Metal CustYes")
replacement_names_future <- c("Previous Action Profit", "Pre-Previous Action Profit", "Previous Action Type", "Pre-Previous Action Type",
                              "Previous Action Type -- Service", "Gold Box Boolean -- Yes",
                              "Is Heavy Metal Cust -- Yes")

name_df_future <- cbind(original_names_future, replacement_names_future) %>% as_tibble()

replace_names_base <- function(vec, ref_df, orig, new) {
  for (i in seq_along(vec)) {
    if (vec[i] %in% ref_df[[orig]]) {
      j <- which(ref_df[[orig]]==vec[i])
      vec[i] <- ref_df[[new]][j]
    } else {
      vec[i] <- vec[i]
    }
  }
  vec
}

replace_names <- function(vec, future_prediction = FALSE) {
  vec <- replace_names_base(vec, name_df, "original_names", "replacement_names")
  
  if(future_prediction == TRUE) {
    vec <- replace_names_base(vec, name_df_future, "original_names_future", "replacement_names_future")
  }
  vec
}