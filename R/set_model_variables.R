# Main predictors
main_preds <- c("action_type", "action_year", "action_month", "row_num",
                "gold_box_boolean", "profit",
                "is_heavy_metal_cust", "dealer_abbrev",  
                "profit_cumsum", "actions_cumsum", "year_as_cust")

# Predictors we can use if we want to look at customers who have all had a sale
sale_preds <- c("sales_price", "new_used", "mileage", "was_heavy_metal_sale")

# Predictors we can use if we want to look at customers who have all had a sale
one_back_preds <- c("last_action_profit", "last_action_type", "days_since_last_touchpoint")


set_model_variables <- function(use_sale_preds = FALSE, use_one_back_preds = FALSE) {
  this_model_preds <- main_preds
  
  if (use_sale_preds==TRUE) {
    this_model_preds <- c(this_model_preds, sale_preds)
  } else if (use_one_back_preds==TRUE) {
    this_model_preds <- c(this_model_preds, one_back_preds)
  } else if (use_sale_preds==TRUE & use_one_back_preds==TRUE) {
    this_model_preds <- c(this_model_preds, sale_preds, one_back_preds)
  }
  
  this_model_preds
}