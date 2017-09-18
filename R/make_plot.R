#
#
# plot_type <- function(type) {
#
# }
#
# # for each of the endoints, pipe each single_param_endpoint through
# # as .x, so both as the second half of the get_<ep> function name
# # and the second argument of the get_ function defined above (so the ep in the fromJSON() call)
#
# single_param_endpoints %>% walk(~ assign(x = paste0("get_", .x),
#                                          value = partial(get_, ep = .x),
#                                          envir = .GlobalEnv))
#
# make_plot <- function(d, var, type = "density") {
#
#   plot_type <- paste0("geom_", type)
#
#   tabl <- d %>%
#     count_(var);
#   ggplot(d) + plot_type(aes_string(var), stat = "count") + theme_minimal() +
#     ggtitle(paste0("Breakdown by ", cap_it(var))) +
#     labs(x = cap_it(var), y = "Count")
# }
#
# make_plot(mtcars, "carb")
