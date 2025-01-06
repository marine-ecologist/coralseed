


#' code for reproducing draws used in coralseed models from infamis tile data
#' brms models and n=1000 draws
#'
#'#'
#' @format A data frame with 100 rows and 3 variables:
#' \describe{
#'   \item{id}{Identifier for each simulation run}
#'   \item{minutes}{Time in minutes}
#'   \item{settlement_probability}{Probability of settlement}
#' }
#' @source See `data/generate_draws.R` for details on data generation.
#' @examples
#' \dontrun{
#` infamis_tiles_draws <- data.frame(
#`   tile = c('16E', '16E', '16E', '16E', '16E', '16E', '16E', '16D', '16D', '16D', '16D', '16D', '16D', '16D', '16C', '16C', '16C', '16C', '16C', '16C', '16C', '16B', '16B', '16B', '16B', '16B', '16B', '16A', '16A', '16A', '16A', '16A', '16A'),
#`   time = c(620, 360, 300, 255, 180, 120, 60, 620, 360, 300, 255, 180, 120, 60, 620, 360, 300, 255, 180, 120, 60, 620, 300, 255, 180, 120, 60, 620, 300, 255, 180, 120, 60),
#`   count = c(22, 20, 16, 15, 12, 6, 4, 25, 23, 23, 22, 19, 16, 7, 28, 23, 21, 21, 17, 14, 6, 21, 12, 12, 9, 6, 2, 8, 7, 6, 6, 4, 2)) |>
#`   dplyr::mutate(competent=100-count) %>%
#`   dplyr::mutate(tile=as.factor(tile)) |>
#`   dplyr::mutate(time = time)
#'
#' infamis_tiles_long <- rbind(
#'   infamis_tiles %>%
#'     dplyr::select(tile, time, count) %>%
#'     tidyr::uncount(count, .remove = FALSE) %>%
#'     dplyr::mutate(settled = 1) %>%
#'     dplyr::select(-count),
#'   infamis_tiles %>%
#'     dplyr::select(tile, time, count) %>%
#'     tidyr::uncount(100 - count, .remove = FALSE) %>%
#'     dplyr::mutate(settled = 0) %>%
#'     dplyr::select(-count)
#' )
#'
#' ### exponential distribution
#' brms_output_exp <- brms::brm(time | cens(1 - settled) ~ (1 | w | tile),
#'                          family =  exponential, init = 0,
#'                          control = list(adapt_delta = 0.99, max_treedepth = 20),
#'                          cores=8, chains=4, iter = 10000, data = infamis_tiles_long)
#'
#' parameter_draws_exp <- brms::as_draws_df(brms_output_exp) %>%
#'   dplyr::slice_sample(n = 1000)
#' usethis::use_data(parameter_draws_exp, overwrite=TRUE)
#'
#'
#'
#' saveRDS(draws_exp, "data/draws_exp.Rds")
#'
#' ### lognormal distribution
#' brms_output_log <- brms::brm(time | cens(1 - settled) ~ (1 | w | tile),
#'                          family =  lognormal, init = 0,
#'                          control = list(adapt_delta = 0.99, max_treedepth = 20),
#'                          cores=8, chains=4, iter = 10000, data = infamis_tiles_long)
#'
#' parameter_draws_log <- brms::as_draws_df(brms_output_log) %>%
#'   dplyr::slice_sample(n = 1000)
#'
#' usethis::use_data(parameter_draws_log, overwrite=TRUE)
#'
#'
#' ### weibull distribution
#' brms_output_weibull <- brms::brm(time | cens(1 - settled) ~ (1 | w | tile),
#'                              family =  weibull, init = 0,
#'                              control = list(adapt_delta = 0.99, max_treedepth = 20),
#'                              cores=8, chains=4, iter = 10000, data = infamis_tiles_long)
#'
#' parameter_draws_weibull <- brms::as_draws_df(brms_output_weibull) %>%
#'   dplyr::slice_sample(n = 1000)
#'
#' usethis::use_data(parameter_draws_log, overwrite=TRUE)
#'
#'}
