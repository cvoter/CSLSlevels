#' Predict lake levels w/BSTS method
#'
#' Function contains Bob Smail's code for predicting lake levels using Bayseian
#' Structural Time Series.
#'
#' @param study_lakes input data frame for BSTS calcs. Comes from
#'                    "//dnr/programs/DG/DG_Projects/Water Use/WU_Central_Sands_Study/data_mgmt/hist_lake_levs/study_lakes_pred_levs_20200302.csv"
#' @param lake_name name of lake to process. Defaults to "Long Lake".
#' @param start_date first date to include in estimated lake level timeseries.
#'                   Defaults to "1938-01-01".
#' @param seed randomization seed for bsts. Defaults to 2017.
#'
#' @importFrom bsts AddSemilocalLinearTrend AddAr AddSeasonal bsts
#' @importFrom dplyr filter select mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats sd
#'
#' @export

predict_lake_levels <- function(study_lakes,
                                lake_name = "Long Lake",
                                start_date = "1938-01-01",
                                seed = 2017) {

  # Extract Dates
  obs_mo_index        <- study_lakes %>%
                         filter(lake_name == !!lake_name) %>%
                         select(.data$obs_mo) %>%
                         mutate(obs_mo = as.POSIXct(.data$obs_mo))
  obs_mo_index$t_step <- seq(1, nrow(obs_mo_index), 1)
  start_t_step        <- obs_mo_index$t_step[obs_mo_index$obs_mo == start_date]

  # Filter to lake of interest
  this_lake <- study_lakes %>%
               filter(lake_name == !!lake_name) %>%
               select(.data$lake_lev_obs, .data$lake_lev_pred, .data$ppt_cdm_z)

  # Add components to BSTS model
  state_spec <- AddSemilocalLinearTrend(list(), y = this_lake$lake_lev_pred)
  state_spec <- AddAr(state_spec, this_lake$lake_lev_pred)
  state_spec <- AddSeasonal(state_spec, this_lake$lake_lev_pred, nseasons = 12)

  # Run Monte Carlo simulations, assume the first 1000 are not useful
  fit1           <- bsts(lake_lev_obs ~ .,
                         state_spec,
                         niter = 5000,
                         data = this_lake,
                         expected.model.size = 2,
                         seed = seed)
  state_contribs <- fit1$state.contributions[1000:5000,,]

  # Turn results into data frame, add contributions to get estimated lake level
  lake_pred <- NULL
  for (i in 1:dim(state_contribs)[1]) {
    this_iter           <- as.data.frame(t(state_contribs[i,,]))
    rownames(this_iter) <- NULL
    this_iter$t_step    <- seq(1:nrow(this_iter))
    this_iter$sim       <- i
    lake_pred[[i]]      <- this_iter
  }
  lake_pred       <- bind_rows(lake_pred)
  lake_pred$level <- rowSums(lake_pred[,1:4])

  # Filter out data prior to first observation
  lake_pred <- lake_pred %>%
               filter(.data$t_step >= start_t_step)

  # Find the mean & standard deviation at each time step
  lake_stats <- lake_pred %>%
                group_by(.data$t_step) %>%
                summarise(mean = mean(.data$level),
                          sd = sd(.data$level),
                          p90 = quantile(.data$level, probs = 0.9),
                          p10 = quantile(.data$level, probs = 0.1)) %>%
                ungroup()
  lake_pred  <- left_join(lake_pred, lake_stats, by = "t_step") %>%
                mutate(SE = (.data$level - .data$mean)^2)

  # Rank models by how close they are to the mean at each time step
  sim_err      <- lake_pred %>%
                  group_by(.data$sim) %>%
                  summarise(RMSE = sqrt(mean(.data$SE))) %>%
                  ungroup() %>%
                  arrange(.data$RMSE)
  sim_err$rank <- seq(1, nrow(sim_err), 1)
  lake_pred    <- left_join(lake_pred, sim_err, by = "sim")


  # Join months
  lake_pred <- left_join(lake_pred, obs_mo_index) %>%
               mutate(lake_name = lake_name) %>%
               select(.data$lake_name, .data$obs_mo, .data$sim, .data$level,
                      .data$mean, .data$sd, .data$p90, .data$p10, .data$SE,
                      .data$RMSE, .data$rank)
  return(lake_pred)
}
