#' A Monte Carlo simulation function
#'
#' This function runs Monte Carlo simulations over a series of three-point estimates
#' @export
#' simulate_tp

simulate_tp <- function(x){

  apply_tp <- function(n){

    sample <- x %>%
      dplyr::filter(time == n)

    sample <- data_frame(sim = triangle::rtriangle(100, sample$min, sample$max, sample$ml)) %>%
      dplyr::mutate(sim_id = 1:100) %>%
      tidyr::spread(sim_id, sim)
  }

  simulations <<- purrr::map_dfr(1:100, apply_tp, .id = "t") %>%
  dplyr::mutate(t = as.numeric(t)) %>%
  dplyr::rename_at(2:101, ~ c(paste0("sim_", 1:100))) %>%
  tidyr::gather("sim", "tp_estimate", 2:101)

  avg_simulation <<- simulations %>%
  dplyr::group_by(t) %>%
  dplyr::summarise(avg_estimate = mean(tp_estimate)) %>%
  dplyr::arrange(t)

  df_plot <- df %>%
  tidyr::gather("type", "estimate", 1:3)

  simulation_plot <<-
  ggplot2::ggplot() +
  geom_line(data = simulations,
            aes(t, tp_estimate, group = sim, colour = sim),
            show.legend = F,
            alpha = 0.1)+
  geom_line(data = avg_simulation, aes(t, avg_estimate, group = 1))+
  geom_line(data = df_plot, aes(time, estimate, group = type), linetype = 2)

}
