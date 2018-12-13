#' A sampling function
#'
#' This function creates randomly sampled three-point estimates
#' @export
#' sample_tp()

sample_tp <- function(x, y){

  df <- data_frame(ml = rnorm(100, x, y)) %>%
    mutate(max = ml * 1.25,
           min = ml * 0.75,
           time = 1:100)

  df

}
