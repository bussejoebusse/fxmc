#' A sampling function
#'
#' This function creates randomly sampled three-point estimates
#' @export
#' sample_tp

sample_tp <- function(n, x){

  df <- data_frame(time = 1:n,
                   ml = rnorm(n, x, 0.1)) %>%
    mutate(max = ml * 1.25,
           min = ml * 0.75)

  df

}
