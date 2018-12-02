df <- data_frame(ml = rnorm(100, 1.3, 0.01)) %>%
  mutate(max = ml + 0.1,
         min = ml - 0.1) %>%
  mutate(time = 1:100)
