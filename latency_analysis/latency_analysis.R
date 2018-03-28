library("jsonlite")
library("tidyverse")

results_dir <- "C:/Users/David_Jankoski/Desktop/opencpu_perftest/latency_test_results/chng_conf/"

file_pattern <- "latency_mr500*"
resfiles <- list.files(results_dir, pattern = file_pattern, full.names = TRUE)

names(resfiles) <-
  list.files(results_dir, pattern = file_pattern, full.names = FALSE) %>%
  stringr::str_extract("mr\\d+_con\\d+")

results <-
  resfiles %>%
  map_df(~ fromJSON(.x, simplifyVector = TRUE, flatten = TRUE), .id = "source")


results <-
  results %>%
  mutate(
    max_requests =
      as.integer(
        stringr::str_sub(
          stringr::str_extract(source, "mr\\d+"),
          start = 3L
        )
      ),

    concurrency =
      as.integer(
        stringr::str_sub(
          stringr::str_extract(source, "con\\d+"),
          start = 4L
        )
      )
  )


table(results$max_requests, results$concurrency,
      dnn = c("max_requests", "concurrency"))

# helper fun - find max but if -Inf ret 0
sum_or_zero <- function(err_col) {
  out <- max(err_col, na.rm = TRUE)
  ifelse(!is.finite(out), 0, out)
}
# helper fun - if col exists apply fun else NULL
# conditionally apply functiom
cond_apply_fun <- function(col, fun, env) {

  col <- substitute(col)
  colnm <- deparse(col)

  if (colnm %in% names(env$.top_env)) {
    res <- fun( eval(col, envir = env) )
  } else {
    res <- NA
  }
  res
}

res_by_con_maxreq <-
  results %>%
  group_by(concurrency, max_requests) %>%
  summarise(n_success = n(),
            err_1 =   cond_apply_fun(`errorCodes.-1`,  sum_or_zero, environment()),
            err_400 = cond_apply_fun(`errorCodes.400`, sum_or_zero,environment()),
            err_500 = cond_apply_fun(`errorCodes.500`, sum_or_zero,environment()),
            avg_response = mean(meanLatencyMs) / 1000,
            min_response = min(minLatencyMs) / 1000,
            max_response = max(maxLatencyMs) / 1000
  ) %>%
  mutate(
    tot_err = sum(err_1, err_400, err_500)
  ) %>%
  select(concurrency:err_500, tot_err, avg_response:max_response)


res_by_con <-
  results %>%
  group_by(concurrency) %>%
  summarise(n_tot = sum(unique(max_requests)),
            n_success = n(),
            perc_success = n_success / n_tot * 100,
            err_1 =   cond_apply_fun(`errorCodes.-1`, sum_or_zero, environment()),
            err_400 = cond_apply_fun(`errorCodes.400`, sum_or_zero, environment()),
            err_500 = cond_apply_fun(`errorCodes.500`, sum_or_zero, environment()),
            avg_response = mean(meanLatencyMs) / 1000,
            min_response = min(minLatencyMs) / 1000,
            max_response = max(maxLatencyMs) / 1000
  ) %>%
  mutate(
    tot_err = sum(err_1, err_400, err_500)
  ) %>%
  select(concurrency:err_500, tot_err, avg_response:max_response)


# lollipop chart
res_by_con %>%
  arrange(avg_response) %>%
  mutate(concurrency = factor(concurrency,unique(res_by_con$concurrency))) %>%
  ggplot(., aes(concurrency, avg_response)) +
  geom_segment(
    aes(x = concurrency, xend = concurrency,
        y = 0, yend = avg_response),
    color="#004358", size=1) +
  geom_point(color = "#FD7400", size = 4, alpha = 0.9) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("concurrency") +
  ylab("avg response time")


# show 1 concur
conc <- 30
results %>%
  filter(concurrency == conc) %>%

  ggplot(., aes(requestIndex, meanLatencyMs, colour = max_requests)) +
  geom_point() + geom_line(aes(group = source)) +
  scale_color_continuous(low = "green3", high = "red3") +
  labs(title = paste0("mean response times for different max num of requests with CONCURRENCY = ", conc))


# custom cols tryouts

firenze_pal <-
  swatches::read_ase("../../../custom_plotting/bubblegum.ase") %>%
  unname()
vitaminc_pal <-
  swatches::read_ase("../../../custom_plotting/vitaminc.ase") %>%
  unname()
bubblegum_pal <-
  swatches::read_ase("../../../custom_plotting/bubblegum.ase") %>%
  unname()
bubblegum_pal %>% swatches::show_palette()


# trelliscopes
results %>%
  filter(max_requests >= 500) %>%
  select(requestIndex, meanLatencyMs, max_requests, concurrency, `errorCodes.400`) %>%
  ggplot(., aes(requestIndex, meanLatencyMs, colour = factor(max_requests))) +
  geom_point() +
  geom_line(aes(group = max_requests)) +
  scale_colour_manual(values = bubblegum_pal) +
  trelliscopejs::facet_trelliscope( ~ concurrency)


results %>%
  #filter(max_requests >= 500) %>%
  ggplot(., aes(x = meanLatencyMs, y = factor(concurrency))) +
  ggridges::geom_density_ridges(scale = 4) +
  ggridges::theme_ridges() +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = "Distribution of response times",
    y = "Number of concurrent clients"
  )
