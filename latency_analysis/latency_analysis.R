library("tidyverse")
library("jsonlite")

results_dir <- "C:/Users/David_Jankoski/Desktop/opencpu_perftest/latency_analysis/results/"

file_pattern <- ".json$"
resfiles <- list.files(results_dir, pattern = file_pattern, full.names = TRUE)

names(resfiles) <-
  list.files(results_dir, pattern = file_pattern, full.names = FALSE) %>%
  stringr::str_extract(".*(?=\\.json)")

results <-
  resfiles %>%
  purrr::map_df(~ jsonlite::fromJSON(.x, simplifyVector = TRUE, flatten = TRUE), .id = "source")

results <-
  results %>%
  # separate out source col into indiv parts
  tidyr::separate(
    source,
    into = c("timestamp", "source", "max_requests", "concurrency"),
    sep = "_", remove = TRUE, convert = TRUE) %>%
  # conver timestamp to char (save space, unless needed further down?)
  mutate(
    timestamp = as.character(timestamp)
  )

# replace weird characters in col names ("errorCodes.-1")
err_cols <- grep("error", colnames(results))
colnames(results)[err_cols] <-
  gsub("[[:punct:]]", "",
       colnames(results)[err_cols]
  )


results %>%
  ggplot(., aes(requestIndex, meanLatencyMs, colour =  concurrency)) +
  geom_point() + geom_line(aes(group = concurrency)) +
  facet_wrap( ~ source) +
  scale_color_continuous(low = "green3", high = "red3") +
  labs(
    title = paste0("mean response times for different sources and concurrency")
  )

# stats for % successful requests, average of average latency
res_stats <-
  results %>%
  group_by(source, concurrency, max_requests) %>%
  summarise(
    n_success = n(),
    mean_latency_sec = mean(meanLatencyMs) / 1000
  ) %>%
  mutate(
    n_success_perc = n_success / max_requests * 100
  )
# stats for errors
errs <-
  results %>%
  group_by(source, concurrency, max_requests) %>%
  summarise_at(
    vars(contains("error")),
    funs(max)
  )

# join both stats dfs
latency_stats <-
  left_join(res_stats, errs, by = c("source", "concurrency", "max_requests"))

# add cpu & mem of docker containers
latency_stats <-
  latency_stats %>%
  mutate(
    specs =
      case_when(
        source == "hellotripv2-demo" ~ "1-1",
        source == "hellotripv2-demo2" ~ "1-4",
        source == "hellotripv2-demo3" ~ "1-8",
        source == "local" ~ "1-1",
        source == "local2" ~ "1-4",
        source == "local3" ~ "1-8"
      ),
    host = ifelse(grepl("local", source), "local", "azure")
  ) %>%
  tidyr::separate(specs, into = c("cpu", "mem"), sep = "-", remove = TRUE, convert = TRUE)


ggplot(latency_stats, aes(concurrency, mean_latency_sec, colour = host)) +
  geom_point() +
  geom_line(aes(group = source)) +
  scale_x_continuous(breaks = 1:12, limits = c(1,12)) +
  scale_y_continuous(breaks = seq(0,18,2), limits = c(0,18)) +
  facet_wrap( ~ cpu + mem,
              labeller = function(x) list(paste("Cpu:", x$cpu, "Mem:", x$mem))
  ) +
  labs(
    title = "Average response times (sec) between different cpu/mem configs"
  ) +
  scale_colour_brewer(palette = "Dark2")





# rethink -------



# helper fun - find max but if -Inf ret 0
# to avoid this
# max(rep(NA_real_, 10), na.rm = T)
inf_to_zero <- function(err_col) {
  out <- max(err_col, na.rm = TRUE)
  ifelse(!is.finite(out), 0, out)
}

# helper fun - if col exists apply fun else NULL
# conditionally apply function
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

min_if_noerr <- function(err_col, lat_col) {

  min(lat_col[is.na(err_col)]) / 1000

}
# summarise stats by source, max requests and concurrency level
res_by_con_maxreq <-
  results %>%
  group_by(source, concurrency, max_requests) %>%
  summarise(n_success = n(),
            err_1 =   cond_apply_fun(`errorCodes.-1`,  inf_to_zero, environment()),
            err_400 = cond_apply_fun(`errorCodes.400`, inf_to_zero,environment()),
            err_500 = cond_apply_fun(`errorCodes.500`, inf_to_zero,environment()),
            avg_response = mean(meanLatencyMs) / 1000,
            min_response = min(minLatencyMs) / 1000,
            min_response2 = min_if_noerr(err_1, minLatencyMs),
            max_response = max(maxLatencyMs) / 1000
  ) %>%
  mutate(
    tot_err = sum(err_1, err_400, err_500)
  ) %>%
  select(concurrency:err_500, tot_err, avg_response:max_response)

# summarise stats by source, concurrency level
res_by_con <-
  results %>%
  group_by(source, concurrency) %>%
  summarise(n_tot = sum(unique(max_requests)),
            n_success = n(),
            perc_success = n_success / n_tot * 100,
            err_1 =   cond_apply_fun(`errorCodes.-1`, inf_to_zero, environment()),
            err_400 = cond_apply_fun(`errorCodes.400`, inf_to_zero, environment()),
            err_500 = cond_apply_fun(`errorCodes.500`, inf_to_zero, environment()),
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
