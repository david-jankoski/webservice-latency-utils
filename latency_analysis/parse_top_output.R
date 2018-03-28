library("tidyverse")

top_output <-
  readLines(
    paste0("C:/Users/David_Jankoski/Desktop/opencpu_perftest/latency_test_results/latency_analysis/",
           "top_output_conc_10.csv")
  )

get_top_metrics <- function(top_output) {

top_output <- readLines(top_output)

splits <- grep(pattern = "top - ", top_output)

groups <- findInterval(seq_along(top_output), splits)
groups <- split(top_output, groups)

# parse top output helper fun ------
prog_bar <- progress_estimated(length(groups))

parse_top_output <- function(z, z_idx) {


  cat("\nProcessing: ", z_idx, "\n")
  prog_bar$pause(0.1)$tick()$print()

  # exit early if output incomplete
  col_idx <- grep("PID USER", z)

  if ( (length(z) == col_idx) || (z[col_idx + 1L] == "") ) return(NULL)

  timestamp <-
    stringr::str_extract(z[1], "top - \\d{2}:\\d{2}:\\d{2}") %>%
    stringr::str_replace("top - ", "") %>%
    strptime(format = "%H:%M:%S") %>%
    as.POSIXct()

  tasks_running <-
    z[grep("^Tasks:", z)] %>%
    stringr::str_extract("\\d+ running") %>%
    stringr::str_replace(" running", "") %>%
    as.numeric()

  cpus <-
    z[grep("^%Cpu\\d", z)] %>%
    stringr::str_extract("\\d+\\.\\d+(?= us)") %>%
    as.numeric() %>%
    purrr::set_names(nm = paste0("Cpu", as.character(0:7)))

  memory <-
    z[ grep("^KiB Mem", z) ] %>%
    stringr::str_extract_all("\\d+") %>%
    unlist() %>%
    as.numeric() %>%
    set_names(nm = c("total", "free", "used", "buff_cache"))

  swap <-
    z[ grep("^KiB Swap", z) ] %>%
    stringr::str_extract_all("\\d+") %>%
    unlist() %>%
    as.numeric() %>%
    set_names(nm = c("total", "free", "used", "avail_mem"))


  col_nms <-
    z[col_idx] %>%
    strsplit("\\s+") %>%
    unlist() %>%
    .[-1L] %>%
    stringr::str_replace_all("^%+|\\+$", "")

  # small hack when top output is only 1 line long
  zend <- max((length(z)-1L), col_idx)

  data <-
    z[ seq.int(col_idx + 1L, zend, 1L) ] %>%
    stringr::str_replace_all("\\s+", ",") %>%
    stringr::str_sub(start = 2L)

  if (length(data) == 1L) data <- paste0(data,"\n")

  data <-
    data %>%
    stringr::str_split(",", simplify = TRUE) %>%
    as.data.frame()

  # this failed when 1st row has diff (single) CMD
  # data <-
  #   data %>%
  #   paste(collapse = "\n") %>%
  #   readr::read_csv(col_names = FALSE)

  colnames(data) <- col_nms

  # get the COMMAND and SWAP cols in order
  # they are mangled up a bit
  unite_cmd_cols <- seq(grep("COMMAND", col_nms), length.out = 3L)
  colnames(data)[unite_cmd_cols] <- paste0("COMMAND", 1:3)
  colnames(data)[length(data)] <- "SWAP"

  data <-
    data %>%
    tidyr::unite(col = "COMMAND", starts_with("COMMAND"), sep = " ") %>%
    mutate(
      PID = as.integer(PID),
      timestamp = timestamp,
      tasks_running = tasks_running,

      memory_total = memory["total"],
      memory_free = memory["free"],
      memory_used = memory["used"],
      memory_buff_cache = memory["buff_cache"],

      swap_total = swap["total"],
      swap_free = swap["free"],
      swap_used = swap["used"],
      swap_avail_mem = swap["avail_mem"]
    )

  data <- cbind(data, as.data.frame(as.list(cpus)))

  data
}

safe_parse_top_output <- purrr::safely(parse_top_output)

top_df <-
  groups %>%

  # 1) - This is more fragile -
  # imap_dfr(parse_top_output, .id = "top_id") %>%
  # tibble::as_tibble()

  # 2) - In case of problems, more robust way -
  imap(safe_parse_top_output) %>%
  map("result") %>%
  bind_rows(.id = "top_id")

top_df <-
  top_df %>%
  mutate_at(
    vars(top_id, VIRT, RES, SHR),
    as.integer
  ) %>%
  mutate_at(
    vars(CPU, MEM),
    as.numeric
  ) %>%
  mutate(
    # add a only hms col as dates are all same
    timestamp_hms = hms::as.hms(timestamp),
    # convert Mebibytes to Megabytes memory
    memory_used_mb = memory_used * 0.001024,
    swap_avail_mem_mb = swap_avail_mem * 0.001024
  )

# Memory used by measurement unit (?) ------
# top_df %>%
#   group_by(top_id) %>%
#   summarise(
#     memory_used = list(unique(memory_used))
#   )

# CPUs by measurement unit -----
cpus_by_id <-
  top_df %>%
  group_by(top_id) %>%
  slice(1) %>%
  select(top_id, starts_with("Cpu")) %>%
  tidyr::gather(key = "cpu_num", value = "cpu_usg", -top_id)

plot_cpus_by_id <-
  ggplot(cpus_by_id, aes(top_id, cpu_usg, colour = cpu_num)) +
  geom_point() +
  geom_line(aes(group = cpu_num)) #+ coord_flip() + scale_x_reverse()

# plotly::ggplotly()

# CPUs by timestamp -----
# top_df %>%
#   group_by(top_id) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(timestamp_hms, starts_with("Cpu")) %>%
#   tidyr::gather(key = "cpu_num", value = "cpu_usg", -timestamp_hms) %>%
#
#   ggplot(., aes(timestamp_hms, cpu_usg, colour = cpu_num)) +
#   geom_point() +
#   geom_line(aes(group = cpu_num))

# Memory used by measurement unit -----
plot_mem_used <-
  top_df %>%
  ggplot(., aes(timestamp_hms, memory_used_mb)) +
  geom_point() + geom_jitter() +
  geom_line(aes(group = 1)) +
  theme(legend.position = "none") +
  scale_colour_gradient() +
  geom_vline(xintercept = unique(top_df$timestamp_hms), linetype = "dashed", alpha = 0.1)


# TOP PID and usage (?) ------
# PIDS_by_topid <-
#   top_df %>%
#   group_by(PID) %>%
#   summarise(count = n_distinct(top_id))
#
# top_pid <-
#   PIDS_by_topid %>%
#   slice(which.max(count)) %>%
#   pull(PID) %>%
#   {filter(top_df, PID == .)}

out <-
  list(
    top_df = top_df,
    cpus_by_id = cpus_by_id,
    plot_cpus_by_id = plot_cpus_by_id,
    plot_mem_used = plot_mem_used
  )

out
}

