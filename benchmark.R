# Title     : Honours Programme
# Objective : Comparing dimensionality reduction algorithms for mixed data
# Created by: Alessandro Serpi
# Created on: 2020-10-15

library("tidyverse")
library("ggfortify")
library("autoplotly")
library("bench")

source("dim_red_utils.R")
source("mixed_datasets.R")


extract_gc <- function(results, names, n_iter) {
  result_gc <- NULL
  for (r in 1:n_iter) {
    for (a in seq_along(names)) {
      i <- results$gc[[a]][r,]
      if (i["level2"] > 0) {
        result_gc <- rbind(result_gc, "2")
      } else if (i["level1"] > 0) {
        result_gc <- rbind(result_gc, "1")
      } else if (i["level0"] > 0) {
        result_gc <- rbind(result_gc, "0")
      } else {
        result_gc <- rbind(result_gc, "none")
      }
    }
  }

  result_gc
}


bench_samples <- function(data_name, n_dims, n_samples, n_iter, n_reps) {
  main_dataset <- do.call(data_name, list())
  reps <- 1:n_reps
  samples <- floor(nrow(main_dataset) / n_samples) * 1:n_samples
source_filename <- "out/results_${data_name}_${sample}_${rep}.rds"

message("Repetition\t\tSamples\t\t\tTime")
for (rep in reps) {
  for (sample in samples) {
    message(rep, "\t\t\t\t", sample, "\t\t\t", Sys.time())
    data <- main_dataset[sample(nrow(main_dataset), sample),]
    data <- data[data %>%  # Select only columns which do not contain all identical cells
                   summarise_all(~n_distinct(.)) %>%
                   select_if(. != 1) %>%
                   colnames()
    ]

    results <- mark(
      ade4 = ade4_wrapper(data, n_dims),
      FAMD = famd_wrapper(data, n_dims),
      PCAmixdata = pcamix_wrapper(pcamix_pre(data), n_dims),
      PCA_1hot = pca_one_hot_wrapper(pca_one_hot_pre(data)),
        iterations = n_iter,
      check = FALSE,
      filter_gc = FALSE,
      relative = FALSE,
        time_unit = "ms"
      )
      saveRDS(results, file = str_interp(source_filename))

      # Remove old elements from memory
      rm(data)
    rm(results)
    gc()
    }
  }

  message("Finalising...")
  all_times <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(all_times) <- c("samples", "algorithm", "exec_time", "gc")
  for (rep in reps) {
    for (sample in samples) {
      results <- readRDS(file = str_interp(source_filename))

    names <- attr(results$expression, "description")
    result_times <- do.call(cbind, results$time)
    colnames(result_times) <- names
    result_times <- pivot_longer(
      data.frame(result_times),
      cols = all_of(names),
      names_to = "algorithm",
      values_to = "exec_time",
      names_transform = list(algorithm = ~parse_factor(.x, levels = names))
    )

      result_gc <- extract_gc(results, names, n_iter)
      all_times <- rbind(all_times, cbind(sample, result_times, result_gc))
  }
}

write_result(all_times, "caravan", "all_times")
p <- ggplot(all_times, aes(x = algorithm, y = exec_time, fill = samples)) +
  geom_violin(trim = TRUE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Execution times", x = "Algorithm", y = "Execution time [s]") +
  theme(legend.position = "none")
autoplotly(p)
}
