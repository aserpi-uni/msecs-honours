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


bench_features <- function(data_name, n_dims, n_feats, n_iter, n_reps, balance=TRUE) {
  main_dataset <- do.call(data_name, list())
  reps <- 1:n_reps
  factors <- main_dataset %>%
    sapply(is.factor) %>%
    Filter(function (x) x, .) %>%
    names()
  not_factors <- setdiff(colnames(main_dataset), factors)

  # Take into account imbalanced datasets
  if (balance) {
    min_feats <- min(length(factors), length(not_factors))
    cat_feats <- num_feats <- floor(min_feats / n_feats) * 1:n_feats
    source_filename <- "out/results_${data_name}_${cat_feat}_${num_feat}_${rep}_bal.rds"
  } else {
    cat_feats <- floor(length(factors) / n_feats) * 1:n_feats
    num_feats <- floor(length(not_factors) / n_feats) * 1:n_feats
    source_filename <- "out/results_${data_name}_${cat_feat}_${num_feat}_${rep}.rds"
  }

  message("Repetition\tCat.feat.\tNum.feat.\tTime")
  for (rep in reps) {
    for (cat_feat in cat_feats) {
      for (num_feat in num_feats) {
        message(rep, "\t\t\t", cat_feat, "\t\t\t", num_feat, "\t\t\t", Sys.time())
        data <- main_dataset[, union(sample(factors, cat_feat), sample(not_factors, num_feat))]

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
  }

  message("Finalising...")
  all_times <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(all_times) <- c("cat_feat", "num_feat", "algorithm", "exec_time", "gc")
  for (rep in reps) {
    for (cat_feat in cat_feats) {
      for (num_feat in num_feats) {
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
        feats <- data.frame( t(c(cat_feat, num_feat)))
        colnames(feats) <- c("cat_feat", "num_feat")
        all_times <- rbind(all_times, cbind(feats, result_times, result_gc))
      }
    }
  }

  all_times$cat_feat <- as.integer(all_times$cat_feat)
  all_times$num_feat <- as.integer(all_times$num_feat)
  if (balance) {
    write_result(all_times, "caravan", "features_times_bal")
  } else {
    write_result(all_times, "caravan", "features_times")
  }

  p <- ggplot(all_times, aes(algorithm, exec_time, color = result_gc)) +
    ggbeeswarm::geom_beeswarm() +
    facet_grid(rows = vars(cat_feat), cols = vars(num_feat))
  autoplotly(p)
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
      data <- slice_sample(main_dataset, sample)
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

  all_times$sample <- as.integer(all_times$sample)
  write_result(all_times, data_name, "samples_times")
  p <- ggplot(all_times, aes(algorithm, exec_time, color = result_gc)) +
    ggbeeswarm::geom_beeswarm() +
    facet_wrap(vars(sample), ncol = 3)
  autoplotly(p)
}


# Uncomment to run
# bench_features("caravan", Inf, 3, 15, 20)
# bench_samples("caravan", Inf, 9, 15, 20)
