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


data_name <- "caravan"
n_dims <- Inf

main_dataset <- do.call(data_name, list())
all_times <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(all_times) <- c("algorithm", "samples", "exec_time")

all_results <- press(
  samples = floor(nrow(main_dataset)/9) * 1:9,
  rep = 1:20,
  {
    data <- main_dataset[sample(nrow(main_dataset), samples),]
    data <- data[data %>%  # Select only columns which do not contain all identical cells
                   summarise_all(~n_distinct(.)) %>%
                   select_if(. != 1) %>%
                   colnames()
    ]
    result <- mark(
      ade4 = ade4_wrapper(data, n_dims),
      FAMD = famd_wrapper(data, n_dims),
      PCAmixdata = pcamix_wrapper(pcamix_pre(data), n_dims),
      PCA_1hot = pca_one_hot_wrapper(pca_one_hot_pre(data)),
      iterations = 15,
      check = FALSE,
      filter_gc = TRUE,
      relative = FALSE,
      time_unit = "ms"
    )

    names <- attr(result$expression, "description")
    result_times <- do.call(cbind, result$time)
    colnames(result_times) <- names
    result_times <- pivot_longer(
      data.frame(result_times),
      cols = all_of(names),
      names_to = "algorithm",
      values_to = "exec_time",
      names_transform = list(algorithm = ~parse_factor(.x, levels = names))
    )

    all_times <- rbind(all_times, cbind(samples, result_times))
    write_result(result_times, data_name, str_interp("${samples}_${rep}_times"))

    save(result, str_interp("out/results_${data_name}_${samples}_${rep}.Rda"))
    result
  }
)

save(all_results, "out/all_results.Rda")
write_result(all_times, data_name, "times")

p <- ggplot(all_times, aes(x = algorithm, y = exec_time, fill = samples)) +
  geom_violin(trim = TRUE) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Execution times", x = "Algorithm", y = "Execution time [s]") +
  theme(legend.position = "none")
autoplotly(p)
