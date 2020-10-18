# Title     : Honours Programme
# Objective : Comparing dimensionality reduction algorithms for mixed data
# Created by: Alessandro Serpi
# Created on: 2020-10-15

library("tidyverse")
library("ggfortify")
library("autoplotly")

source("dim_red_utils.R")
source("mixed_datasets.R")

data_name <- "caravan"
n_dims <- Inf


data <- do.call(data_name, list())
result <- bench::mark(
  ade4 = ade4_wrapper(data, n_dims),
  FAMD = famd_wrapper(data, n_dims),
  PCAmixdata = pcamix_wrapper(pcamix_pre(data), n_dims),
  PCA_1hot = pca_one_hot_wrapper(pca_one_hot_pre(data)),
  iterations = 10,
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

p <- ggplot(result_times, aes(x = algorithm, y = exec_time, fill = algorithm)) +
  geom_violin(trim = TRUE) +
  geom_boxplot(colour = "black", fill = "transparent", width = .5) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Execution times", x = "Algorithm", y = "Execution time [s]") +
  theme(legend.position = "none")
autoplotly(p)

write_result(result_times, data_name, "times_2")
