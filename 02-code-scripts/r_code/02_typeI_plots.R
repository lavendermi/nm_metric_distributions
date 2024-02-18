### Save size: 3.75 x 8.0
# This script analyses the data that is used to produce Figure 1.

rm(list = ls(all = TRUE))
library(lattice)
library(tidyverse)
library(viridis)
library(lubridate)
source("../../02-code-scripts/r_code/common_code.R")

trellis.par.set(canonical.theme(color = FALSE))

df.typeI <- readRDS(file = "../../01-raw_data/typeI/df.typeI.rds")

# set the universal alpha level for all of the tests
alpha <- 1 - 0.05

# determine the significance of the null model test. This is simple proportion
# assessment. That is, if our observed metric is more extreme (in the tail) than
# 95% of the values in the null distribution we set it to "1" to indicate that
# it is a significant result. NOTE: this includes NA, NaN and Infinity values in
# the null model. What I mean by this is that if we have a list of null metric
# values: c(1, 2, 1, 3, 1, NA, NaN, 4, Inf) the proportion includes Infinity
# values but not NaN values. So I think what we want to do is exclude or fail
# any nm x metric combinations that yield NaN values. These will be any that
# have ne_NaN_count values < 1000

# If both lt and gt are == 0 then the metric x null model combination never works (NaN)
# If ne_NaN_count < 1000 then the metric x null doesn't work sometimes.

# This code set a flag for significance.
#     1 => is significant
#     0 => not significant
#     NA => there is a problem with nm x metric combo
df.typeI$sig <- ifelse(df.typeI$lt > alpha | df.typeI$gt > alpha, 1,
  ifelse((df.typeI$lt == 0 & df.typeI$gt == 0) | df.typeI$ne_NaN_count < 950, NA, 0)
)

# set things up to run a binomial test to determine if proportion of significant
# tests is greater than 0.05. What we are doing is checking to see if the number
# of significant tests for each combination of null model x metric x
# distribution is significant.
row_count <- length(nm_list) * length(ordered_metrics) * length(dist_list)

p_success <- numeric(length = row_count)
sig <- numeric(length = row_count)
n_list <- character(length = row_count)
m_list <- character(length = row_count)
d_list <- character(length = row_count)
counter <- 1

pb <- txtProgressBar(min = 0, max = row_count, initial = 0, style = 3)

for (n_m in nm_list) {
  sub0 <- subset(df.typeI, nm == n_m)
  for (m in ordered_metrics) {
    sub1 <- subset(sub0, metric == m)
    for (d in dist_list) {
      subs <- subset(sub1, dist == d)
      setTxtProgressBar(pb, counter)

      if (sum(is.na(subs$sig)) == 0) { # only do this for nm x metric without problems
        bi_res <- binom.test(
          x = sum(subs$sig, na.rm = T),
          n = 1000,
          p = 0.05,
          alternative = "greater"
        )

        p_success[counter] <- bi_res$estimate
        sig[counter] <- ifelse(bi_res$p.value < 0.05, 1, 0) # 1 means more sig tests than desired
        n_list[counter] <- n_m
        m_list[counter] <- m
        d_list[counter] <- d
      } else {
        p_success[counter] <- NA
        sig[counter] <- NA # Set this NA so we can exclude combos from plots
        n_list[counter] <- n_m
        m_list[counter] <- m
        d_list[counter] <- d
      }
      counter <- counter + 1
    }
  }
}

close(pb)

# Store the results of the binomial tests in a data frame and set the data types
bi_test_I <- data.frame(nm = as.factor(n_list), metric = as.factor(m_list), dist = as.factor(d_list), sig, p_success)
bi_test_I$metric <- factor(bi_test_I$metric, levels = ordered_metrics)
bi_test_I$dist <- factor(bi_test_I$dist, levels = dist_list)
bi_test_I$nm <- factor(bi_test_I$nm, levels = rev(nm_list))

# calculate the proportion of each combination of nm x metric x dist that produce significant nm tests.
# in this case significant is bad (type I error) so they should be below 5%
typI.sig <- aggregate(sig ~ metric + nm + dist,
  FUN = function(x) mean(x),
  data = df.typeI
)

##### okay let's plot things!
plot_data <- left_join(bi_test_I, typI.sig, 
  by = c("metric", "nm", "dist")
) %>%
  select(metric, nm, dist, sig = sig.x, p_success)

plot_data <- merge(plot_data, display_code_df, all.x = T)

plot_data$display <- factor(plot_data$display, levels = ordered_metrics_plots)


# write.csv(typI.sig, file = "typeI_rates.csv")

quartz(type = 'pdf', 
       file = '../../04-results/figure_01.pdf',
       width = 11,
       height = 8)

ggplot(plot_data, aes(x = display, y = nm)) +
  geom_tile(aes(
    fill = p_success,
    width = 0.925,
    height = 0.925
  )) +
  scale_fill_viridis_c(
    na.value = "white",
    limits = c(0, 1),
    alpha = 0.9
  ) +
  geom_point(
    aes(
      color = as.factor(sig),
      shape = as.factor(sig)
    ),
    show.legend = F,
    size = 2.5
  ) +
  # geom_rect(mapping=aes(xmin=0.5, xmax=4.5, ymin=1.5, ymax=10.5), colour = "red", alpha = 0.5, fill="") +
  scale_colour_viridis_d(direction = -1, ) +
  scale_shape_manual(values = c(19, NA)) +
  labs(fill = "") +
  xlab("Co-occurrence metric") +
  ylab("Null model algorithm") +
  theme_bw() +
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 90),
    axis.text.x.bottom = element_text(vjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  # facet_grid(vars(dist))
  facet_wrap(~dist, nrow = 3)

dev.off()

# ggsave(
#   filename = "../../04-results/figure_01.pdf",
#   plot = last_plot(),
#   device = "pdf",
#   scale = 1,
#   width = 11,
#   height = 8,
#   units = "in"
# )

plot_data %>% 
  dplyr::count(sig == 0)

uniques <-
  plot_data %>%
  filter(sig == 0) %>%
  select(nm, metric) %>%
  distinct()

total_poss_comb <- 85 * 10 * 3
