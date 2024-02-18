### Save size: 7.0 x 7.0
library(plyr)
library(tidyverse)
library(viridis)
source("../../02-code-scripts/r_code/common_code.R")

# typeI_plots.R needs to be run before we run this
# source("typeI_plots.R")

df.typeII <- readRDS(file = "../../01-raw_data/typeII/df.typeII.rds")

# we want to skip looking at anything that failed the type I. This will help
# keep the plots easier to read and prevent confusion (bias) over combos that
# "appear" to do well for type II despite failing the type I. To do this we
# merge the result of the Type I with the type II binomial tests which gives us
# access to the results.
df.typeII <- merge(df.typeII, bi_test_I, by = c("metric", "nm", "dist"))

# reduce the data to only those combos that passed the type I binomial tests
working.typeII <- droplevels(subset(df.typeII, sig == 0))

# set the p-critical value for all tests (including binomial)
p_crit <- 0.80

# This code set a flag for significance.
#     1 => is significant
#     0 => not significant
#     NA => there is a problem with nm x metric combo

working.typeII$sig.II <- ifelse(working.typeII$sig == 1, 0,
  ifelse(working.typeII$lt == 0 & working.typeII$gt == 0, NA,
    ifelse(working.typeII$metric %in% neg_metrics &
      working.typeII$dir %in% c("negative") & working.typeII$lt > p_crit, 1,
    ifelse(working.typeII$metric %in% neg_metrics &
      working.typeII$dir %in% c("positive") & working.typeII$gt > p_crit, 1,
    ifelse(working.typeII$metric %in% pos_metrics &
      working.typeII$dir %in% c("positive") & working.typeII$lt > p_crit, 1,
    ifelse(working.typeII$metric %in% pos_metrics &
      working.typeII$dir %in% c("negative") & working.typeII$gt > p_crit, 1, 0)
    )
    )
    )
  )
)

#####
# Use binomial test to determine if proportion of significant tests is greater than p-crit
row_count <- length(unique(working.typeII$nm)) *
  length(unique(working.typeII$metric)) *
  length(unique(working.typeII$dist)) *
  length(unique(working.typeII$dir))

p_success <- numeric(length = row_count)
sig <- numeric(length = row_count)
n_list <- character(length = row_count)
m_list <- character(length = row_count)
d_list <- character(length = row_count)
dr_list <- character(length = row_count)
counter <- 1

pb <- txtProgressBar(min = 0, max = row_count, initial = 0, style = 3)

for (n_m in nm_list) {
  sub0 <- subset(working.typeII, nm == n_m)
  for (m in ordered_metrics) {
    sub1 <- subset(sub0, metric == m)
    for (d in dist_list) {
      sub2 <- subset(sub1, dist == d)
      for (dr in dir_list) {
        subs <- subset(sub2, dir == dr)
        setTxtProgressBar(pb, counter)

        if (sum(!is.na(subs$sig)) != 0) { # only do this for nm x metric without problems or that passed type I
          bi_res <- binom.test(
            x = sum(subs$sig.II, na.rm = T),
            n = nrow(subs),
            p = p_crit,
            alternative = "greater"
          )

          p_success[counter] <- bi_res$estimate
          sig[counter] <- ifelse(bi_res$p.value < 0.05, 0, 1) # 1 means more sig tests than desired
          n_list[counter] <- n_m
          m_list[counter] <- m
          d_list[counter] <- d
          dr_list[counter] <- dr
        } else {
          p_success[counter] <- NA
          sig[counter] <- NA # Set this NA so we can exclude combos from plots
          n_list[counter] <- n_m
          m_list[counter] <- m
          d_list[counter] <- d
          dr_list[counter] <- dr
        }
        counter <- counter + 1
      }
    }
  }
}

close(pb)

# Store the results of the binomial tests in a data frame and set the data types
bi_test_II <- data.frame(nm = as.factor(n_list), metric = as.factor(m_list), dist = as.factor(d_list), dir = as.factor(dr_list), sig, p_success)
bi_test_II$metric <- factor(bi_test_II$metric, levels = ordered_metrics)
bi_test_II$dist <- factor(bi_test_II$dist, levels = dist_list)
bi_test_II$nm <- factor(bi_test_II$nm, levels = rev(nm_list))
bi_test_II$dir <- factor(bi_test_II$dir, levels = dir_list)


#####

levels(bi_test_II$dir)[levels(bi_test_II$dir) == "negative"] <- "Maximal Negative"
levels(bi_test_II$dir)[levels(bi_test_II$dir) == "positive"] <- "Maximal Positive"

bi_test_II$dir <- factor(bi_test_II$dir, levels = c("Maximal Negative", "Maximal Positive"))

typII.sig <- aggregate(sig ~ metric + nm + dir + dist,
  FUN = function(x) mean(x),
  data = bi_test_II
)

##############

plot_data_II <-
  left_join(typII.sig, bi_test_II, by = c("metric", "nm", "dist", "dir")) %>%
  select(metric, nm, dist, dir, sig = sig.y, p_success) # %>%
# filter(p_success != 0))
# filter(sig == 0)

plot_data_II <- merge(plot_data_II, display_code_df, all.x = T)

plot_data_II$display <- factor(plot_data_II$display, levels = ordered_metrics_plots)

quartz(type = 'pdf', 
       file = '../../04-results/figure_02.pdf',
       width = 11,
       height = 4.25)

ggplot(plot_data_II, aes(x = display, y = nm)) +
  geom_tile(aes(fill = 1 - p_success, width = 0.925, height = 0.925)) +
  scale_fill_viridis_c(na.value = "white", limits = c(0, 1), alpha = 0.9) +
  geom_point(aes(color = as.factor(sig), shape = as.factor(sig)), show.legend = F, size = 2) +
  scale_colour_viridis_d(direction = -1, alpha = 1.0) +
  scale_shape_manual(values = c(19, NA)) +
  labs(fill = "") +
  xlab("Co-occurrence metric") +
  ylab("Null model algorithm") +
  theme_bw() +
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 90, size = 9),
    axis.text.x.bottom = element_text(vjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_grid(vars(dist), vars(dir))

dev.off()

# ggsave(
#   filename = "../../04-results/figure_02.pdf",
#   plot = last_plot(),
#   device = "pdf",
#   scale = 1,
#   width = 11,
#   height = 4.25,
#   units = "in"
# )


plot_data_II %>%
  group_by(dir) %>% 
  dplyr::count(sig == 0)

plot_data_II %>%
  filter(p_success >= 0.80) %>%
  select(nm, metric) %>%
  distinct() %>%
  count()

total_poss_comb <- 85 * 10 * 3

