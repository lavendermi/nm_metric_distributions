### Save size: 9.0 x 9.0

library(plyr)
library(tidyverse)
library(viridis)

df.noise <- readRDS(file = "../../01-raw_data/noise/df.noise.rds")

# we want to be able to skip combos with bad type I & type II error rates
# merge with type II plot data (bad type I removed already) so we can filter out
# bad type II combos
working_noise <- merge(plot_data_II, df.noise, by = c("metric", "nm", "dist", "dir"))

#drop the type I & II failures
working_noise <- droplevels(subset(working_noise, sig == 0))


working_noise <- working_noise %>% 
  group_by(metric, nm, dist, dir) %>% 
  slice_sample(n=100) 


# calculate the mean noise for each combo
mean_noise <- aggregate(percent.step ~ metric + nm + dir + dist, FUN = mean, data = working_noise)

mean_noise <- merge(mean_noise, display_code_df, all.x = T)

mean_noise$display <- factor(mean_noise$display, levels = ordered_metrics_plots)

quartz(type = 'pdf', 
       file = '../../04-results/figure_03.pdf',
       width = 8.5,
       height = 7)

ggplot(mean_noise, aes(x = display, y = nm)) +
  geom_tile(aes(fill = percent.step, width = 0.925, height = 0.925)) +
  scale_fill_viridis_c(na.value = "white", limits = c(0, 1.0)) +
  geom_text(aes(label = paste(format(round(percent.step, 3) * 100, nsmall = 1), "%", sep = "")), size = 3, colour = "white") +
  labs(fill = "") +
  xlab("Co-occurrence metric") +
  ylab("Null model algorithm") +
  theme_bw() +
  theme(
    # legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 90),
    axis.text.x.bottom = element_text(vjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_grid(vars(dist), vars(dir))

dev.off()

# ggsave(
#   filename = "../../04-results/figure_03.pdf",
#   plot = last_plot(),
#   device = "pdf",
#   scale = 1,
#   width = 8.5,
#   height = 7,
#   units = "in"
# )



nrow(unique(df.noise[, c("species", "plots")]))
unique(df.noise$plots)
unique(df.noise$species)

summary(df.noise)

working_noise %>% dplyr::count(nm, dir, dist, metric)


# # plot the relationship between plots, species and noise tolerance. That is, do
# # the number of species or plots influence the noise tolerance
# working_noise %>% 
#   ggplot(aes(x = species, y = plots)) +
#   scale_color_viridis_c(limits = c(0, 1.0), alpha = 0.7) +
#   geom_point(aes(colour = percent.step, size = 1.5 )) +
#   scale_size(guide = "none") +
#   theme_bw() +
#   theme(
#     # legend.position = "none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8),
#     axis.text.x = element_text(angle = 90),
#     axis.text.x.bottom = element_text(vjust = 0.5),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank()
#   ) +
#   facet_wrap(~nm+metric+dir+dist, scales = "free", nrow = 4)

# working_noise %>%
#   group_by(metric, nm, dir, dist) %>%
#   dplyr::summarise(mean = mean(percent.step * 100), sd = sd(percent.step * 100)) %>%
#   ggplot(aes(x = metric, y = nm)) +
#     scale_color_viridis_c(alpha = 0.7) +
#     geom_point(aes(colour = mean, size = sd)) +
#     # scale_size(guide = "none") +
#     theme_bw() +
#     theme(
#       # legend.position = "none",
#       panel.spacing = unit(0.1, "lines"),
#       strip.text.x = element_text(size = 8),
#       axis.text.x = element_text(angle = 90),
#       axis.text.x.bottom = element_text(vjust = 0.5),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank()
#     ) +
#     facet_wrap(~ dir)
