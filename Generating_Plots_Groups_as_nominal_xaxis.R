###############################################################################
### Script for producing and saving the plots                   ###############
###############################################################################

### Generating the data
library(bayestestR) # for nearly perfectly distributed empirical data
library(tidyverse)
library(hrbrthemes)
library(ggdist)
library(ggridges)
library(ggbeeswarm)
library(dabestr)

set.seed(823876)
data <- tibble(Paper = round(distribution_normal(309, 53, 15), 0),
               Tablet = round(distribution_normal(309, 47, 15), 0)) %>% 
  gather(Condition, Testscore)


# Gradientintervalplot - Groups as nominal x-axis ####
ggplot(data, aes(Condition, Testscore)) + 
  stat_gradientinterval() + 
  ylim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Gradientinterval Plot", subtitle = "Groups as nominal x-axis")

ggsave(
  file = "plots/gradientintervalplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Boxplot - Groups as nominal x-axis ####
ggplot(data, aes(Condition, Testscore)) + 
  geom_boxplot() + 
  ylim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Box Plot", subtitle = "Groups as nominal x-axis")

ggsave(
  file = "plots/boxplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Violinplot - Groups as nominal x-axis ####
ggplot(data, aes(Condition, Testscore)) + 
  geom_violin() + 
  ylim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Violin Plot", subtitle = "Groups as nominal x-axis")

ggsave(
  file = "plots/violinplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Density plot - Groups as nominal x-axis ####
ggplot(data, aes(Testscore, Condition)) + 
  geom_density_ridges() +
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Density Plot", subtitle = "Groups as nominal x-axis") +
  coord_flip()

ggsave(
  file = "plots/densityplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)

# Halfeye - Groups as nominal x-axis ####
ggplot(data, aes(Testscore, Condition)) + 
  stat_halfeye() + 
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Halfeye Plot", subtitle = "Groups as nominal x-axis") +
  coord_flip()

ggsave(
  file = "plots/halfeyeplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Slabintervals - Groups as nominal x-axis ####
ggplot(data,
       aes(Testscore, Condition, fill = factor(stat(quantile)))) + 
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.03, .165, .835, .97),
    color = "black"
  ) +
  scale_fill_manual(values = c("lightgrey", 
                               "grey", 
                               "darkgrey", 
                               "grey", 
                               "lightgrey")) +
  coord_flip() +
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Slabintervals Plot", subtitle = "Groups as nominal x-axis") +
  theme(legend.position = "none")

ggsave(
  file = "plots/slabintervalsplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# CDF Bar Plot - Groups as nominal x-axis ####
ggplot(data, aes(Testscore, Condition)) + 
  stat_cdfinterval() + 
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "CDF Bar Plot", subtitle = "Groups as nominal x-axis") +
  coord_flip()

ggsave(
  file = "plots/cdfbarplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Histograms - Groups as nominal x-axis ####
ggplot(data, aes(Testscore, Condition)) + 
  stat_histinterval(slab_type = "histogram",
                    interval_alpha = 0,
                    point_alpha = 0) + 
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Histogram", subtitle = "Groups as nominal x-axis") +
  coord_flip()

ggsave(
  file = "plots/histogram_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Dotplot - Groups as nominal x-axis ####
ggplot(data, aes(Testscore, Condition)) + 
  geom_dotsinterval(layout = c("bin"), 
                    binwidth = 1.0) + 
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Dot Plot", subtitle = "Groups as nominal x-axis")  +
  coord_flip()

ggsave(
  file = "plots/dotplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Jitterplot - Groups as nominal x-axis ####
ggplot(data, aes(Testscore, Condition)) + 
  geom_jitter(height = .3) + 
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Jitter Plot", subtitle = "Groups as nominal x-axis")  +
  coord_flip()

ggsave(
  file = "plots/jitterplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Quantile Dotplots - Groups as nominal x-axis ####
ggplot(data, aes(Testscore, Condition)) + 
  stat_dots(quantiles = 50) +
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Quantile Dot Plot", subtitle = "Groups as nominal x-axis")  +
  coord_flip()

ggsave(
  file = "plots/quantiledotplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Beeswarmplots - Groups as nominal x-axis ####
ggplot(data, aes(Testscore, Condition)) + 
  stat_dots(layout = "swarm", side = "both") +
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Beeswarm Plot", subtitle = "Groups as nominal x-axis")  +
  coord_flip()

ggsave(
  file = "plots/beeswarmplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Sinaplots - Groups as nominal x-axis ####
ggplot(data, aes(Condition, Testscore)) + 
  ggforce::geom_sina() + 
  ylim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Sina Plot", subtitle = "Groups as nominal x-axis")

ggsave(
  file = "plots/sinaplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Rug Plot - Groups as nominal x-axis ####
ggplot(data, aes(Condition, Testscore)) + 
  geom_point(position = position_jitter(w = 0, h = .5), 
             shape = 95,
             size = 9,
             alpha = .4) +
  ylim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Rug Plot", subtitle = "Groups as nominal x-axis") 

ggsave(
  file = "plots/rugplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Raincloud Plot - Groups as nominal x-axis ####
ggplot(data, aes(Condition, Testscore)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  ylim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Raincloud Plot", subtitle = "Groups as nominal x-axis")

ggsave(
  file = "plots/raincloudplot_groups_as_nominal_xaxis.svg",
  width = 6,
  height = 6,
  device = "svg"
)

