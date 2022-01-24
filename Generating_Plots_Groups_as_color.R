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
  gather(Condition, Testscore) %>% 
  mutate(dummy = 1)

# Boxplot - Groups as color ####
ggplot(data, aes(Testscore, dummy, color = Condition, fill = Condition)) + 
  geom_boxplot(alpha = .3, position = "identity",
               width = .3) + 
  ylim(c(.5, 1.5)) + 
  theme_ipsum() + 
  labs(title = "Box Plot", 
       subtitle = "Groups as color") +
  ylab("") + 
  theme(axis.text.y = element_blank())

ggsave(
  file = "plots/boxplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Violinplot - Groups as color ####
ggplot(data, aes(Testscore, dummy, color = Condition, fill = Condition)) + 
  geom_violin(alpha = .3, position = "identity",
               width = .3) + 
  ylim(c(.5, 1.5)) + 
  theme_ipsum() + 
  labs(title = "Box Plot", 
       subtitle = "Groups as color") +
  ylab("") + 
  theme(axis.text.y = element_blank())

ggsave(
  file = "plots/violinplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)



# Density plot - Groups as color ####
ggplot(data, aes(Testscore, 
                 color = Condition,
                 fill = Condition)) + 
  geom_density(alpha = .3) +
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Density Plot", subtitle = "Groups as color")

ggsave(
  file = "plots/densityplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)

# Halfeye - Groups as color ####
ggplot(data, aes(Testscore, 
                 color = Condition,
                 fill = Condition)) + 
  stat_halfeye(alpha = .3) + 
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Halfeye Plot", subtitle = "Groups as color") +
  ylab("") + 
  theme(axis.text.y = element_blank())

ggsave(
  file = "plots/halfeyeplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)



# CDF Bar Plot - Groups as color ####
ggplot(data, aes(Testscore, 
                 color = Condition,
                 fill = Condition)) + 
  stat_cdfinterval(alpha = .3) + 
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "CDF Bar Plot", subtitle = "Groups as color") +
  ylab("") + 
  theme(axis.text.y = element_blank())

ggsave(
  file = "plots/cdfbarplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Histograms - Groups as color ####
ggplot(data) + 
  geom_histogram(mapping = aes(x = Testscore, y = ..count.., 
                               color = Condition, fill = Condition), 
                 stat = "bin", bins = 25, size = 0.5,
                 alpha = 0.3,
                 position = "identity") +
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Histogram", subtitle = "Groups as color")

ggsave(
  file = "plots/histogram_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Dotplot - Groups as color ####
ggplot(data, aes(Testscore, fill = Condition)) + 
  geom_dotsinterval(layout = c("bin"), 
                    alpha = .3) + 
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Dot Plot", subtitle = "Groups as color")  +
  ylab("") + 
  theme(axis.text.y = element_blank())

ggsave(
  file = "plots/dotplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Jitterplot - Groups as color ####
ggplot(data, aes(Testscore, 
                 dummy, 
                 fill = Condition, 
                 color = Condition)) + 
  geom_jitter(height = .3,
              alpha = .6) + 
  xlim(c(0, 100)) + 
  ylim(c(0,2)) +
  ylab("") +
  theme_ipsum() + 
  labs(title = "Jitter Plot", subtitle = "Groups as color") +
  theme(axis.text.y = element_blank())

ggsave(
  file = "plots/jitterplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Quantile Dotplots - Groups as color ####
ggplot(data, aes(Testscore, 
                 color = Condition,
                 fill = Condition)) + 
  stat_dots(quantiles = 50, alpha = .3) +
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Quantile Dot Plot", subtitle = "Groups as color")  +
  ylab("") + 
  theme(axis.text.y = element_blank())

ggsave(
  file = "plots/quantiledotplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Beeswarmplots - Groups as color ####
ggplot(data, aes(Testscore, 
                 color = Condition,
                 fill = Condition)) + 
  stat_dots(layout = "swarm", 
            side = "both",
            alpha = .3) +
  xlim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Beeswarm Plot", subtitle = "Groups as color")  +
  ylab("") + 
  theme(axis.text.y = element_blank())

ggsave(
  file = "plots/beeswarmplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Sinaplots - Groups as color ####
ggplot(data, aes(dummy, Testscore, 
                 color = Condition,
                 fill = Condition)) + 
  ggforce::geom_sina(alpha = .3,
                     position = "identity") + 
  ylim(c(0, 100)) + 
  xlab("") +
  theme_ipsum() + 
  labs(title = "Sina Plot", subtitle = "Groups as color") +
  coord_flip() +
  theme(axis.text.y = element_blank())

ggsave(
  file = "plots/sinaplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Rug Plot - Groups as color ####
ggplot(data, aes(Testscore,
                 dummy,
                 color = Condition,
                 fill = Condition)) + 
  geom_point(position = position_jitter(w = .5, h = 0), 
             shape = 124,
             size = 6,
             alpha = .3) +
  xlim(c(0, 100)) + 
  ylab("") +
  theme_ipsum() + 
  labs(title = "Rug Plot", subtitle = "Groups as color") +
  theme(axis.text.y = element_blank())

ggsave(
  file = "plots/rugplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)


# Raincloud Plot - Groups as color ####
ggplot(data, aes(y = Testscore,
                 x = dummy,
                 fill = Condition,
                 color = Condition)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA,
    alpha = .3) + 
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  ylim(c(0, 100)) + 
  theme_ipsum() + 
  labs(title = "Raincloud Plot", subtitle = "Groups as color") +
  coord_flip() +
  xlab("") + 
  theme(axis.text.y = element_blank())

ggsave(
  file = "plots/raincloudplot_groups_as_color.svg",
  width = 6,
  height = 6,
  device = "svg"
)