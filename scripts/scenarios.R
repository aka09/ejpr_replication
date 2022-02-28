#### Script information ####
# This script reproduces figures 4, 5a, 5b, 6, S5a, and S5b from the article:
# "The Partisan Nature of Support for Democratic Backsliding: A Comparative Perspective"
# Forthcoming in the European Journal of Political Research
#-------------------------------------------------------------------------------#

#### Packages and data ####
library(DescTools)
library(tidyverse)
library(extrafont)

# Loading in dataset that contains both US and Canada conjoint 
load("data/pooled_conjoint.RData")

# Custom function for scenarios
source("functions/conjoint_scenario.R")

# Functions for confidence intervals
lwr_conf <- function(vectors){
  meanci <- MeanCI(vectors, na.rm = T)
  return(nth(meanci, 2))
}

upr_conf <- function(vectors){
  meanci <- MeanCI(vectors, na.rm = T)
  return(nth(meanci, 3))
}

#------------------------------------------------------------------------------#
#### Main body figures ####
#------------------------------------------------------------------------------#

##### Figure 4 #####
filter(conjoint_pooled, 
       partyid %in% c("Conservative", "Liberal", "New Democrat (NDP)",
                      "Democrat", "Republican")) %>% 
  conjoint_scenario(defect_cause = "abortion_distance") %>% 
  mutate(Country = factor(Country, levels = c("United States", "Canada"))) %>% 
  ggplot(aes(x = Country, col = group, y = mean, 
             ymin = lwr, ymax = upr, shape = group)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(position = position_dodge(0.5), width = 0.25, size = 1) +
  theme_bw() %+replace%
  theme(legend.text.align = 0) +
  labs(x = "",
       y = "Proportion choosing the undemocratic candidate") +
  scale_color_grey(name = "The respondent is closest to the abortion stance of:",
                   start = 0.1, end = 0.7) +
  scale_shape_manual(name = "The respondent is closest to the abortion stance of:",
                     values = c(16, 15, 17)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0.17, 0.67)

ggsave("figures/figure_4.pdf", height = 7, width = 10, dpi = 1000)

##### Figure 5a #####
filter(conjoint_pooled, 
       partyid %in% c("Democrat", "Republican")) %>% 
  conjoint_scenario(defect_cause = "abortion_distance",
                    by = "partyid") %>% 
  ggplot(aes(x = by, col = group, y = mean, 
             ymin = lwr, ymax = upr, shape = group)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(position = position_dodge(0.5), width = 0.25, size = 1) +
  theme_bw() %+replace%
  theme(legend.text.align = 0) +
  labs(x = "Respondent Party ID",
       y = "Proportion choosing the undemocratic candidate") +
  scale_color_grey(name = "The respondent is closest to the abortion stance of:",
                   start = 0.1, end = 0.7) +
  scale_shape_manual(name = "The respondent is closest to the abortion stance of:",
                     values = c(16, 15, 17)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(limits = c(0.10, 0.72),
                     breaks = seq(0.1, 0.7, 0.1))

ggsave("figures/figure_5a.pdf", height = 7, width = 10, dpi = 1000)

##### Figure 5b #####
filter(conjoint_pooled, 
       partyid %in% c("Conservative", "Liberal", "New Democrat (NDP)")) %>% 
  conjoint_scenario(defect_cause = "abortion_distance",
                    by = "partyid") %>% 
  ggplot(aes(x = by, col = group, y = mean, 
             ymin = lwr, ymax = upr, shape = group)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(position = position_dodge(0.5), width = 0.25, size = 1) +
  theme_bw() %+replace%
  theme(legend.text.align = 0) +
  labs(x = "Respondent Party ID",
       y = "Proportion choosing the undemocratic candidate") +
  scale_color_grey(name = "The respondent is closest to the abortion stance of:",
                   start = 0.1, end = 0.7) +
  scale_shape_manual(name = "The respondent is closest to the abortion stance of:",
                     values = c(16, 15, 17)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(limits = c(0.10, 0.72),
                     breaks = seq(0.1, 0.7, 0.1))

ggsave("figures/figure_5b.pdf", height = 7, width = 10, dpi = 1000)

##### Figure 6 #####
filter(conjoint_pooled, 
       partyid %in% c("Conservative", "Liberal", "New Democrat (NDP)",
                      "Democrat", "Republican")) %>% 
  conjoint_scenario(defect_cause = "welfare_distance") %>% 
  ggplot(aes(x = Country, col = group, y = mean, 
             ymin = lwr, ymax = upr, shape = group)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(position = position_dodge(0.5), width = 0.25, size = 1) +
  theme_bw() %+replace%
  theme(legend.text.align = 0) +
  labs(x = "",
       y = "Proportion choosing the undemocratic candidate") +
  scale_color_grey(name = "The respondent is closest to the welfare stance of:",
                   start = 0.1, end = 0.7) +
  scale_shape_manual(name = "The respondent is closest to the welfare stance of:",
                     values = c(16, 15, 17)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  ylim(0.17, 0.67)

ggsave("figures/figure_6.pdf", height = 7, width = 10, dpi = 1000)

#------------------------------------------------------------------------------#
#### Supplemental appendix ####
#------------------------------------------------------------------------------#

##### Figure S5a #####
filter(conjoint_pooled, 
       partyid %in% c("Democrat", "Republican")) %>% 
  conjoint_scenario(defect_cause = "welfare_distance",
                    by = "partyid") %>% 
  ggplot(aes(x = by, col = group, y = mean, 
             ymin = lwr, ymax = upr, shape = group)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(position = position_dodge(0.5), width = 0.25, size = 1) +
  theme_bw() %+replace%
  theme(legend.text.align = 0) +
  labs(x = "Respondent Party ID",
       y = "Proportion choosing the undemocratic candidate") +
  scale_color_grey(name = "The respondent is closest to the welfare stance of:",
                   start = 0.1, end = 0.7) +
  scale_shape_manual(name = "The respondent is closest to the welfare stance of:",
                     values = c(16, 15, 17)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(limits = c(0.10, 0.72),
                     breaks = seq(0.1, 0.7, 0.1))

ggsave("figures/figure_s5a.png", height = 7, width = 10, dpi = 1000)

##### Figure S5b #####
filter(conjoint_pooled, 
       partyid %in% c("Conservative", "Liberal", "New Democrat (NDP)")) %>% 
  conjoint_scenario(defect_cause = "welfare_distance",
                    by = "partyid") %>% 
  ggplot(aes(x = by, col = group, y = mean, 
             ymin = lwr, ymax = upr, shape = group)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(position = position_dodge(0.5), width = 0.25, size = 1) +
  theme_bw() %+replace%
  theme(legend.text.align = 0) +
  labs(x = "Respondent Party ID",
       y = "Proportion choosing the undemocratic candidate") +
  scale_color_grey(name = "The respondent is closest to the welfare stance of:",
                   start = 0.1, end = 0.7) +
  scale_shape_manual(name = "The respondent is closest to the welfare stance of:",
                     values = c(16, 15, 17)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(limits = c(0.10, 0.72),
                     breaks = seq(0.1, 0.7, 0.1))

ggsave("figures/figure_s5b.png", height = 7, width = 10, dpi = 1000)
