#### Script information ####
# This script reproduces figures 2a, 3a, 7, A2a, A3a, A4a, S1a, S2a, S3a, S4a, 
# and S6 from the article:
# "The Partisan Nature of Support for Democratic Backsliding: A Comparative Perspective"
# Forthcoming in the European Journal of Political Research
#-------------------------------------------------------------------------------#

#### Packages and data ####
library(plyr)
library(tidyverse)
library(cregg)
library(viridis)
library(extrafont)
library(magrittr)
library(ggrepel)

# Loading in data from US conjoint
load("data/us_conjoint.RData")

# Custom function to prepare conjoint data for plotting
source("functions/prepare_conjoint.R")

# Custom function to plot conjoint results
source("functions/plot_subgroup.R")

# Custom ggplot theme
theme_cjoint <- function(){
  theme_bw(base_size = 17,
           base_family = "Times") %+replace%
    theme(legend.position = "none", # suppressing legend
          axis.title.x = element_text(size = 12),
          axis.text.y = element_text(hjust = 0)) # left-align y axis text
}


# Storing regression formula for conjoint analysis
# formula for conjoint analysis
f1 <- selected ~ Age + `Political Experience` + `Sex` + `Legislative checks` + 
  `Judicial checks` + `Abortion` + `Welfare spending`

#-------------------------------------------------------------------------------#
#### Main body figures ####
#-------------------------------------------------------------------------------#

##### Figure 2a ######
usa_conjoint %>% 
  # Respondents who answer "None of these" are considered as Independent
  mutate(partyid = recode(partyid, "None of these" = "Independent")) %>% 
  # Custom function creates better output for plotting
  prepare_conjoint(formula = f1, by = ~partyid, country = "usa") %>% 
  filter(BY %in% c("Democrat", "Republican", "Independent", 
                   "ref_category", NA_character_)) %>% 
  # only plotting AMCEs for the two democratic attributes 
  filter((feature %in% c("Legislative checks", "Judicial checks")) |
           (level %in% c("Legislative checks:", "Judicial checks:"))) %>% 
  # Using the custom function to plot
  plot_subgroup(format = "partial2") +
  scale_x_continuous(breaks = c(-.15, -.10, -0.05, 0, 0.05, 0.10, 0.15),
                     limits = c(-.15, .15)) +
  # using parties' official hex codes to set colors 
  scale_color_manual(values = c("Democrat" = "#0015BC",
                                "Independent" = "grey",
                                "Republican" = "#E9141D",
                                "reference" = "black")) +
  labs(y = "",
       x = "Estimated AMCE",
       caption = "Republicans in red; Independents in grey; Democrats in blue")

ggsave("figures/figure_2a.pdf", height = 5, width = 10, dpi = 1000)

##### Figure 3a #####
usa_conjoint %>% 
  # Only strong Democrats and Republicans
  filter(partyid %in% c("Democrat", "Republican") &
           pid_strength == "Very strongly") %>% 
  # Getting rid of unused factor levels by re-coercing to factor
  mutate(partyid = factor(partyid)) %>% 
  prepare_conjoint(formula = f1, by = ~partyid, country = "usa") %>% 
  plot_subgroup(format = "partial2") +
  scale_x_continuous(breaks = c(-.15, -.10, -0.05, 0, 0.05, 0.10, 0.15),
                     limits = c(-.18, .18)) +
  scale_color_manual(values = c("Democrat" = "blue",
                                "Republican" = "red",
                                "reference" = "black")) +
  labs(y = "",
       x = "Estimated AMCE",
       caption = "Republicans in red; Democrats in blue")

ggsave("figures/figure_3a.pdf", height = 5, width = 10, dpi = 1000)

##### Figure 7 #####
usa_conjoint  %>% 
  filter(partyid == "Republican") %>% 
  prepare_conjoint(formula = f1, by = ~trump_app, country = "usa") %>% 
  filter(((feature %in% c("Legislative checks", "Judicial checks")) |
            (level %in% c("Legislative checks:", "Judicial checks:")))) %>%
  # to get rid of "NA" panel but keep attribute headerss
  mutate(BY = ifelse(is.na(estimate), "Strongly approve", as.character(BY)) %>% 
           factor(levels = c("Strongly approve", "Somewhat approve", 
                             "Somewhat disapprove", "Strongly disapprove"))) %>% 
  ggplot(aes(x = estimate, y = fct_rev(level) ,
             xmin = lower, xmax = upper)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3, position = position_dodge(-0.8)) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(-0.8)) +
  labs(y = "",
       x = "Estimated AMCE") +
  scale_x_continuous(breaks = c(-.2, -.1, 0, .1, .2),
                     limits = c(-.22, 0.22)) +
  facet_wrap(~BY) +
  theme_cjoint()

ggsave("figures/figure_7.pdf", height = 5, width = 10, dpi = 1000)

#-------------------------------------------------------------------------------#
#### Appendix figures ####
#-------------------------------------------------------------------------------#

##### Figure A2a #####
# For the "shutting down Congress" attribute level
cregg::cj(usa_conjoint, formula = f1, id = ~id, estimate = "amce", 
          by = ~task) %>%
  filter(level == "Shut down Congress") %>%
  ggplot() +
  geom_point(aes(x = task, y = estimate), size = 3) +
  geom_pointrange(aes(x = task, y = estimate, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "",
       x = "Task Number",
       y = "Estimated AMCEs") +
  theme_bw(base_size = 17) +
  ylim(-0.17, 0.03)

ggsave("figures/figure_a2a.pdf", height = 7, width = 12, dpi = 1000)

##### Figure A2b #####
# For the "ignoring courts" attribute level
cregg::cj(usa_conjoint, formula = f1, id = ~id, estimate = "amce", 
          by = ~task) %>%
  filter(level == "Not be bound by courts") %>%
  ggplot() +
  geom_point(aes(x = task, y = estimate), size = 3) +
  geom_pointrange(aes(x = task, y = estimate, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "",
       x = "Task Number",
       y = "Estimated AMCEs") +
  theme_bw(base_size = 17) +
  ylim(-0.17, 0.03)

ggsave("figures/figure_a2b.pdf", height = 7, width = 12, dpi = 1000)

###### Nested model comparison test ######
# This function from the "cregg" packages compares a baseline model with no 
# interactions to a model that interacts each attribute with task number. 
# F = 1.09 (p = 0.34), so we fail to reject the null, i.e. no evidence that the 
# AMCEs differ by task number. 
cj_anova(usa_conjoint, f1, by = ~task)

##### Figure A4a #####
usa_conjoint %>% 
  prepare_conjoint(formula = f1, by = ~profile, country = "usa") %>% 
  mutate(profile = ifelse(estimate == 0, "ref_category", profile)) %>% 
  ggplot(aes(x = estimate, y = fct_rev(level))) +
  geom_vline(xintercept = 0)+
  geom_point(aes(col = profile), size = 3, position = position_dodge(0.7)) +
  geom_errorbar(aes(xmin = lower, xmax = upper, col = profile), 
                width = 0, size = 1, position = position_dodge(0.7)) +
  labs(y = "") +
  theme_bw(base_size = 14) %+replace%
  theme(axis.text.y = element_text(hjust=0),
        legend.position = "none") +
  labs(x = "Estimated AMCE") +
  scale_color_manual(values = c(viridis(2), "black"),
                     name = "Profile")

ggsave("figures/figure_a4a.pdf", height = 7, width = 12, dpi = 1000)

##### Figure A5a #####
cregg::cj(usa_conjoint, f1, id = ~id, by = ~dem1_row, estimate = "amce") %>% 
  filter(level == "Shut down Congress") %>% 
  plyr::rbind.fill(cregg::cj(usa_conjoint, f1, id = ~id, by = ~dem2_row, estimate = "amce") %>% 
                     filter(level == "Not be bound by courts")) %>% 
  ggplot(aes(x = level, y = estimate, ymin = lower, ymax = upper, col = BY, shape = BY)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  geom_errorbar(position = position_dodge(0.5), width = 0.25, size = 1) +
  scale_color_grey(name = "Row of democratic attribute") +
  scale_shape_manual(name = "Row of democratic attribute",
                     values = c(16, 15, 17, 18)) +
  labs(y = "Estimated AMCE",
       x = "Democratic attribute") +
  theme_bw(base_size = 17) +
  theme(legend.title = element_text(size = 13)) +
  ylim(-0.12, 0)

ggsave("figures/figure_a5a.pdf", height = 7, width = 11, dpi = 1000)

#------------------------------------------------------------------------------#
#### Supplementary appendix ####
#------------------------------------------------------------------------------#

##### Figure S1a #####

usa_conjoint %>% 
  mutate(partyid = recode(partyid, "None of these" = "Independent")) %>% 
  prepare_conjoint(formula = f1, by = ~partyid, country = "usa") %>% 
  filter(BY %in% c("Democrat", "Republican", "Independent", 
                   "ref_category", NA_character_)) %>% 
  plot_subgroup() +
  theme_bw(base_size = 17,
           base_family = "Times New Roman") %+replace%
  theme(legend.position = "none",
        axis.text.y = element_text(hjust = 0)) +
  scale_x_continuous(breaks = c(-.4, -.3, -.2, -.1, 0, .1, .2, .3, .4),
                     limits = c(-.4, .4)) +
  scale_color_manual(values = c("Democrat" = "blue",
                                "Independent" = "grey",
                                "Republican" = "red",
                                "reference" = "black")) +
  labs(y = "",
       x = "Estimated AMCE",
       caption = "Republicans in red; Independents in grey; Democrats in blue")

ggsave("figures/figure_s1a.png", height = 9, width = 8, dpi = 1000)

##### Figure S2a #####
usa_conjoint %>% 
  filter(partyid %in% c("Democrat", "Republican") & 
           pid_strength == "Very strongly") %>% 
  # Getting rid of unused factor levels
  mutate(partyid = factor(partyid)) %>% 
  prepare_conjoint(formula = f1, by = ~partyid, country = "usa") %>% 
  plot_subgroup() +
  theme_bw(base_size = 17,
           base_family = "Times New Roman") %+replace%
  theme(legend.position = "none",
        axis.text.y = element_text(hjust = 0)) +
  scale_x_continuous(breaks = c(-.4, -.3, -.2, -.1, 0, .1, .2, .3, .4),
                     limits = c(-.4, .4)) +
  scale_color_manual(values = c("Democrat" = "blue",
                                "Republican" = "red",
                                "reference" = "black")) +
  labs(y = "",
       x = "Estimated AMCE",
       caption = "Republicans in red; Democrats in blue")

ggsave("figures/figure_s2a.png", height = 9, width = 8, dpi = 1000)

##### Figure S3a ######

# Using the conjoint data, the raw numbers are off (due to multiple tasks per
# respondent) but the %s are okay 
filter(usa_conjoint, 
       partyid %in% c("Democrat", "Republican", "Independent") &
         !is.na(abortion_view_recode)) %>% 
  mutate(
    partyid = factor(partyid, 
                     levels = c("Republican", "Democrat", "Independent")),
    abortion_view_recode = recode(
      abortion_view_recode, 
      "Abortion allowed for any reason" = "Any reason",
      "Abortion never allowed" = "Never allowed",
      "Abortion only in first 12 weeks" = "12 weeks",
      "Abortion only mother's life at risk" = "To save mother"
    ) %>% 
      factor(levels = c("Any reason", "12 weeks", 
                        "To save mother", "Never allowed"))
  ) %>% 
  ggplot(aes(x = abortion_view_recode, y = ..prop..*100,
             group = partyid)) +
  geom_bar(stat = "count") + 
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  facet_wrap(~partyid,
             nrow = 2) +
  theme_bw(base_size = 15) +
  labs(x = "Preference on abortion",
       y = "Percentage of respondents")

ggsave("figures/figure_s3a.pdf", height = 9, width = 11, dpi = 1000)

##### Figure S4a ######
filter(usa_conjoint, 
       partyid %in% c("Democrat", "Republican", "Independent") &
         !is.na(welfare_view_recode)) %>% 
  mutate(
    partyid = factor(partyid, 
                     levels = c("Republican", "Democrat", "Independent")),
    welfare_view_recode = recode(
      welfare_view_recode, "Decrease welfare spending" = "Decrease",
      "Increase welfare spending" = "Increase",
      "Slash welfare spending" = "Slash",
      "Keep welfare spending the same" = "Keep as is"
    ) %>% 
      factor(levels = c("Slash", "Decrease", "Keep as is", "Increase"))
  ) %>% 
  ggplot(aes(x = welfare_view_recode, y = ..prop..*100,
             group = partyid)) +
  geom_bar(stat = "count") + 
  scale_y_continuous(limits = c(0, 50),
                     breaks = seq(0, 50, 10)) +
  facet_wrap(~partyid,
             nrow = 2) +
  theme_bw(base_size = 15) +
  labs(x = "Preference on welfare spending",
       y = "Percentage of respondents")

ggsave("figures/figure_s4a.pdf", height = 9, width = 11, dpi = 1000)

##### Figure S6 #####
usa_conjoint  %>% 
  filter(partyid == "Republican") %>% 
  prepare_conjoint(formula = f1, by = ~trump_app, country = "usa") %>% 
  # to get rid of "NA" panel but keep attribute headerss
  mutate(BY = ifelse(is.na(estimate), "Strongly approve", as.character(BY)) %>% 
           factor(levels = c("Strongly approve", "Somewhat approve", 
                             "Somewhat disapprove", "Strongly disapprove"))) %>% 
  ggplot(aes(x = estimate, y = fct_rev(level) ,
             xmin = lower, xmax = upper, col = feature)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3, position = position_dodge(-0.8)) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(-0.8)) +
  labs(y = "",
       x = "Estimated AMCE") +
  scale_x_continuous(limits = c(-.4, 0.4)) +
  facet_wrap(~BY) +
  theme_cjoint() %+replace%
  theme(axis.text.y = element_text(size = 12, hjust = 0))

ggsave("figures/figure_s6.pdf", height = 11, width = 10, dpi = 1000)

