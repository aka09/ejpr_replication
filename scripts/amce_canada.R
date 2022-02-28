#### Script information ####
# This script reproduces figures 2b, 3b, A2b, A3b, A4b, S1b, S2b, S3b, and S4b 
# from the article:
# "The Partisan Nature of Support for Democratic Backsliding: A Comparative Perspective"
# Forthcoming in the European Journal of Political Research
#-------------------------------------------------------------------------------#

#### Packages and data ####
library(tidyverse)
library(cregg)
library(magrittr)
library(viridis)
library(extrafont)
library(ggrepel)

# Loading in data from Canadian conjoint
load("data/can_conjoint.RData")

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

##### Figure 2b #####

can_conjoint %>% 
  # Custom function creates better output for plotting
  prepare_conjoint(formula = f1, by = ~partyid, country = "canada") %>% 
  filter(BY %in% c("Conservative", "Liberal", "New Democrat (NDP)",
                   "None of these", "reference", NA_character_)) %>% 
  # only plotting AMCEs for the two democratic attributes 
  filter((feature %in% c("Legislative checks", "Judicial checks")) |
           (level %in% c("Legislative checks:", "Judicial checks:"))) %>% 
  # Using the custom function to plot
  plot_subgroup(format = "partial2") +
  scale_x_continuous(breaks = c(-.15, -.10, -0.05, 0, 0.05, 0.10, 0.15),
                     limits = c(-.15, .15)) +
  # using parties' official hex codes to set colors 
  scale_color_manual(values = c("Conservative" = "#1A4782",
                                "None of these" = "grey",
                                "Liberal" = "#D71920",
                                "New Democrat (NDP)" = "#F37021",
                                "reference" = "black")) +
  labs(y = "",
       x = "Estimated AMCE",
       caption = "Conservatives in blue; Liberals in red; New Democrats in orange; Non-partisans in grey")

ggsave("figures/figure_2b.pdf", height = 5, width = 10, dpi = 1000)

##### Figure 3b #####
can_conjoint %>% 
  # Only strong identifiers with 3 major parties
  filter(pid_strength == "Very strongly" &
           partyid %in% c("Conservative", "Liberal", "New Democrat (NDP)")) %>%
  # Getting rid of unused factor levels by re-coercing to factor
  mutate(partyid = factor(partyid)) %>% 
  prepare_conjoint(formula = f1, by = ~partyid, country = "canada") %>% 
  # reordering factor for figure
  mutate(BY = factor(BY, levels = c("New Democrat (NDP)", "Conservative", "Liberal"))) %>% 
  plot_subgroup(format = "partial2") +
  scale_x_continuous(breaks = c(-.15, -.10, -0.05, 0, 0.05, 0.10, 0.15),
                     limits = c(-.18, .18)) +
  scale_color_manual(values = c("Conservative" = "#1A4782",
                                "None of these" = "grey",
                                "Liberal" = "#D71920",
                                "New Democrat (NDP)" = "#F37021",
                                "reference" = "black")) +
  labs(y = "",
       x = "Estimated AMCE",
       caption = "Conservatives in blue; Liberals in red; New Democrats in orange")

ggsave("figures/figure_3b.pdf", height = 5, width = 10, dpi = 1000)


#-------------------------------------------------------------------------------#
#### Appendix figures ####
#-------------------------------------------------------------------------------#

##### Figure A3a #####

# For the "shutting down Congress" attribute level
cregg::cj(can_conjoint, formula = f1, id = ~id, estimate = "amce", 
          by = ~task) %>%
  filter(level == "Shut down Parliament") %>%
  ggplot() +
  geom_point(aes(x = task, y = estimate), size = 3) +
  geom_pointrange(aes(x = task, y = estimate, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "",
       x = "Task Number",
       y = "Estimated AMCEs") +
  theme_bw(base_size = 17) +
  ylim(-0.17, 0.03)

ggsave("figures/figure_a3a.pdf", height = 7, width = 12, dpi = 1000)

##### Figure A3b #####
# For the "ignoring courts" attribute level
cregg::cj(can_conjoint, formula = f1, id = ~id, estimate = "amce", 
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

ggsave("figures/figure_a3b.pdf", height = 7, width = 12, dpi = 1000)

##### Figure A4b #####
can_conjoint %>% 
  prepare_conjoint(formula = f1, by = ~profile, country = "canada") %>% 
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

ggsave("figures/figure_a4b.pdf", height = 7, width = 12, dpi = 1000)

##### Figure A5b #####
# Row-specific AMCEs 
cregg::cj(can_conjoint, f1, id = ~id, by = ~dem1_row, estimate = "amce") %>% 
  filter(level == "Shut down Parliament") %>% 
  plyr::rbind.fill(cregg::cj(can_conjoint, f1, id = ~id, by = ~dem2_row, estimate = "amce") %>% 
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

ggsave("figures/figure_a5b.pdf", height = 7, width = 11, dpi = 1000)

#------------------------------------------------------------------------------#
#### Supplementary appendix ####
#------------------------------------------------------------------------------#

##### Figure S1b #####

can_conjoint %>% 
  prepare_conjoint(formula = f1, by = ~partyid, country = "canada") %>% 
  filter(BY %in% c("Conservative", "Liberal", "New Democrat (NDP)",
                   "None of these", "reference", NA_character_)) %>% 
  plot_subgroup() +
  theme_bw(base_size = 17,
           base_family = "Times New Roman") %+replace%
  theme(legend.position = "none",
        axis.text.y = element_text(hjust = 0)) +
  scale_x_continuous(breaks = c(-.4, -.3, -.2, -.1, 0, .1, .2, .3, .4),
                     limits = c(-.4, .4)) +
  # using parties' official hex codes to set colors 
  scale_color_manual(values = c("Conservative" = "#1A4782",
                                "None of these" = "grey",
                                "Liberal" = "#D71920",
                                "New Democrat (NDP)" = "#F37021",
                                "reference" = "black")) +
  labs(y = "",
       x = "Estimated AMCE",
       caption = "Conservatives in blue; Liberals in red; New Democrats in orange; Non-partisans in grey")

ggsave("figures/figure_s1b.png", height = 9, width = 8, dpi = 1000)

##### Figure S2b #####
can_conjoint %>% 
  filter(partyid %in% c("Conservative", "Liberal", "New Democrat (NDP)") & 
           pid_strength == "Very strongly") %>% 
  # Getting rid of unused factor levels
  mutate(partyid = factor(partyid)) %>% 
  prepare_conjoint(formula = f1, by = ~partyid, country = "canada") %>% 
  mutate(BY = factor(BY, 
                     levels = c("New Democrat (NDP)", "Conservative", "Liberal"))) %>%
  plot_subgroup() +
  theme_bw(base_size = 17,
           base_family = "Times New Roman") %+replace%
  theme(legend.position = "none",
        axis.text.y = element_text(hjust = 0)) +
  scale_x_continuous(breaks = c(-.4, -.3, -.2, -.1, 0, .1, .2, .3, .4),
                     limits = c(-.4, .4)) +
  scale_color_manual(values = c("New Democrat (NDP)" = "#F37021",
                                "Conservative" = "#1A4782",
                                "Liberal" = "#D71920",
                                "reference" = "black")) +
  labs(y = "",
       x = "Estimated AMCE",
       caption = "Conservatives in blue; Liberals in red; New Democrats in orange")

ggsave("figures/figure_s2b.png", height = 9, width = 8, dpi = 1000)

##### Figure S3b ######

# Using the conjoint data, the raw numbers are off (due to multiple tasks per
# respondent) but the %s are okay 
filter(can_conjoint, 
       partyid %in% c("None of these", "Liberal", 
                      "Conservative", "New Democrat (NDP)") &
         !is.na(abortion_view_recode)) %>% 
  mutate(
    partyid = factor(partyid, 
                     levels = c("Conservative", "Liberal", 
                                "New Democrat (NDP)", "None of these")),
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

ggsave("figures/figure_s3b.pdf", height = 9, width = 11, dpi = 1000)

##### Figure S4b ######
filter(can_conjoint, 
       partyid %in% c("None of these", "Liberal",
                      "Conservative", "New Democrat (NDP)") &
         !is.na(welfare_view_recode)) %>% 
  mutate(
    partyid = factor(partyid, 
                     levels = c("Conservative", "Liberal", 
                                "New Democrat (NDP)", "None of these")),
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

ggsave("figures/figure_s4b.pdf", height = 9, width = 11, dpi = 1000)
