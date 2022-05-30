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
library(ggridges)
library(DescTools)

## Data for figure 1: CES and ANES
# Loading in 2019 Canadian Election Study
load("data/ces2019.RData")
# Loading in 2020 American National Election Study
load("data/anes2020.RData")

# Loading in data from US conjoint
load("data/us_conjoint.RData")

# Loading in data from Canadian conjoint
load("data/can_conjoint.RData")

# Loading in dataset that contains both US and Canada conjoint 
load("data/pooled_conjoint.RData")

# Custom functions
source("functions/ejpr_functions.R")

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

#-------------------------#
##### Figure 1 #####
#-------------------------#

#-------------------------#
###### Manipulating CES ######
#-------------------------#
ces2019 %<>%
  # Non-Quebec citizens only 
  filter(cps19_province != "Quebec" & cps19_citizenship == "Canadian citizen") %>% 
  # renaming variables to more intuitive names
  rename(partyid = cps19_fed_id,
         feel_libs = cps19_party_rating_23,
         feel_cons = cps19_party_rating_24,
         feel_ndp = cps19_party_rating_25) %>%
  # feeling thermometers between different partisan groups
  mutate(
    feel_own = case_when(partyid == "Liberal" ~ feel_libs,
                         partyid == "Conservative" ~ feel_cons,
                         partyid == "ndp" ~ feel_ndp),
    feel_libs_ndp = case_when(partyid == "Liberal" ~ feel_ndp,
                              partyid == "ndp" ~ feel_libs),
    feel_libs_cons = case_when(partyid == "Liberal" ~ feel_cons,
                               partyid == "Conservative" ~ feel_libs),
    feel_cons_ndp = case_when(partyid == "Conservative" ~ feel_ndp,
                              partyid == "ndp" ~ feel_cons),
    # difference between feeling toward own party and feeling toward other party
    `Liberal and NDP` = feel_own - feel_libs_ndp,
    `Liberal and Conservative` = feel_own - feel_libs_cons,
    `Conservative and NDP` = feel_own - feel_cons_ndp
  )

# Transforming into long format to facilitate visualization 
ces2019_long <- ces2019 %>% 
  select(`Liberal and NDP`:`Conservative and NDP`) %>% 
  pivot_longer(cols = everything(),
               names_to = "type",
               values_to = "feel_partisan_gap") %>% 
  mutate(country = "Canada")


#-------------------------#
###### Manipulating ANES ######
#-------------------------#
anes2020 %<>% 
  mutate(partyid = case_when(V201228 == 1 ~ "Democrat",
                             V201228 == 2 ~ "Republican",
                             V201228 == 3 ~ "Independent"),
         feel_dems = ifelse(V201156 %in% c(-9, 998), NA, V201156),
         feel_reps = ifelse(V201157 %in% c(-9, 998), NA, V201157),
         feel_ownparty = case_when(partyid == "Democrat" ~ feel_dems,
                                   partyid == "Republican" ~ feel_reps),
         feel_opposite = case_when(partyid == "Democrat" ~ feel_reps,
                                   partyid == "Republican" ~ feel_dems),
         feel_partisan_gap = feel_ownparty - feel_opposite,
         type = "Democrat and Republican",
         country = "USA")

# joining rows for easier visualization 
pooled <- bind_rows(
  anes2020, ces2019_long
) %>% 
  # reordering factor for a specific order in ggplot
  mutate(type = factor(type, levels = c("Liberal and NDP",
                                        "Conservative and NDP",
                                        "Liberal and Conservative",
                                        "Democrat and Republican")))

#-------------------------#
###### Plotting #######
#-------------------------#
# Figure 1 (in color)
ggplot(pooled, aes(x = feel_partisan_gap, y = type)) +
  geom_violin(aes(fill = type)) +
  geom_boxplot(width = 0.15, col = "white", alpha = 0.4, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c(rep("#ff0000", 3), "#3c3b6e")) +
  # coord_cartesian limits the x axis scale but retains full data for boxplot
  coord_cartesian(xlim = c(-50, 100)) +
  theme_bw(base_size = 13) +
  labs(x = "Gap between in-party and out-party feeling thermometer",
       caption = "Scale of x axis limited to -50; vertical lines show 1st quartile, median, and 3rd quartile",
       y = "") +
  # suppressing legends 
  guides(fill = FALSE)

ggsave("figures/figure_1.pdf", height = 7 , width = 9, dpi = 1000)

#-------------------------#
###### Median difference ######
#-------------------------#

# Computed below are the median differences in affect towards one's own party
# and other parties.The statistics are presented in the text.
pooled %>% 
  group_by(type) %>% 
  summarise(median_gap = median(feel_partisan_gap, na.rm = T))

#-------------------------#
##### Figure 2a ######
#-------------------------#
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

###### After publication: new figure ######
library(extrafont)
library(ggtext)
theme_custom <- function(legend.position = "bottom", base_size = 13,
         hjust.y = 1, axis.text.y = element_text(hjust=0.5)){
  theme_bw(base_size = base_size,
           base_family = "Fira Sans") %+replace%
    theme(legend.position = legend.position,
          panel.grid.minor = element_blank(),
          plot.title = element_markdown(face = "bold", size = rel(1.3), hjust = 0),
          plot.subtitle = element_markdown(face = "plain", size = rel(1.3)),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
          axis.title.y = element_text(margin = margin(r = 10), hjust = hjust.y, angle = 90),
          axis.text.y = axis.text.y)
}
usa_conjoint  %>% 
  filter(partyid == "Republican") %>% 
  prepare_conjoint(formula = f1, by = ~trump_app, country = "usa") %>% 
  filter(level %in% c("    Shut down Congress", "    Not be bound by courts")) %>% 
  # to get rid of "NA" panel but keep attribute headerss
  mutate(BY = ifelse(is.na(estimate), "Strongly approve", as.character(BY)) %>% 
           factor(levels = c("Strongly approve", "Somewhat approve", 
                             "Somewhat disapprove", "Strongly disapprove"))) %>% 
  ggplot(aes(x = estimate, y = fct_rev(level) ,
             xmin = lower, xmax = upper, col = BY)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 3, position = position_dodge(-0.8)) +
  geom_errorbar(width = 0, size = 1, position = position_dodge(-0.8)) +
  labs(y = "",
       x = "Estimated AMCE") +
  scale_x_continuous(breaks = seq(-0.2, 0.2, 0.05),
                     limits = c(-.22, 0.22)) +
  scale_color_grey(name = "Trump approval") +
  theme_custom()

ggsave("figures/fig_7_afterpub.png", height = 7, width = 12)

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

ggsave("figures/figure_s1a.pdf", height = 9, width = 8, dpi = 1000)

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

ggsave("figures/figure_s1b.pdf", height = 9, width = 8, dpi = 1000)

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

ggsave("figures/figure_s2a.pdf", height = 9, width = 8, dpi = 1000)

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

ggsave("figures/figure_s2b.pdf", height = 9, width = 8, dpi = 1000)

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

