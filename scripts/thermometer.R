#### Script information ####
# This script reproduces figure 1 from the article:
# "The Partisan Nature of Support for Democratic Backsliding: A Comparative Perspective"
# Forthcoming in the European Journal of Political Research
#-------------------------------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(magrittr)
library(ggridges)

# Loading in 2019 Canadian Election Study
load("data/ces2019.RData")

# Loading in 2020 American National Election Study
load("data/anes2020.RData")

#-------------------------#
#### Manipulating CES ####
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
#### Manipulating ANES ####
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
#### Figure 1 ####
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
#### Median difference ####
#-------------------------#

# Computed below are the median differences in affect towards one's own party
# and other parties.The statistics are presented in the text.
pooled %>% 
  group_by(type) %>% 
  summarise(median_gap = median(feel_partisan_gap, na.rm = T))
