This zipped file contains replication materials for the article:
"The Partisan Nature of Support for Democratic Backsliding: A Comparative Perspective"
Forthcoming in the *European Journal of Political Research*. In this readme file 
are instructions on how to navigate the replication files. All analyses were
conducted in R 4.1.0. 
Contact Olivier Bergeron-Boutin (olivier.bergeron-boutin@mail.mcgill.ca) for 
any questions. 

-------------------------
** Data **
-------------------------

"anes_2020.dta" -- Data from the 2020 American National Election Study. Used to 
produce Figure 1 in the main text. 

"can_conjoint.RData" -- Data from a conjoint experiment embedded in a survey fielded 
in Canada in October 2019. The data is at the profile-level: one observation is one 
conjoint candidate that was presented to a respondent. 

"ces2019.dta" -- Data from the 2019 Canadian Election Study. Used to 
produce Figure 1 in the main text. 

"pooled_conjoint.RData" -- Data from the Canadian and American conjoint experiments
in a single file. The data is at the profile-level: one observation is one conjoint 
candidate that was presented to a respondent. 

"us_conjoint.RData" -- Data from a conjoint experiment embedded in a survey fielded 
in the United States in October 2019. The data is at the profile-level: one observation 
is one conjoint candidate that was presented to a respondent. 

------------------------
** R scripts **
------------------------

"amce_canada.R" -- produces figures 2b, 3b, A2b, A3b, A4b, S1b, S2b, S3b, and S4b. 

"amce_usa.R" -- produces figures 2a, 3a, 7, A2a, A3a, A4a, S1a, S2a, S3a, S4a, and S6.

"scenarios.R" -- produces figures 4, 5a, 5b, 6, S5a, and S5b. 

"thermometer.R" -- produces figure 1. 


-------------------------
** Functions **
-------------------------

"conjoint_scenario.R" -- Takes in data from the conjoint experiments and manipulates 
it to recover the % of respondents voting for the undemocratic candidate under 
different scenarios of policy congruence on abortion and welfare.

"plot_subgroup.R" -- Takes in a dataset of conjoint results produced by "prepare_conjoint.R"
and produces a plot of the AMCEs and associated confidence intervals. The function 
can produce plots of all attributes or a limited number of attributes (the "format" argument) and
can add labels next to each estimate that identify the AMCE estimate and the 
confidence interval (the "numbers" argument). 

"prepare_conjoint.R" -- Takes in data from the conjoint experiments and produces 
a dataframe of conjoint results (AMCEs, confidence intervals, etc.). This function 
uses the "cregg" package to produce a dataframe of AMCEs. It then loops through the 
dataset and adds a row identifying each of the 7 candidate attributes. It also 
adds indentation at the beginning of the character values for the attribute levels 
(so that the AMCE plots have a clear separation between attributes and attribute levels). 
