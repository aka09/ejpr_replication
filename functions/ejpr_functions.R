plot_subgroup <- function(data, format = "full", numbers = FALSE, by = NULL){
  
  # storing for use with scale_color_manual
  # by default, the function will use the viridis package to pick colors for the different subgroups
  nb_subgroups <- length(levels(data$BY))
  
  # storing levels of the subgroups
  levels_subgroups <- levels(data$BY)
  
  # To plot only a subset of all attributes
  # 2 options here: the democratic and policy attributes (4 in total), or only
  # the democratic attributes (2 in total)
  if(format == "partial4"){
    data %<>% 
      filter((feature %in% c("Legislative checks", "Judicial checks", "Abortion", "Welfare spending")) |
               (level %in% c("Legislative checks:", "Judicial checks:", "Abortion:", "Welfare spending:")))
  }else if(format == "partial2"){
    data %<>% 
      filter((feature %in% c("Legislative checks", "Judicial checks")) |
               (level %in% c("Legislative checks:", "Judicial checks:")))
  }
  plot <- data %>% 
    mutate(
      # this allows one to easily add a label for each point that shows the AMCE, 
      # the lower bound of the CI and the upper bound of the CI
      # add numbers = TRUE as an argument to the function for this functionality
      label = ifelse(
        estimate != 0,
        paste0(round(estimate,3), " [", round(lower,3), ",", round(upper,3),"]"),
        NA
      ),
      # this creates a category of party ID called "reference", which will be 
      # given the color black and used to represent the reference level of an 
      # attibute 
      BY = if_else(estimate == 0, "reference", as.character(BY)) %>% 
        factor(levels = c("reference", levels_subgroups))
    )
  
  plot <- ggplot(plot, 
                 aes(x = estimate, y = fct_rev(level) ,
                     xmin = lower, xmax = upper, col = fct_rev(BY), label = label)) +
    geom_vline(xintercept = 0) +
    geom_point(size = 3, position = position_dodge(-0.8)) +
    geom_errorbar(width = 0, size = 1, position = position_dodge(-0.8)) +
    labs(y = "",
         x = "Estimated AMCE") +
    scale_x_continuous(breaks = c(-.2, -.1, 0, .1, .2, .3),
                       limits = c(-.28, .335)) +
    # by default, the colors will be black (for reference level) and colors 
    # picked by the viridis package
    # can is overriden after calling the plot_subgroup() function
    scale_color_manual(values = c("black", viridis(nb_subgroups))) +
    theme_cjoint()
  
  # add geom for numbers
  # This will show a label of the type: AMCE [CILOW; CIHIGH]
  if(numbers == TRUE){
    plot <- plot +
      geom_label_repel(size = 3, position = position_dodge(-0.8))
  }
  
  return(plot)
  
}

conjoint_scenario <- function(data, defect_cause, by = "NULL"){
  
  results <- data %>% 
    # analyze at the task level
    group_by(task, id, Country) %>% 
    # only task with one fully democratic and one fully undemocratic candidate
    filter(length(unique(`Legislative checks`)) == 2 & 
             length(unique(`Judicial checks`)) == 2) %>% 
    filter((`Legislative checks` == "Shut down legislature" & 
              `Judicial checks` == "Not be bound by courts") | 
             (`Legislative checks` == "Work with legislature" & 
                `Judicial checks` == "Adhere to court decisions"))
  
  
  if(defect_cause == "abortion_distance"){
    results <- results %>% 
      # Recoding abortion postions to numeric (1-4), with 1 = most favorable 
      mutate(
        abortion_view_numeric = recode(
          abortion_view_recode,
          "Abortion allowed for any reason" = 1,
          "Abortion only in first 12 weeks" = 2,
          "Abortion only mother's life at risk" = 3,
          "Abortion never allowed" = 4
        ),
        # Recoding the conjoint candidate's abortion position to 1-4
        abortion_cand_numeric = recode(
          Abortion, 
          "Abortion allowed for any reason" = 1,
          "Abortion only in first 12 weeks" = 2,
          "Abortion only mother's life at risk" = 3,
          "Abortion never allowed" = 4
        )
      ) %>% 
      dplyr::mutate(
        # distance is the respondents' abortion view (1-4) minus the candidate's abortion view
        distance = abs(abortion_view_numeric - abortion_cand_numeric),
        # inferring different "scenarios" of abortion congruence
        group = case_when(
          # If distance with a given candidate is at minimum (at task level), 
          # the respondent agrees with given candidate at least as much as with other candidate
          # If distance is at minimum but the length of distance is 1, then the 
          # distance is the same with both candidates
          distance == min(distance) & length(unique(distance)) == 2 ~ "The undemocratic candidate",
          distance == min(distance) & length(unique(distance)) == 1 ~ "Both candidates equally",
          distance == max(distance) & length(unique(distance)) == 2 ~ "The democratic candidate",
        ) %>% 
          factor(levels = c("The democratic candidate", "Both candidates equally",
                            "The undemocratic candidate"))
      )
  }else if(defect_cause == "welfare_distance"){
    results <- results %>% 
      mutate(
        welfare_view_numeric = recode(
          welfare_view_recode,
          "Increase welfare spending" = 1,
          "Keep welfare spending the same" = 2,
          "Decrease welfare spending" = 3,
          "Slash welfare spending" = 4
        ),
        welfare_cand_numeric = recode(
          `Welfare spending`, 
          "Increase welfare spending" = 1,
          "Keep welfare spending the same" = 2,
          "Decrease welfare spending" = 3,
          "Slash welfare spending" = 4
        )
      ) %>% 
      dplyr::mutate(distance = abs(welfare_view_numeric - welfare_cand_numeric),
                    group = case_when(
                      distance == min(distance) & length(unique(distance)) == 2 ~ "The undemocratic candidate",
                      distance == min(distance) & length(unique(distance)) == 1 ~ "Both candidates equally",
                      distance == max(distance) & length(unique(distance)) == 2 ~ "The democratic candidate",
                    ) %>% factor(levels = c("The democratic candidate", "Both candidates equally",
                                            "The undemocratic candidate"))
      )
  }else{
    stop("The 'defect_cause' input is not valid.")
  }
  
  # analyzing just the D- candidate
  results <- filter(results, `Legislative checks` == "Shut down legislature" & 
                      `Judicial checks` == "Not be bound by courts")
  
  
  # Results by scenario
  results <- results %>% 
    ungroup() %>% 
    group_by(group, Country, eval(parse(text=by))) %>% 
    dplyr::summarise(mean = mean(selected, na.rm = T),
                     lwr = lwr_conf(selected),
                     upr = upr_conf(selected),
                     n = n())
  
  if(by != "NULL"){
    results <- dplyr::rename(results, by = `eval(parse(text = by))`)
  }
  
  # Adding a label if we want to graph the actual estimates + CI + n
  results <- results %>% 
    mutate(label = paste0(round(mean,3), " [", round(lwr,3), ",", round(upr,3),"]", "n=", n)) %>% 
    filter(!is.na(group)) %>% 
    arrange(group)
  
  
  return(results)
}

prepare_conjoint <- function(data, formula = f1, by = NULL, country = NULL){
  
  if(is.null(by)){
    cat("Please indicate by which subgroup you want to produce the results.")
  }
  
  else if(!(is.null(by))){
    cj <- cregg::cj(data, f1, id = ~id, by = by) %>% 
      mutate(
        level = ifelse(estimate == 0, paste0("(Baseline = ", level, ")"), as.character(level))
      )
    
    # For loop that adds a row identifying the attribute before each group
    # of attribute levels 
    for(i in 1:nrow(cj)){
      if(i == 1){
        cj <- add_row(cj, level = paste(cj$feature[i], ":", sep = ""))
      }else if(cj$feature[i] != cj$feature[i-1]){
        cj <- add_row(cj, level = paste(cj$feature[i], ":", sep = ""))
      }
    }
    
    cj$level <- ifelse(
      !is.na(cj$estimate), paste("    ", cj$level, sep = ""), cj$level
    )
    
    # Reording attributes/attribute levels in desired order 
    if(country == "usa"){
      cj$level <-  factor(cj$level, 
                          levels = c("Legislative checks:", 
                                     "    (Baseline = Work with Congress)", 
                                     "    Shut down Congress",
                                     "Judicial checks:", 
                                     "    (Baseline = Adhere to court decisions)", 
                                     "    Not be bound by courts",
                                     "Abortion:", 
                                     "    (Baseline = Abortion allowed for any reason)", 
                                     "    Abortion never allowed",
                                     "    Abortion only mother's life at risk",
                                     "    Abortion only in first 12 weeks",
                                     "Welfare spending:", 
                                     "    (Baseline = Keep welfare spending the same)", 
                                     "    Decrease welfare spending",
                                     "    Increase welfare spending",
                                     "    Slash welfare spending", 
                                     "Sex:",
                                     "    (Baseline = Male)",
                                     "    Female",
                                     "Political Experience:",
                                     "    (Baseline = No political experience)",
                                     "    Mayor", 
                                     "    Member of Congress", 
                                     "    Member of state legislature",
                                     "Age:",
                                     "    (Baseline = 37)",
                                     "    39",
                                     "    43",
                                     "    45",
                                     "    52",
                                     "    57",
                                     "    61",
                                     "    66",
                                     "    71",
                                     "    75"))
    }
    
    if(country == "canada"){
      cj$level <-  factor(cj$level, 
                          levels = c("Legislative checks:", 
                                     "    (Baseline = Work with Parliament)", 
                                     "    Shut down Parliament",
                                     "Judicial checks:", 
                                     "    (Baseline = Adhere to court decisions)", 
                                     "    Not be bound by courts",
                                     "Abortion:", 
                                     "    (Baseline = Abortion allowed for any reason)", 
                                     "    Abortion never allowed",
                                     "    Abortion only mother's life at risk",
                                     "    Abortion only in first 12 weeks",
                                     "Welfare spending:", 
                                     "    (Baseline = Keep welfare spending the same)", 
                                     "    Decrease welfare spending",
                                     "    Increase welfare spending",
                                     "    Slash welfare spending", 
                                     "Sex:",
                                     "    (Baseline = Male)",
                                     "    Female",
                                     "Political Experience:",
                                     "    (Baseline = No political experience)",
                                     "    Mayor", 
                                     "    Member of Parliament", 
                                     "    Member of provincial legislature",
                                     "Age:",
                                     "    (Baseline = 37)",
                                     "    39",
                                     "    43",
                                     "    45",
                                     "    52",
                                     "    57",
                                     "    61",
                                     "    66",
                                     "    71",
                                     "    75"))
    }
    
    return(cj)
  }
}

# Functions for confidence intervals
lwr_conf <- function(vectors){
  meanci <- MeanCI(vectors, na.rm = T)
  return(nth(meanci, 2))
}

upr_conf <- function(vectors){
  meanci <- MeanCI(vectors, na.rm = T)
  return(nth(meanci, 3))
}