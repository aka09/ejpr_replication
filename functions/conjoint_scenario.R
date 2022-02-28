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