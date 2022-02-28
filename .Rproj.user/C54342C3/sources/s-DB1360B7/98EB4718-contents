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