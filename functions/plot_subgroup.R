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