library(ggplot2)
library(scales)
library(stringr)
library(ggpubr)
library(treemapify)


varname = "monitor_issues"
brewerpal = "Accent" 
subtitle = "observations"
title = "Number of issues reported by monitors"
titlebreak = 60
vjust = -14
hjust = .5
facetvar = "district_name" 
phase = FALSE

df = kpi_df
df_kpi <- df
# Plots multiple choice (select_multiple in XLSFom) variables presented as percentages of total obs.

# Creates long auxiliary dataframe with variable of interest (select_multiple)
if(phase == FALSE) {
  df <- df %>% 
    select(internal_cluster, internal_district, district_name, contains(varname)) %>% 
    reshape2::melt(., id.var = c("internal_cluster", "internal_district", "district_name"), 
                   variable.name = varname) %>% 
    na.omit() %>%
    as_tibble() 
  
  # Binds and melts auxiliary dataframe
  df <-  df %>% 
    cbind(str_split_fixed(unlist(df$value), " ", 7)) %>% 
    as.data.frame() %>% mutate_all(., as.character) %>% 
    mutate_all(na_if, "") %>%
    select(-contains(varname),-value) %>% 
    reshape2::melt(., id.var = c("internal_cluster", "internal_district", "district_name")) %>% 
    na.omit() %>% 
    mutate(value = gsub("_", " ", unlist(value))) %>% 
    filter(value != "Other")
} else {

  df <- df %>% 
    select(internal_phase, internal_cluster, internal_district,
           district_name, contains(varname)) %>% 
    reshape2::melt(., id.var = c("internal_phase", "internal_cluster", "internal_district", "district_name"), 
                   variable.name = varname) %>% 
    na.omit() %>%
    as.data.frame() 
  
  # Binds and melts auxiliary dataframe
  df <-  df %>% 
    cbind(str_split_fixed(unlist(df$value), " ", 7)) %>% 
    as.data.frame() %>% mutate_all(., as.character) %>% 
    mutate_all(na_if, "") %>%
    select(-contains(varname),-value) %>% 
    melt(., id.var = c("internal_cluster", "district_name", "internal_phase", "internal_district")) %>% 
    na.omit() 
}

# Lists phases in the dataset 
var_list <- df %>% select_(facetvar) %>% 
  unlist() %>% unique() %>% as.character() 

for (i in seq_along(var_list)) {
  # Filters data
  data <- df %>% 
    filter(UQ(as.name(facetvar)) %in% unlist(var_list)[i])
  
  if (nrow(data) > 1) {
    
    # Plots results with custom function
    plot <- 
      topbarplot(gsub("_", " ", unlist(data$value)) %>% table, 
                 title = paste0(title, " (",gsub("_", " ", 
                                                 unlist(var_list[i])), ")"),
                 brewerpal = brewerpal, 
                 label = subtitle, hjust = hjust, vjust = vjust)
    
    print(plot)
    
  }
}