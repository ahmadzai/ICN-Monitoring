# function to capitalize the names
capitalize <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Function to download data from ONA.IO
fromona <- function(ona_data) {
  
  library(dplyr)
  # ona_data = any JSON file resulting from an REST API GET request.
  
  # Loads required libraries
  library(httr, dplyr, jsonlite)
  
  # Reading data content as text 
  df <- httr::content(ona_data, "text") %>% 
    ## Transforming into JSON (Java Script Oriented Notation)
    jsonlite::fromJSON(., flatten = TRUE)
    # rjson::fromJSON(., simplify = TRUE)

  # Saving data as a R dataframe  
  df <-  as.data.frame(df)
  
  # Partsing file and transforming in tibble
  df <- apply(df, 2, as.character) %>% 
    as_tibble(.) %>% 
    ## Reordering columns alphabetically
    select(noquote(order(colnames(.)))) 
  
  # Adjusts names of dataframe to valid R names
  names(df) <- make.names(names(df), 
                          unique = FALSE, allow_ = TRUE) 
  
  # Replacing dots in variable names for MongoDB
  names(df) <- gsub("\\.", "_", names(df)) 
  
  # Replacing X_ in variable names for simplification
  names(df) <- gsub("^X_", "", names(df)) 
  
  # Returns dataframe
  df
  
}

# function filter by number less then 
filter_table_by_num_less_than <- function(df, variable, problems_num) {
  
  df_mosaic <- df %>%
    # Selects variables of interest
    select(variable,
           region,
           province_name,
           internal_district,
           district_name,
           internal_subdistrict, 
           internal_cluster,
           internal_team_number) %>% 
    na.omit()  %>%
    # Filters those with problems
    filter(UQ(as.name(variable)) < problems_num) %>% 
    # Creates new variable with ID
    # mutate(id = paste(province_name,
    #                   cluster,
    #                   internal_team_number,
    #                   sep="-")
    # ) %>%
    group_by(region, province_name, internal_district, district_name, 
             internal_subdistrict, internal_cluster, internal_team_number) %>%
    dplyr::summarise(freq = n()) %>%
    ungroup() %>%
    rename(Region = region, 
           Province = province_name, 
           District = district_name,
           Subdistrict = internal_subdistrict,
           Cluster = internal_cluster,
           Team = internal_team_number,
           `Freq. Reported` = freq) %>%
    select(Province, District, Subdistrict, Cluster, Team, `Freq. Reported`)
}

# for printing nice word tables, thanks to flextable and officer ;-) 
print_table <- function(data_table, cols_align_center = NULL, 
                        zero_decimal = TRUE, cols_width = NULL) {
  
  # data_table passed to this function should be properly designed
  # should only have columns that need for printing in table
  # column names should also be properly named
  ## cols_align_center = c("col1", "col2", "col3")
  ## zero_decimal (TRUE|FALSE)
  ## cols_width = c(1, 1, 1.2, 2) # for all columns in inch
  
  library(dplyr)
  library(officer)
  library(flextable)
  tbl <- regulartable(data_table) %>%
    
    theme_zebra(., odd_header = "transparent", even_header = "transparent") %>%
    align(align = "left", part = "all") %>%
    # set_header_labels(district_name = "District", 
    #                   internal_monitorname = "Monitor") %>%
    hline(., border = fp_border(width = .75, color = "#007FA6"), part = "body" ) %>%
    
    hline(., border = fp_border(width = 1.25, color = "#007FA6"), part = "header" ) %>%
    
    fontsize(., size = 9, part = "all") %>%
    
    # set calibri font, i like these fonts :-) 
    #font(., fontname = "verdana", part = "all") %>%
    
    set_formatter_type(., fmt_double = "%.01f")
  
  if(zero_decimal == TRUE) {
    tbl <- set_formatter_type(tbl, fmt_double = "%.00f")
  }
  
  if(!is.null(cols_align_center)) {
    tbl <- align(tbl, j = cols_align_center, align = "center", part = "all")
  }
  # print table
  tbl <- autofit(tbl)
  
  if(!is.null(cols_width)) {
    tbl <- width(tbl, j = colnames(data_table), width = cols_width)
  }
  
  tbl
  
}

# function for printing maps for the regions
print_map <- function(map, mapdata, regname) {
  library(ggmap)
  ggmap(map) +
    geom_point(data = mapdata, 
               aes(x = lon, y = lat, 
                   fill = district_name, colour=district_name, shape=internal_phase), 
               size = 2.5, alpha = .7) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
    # Includes border
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
    labs(x="Longitude", y="Latitude", fill="Districts") + 
    labs(color = "Reported district", shape = "Phase of collection") +
    #scale_color_brewer(palette="Set3") +
    # Sets title and subtitle (incl. str_wrap for text wrapping)
    ggtitle(paste(stringr::str_wrap("Submission locations coloured by district name", width = 80),
                  " (", regname, ")", sep = ""),
            subtitle =  
              paste("N = ",
                    format(nrow(mapdata %>% select(lon, lat) %>% na.omit()), 
                           big.mark=","), 
                    " form submissions with GPS coordinates", sep = "")) + 
    # Sets options for subtitles
    theme(plot.subtitle = element_text(size=12, color="black")) + 
    # Sets options for X axis title
    theme(axis.title.x = element_text(size = 12, angle = 00)) + 
    # Sets options for Y axis title
    theme(axis.title.y = element_text(size = 12, angle = 90)) + 
    # Sets options for X axis text
    theme(axis.text.x = element_text(colour="grey20", size = 10,  
                                     face="plain", angle = 00),
          # Sets options for Y axis text
          axis.text.y = element_text(colour="grey20", size = 10, face="plain"), 
          # Sets options for plot title
          plot.title = element_text(size = 14, face = "bold", vjust=1.2)
    ) 
}

# common group by function
my_group_by <- function(df) {
  library(dplyr)
  df <- df %>% group_by(internal_cluster, internal_subdistrict, internal_district, 
                        district_name, province_name, region)
  return(df)
}

# common arrange by function
my_arrange_by <- function(df) {
  library(dplyr)
  df <- df %>% arrange(region, province_name, internal_district, internal_subdistrict, 
                       internal_cluster)
  return(df)
}

# in case the variable not found in the dataset
var_not_found <- function(dataframe) {
  notfound <- rep(FALSE, nrow(dataframe %>%
                                my_group_by(.) %>% 
                                dplyr::summarise(count = dplyr::n())
                              )
                  )
  return(notfound)
}

# common indicators issue function
issues_common <- function(dataframe, varname, meltvars, 
                          selectvars = c("region", "province_name", "district_name",
                                         "internal_district", "internal_subdistrict",
                                         "internal_cluster", "internal_date")) {
  
  library(dplyr)
  library(reshape2)
  library(lubridate)
  
  # Creates rule for missing data (considered as FALSE)
  # Creates rule for missing data (considered as FALSE)
  if (dataframe %>% 
      select(contains(varname), -contains("_other")) %>% 
      length() == 0) { 
    
    return(FALSE)
    
  } else {
    # Performs the main code for available variables        
    dt <- dataframe %>% 
      select(one_of(selectvars), -contains("_other"),
             contains(
               as.character(
                 varname
               )
             )
      )  %>% 
      ## Reshapes from wide to long dataset
      reshape2::melt(., id = meltvars)
    return(dt)
  }
}

# Creates function to output binary result of TRUE or FALSE for indicators for each cluster
indissue <- function(dataframe, varname, issues = c("Disagree", "Strongly_disagree")){
  ## dataframe = any dataframe
  ## varname = name or part of the name of the variable(s) of interest 
  ## Required packages
  library(dplyr)
  library(reshape2)
  library(lubridate)
  

  df <- issues_common(dataframe, varname, c("internal_date", "region", "province_name",
                                            "district_name", "internal_district",
                                            "internal_subdistrict",
                                            "internal_cluster"))
  # Varname not found, will trigger a warning but no worries
  if(df == FALSE) {
    
    return(var_not_found(dataframe))
    
  } else {
  
    df <- df %>%
      mutate(problem = ifelse(value %in% issues, TRUE, FALSE))  %>%
      # Filters unique occurrences
      unique()  %>%
      # Groups by key variables for distinction of units
      my_group_by(.) %>%

      # Detects if any cluster has issues
      dplyr::summarise(issue = any(problem == TRUE))  %>%

      my_arrange_by(.) %>%
      # Selects, ungroup and unlist vector
      ungroup() %>%
      select(issue) %>%
      unlist() %>%
      as.vector()

      return(df)
  }

}


# Creates function to output binary result of TRUE or FALSE for indicators for each cluster
indissue_num_min <- function(dataframe, varname, respondent = "Supervisor", min = 4){
  ## dataframe = any dataframe
  ## varname = name or part of the name of the variable(s) of interest 
  ## Required packages
  library(dplyr)
  library(reshape2)
  library(lubridate)
  
  # Creates rule for missing data (considered as FALSE)
  # Creates rule for missing data (considered as FALSE)
  vars <- c("internal_date", "internal_respondent", 
            "region", "province_name", 
            "district_name", "internal_district", 
            "internal_subdistrict", "internal_cluster")
  df <- issues_common(dataframe, varname, meltvars = vars, selectvars = vars)
  if (df == FALSE) {
    
    return(var_not_found(dataframe))
  
  } else {
    # Performs the main code for available variables   
    df <- df %>%
      ## Omits NAs
      # na.omit() %>% 
      ## Creates variable indicateing if the indicator includes signs of isses
      mutate(problem = ifelse(!(suppressWarnings(as.numeric(value)) >= as.numeric(min)), 
                              TRUE,
                              FALSE)
      )  %>% 
      # Filters unique occurrences
      #unique()  %>%
      # Groups by key variables for distinction of units
      my_group_by(.) %>% 
      # Detects if any cluster has issues
      dplyr::summarise(issue = any(problem == TRUE))  %>%
      
      my_arrange_by(.) %>%
      # Selects, ungroup and unlist vector 
      ungroup() %>% 
      select(issue) %>%
      unlist() %>% 
      as.vector()
    
    return(df)
    
  }
}




# Creates function to output binary result of TRUE or FALSE for indicators for each cluster
indissue_phone <- function(dataframe, varname = "cellphone"){
  ## dataframe = any dataframe
  ## varname = name or part of the name of the variable(s) of interest 
  ## Required packages
  library(dplyr)
  library(reshape2)
  library(lubridate)
  
  # Creates rule for missing data (considered as FALSE)
  df <- issues_common(dataframe, varname, c("internal_date", "region", "province_name", 
                                            "district_name", "internal_district", 
                                            "internal_subdistrict", "internal_cluster"))
  if (df == FALSE) {
    
    return(var_not_found(dataframe))
    
  } else {
    # Performs the main code for available variables 
    df <- df %>%
      ## Omits NAs
      # na.omit() %>% 
      ## Creates variable indicateing if the indicator includes signs of issues
      mutate(problem = ifelse(all(is.na(value)), TRUE, FALSE))  %>% 
      # Filters unique occurrences
      #unique()  %>%
      # Groups by key variables for distinction of units
      my_group_by(.) %>% 
      # Detects if any cluster has issues
      dplyr::summarise(issue = any(problem == TRUE))  %>%
      
      my_arrange_by(.) %>%
      # Selects, ungroup and unlist vector 
      ungroup() %>% 
      select(issue) %>%
      unlist() %>% 
      as.vector()
    
    
    return(df)
    
  }
  
}



# Function to counts TRUE and FALSE observations resulting from a pre-specified indicator criteria 
# based on number of occurrences of choice options (for multiple_count questions stored as string in a single column)

indissue_count <- function(dataframe, varname, min = 4, issues = c("Other", "None_of_the_above", "Not_sure")){
  ## dataframe = any dataframe
  ## varname = name or part of the name of the variable(s) of interest 
  ## min = minimum number of occurences of words (selection options)  
  ## count_options = String with answer options that should be counted   
  
  ## Required packages
  library(dplyr)
  library(reshape2)
  library(lubridate)
  
  # Creates rule for missing data (considered as FALSE)
  vars <- c("internal_date", "internal_respondent", "region", "province_name", 
            "district_name", "internal_district", 
            "internal_subdistrict", "internal_cluster")
  df <- issues_common(dataframe, varname, meltvars = vars,
                      selectvars = vars)
  if (df == FALSE) {
    
    return(var_not_found(dataframe))
    
  } else {
    # Performs the main code for available variables   
    df <- df %>% 
      # Adjusts class of dataframe (for visualisation during development)
      as_tibble(.) %>% 
      # Groups auxiliary dataframe
      my_group_by(.) 
    
    # Removing options that are not of interest
    df$value <-  gsub(paste(issues,
                            collapse="|"), "",  
                      df$value) 
    
    df$count <- ifelse(is.na(df$value), NA,
                       sapply(strsplit(df$value, " "), 
                              length
                       ))
    
    df <- df %>% 
      # Creates variable with count
      mutate(problem = ifelse(
        count >= min,
        FALSE, # Indicates no problem
        TRUE # Indicates problem
      ))  %>%
      select(count, internal_cluster, internal_district, 
             district_name, province_name, region,
             problem, value, internal_respondent) %>% 
      #unique()  %>% 
      my_group_by(.) %>% 
      # Detects if any cluster has issues
      dplyr::summarise(issue = any(problem == TRUE)) %>%
      
      my_arrange_by(.) %>%
      # Selects, ungroup and unlist vector 
      ungroup() %>% 
      select(issue) %>%
      unlist() %>% 
      as.vector()
      
      return(df)
    
  }
  
}



# Function to check number of months from a string variable with full month name.

indissue_monthcount <- function(dataframe, varname, max = 2){
  ## dataframe = any dataframe
  ## varname = name or part of the name of the variable(s) of interest 
  ## Required packages
  library(dplyr)
  library(reshape2)
  library(lubridate)
  library(zoo)
  
  # Creates rule for missing data (considered as FALSE)
  df <- issues_common(dataframe, varname, c("internal_date", "region", "province_name", 
                                            "district_name", "internal_district", 
                                            "internal_subdistrict", "internal_cluster"))
  if (df == FALSE) {
    
    return(var_not_found(dataframe))
    
  } else {
    
    # Creates auxiliary factor vector with months  
    months <- factor(
      c("January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December"),
      levels = c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December"
      )
    )
    
    # Performs the main code for available variables  
    df <- df %>%
      # Filters unique occurrences
      #unique() # %>%
      
      # Creates variable with year and month
      mutate(yearmon = ifelse(
        # Saves NAs
        is.na(value), NA,
        # Pastes year of reporting to variable  
        paste(
          zoo::as.yearmon(ymd(internal_date)) %>% format("%Y"),
          # Transforms month data using auxiliary vector
          factor(value, levels = levels(months)) %>% 
            ## Changes to numeric class
            as.numeric() %>% 
            ## Adds leading zero
            paste0(ifelse(.>9, "", "0"),.) 
          
          , 
          
          sep = "-") %>% 
          paste0("-01")
      )
      )
    
    # Converts auxiliary variable into dates     
    df$yearmon <- df$yearmon %>% lubridate::ymd() %>% as.POSIXlt()
    
    # Creates variable with elapsed months from internal_date's date to last payment
    df$count <- 12 * 
      (as.POSIXlt(ymd(df$internal_date))$year - df$yearmon$year) + 
      (as.POSIXlt(ymd(df$internal_date))$mon - df$yearmon$mon)
    
    df <- df %>% select(-yearmon) %>%  
      # Creates variable with count
      mutate(problem = ifelse(
        count <= max,
        FALSE, # Indicates no problem
        TRUE # Indicates problem
      )) %>%
      select(count, internal_cluster, internal_subdistrict, internal_district, 
             district_name, province_name, region,
             problem, value) %>% 
      # unique() %>% 
      my_group_by(.) %>% 
      # Detects if any cluster has issues
      dplyr::summarise(issue = any(problem == TRUE)) %>%
      
      my_arrange_by(.) %>%
      # Selects, ungroup and unlist vector 
      ungroup() %>% 
      select(issue) %>%
      unlist() %>% 
      as.vector()
    
    return(df)
    
  }
  
}



# Function to plot select_multiple variables
multiplot <- function(df, 
                      varname = "monitor_tasks", 
                      brewerpal = "Set2", 
                      subtitle = "observations",
                      title = "Plot", 
                      titlebreak = 60,
                      vjust = -14,
                      hjust = .5,
                      facetvar = "internal_phase", 
                      phase = FALSE
) {
  
  # Creates long auxiliary dataframe with variable of interest (select_multiple)
  if(phase == FALSE) {
    df <- df %>% 
      select(internal_cluster, internal_district, internal_subdistrict, 
             district_name, contains(varname)) %>% 
      reshape2::melt(., id.var = c("internal_cluster", "internal_subdistrict", 
                                   "internal_district", "district_name"), 
                     variable.name = varname) %>% 
      na.omit() %>%
      as_tibble() 
    
    # Binds and melts auxiliary dataframe
    df <-  df %>% 
      cbind(str_split_fixed(unlist(df$value), " ", 7)) %>% 
      as.data.frame() %>% mutate_all(., as.character) %>% 
      mutate_all(na_if, "") %>%
      select(-contains(varname),-value) %>% 
      reshape2::melt(., id.var = c("internal_cluster", "internal_subdistrict", 
                                   "internal_district", "district_name")) %>% 
      na.omit() %>% 
      mutate(value = gsub("_", " ", unlist(value))) %>% 
      filter(value != "Other")
  } else {
    
    df <- df %>% 
      select(internal_phase, internal_cluster, internal_subdistrict,
             internal_district,
             district_name, contains(varname)) %>% 
      reshape2::melt(., id.var = c("internal_phase", "internal_cluster", "internal_subdistrict",
                                   "internal_district", "district_name"), 
                     variable.name = varname) %>% 
      na.omit() %>%
      as.data.frame() 
    
    # Binds and melts auxiliary dataframe
    df <-  df %>% 
      cbind(str_split_fixed(unlist(df$value), " ", 7)) %>% 
      as.data.frame() %>% mutate_all(., as.character) %>% 
      mutate_all(na_if, "") %>%
      select(-contains(varname),-value) %>% 
      melt(., id.var = c("internal_cluster", "internal_subdistrict",
                         "district_name", "internal_phase", "internal_district")) %>% 
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
}


# Custom function to plot results

evalbarplot <- function(df, variable, title, ylab = "Respondents", brewerpal = "RdYlGn", varlevel = "likert", title_break = 60) {
  ## df = some dataframe
  ## variable = variables of interest for summary
  ## varz must be numeric
  ## maxval = maximum number of summary rows
  ## title = string with plot title
  ## ylab = label for vertical axis
  ## brewerpal = string with brewer.pal palette (see: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)
  ## Plots will exclude results if the palette number of colors has less 
  ## colors than bars.
  ## max.obs = numeric value limiting the number of results to plot
  
  # Loads required packages
  library(ggplot2) ## For plotting
  library(scales) ## For % annotation
  library(dplyr) ## For data wrangling
  library(lazyeval) ## For summarising with the interp function
  library(stringr) ## For text wrapping (e.g. titles)
  library(RColorBrewer) ## For plot colors
  library(treemapify)
  
  
  
  # Creates vector with evaluation scales
  likert <- c("Strongly agree", "Agree", "Slightly agree", 
              "Slightly disagree", "Disagree", "Strongly disagree")
  
  vgood_vpoor <- c("Very good", "Good", "Regular", "Poor", "Very poor",
                   "Not sure", "Not applicable")
  
  vhigh_vlow <- c("Very high", "High", "Average", "Low", "Very low",
                  "Not sure", "Not applicable")
  
  # Checks if variable exists in the dataset
  if (
    variable %in% names(df)
  ){
    
    df_summary <- df  %>% 
      # Groups by key variable
      select_(variable)
    
    # Replaces underscores
    df_summary <- data.frame(lapply(df_summary, function(x) {
      gsub("_", " ", x)
    }))
    
    
    df_summary[,1] <- if(
      varlevel == "likert"
    ){
      factor(df_summary[,1] %>% unlist(),
             levels = c("Strongly agree", "Agree", "Slightly agree", 
                        "Slightly disagree", "Disagree", "Strongly disagree"))
    } else {
      if(
        varlevel == "vgood_vpoor"
      ){
        factor(df_summary[,1] %>% unlist(),
               levels =c("Very good", "Good", "Regular", "Poor", "Very poor",
                         "Not sure", "Not applicable"))
      } else {
        if(
          varlevel == "vhigh_vlow"
        ){
          factor(df_summary[,1] %>% unlist(),
                 levels = c("Very high", "High", "Average", "Low", "Very low",
                            "Not sure", "Not applicable"))
        }
      }
    }
    
    
    # Subsets dataset 
    df_summary <-  df_summary %>% 
      # Groups by key variable
      group_by_(variable)  %>% 
      ## Summarises frequencies by answer option
      dplyr::summarise(freq = n()) %>% 
      ## Omits NAs
      na.omit() 
    
    if (nrow(df_summary) >=1) {
      
      # Colours
      colours = (brewer.pal(name="RdYlGn", n=nlevels(df_summary[,1] %>% unlist())))
      names(colours) = rev(levels(df_summary[,1] %>% unlist()))
      
      # Plots data
      ggplot(data = df_summary, 
             aes(x = df_summary[,1] %>% unlist(), y = freq, 
                 fill = df_summary[,1] %>% unlist()
             ) 
      ) +
        # Determines type of plot
        geom_bar(stat = "identity") + 
        # Sets scale to also show empty levels of factor  
        scale_x_discrete(drop=FALSE) + 
        # Flips plot coordinates  
        coord_flip()  +
        # fills colors
        scale_fill_manual(values=colours) +
        # Uses classic theme 
        theme_classic() +
        # Sets labels and title
        labs(x = "",  y =  ylab, 
             # Wraps plot title to 70 characters
             title = str_wrap(title, width = title_break),
             # Creates subtitle with number of observations
             subtitle =  
               paste("N = ",
                     format(sum(df_summary[,2]), 
                            big.mark=","), " obs.", sep = "")
        )  + 
        # Sets options for subtitles
        theme(plot.subtitle = element_text(size=11, color="black")) + 
        # Sets options for X axis title
        theme(axis.title.x = element_text(size = 11, angle = 00)) + 
        # Sets options for Y axis title
        theme(axis.title.y = element_text(size = 11, angle = 90)) + 
        # Sets options for X axis text
        theme(axis.text.x = element_text(colour="grey20", size = 11,  
                                         face="plain", angle = 00),
              # Sets options for Y axis text
              axis.text.y = element_text(colour="grey20", size = 11, face="plain"), 
              # Sets options for plot title
              plot.title = element_text(size = 12, face = "bold", vjust=1.2)
        ) +
        # Sets options for X axis line
        theme(axis.line.x = element_line(color = "black", size = 0.5), 
              # Sets options for Y axis
              axis.line.y = element_line(color = "black", size = 0.5)
        ) +
        # Sets options for data labels (text annotations)
        geom_text(
          aes(label = 
                percent(
                  freq / sum(freq)
                )
          ), 
          hjust = -0.2, size = 3.5) +
        
        scale_y_continuous(expand = c(.01, .05), # Space between bars and axis
                           # Setting automatic limites based on data
                           limits = c(0, max(df_summary[, 2]*1.10)),
                           # Ensuring only integer breaks                            
                           breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))
        ) +
        # Removes legend 
        theme(legend.position="none")
      
    }
  }
}



# Function to plot tree maps 
toptreemap <- function(df, 
                       variable = "monitor_vaccinerecords", # Example of variable
                       title = "Some title", # Example of title
                       brewerpal = "Set3", # Brewer palette see display.brewer.all()
                       problems = "No", # Criteria of problems (single or multiple options)
                       title_break = 70) { # Number of characters for line break in the title
  
  if (
    variable %in% names(df)
  ){
    
    df_mosaic <- df %>%
      # Selects variables of interest
      select(variable,
             region,
             province_name,
             district_name,
             internal_cluster,
             internal_team_number)  %>%
      # Filters those with problems
      filter(UQ(as.name(variable)) %in% problems) %>% 
      # Creates new variable with ID
      mutate(id = paste(province_name,
                        district_name,
                        internal_cluster,
                        internal_team_number,
                        sep="-")
      ) %>%
      group_by(region, district_name, id) %>%
      dplyr::summarise(freq = n())
    
    
    # Creates expanded color pallete (up to 25)
    mypal <- colorRampPalette(brewer.pal(12, "Reds"))
    
    if(nrow(df_mosaic) >= 1) {
      
      p <- ggplot2::ggplot(df_mosaic, ggplot2::aes(area = freq, fill = id, label=id, subgroup=region),
                           fixed = TRUE) +
        geom_treemap() +
        # Positions legend 
        theme(legend.position="none") + 
        # Customize your title
        labs(title=str_wrap(title, title_break)) +
        # Adds labels to treemap
        geom_treemap_text(color = "black", size = 10, place = "centre") +
        # Fills colors
        scale_fill_manual( values = mypal(nrow(df_mosaic)) ) + 
        #scale_fill_manual( values = rep(c("#FF6633"), nrow(df_mosaic)) ) + 
        theme(plot.title = element_text(size = 12, face = "bold"))  
      
      return(p)
      
    }
  }
}



# Function to plot tree maps 
toptreemap_num <- function(df, 
                           variable = "paydelay", # Example of variable
                           tree_title = "Cases where social mobilisers reported delay in payments", # Example of title
                           brewerpal = "Reds", # Brewer palette see display.brewer.all()
                           problems_num = 50, # Criteria of problems (single or multiple options)
                           tree_titlebreak = 65,
                           lessthan = FALSE) { # Number of characters for line break in the title
  
  library(treemapify)
  
  # Checks if variable exists in the dataset
  if (
    variable %in% names(df)
  ){
    if(lessthan == TRUE) {
      
      df_mosaic <- df %>%
        # Selects variables of interest
        select(variable,
               region,
               province_name,
               district_name,
               internal_subdistrict,
               cluster,
               internal_team_number) %>% 
        na.omit()  %>%
        # Filters those with problems
        filter(UQ(as.name(variable)) < problems_num) %>% 
        # Creates new variable with ID
        mutate(id = paste(province_name,
                          cluster,
                          internal_team_number,
                          sep="-")
        ) %>%
        group_by(region, district_name, id) %>%
        dplyr::summarise(freq = n()) 
      
    }else{
      
      df_mosaic <- df %>%
        # Selects variables of interest
        select(variable,
               region,
               province_name,
               district_name,
               cluster,
               internal_team_number) %>% 
        na.omit()  %>%
        # Filters those with problems
        filter(UQ(as.name(variable)) > problems_num) %>% 
        # Creates new variable with ID
        mutate(id = paste(province_name,
                          cluster,
                          internal_team_number,
                          sep="-")
        ) %>%
        group_by(region, district_name, id) %>%
        dplyr::summarise(freq = n()) 
      
    }
    
    # Creates expanded color pallete (up to 25)
    mypal <- colorRampPalette(brewer.pal(9, "Reds"))
    
    
    if(nrow(df_mosaic) >= 1) {
      
      ggplot2::ggplot(df_mosaic, 
                      ggplot2::aes(area = freq, fill = id, label=id)) +
        geom_treemap() +
        # Positions legend 
        theme(legend.position="none") + 
        # Customize your title
        labs(title=str_wrap(tree_title, tree_titlebreak)) +
        # Adds labels to treemap
        geom_treemap_text(color = "black", size = 10, 
                          fontface = "bold", place = "centre") + 
        # Fills colors
        scale_fill_manual( values = mypal(nrow(df_mosaic)) ) + 
        theme(plot.title = element_text(size = 12, face = "bold"))  
      
    }
  }
}



dfbarplot <- function(df, varz, varx, vary, maxval = Inf, 
                      title = "Plot", ylab = "Observations", 
                      brewerpal = "RdYlGn", dist = 1.2, textsize = 2.5, 
                      angle = 30, hjust = 0.2, colour="lightgray", 
                      titlebreak = 70, vjust = -1.1, scales = "free_x", nrow = NULL, ncol = NULL) {
  ## df = some two column summary dataframe
  ## varz, varx and varz = variables of interest for summary
  ## varz must be numeric
  ## maxval = maximum number of summary rows
  ## title = string with plot title
  ## ylab = label for vertical axis
  ## brewerpal = string with brewer.pal palette (see: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)
  ## Plots will exclude results if the palette number of colors has less 
  ## colors than bars.
  ## max.obs = numeric value limiting the number of results to plot
  ## dist = distance between barplot top and plot frame
  
  # Loads required packages
  library(ggplot2) ## For plotting
  library(scales) ## For % annotation
  library(dplyr) ## For data wrangling
  library(lazyeval) ## For summarising with the interp function
  library(stringr) ## For text wrapping (e.g. titles)
  
  # Subsets dataset 
  df_summary <- df %>% 
    select_(varz, varx, vary)  %>% 
    ## Omits NAs
    na.omit() %>% 
    ## Groups subset by region
    group_by_(varz, varx) %>% 
    dplyr::summarize_(total = interp(~sum(x), x=as.name(vary))) %>% 
    rename_(x = varz, y = varx)  %>% 
    ## Arranges results in descending order
    arrange(desc(total)) %>%
    ## Heads only top five
    head(maxval)
  
  ggplot(data = df_summary, 
         aes(x = reorder(y, -total, sum), 
             y = total)) + 
    # Sets plot type as bar plot
    geom_bar(aes(
      #reorder(df_summary[,1], -total, sum),
      fill = reorder(y, -total, sum)), 
      stat = "identity", position = "dodge", colour=colour) +
    # Sets color palette
    scale_fill_brewer(palette = brewerpal, name = varz)  +
    # Sets plot theme
    theme_bw() +
    # Prepares plot annotations in % (uses package scales)
    geom_text(aes(label = scales::percent(total/sum(total))
    ),  
    vjust=vjust, hjust=hjust, color="black", size=textsize, angle = angle) +
    facet_wrap(names(df_summary[,1]), 
               # Drops unused levels in facets
               scales = scales, nrow = nrow, ncol = ncol) +
    
    # Removes plot legend
    theme(legend.position="") +
    # Sets axis formatting
    theme(axis.text.x = element_text(angle = angle, vjust=.5, hjust = .5)) +
    theme(axis.title.x = element_blank()) + 
    theme(plot.title = element_text(size = 14, face = "bold")) +
    # Sets title and subtitle (incl. str_wrap for text wrapping)
    ggtitle(stringr::str_wrap(title, width = titlebreak)) +
    # Adjust scales 
    scale_y_continuous(limits = c(0, (max(df_summary$total) * 1.1))) +
    # Defines label for y axis (must be defined upon function call)
    labs(y = ylab) +
    # Adjust scales 
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) *
                                                                      1.1))))) +
    coord_cartesian(ylim = c(0, max(df_summary$total) * dist))
  
}


# Creates custom function to plot barplots from tables

topbarplot <- function(table, title = "Some plot", label = "Participants", 
                       brewerpal = "Accent", max.obs = Inf, titlebreak = 60, vjust = 0.5, hjust = -5) {
  ## table = some table
  ## title = string with plot title
  ## label = label of horizontal axis
  ## brewerpal = string with brewer.pal palette (see: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)
  ## max.obs = numeric value limiting the number of results to plot
  
  library(ggplot2)
  library(scales)
  library(stringr)
  library(treemapify)
  
  if (length(table) == 1) {
    # Condition for plotting varibales with one level (e.g. subsets)
    
    ## Transforming auxiliary dataset
    table <- as.data.frame(table)
    
    ## Changes from frequency table to dataframe  
    # table <- rep(table[1], table[2]) %>% 
    #   as.matrix() %>% 
    #   as.data.frame(row.names = FALSE) %>% 
    #   rename(value=V1) %>% as.data.frame()
    
    table <- rep(table[1], table[2]) %>% 
      as.matrix() %>% 
      as.data.frame() %>% 
      rename(value=V1) %>% as.data.frame()
    
    ## Transforming in factor
    table$value <- table$value %>% unlist() %>% factor()
    
    p <- ggplot(table, 
                aes(x=value)) +
      geom_bar(aes(fill = value), stat = "count") + 
      theme_minimal() + 
      coord_flip() + 
      theme(legend.position="none")  + 
      scale_fill_brewer(palette = brewerpal) + 
      # Includes plot title 
      ggtitle(str_wrap(title, width = titlebreak),
              subtitle =  
                paste("N = ",
                      nrow(table), " obs.", sep = "")
      ) + 
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = 12), 
            axis.title=element_text(size = 12)) + 
      theme(plot.title = element_text(size = 14, face = "bold")) +
      # Adjust scales 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      #breaks = pretty_breaks()) +
      # Includes % values on the plot
      geom_text(aes(y = ((..count..)/sum(..count..)), 
                    label = scales::percent((..count..)/sum(..count..))),
                stat = "count",   
                vjust = vjust, hjust = hjust, color = "black", size = 4) 
    # Prints plot
    p 
    
  } else{
    
    # Subsets dataset 
    table <- table %>% 
      ## Sorts table in decreasing order
      sort(decreasing = TRUE) %>% 
      ## Converts table into a dataframe
      as.data.frame() %>% 
      ## Heads max. of desired results
      head(max.obs) 
    
    p <- ggplot(table, aes(x=table[,1], fill = table[,1], 
                           y=table[,2])) + labs(y = label) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_minimal() + 
      coord_flip() + 
      theme(legend.position="none")  + 
      # Includes plot title 
      ggtitle(str_wrap(title, width = 60),
              subtitle =  
                paste("N = ",
                      format(sum(table[,2]), 
                             big.mark=","), " obs.", sep = "")
      ) + 
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      # Prints values
      geom_text(aes(label=scales::percent(table[,2]/table %>% 
                                            select(Freq) %>% unlist() %>% sum())), 
                hjust = -0.2, size = 4)  +
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = 12), 
            axis.title=element_text(size = 12)) + 
      theme(plot.title = element_text(size = 14, face = "bold")) +
      # Adjust scales 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))), 
                         limits = c(0, max(table[,2]*1.15))) 
    
    # Runs condition on number of levels for setting colours
    if(length(table) <= 12){ 
      # Sets color palette  
      
      p + scale_fill_brewer(palette = brewerpal)
      
    }else{
      
      # Sets color palette  
      p + scale_color_hue()
      
    }
  }
}

# Bar Plot improved
barplot <- function(table, title, brewerpal, max.obs = Inf, titlebreak=60) {
  
  library(ggplot2)
  library(scales)
  library(stringr)

  
  if (length(table) == 1) {
    # Condition for plotting varibales with one level (e.g. subsets)
    
    ## Transforming auxiliary dataset
    table <- as.data.frame(table)
    
    vjust = 0.5 
    hjust = -5
    
    ## Changes from frequency table to dataframe  
    table <- rep(table[1], table[2]) %>% 
      as.matrix() %>% 
      as.data.frame() %>% 
      rename(value=V1) %>% as.data.frame()
    
    ## Transforming in factor
    table$value <- table$value %>% unlist() %>% factor()
    
    p <- ggplot(table, 
                aes(x=value)) +
      geom_bar(aes(fill = value), stat = "count") + 
      theme_minimal() + 
      coord_flip() + 
      theme(legend.position="none")  + 
      scale_fill_brewer(palette = brewerpal) + 
      # Includes plot title 
      ggtitle(str_wrap(title, width = titlebreak),
              subtitle =  
                paste("N = ",
                      nrow(table), " obs.", sep = "")
      ) + 
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = 12), 
            axis.title=element_text(size = 12)) + 
      theme(plot.title = element_text(size = 14, face = "bold")) +
      # Adjust scales 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      #breaks = pretty_breaks()) +
      # Includes % values on the plot
      geom_text(aes(y = ((..count..)/sum(..count..)), 
                    label = scales::percent((..count..)/sum(..count..))),
                stat = "count",   
                vjust = vjust, hjust = hjust, color = "black", size = 4) 
    # Prints plot
    p 
    
  } else {
    
    table <- table %>% 
      ## Sorts table in decreasing order
      sort(decreasing = TRUE) %>% 
      ## Converts table into a dataframe
      as.data.frame() %>% 
      ## Heads max. of desired results
      head(max.obs) 
    p <- ggplot(table, aes(x=table[,1], fill = table[,1], 
                           y=table[,2])) + labs(y = label) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_minimal() + 
      coord_flip() + 
      theme(legend.position="none")  + 
      # Includes plot title 
      ggtitle(str_wrap(title, width = 60),
              subtitle =  
                paste("N = ",
                      format(sum(table[,2]), 
                             big.mark=","), " obs.", sep = "")
      ) + 
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      # Prints values
      geom_text(aes(label=scales::percent(table[,2]/table %>% 
                                            select(Freq) %>% unlist() %>% sum())), 
                hjust = -0.2, size = 4)  +
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = 12), 
            axis.title=element_text(size = 12)) + 
      theme(plot.title = element_text(size = 14, face = "bold")) +
      # Adjust scales 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))), 
                         limits = c(0, max(table[,2]*1.15))) 
    
    # Runs condition on number of levels for setting colours
    if(length(table) <= 12){ 
      # Sets color palette  
      
      p + scale_fill_brewer(palette = brewerpal)
      
    }else{
      
      # Sets color palette  
      p + scale_color_hue()
      
    }
  }
}



topbartreeplot <- function(
  df,
  variable = "sm_access", # Variable 
  brewerpal = "Set1",  # Colour palette for barplot
  subtitle = "observations", # Subtitle clarification for barplot
  title = "Plot", # Barplot title
  titlebreak = 90, # Number of characters in barplot title
  hjust = -0.1, # Horizontal justification of barplot annotation
  tree_title = "Cases that social mobilizers can visit few or very few houses in their assigned area", # Title of tree map
  tree_titlebreak = 40, # Number of characters in tree map 
  problems = c("Can_visit_few_houses", "Can_visit_very_few_houses"), # Parameters for problems
  label = "Observations",
  single_hjust = -5 # For plots with single variable only
) { 
  
  library(ggplot2)
  library(scales)
  library(stringr)
  library(ggpubr)
  library(forcats)
  library(vcdExtra)
  library(RColorBrewer)
  
  
  # Loads required custom function for tree maps
  ############  
  
  # Cleaning key variable and problems
  df[variable] <-  gsub("_", " ", unlist(df[variable]) %>% as.vector()) 
  problems <- gsub("_", " ", problems)
  
  # Prepares auxiliary table  
  table <- df %>% 
    select_(variable) %>% 
    unlist()  %>% 
    table() %>% 
    as.data.frame() 
  
  
  # Adjusts names
  names(table) <- c(variable, "freq")  
  
  
  # # Condition for plotting varibales with one level (e.g. subsets)    
  if (nrow(table) == 1) {
    
    ## Changes from frequency table to dataframe
    table <- vcdExtra::expand.dft(table, freq = "freq") %>% 
      as_tibble
    
    ## Changing names of auxiliary dataset
    names(table) <-  "value"
    
    p <- ggplot(table,
                aes(x=value)) +
      labs(y = label) +
      geom_bar(aes(fill = value), stat = "count") +
      theme_minimal() +
      coord_flip() +
      theme(legend.position="none") +
      scale_fill_brewer(palette = brewerpal) +
      # Includes plot title
      ggtitle(str_wrap(title, width = titlebreak),
              subtitle =
                paste("N = ",
                      nrow(table), " obs.", sep = "")
      ) +
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = 11),
            axis.title=element_text(size = 11)) +
      theme(plot.title = element_text(size = 13, face = "bold")) +
      # Adjust scales
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      #breaks = pretty_breaks()) +
      # Includes % values on the plot
      geom_text(aes(y = ((..count..)/sum(..count..)),
                    label = scales::percent((..count..)/sum(..count..))),
                stat = "count",
                vjust = 0.5, hjust = single_hjust, color = "black", size = 4)
    
    # Prints plot
    return(p)
    
  }else{
    
    p <- ggplot(table, 
                aes_string(
                  # Reorders x by frequency 
                  x = paste0("reorder(", colnames(table)[1],", -freq)"), 
                  fill = variable, 
                  y="freq")) + 
      labs(y = label) +
      geom_bar(position = "dodge", stat = "identity") +
      theme_minimal() +
      coord_flip() +
      theme(legend.position="none") +
      scale_fill_brewer(palette = brewerpal) +
      # Includes plot title
      ggtitle(str_wrap(title, width = titlebreak),
              subtitle =
                paste("N = ",
                      format(sum(table$freq),
                             big.mark=","), " obs.", sep = "")
      ) +
      # Removes title for y axis
      theme(axis.title.y = element_blank()) +
      # Prints values
      geom_text(aes(label=scales::percent(table$freq/table %>%
                                            select(freq) %>% unlist() %>% sum())),
                hjust = -0.2, size = 4)  +
      # Changes size and format of axis and title
      theme(axis.text=element_text(size = 11),
            axis.title=element_text(size = 11)) +
      theme(plot.title = element_text(size = 13, face = "bold")) +
      # Adjust scales
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))),
                         limits = c(0, max(table$freq*1.15)))
    
    
    return(p)
    
  }
}



fastplot <- function(dataframe, 
                     varname = "sm_access", 
                     title = "Plot", brewerpal = "Accent", vjust = 0.5, hjust = -4) {
  # Creates auxiliary dataframe
  df <- dataframe %>% 
    # Selects variable of interest
    select_(varname) %>% 
    # Ommits NAs (CHECK CAREFULLY EACH CASE TO SEE IF THIS IS OKAY)
    na.omit() %>% 
    # Renames variable for plotting
    rename_(value = varname) %>% 
    # Replaces underscores by spaces
    mutate(value = gsub("_", " ", value)) %>% 
    # Filter character NAs
    filter(value != "NA")  
  
  # Creates plot
  ggplot(data = df, aes(value)) +
    geom_bar(aes(fill = value, group = value), stat = "count", position = "dodge") +
    theme_classic() + 
    # Removes title for y axis
    theme(axis.title.y = element_blank()) +
    # Changes size and format of axis and title
    theme(axis.text=element_text(size = 12), 
          axis.title=element_text(size = 12)) + 
    theme(plot.title = element_text(size = 14, face = "bold")) + 
    coord_flip() + 
    theme(legend.position="none")  + 
    # Removes title for y axis
    theme(axis.title.y = element_blank()) +
    # Sets color palette
    scale_fill_brewer(palette = brewerpal) +
    # Sets title and subtitle (incl. str_wrap for text wrapping)
    ggtitle(stringr::str_wrap(title, width = 64),
            subtitle =  
              paste("N = ",
                    format(nrow(df), 
                           big.mark=","), " obs.", sep = "")
    ) +
    # Adjust scales 
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    #breaks = pretty_breaks()) +
    # Includes % values on the plot
    geom_text(aes(y = ((..count..)/sum(..count..)), 
                  group = value,
                  label = scales::percent((..count..)/sum(..count..))),
              stat = "count",
              position = position_dodge(width = 1),
              vjust = vjust, hjust = hjust, color = "black", size = 4) 
  
  
}


# Function to print binary plots with colors indicating problems
fastplot_binary <- function(dataframe, 
                            varname = "sm_access", 
                            title = "Plot", 
                            palette = c("#0072B2", "#D55E00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7"),
                            vjust = 0.5, hjust = -4) {
  # Creates auxiliary dataframe
  df <- dataframe %>% 
    # Selects variable of interest
    select_(varname) %>% 
    # Ommits NAs (CHECK CAREFULLY EACH CASE TO SEE IF THIS IS OKAY)
    na.omit() %>% 
    # Renames variable for plotting
    rename_(value = varname) %>% 
    # Replaces underscores by spaces
    mutate(value = gsub("_", " ", value)) %>% 
    # Filter character NAs
    filter(value != "NA")  
  
  # Creates plot
  ggplot(data = df, aes(value)) +
    geom_bar(aes(fill = value), stat = "count") +
    theme_classic() + 
    # Removes title for y axis
    theme(axis.title.y = element_blank()) +
    # Changes size and format of axis and title
    theme(axis.text=element_text(size = 12), 
          axis.title=element_text(size = 12)) + 
    theme(plot.title = element_text(size = 14, face = "bold")) + 
    coord_flip() + 
    theme(legend.position="none")  + 
    # Removes title for y axis
    theme(axis.title.y = element_blank()) +
    # Sets color palette
    scale_fill_manual(values = palette) +
    # Sets title and subtitle (incl. str_wrap for text wrapping)
    ggtitle(stringr::str_wrap(title, width = 64),
            subtitle =  
              paste("N = ",
                    format(nrow(df), 
                           big.mark=","), " obs.", sep = "")
    ) +
    # Adjust scales 
    scale_y_continuous(breaks = pretty_breaks()) +
    # Includes % values on the plot
    geom_text(aes(y = ((..count..)/sum(..count..)), 
                  label = scales::percent((..count..)/sum(..count..))),
              stat = "count",   
              vjust = vjust, hjust = hjust, color = "white", 
              size = 5) 
  
  
}

# Custom function to plot Bayesian / probabilistic bootstrapped confidence intervals

topbootplot <- function(var, seed = 1234, col = "gold3", title = "Boot") {
  ## var = numeric variable e.g. df$income
  ## stat = statistic
  ## seed = numeric input for seet.seed() reproducibility
  ## color = color
  ## title = plot title  
  
  library(bayesboot) # Requiring package
  
  ## Setting random generation seed to allow for reproducibility
  set.seed(seed)
  
  # Conducting Bayesian bootstrap (requires package "bayesboot")
  bp <- bayesboot(as.numeric(var), mean)
  
  ## Ploting results of Bayesian bootstrap
  plot(bp, cex = .7, 
       cex.lab = .7, 
       cex.axis= .7,
       cex.main=0.8, 
       cex.sub=0.8,
       col = col,
       main = title)
  
}


# Function to print vectorised heatmaps
vecheatmap <- function(
  df, # Long data frame
  filtervar = "Provinces", # Filter variable
  filterparam = "Nangarhar", # Filter parameter
  x.axis = "Indicators", # X axis variable
  y.axis = "Clusters",  # Y axis variable
  n = "" # n could be "N = " if needed N
) { 
  
  
  
  fill = "Status" # Fill variable
  title = paste0("Heatmap of ", 
                 tolower(y.axis),  
                 " and core ", 
                 tolower(x.axis), 
                 " of problems (", filterparam, ")")
  titlebreak = 80 # Nr. of characters to break title
  subtitle = " identified problems" # Subtitle unit
  subtitlevar = df$Situation # Subtitle variable
  brewerpal = "Set1" # Color palette
  filtercond = "==" # Character vector with selection condition
  
  # Filters dataset
  ## Creates filter expression
  filterexpr <- paste(filtervar, filtercond, "'", 
                      filterparam, "'", sep="")
  ## Filters using expression
  df <- df %>% filter_(filterexpr)
  
  # Creates vector with colors for Status based on condition
  ## Creates auxiliary vector
  status <- df %>% ungroup() %>%
    select(Status)  %>% unique() 
  
  if (status %>% nrow() == 1 & status[[1]][1] == "Passed"){
    
    colors <- c("#377EB8")
  }
  if (status %>% nrow() == 1 & status[[1]][1] == "Problems"){
    
    colors <- c("#E41A1C")
    
  }else{
    
    colors <- c("#377EB8", "#E41A1C")
  }
  
  # Prints heat map
  heatplot <- ggplot(data = df, aes_string(x = x.axis, y = y.axis)) +
    geom_tile(colour = "darkgrey", size=0.20, aes_string(fill = fill)) +
    ggtitle(stringr::str_wrap(title, width = titlebreak),
            subtitle =  
              paste(n,
                    format(sum(df$Situation, na.rm = TRUE), 
                           big.mark=","), subtitle, sep = "")) + 
    # Sets theme
    theme_minimal() +
    # Sets colors
    scale_fill_manual(values=colors) +
    #scale_fill_brewer(palette = brewerpal, direction = -1) +
    # Places legend 
    theme(legend.position="right") + 
    # Sets options for X axis text
    theme(axis.text.x = element_text(colour="grey20", size = 8,  
                                     face="plain", angle = 60,  
                                     vjust=1, hjust=1),
          # Sets options for Y axis text
          axis.text.y = element_text(colour="grey20", size = 8, face="plain"), 
          # Sets options for plot title
          plot.title = element_text(size = 14, face = "bold", vjust=1.2))
  
  return(heatplot)
  
}



multipercentplot <- function(
  df, # Data frame
  variable = "sm_access", # Variable 
  brewerpal = "Set2",
  subtitle = "observations", # Subtitle clarification for barplot
  title = "Plot", # Barplot title
  titlebreak = 60, # Number of characters in barplot title
  hjust = -0.1, # Horizontal justification of barplot annotation
  tree_title = "Cases that social mobilizers can visit few or very few houses in their assigned area", # Title of tree map
  tree_titlebreak = 90, # Number of characters in tree map 
  problems = c("Can_visit_few_houses", "Can_visit_very_few_houses"), # Parameters for problems
  heights = c(3, 2)) { # Height of plot grids
  
  
  ## df = some data frame
  ## title = string with plot title
  ## label = label of horizontal axis
  ## brewerpal = string with brewer.pal palette (see: https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)
  ## max.obs = numeric value limiting the number of results to plot
  
  library(ggplot2)
  library(scales)
  library(stringr)
  library(ggpubr)
  library(treemapify)
  
  # Plots multiple choice (select_multiple in XLSFom) variables presented as percentages of total obs.
  
  # Creates long auxiliary dataframe with variable of interest (select_multiple)
  data <- df %>% 
    select(internal_cluster, internal_district, contains(variable)) %>% 
    na.omit() %>% 
    reshape2::melt(., id.var = c("internal_cluster", "internal_district"), 
                   variable.name = variable) %>% 
    tbl_df() 
  
  # Binds and melts auxiliary dataframe
  data <-  data %>% 
    cbind(str_split_fixed(data$value, " ", 7)) %>% #\\s+
    as_tibble() %>% mutate_all(., as.character) %>% 
    mutate_all(na_if, "") %>%
    select(-contains(variable),-value) %>% 
    reshape2::melt(., id.var = c("internal_cluster", "internal_district")) %>% 
    na.omit() %>% 
    mutate(value = gsub("_", " ", value)) %>% 
    filter(value != "Other")
  
  
  # Prepares table with %
  
  var_table <- round(data$value %>% 
                       table() / (df %>%
                                    select(contains(variable)) %>% 
                                    na.omit %>% 
                                    nrow())*100,1) 
  
  # Transforms table into dataframe and change names of variables
  var_df <-  as.data.frame(var_table) 
  colnames(var_df)<-c("Inputs","Percentage (%)")
  
  # Reorders items
  var_df$Inputs <- factor(var_df$Inputs, 
                          levels = var_df$Inputs[order(var_df$`Percentage (%)`)])
  
  
  
  p <- ggplot(data = var_df, aes(x = Inputs, y = `Percentage (%)`, fill = Inputs)) + 
    # geom_bar(stat = "identity") +
    geom_bar(position = "dodge", stat = "identity") +
    theme_minimal() + 
    coord_flip() + 
    theme(legend.position="none")  + 
    scale_fill_brewer(palette = brewerpal) + 
    # Includes plot title 
    ggtitle(str_wrap(title, width = titlebreak), 
            subtitle = paste("N =", df %>% 
                               select(contains(variable)) %>% 
                               na.omit() %>% 
                               nrow(), " observations")
    ) + 
    # Removes title for y axis
    theme(axis.title.y = element_blank()) +
    geom_text(aes(label=scales::percent(`Percentage (%)`/100)),
              hjust = hjust, size = 3.5, col="black") +
    
    # Changes size and format of axis and title
    theme(axis.text=element_text(size = 12), 
          axis.title=element_text(size = 12)) + 
    theme(plot.title = element_text(size = 14, face = "bold")) +
    # Adjust scales 
    scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))), 
                       limits = c(0, max(var_df[,2]*1.3))) 
  
  # Modified code
  # Cases without problems
  return(p)
  
}


