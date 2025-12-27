# library(dplyr)
# library(arrow)
# source("configurations/path_config.R")


get_friends_family_community <- function(files_list){
  
  datasets_list = list()
  
  ############################################################
  # Parental Monitoring [Youth]
  ############################################################
  if("fc_y_pm" %in% files_list){
    parent_mornitor_y  <- read_parquet(file.path(abcd_main_data_path, "fc_y_pm.parquet"))
    
    parent_mornitor_y <- parent_mornitor_y %>% select(-matches("_(nm|adm|dtt)$"))
    
    parent_mornitor_y <- parent_mornitor_y %>% 
      mutate(across(matches("00"), ~ as.numeric(as.character(.x))))
    
    datasets_list[["fc_y_pm"]] = parent_mornitor_y
  }
  
  ############################################################
  ############################################################
  
  # check that all files were loaded
  if(length(files_list) != length(datasets_list)){
    missing_files = setdiff(files_list, names(datasets_list) )
    print(paste0("The following files weren't loaded: ", paste(missing_files, collapse = " ; ")))
  }
  
  # combine data
  combined_dataframe <- Reduce(\(x, y) merge(x, y, all = T), datasets_list)
  return(combined_dataframe)
  
}





