# library(arrow)
# source("configurations/path_config.R")
# source("data organization/data_utility_fun.R")

get_ksads_y <- function(ksad_y_tables){


  datasets_list = list()
  
  #########################################################
  # Depression
  #########################################################
  if("Depression" %in% ksad_y_tables){
    depression_df<- read_parquet(file.path(abcd_main_data_path, "mh_y_ksads__dep.parquet"))
    
    depression_df = replace_888_with_0(depression_df)
    
    depression_df$depression_dx_y = create_diagnosis_summary_var(depression_df, T)
    
    depression_df = depression_df[,c("participant_id", "session_id", "depression_dx_y")]
    
    datasets_list[["Depression"]] = depression_df
  }
  
  
  
  ############################################################
  ############################################################

  # check that all files were loaded
  if(length(ksad_y_tables) != length(datasets_list)){
    missing_files = setdiff(ksad_y_tables, names(datasets_list) )
    print(paste0("The following KSADS Modules weren't loaded: ", paste(missing_files, collapse = " ; ")))
  }

  # combine data
  combined_dataframe <- Reduce(\(x, y) merge(x, y, all = T), datasets_list)

  return(combined_dataframe)
}


