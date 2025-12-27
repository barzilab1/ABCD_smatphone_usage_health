# library(dplyr)
# library(readr)
# library(janitor)
# source("configurations/path_config.R")


get_mental_health <- function(files_list){

  datasets_list = list()

  ############################################################
  # Brief Problem Monitor [Youth]
  ############################################################
  if("mh_y_bpm" %in% files_list){
    bpm <- read_parquet(file.path(abcd_main_data_path, "mh_y_bpm.parquet"))
    # Binarize bpm at 65
    # T scores <65 are considered to be in the normal range. T scores >=65 are sufficiently elevated to be of concern. # 1=concern, 0=normal
    # https://documents.acer.org/ASEBA_Brief_Problem_Monitor_Manual.pdf
    bpm <- bpm %>%
      select(matches("participant|session|mh_y_bpm_sum|mh_y_bpm_tscore")) %>%
      mutate(bpm_T_bin_concern = case_when(mh_y_bpm_tscore < 65 ~ 0, mh_y_bpm_tscore >= 65 ~ 1, TRUE ~ NA_real_))

    datasets_list[["mh_y_bpm"]] = bpm
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








