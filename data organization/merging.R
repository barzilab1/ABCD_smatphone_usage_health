# library(dplyr)
# library(lubridate)
# library(janitor)
# source("data organization/data_utility_fun.R")



merge_all_data <- function(..., tp_include = NULL){

  datasets = list(...)

  combined_datasets <- Reduce(\(x, y) { if(nrow(y) != 0) merge(x, y, all = T) else x }, datasets)

  if(!is.null(tp_include)){
    combined_datasets <- combined_datasets %>% filter(grepl(paste0(tp_include, collapse = "|"), session_id)) %>% droplevels()
  }
  combined_datasets = remove_empty(combined_datasets, "cols")

  if("ph_y_anthr_age" %in% colnames(combined_datasets)) {
    combined_datasets = get_BMI_percentiles(combined_datasets)
  }

  saveRDS(combined_datasets, file = "data/data_long.rds")
  return(combined_datasets)

}
