# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c("dplyr",
               "purrr",
               "arrow",
               "ABCDscores",
               "stringr",
               "lubridate",
               "janitor",
               "cdcanthro",
               "data.table",
               "fastDummies",
               "miceRanger"
               ),
  format = "feather" # default format, unless specifically specified in targets
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  # , controller = crew::crew_controller_local(workers = 6)
  #

)


# Run the R scripts in the R/ folder with your custom functions:
tar_source("configurations")
tar_source("data organization")


list(

  # load data
  tar_target(abcd_general_data, get_abcd_general()),
  tar_target(friends_family_community_data, get_friends_family_community(friends_family_community_tables)),
  tar_target(ksads_y_data, get_ksads_y(ksad_y_tables)),
  tar_target(mental_health_data, get_mental_health(mental_health_tables)),
  tar_target(novel_technologies_data, get_novel_technologies(novel_technologies_tables)),
  tar_target(physical_health_data, get_physical_health(physical_health_tables)),

  # merge
  tar_target(merged_data,
             merge_all_data(tp_include = tp_include
                            , abcd_general_data
                            , friends_family_community_data
                            , ksads_y_data
                            , mental_health_data
                            , novel_technologies_data
                            , physical_health_data
             )),

  # Create long data for models/analyses
  tar_target(data_models, get_mod_data(merged_data), format = "rds"),
  tar_target(data_imp, get_imp_data(merged_data), format = "rds")
)
