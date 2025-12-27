## {{Smartphone Acquisition and Use at Age 13 and Their Associations with Health Outcomes at Age 14}}


The current project utilizes data from ABCD version 6.1, as defined in [ABCD_tables_config.R](configurations/ABCD_tables_config.R).

1. Data Creation:
This part uses the [targets](https://books.ropensci.org/targets/) package to create the dataset.   
Ensure the [path_config.R](configurations/path_config.R) is set up with the location of the ABCD 6.1 files.   
To create the datasets, run the function targets::tar_make() which can be found in the [run.R](run.R) file.   
All data cleaning processes are in the [data organization](data organization) folder.

2. Run the [analysis/1_descriptive_table1.R](analysis/1_descriptive_table1.R) to generate Table 1 and table comapring included and non-inclued participants.
3. Run the [analysis/2_analysis.R](analysis/2_analysis.R) to generate all models in the main and sensitivity analyses.
4. Run the [analysis/3_creating_figures.R](analysis/3_creating_figures.R) to generate all figures.
