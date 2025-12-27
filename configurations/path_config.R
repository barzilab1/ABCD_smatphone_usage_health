############# file paths #############

#path to abcd Restricted Access folder
root = "~"
box_path = "Library/CloudStorage/Box-Box"
abcd_box_path = "2. Barzi Lab - Restricted Access/2-ABCD"
abcd_box_data_path = "ABCD data"
abcd_version = "6.1"
abcd_bids_main_path = "tabulated"

prs_box_path = "Ran_Barzilay"


abcd_data = file.path(root, box_path, abcd_box_path, abcd_box_data_path)
abcd_main_data_path = file.path(abcd_data, abcd_version, abcd_bids_main_path)


