#Calculating IRR

coded_studies <- readr::read_csv("Review/Coding/coded_studies/Review Validity_13.09.csv")
base_studies <- readr::read_csv("Review/data/complete_list_coded.csv")

df_merged <- dplyr::left_join(base_studies, coded_studies, by = c("Title" = "Title"))

#Plots
studies 
