# PURPOSE: Run descriptive analysis on the clean data set of the UNDP Return Assessment. The script takes an excel file named "Clean Data" as input and adds sheets to the "Analysis Results" excel file for every new invocation of the analyze_data_X function
# AUTHOR: Cody Adelson | Data Specialist
# DATE CREATED: April 11, 2023
# NOTES: 1- Please DO NOT RENAME the INPUT and OUTPUT file names 
#        2- The order of the sheets within the INPUT file matters, first sheet must be HH level data and the second must be Individual level data

#-------------------------- Libraries [Dependencies] ---------------------------

library(tidyverse)
library(readxl)
library(illuminate)
library(srvyr)
library(openxlsx)

#-------------------------------- Util Functions -------------------------------

#--------------------------------- Data Import ---------------------------------

clean_dataset_hh <- read_excel("input/UNDP_raw_ data_-_all_versions_-_False_-_2023-04-16-06-40-00.xlsx", guess_max = 5000) %>% fix_data_type() #%>% select(-)

#---------------------- Data Transformation (Re-Coding) ------------------------

#
# clean_dataset_hh <- clean_dataset_hh %>% mutate(...)
#

#-------------------------------- Analysis Function ----------------------------

analyze_data_hh <- function(cols_to_analyze, compare_by = NULL)
{
  analysis_results <- survey_analysis(df = clean_dataset_hh, vars_to_analyze = cols_to_analyze, disag = compare_by)
  
  analysis_file <- loadWorkbook("./output/Analysis Results.xlsx")
  analysis_sheets <- sheets(analysis_file)
  new_sheet_number <- analysis_sheets %>% str_split(" ") %>% enframe() %>% unnest(cols = c(value)) %>% pull("value") %>% parse_number() %>% max(na.rm = T) + 1
  new_sheet_name <- str_c("Analysis ", new_sheet_number)
  addWorksheet(analysis_file, new_sheet_name)
  writeData(analysis_file, new_sheet_name, analysis_results)
  saveWorkbook(analysis_file, "./output/Analysis Results.xlsx", overwrite = T)
}

#----------------------------------- Analysis ----------------------------------

all_data_variables_hh <- names(clean_dataset_hh)

#remove unused variables
all_data_variables_hh <- all_data_variables_hh [! all_data_variables_hh %in% c(
  "start", "end", "deviceid", "audit", "audit_URL", "enumerator_num", "ben_name_ar", "ben_name_eng",
  "num_duplicates", "hh_member_interviewed_name", "CHECK_hh_size", "CHECK_intended_go_aftercamp_district",
  "CHECK_displace_status", "CHECK_displace_stat", "CHECK_months_in_this_location", "CHECK_school_formal_enrollment_male",
  "CHECK_school_formal_enrollment_female", "CHECK_access_employment_livelihood", "CHECK_returned_to_same_job", "_id", "_uuid",
  "_submission_time", "_status", "_index")] 

# Analyze dataset first altogether, and then by disaggregates 
analyze_data_hh(all_data_variables_hh)                                            #summarize all data variables (columns)
analyze_data_hh(all_data_variables_hh, "current_governorate")                     #summarize all data variables (columns) and compare the figures by governorate and district
analyze_data_hh(all_data_variables_hh, "current_district")  
analyze_data_hh(all_data_variables_hh, "governorate_aoo")  
analyze_data_hh(all_data_variables_hh, "district_aoo")  
analyze_data_hh(all_data_variables_hh, "gender_resp")  
