# load packages ================================================================

library("dplyr") 
library(illuminate)
library(tidyr)
library(readxl)
library(openxlsx)
library(stringr)
library(sf)
library(leaflet)
library(devtools)
library(srvyr)
library(purrr)


# load and restructure data ====================================================

data <- read.csv("input/data.csv", na.strings = c("NA", ""))


# build function ===============================================================

# initialize cleaning log
cleaning_log <- data.frame()
# define log function, which include 2 arguments: the variable name and the issue specification
log <- function(var, issue_name){
  
  log_new <- data.frame("enumerator" = data_filter$enumerator_num,
                        "ben name" = data_filter$ben_name_eng,
                        uuid = data_filter$X_uuid,
                        "phone number" = data_filter$ben_phone_number,
                        variable = var,
                        issue = issue_name,
                        "old value" = data_filter[[var]],
                        "new value" = "",
                        "check ok" = "",
                        "modifyed by" = ""
  )
  
  cleaning_log <- rbind(cleaning_log, log_new)
}

# Survey  duration
source("Load_Audit.R")


day_to_run <- Sys.Date()

path_unzip <- "99_temp/01_audit"
audit_zip_dir<-"Input/"
audit_zipfile <-paste0(audit_zip_dir,"#aQBhmMbKgfUSHKD5voYJ3M_",str_replace_all (day_to_run,"-","_"),".zip")
copy_zip_to<-paste0("99_temp/01_audit",day_to_run,".zip")
audit_node<-"/#aQBhmMbKgfUSHKD5voYJ3M/"




audit_yes<-Load_Audit(data=data,
                      path.to.zip = audit_zipfile,
                      path.to.unzip = path_unzip,
                      copy.zip = TRUE,
                      path.to.copy.zip = copy_zip_to,
                      delete.unzipped=TRUE
)

quick_survey(df = data,consent_column_name = "consent",uuid_col_name = "X_uuid",
             audit_node =audit_node,
             start_question_node = "speak_ben",end_question_node ="any_general_issues",
             min_allowable_duration = 20,audit_yes = audit_yes)


surveys_with_duration$duration_minutes <- if_else(is.infinite(surveys_with_duration$duration_minutes) ,20, surveys_with_duration$duration_minutes)
surveys_with_duration_check <- surveys_with_duration %>% filter(duration_minutes < 20)

subset_1 <- data %>% select(X_uuid, enumerator_num,ben_name_eng ,ben_phone_number) %>% mutate(
  variable = "", issue = "time below 20 minutes",old.value = "", new.value = "", Comments = "", modifyed.by = "")

surveys_with_duration_check <- surveys_with_duration_check %>% select(uuid, duration_minutes) %>% left_join(subset_1, by = c("uuid"= "X_uuid")
) %>% select(enumerator_num, ben_name_eng , uuid, ben_phone_number,variable, issue, old.value, new.value, Comments,
             modifyed.by)

surveys_with_duration_check$old.value <- as.character(surveys_with_duration_check$old.value)


# build log ====================================================================

# check 1
# define condition inside a simple dplyr filter
data_filter <- data %>% filter(CHECK_hh_size == "CHECK")
# create a cleaning log by specifying the variable name and the issue inside the log function
cleaning_log <- log(var = "num_hh_members", issue_name = "check that this number is the same as the sum of the questions disaggregated by age")
cleaning_log <- log(var = "hh_size", issue_name = "check that this number is the same as the sum of the questions disaggregated by age")

# check 2
data_filter <- data %>% filter(CHECK_intended_go_aftercamp_district == "CHECK")
cleaning_log <- log(var = "current_district", issue_name = "Check if this is different from the current district ")
cleaning_log <- log(var = "intended_go_aftercamp_district", issue_name = "Check if this is different from the current district ")

# check 3
data_filter <- data %>% filter(CHECK_displace_status == "CHECK")
cleaning_log <- log(var = "district_aoo", issue_name = "check if  AoO district is same as current district but reported not beeing in their curent location before beeing displaced")
cleaning_log <- log(var = "current_district", issue_name = "check if  AoO district is same as current district but reported not beeing in their curent location before beeing displaced.")
cleaning_log <- log(var = "displace_status", issue_name = "check if  AoO district is same as current district but reported not beeing in their curent location before beeing displaced.")

# check 4
data_filter <- data %>% filter(CHECK_displace_stat == "CHECK")
cleaning_log <- log(var = "district_aoo", issue_name = "check if  AoO district is same as current district but reported not beeing in their curent location before beeing displaced")
cleaning_log <- log(var = "current_district", issue_name = "check if  AoO district is same as current district but reported not beeing in their curent location before beeing displaced.")
cleaning_log <- log(var = "displace_status", issue_name = "check if  AoO district is same as current district but reported not beeing in their curent location before beeing displaced.")

# check 5
data_filter <- data %>% filter( CHECK_months_in_this_location== "CHECK")
cleaning_log <- log(var = "months_in_this_location", issue_name = "check if months in current location is more than 22 months as they were displaced since may 2021 (according to ToR)")

# check 6
data_filter <- data %>% filter( CHECK_school_formal_enrollment_male== "CHECK")
cleaning_log <- log(var = "hh_male_6_17", issue_name = "check if children 6-17 were selected previously")
cleaning_log <- log(var = "school_formal_enrollment", issue_name = "check if children 6-17 were selected previously")

# check 7
data_filter <- data %>% filter(CHECK_school_formal_enrollment_female == "CHECK")
cleaning_log <- log(var = "hh_female_6_17", issue_name = "check if children 6-17 were selected previously")
cleaning_log <- log(var = "school_formal_enrollment", issue_name = "check if children 6-17 were selected previously")

# check 8
data_filter <- data %>% filter( CHECK_access_employment_livelihood== "CHECK")
cleaning_log <- log(var = "access_employment_livelihood", issue_name = "check if reported not_having_access/not_available to employment and livelihood oportunities and also reported they don't have anyone in the household that is not working and loking for a job")
cleaning_log <- log(var = "seeking_job", issue_name = "check if reported not_having_access/not_available to employment and livelihood oportunities and also reported they don't have anyone in the household that is not working and loking for a job")

# check 9
data_filter <- data %>% filter( CHECK_returned_to_same_job== "CHECK")
cleaning_log <- log(var = "returned_to_same_job", issue_name = "check if reported not_having_access/not_available to employment and livelihood oportunities and also reported they returned to the job they had prior to displacemnet")
cleaning_log <- log(var = "access_employment_livelihood", issue_name = "check if reported not_having_access/not_available to employment and livelihood oportunities and also reported they returned to the job they had prior to displacemnet")

# check 10

data_filter <- data %>% filter(female_move_freely == "yes" & hh_female_6_17 == 0 |female_move_freely == "yes" & hh_female_more_18 == 0 )
cleaning_log <- log(var = "female_move_freely", issue_name = "check if female members of HH were selected")
cleaning_log <- log(var = "hh_female_6_17", issue_name = "check if female members of HH were selected")
cleaning_log <- log(var = "hh_female_more_18", issue_name = "check if female members of HH were selected")

data_filter <- data %>% filter(female_move_freely == "no" & hh_female_6_17 == 0 |female_move_freely == "yes" & hh_female_more_18 == 0 )
cleaning_log <- log(var = "female_move_freely", issue_name = "check if female members of HH were selected")
cleaning_log <- log(var = "hh_female_6_17", issue_name = "check if female members of HH were selected")
cleaning_log <- log(var = "hh_female_more_18", issue_name = "check if female members of HH were selected")

# check 11
data_filter <- data %>% filter( male_move_freely== "yes" & hh_male_6_17 == 0 |male_move_freely== "yes" & hh_male_more_18 == 0 )
cleaning_log <- log(var = "male_move_freely", issue_name = "check if male members of HH were selected")
cleaning_log <- log(var = "hh_male_6_17", issue_name = "check if male members of HH were selected")
cleaning_log <- log(var = "hh_male_more_18", issue_name = "check if male members of HH were selected")

data_filter <- data %>% filter( male_move_freely== "no" & hh_male_6_17 == 0 |male_move_freely== "yes" & hh_male_more_18 == 0 )
cleaning_log <- log(var = "male_move_freely", issue_name = "check if male members of HH were selected")
cleaning_log <- log(var = "hh_male_6_17", issue_name = "check if male members of HH were selected")
cleaning_log <- log(var = "hh_male_more_18", issue_name = "check if male members of HH were selected")

# other checks

data_filter <- data %>% filter( other_reson_not_returned_aoo != "")
cleaning_log <- log(var = "other_reson_not_returned_aoo", issue_name = "check")

data_filter <- data %>% filter( other_reson_returned_aoo != "")
cleaning_log <- log(var = "other_reson_returned_aoo", issue_name = "check")

data_filter <- data %>% filter( market_barriers_other != "")
cleaning_log <- log(var = "market_barriers_other", issue_name = "check")

data_filter <- data %>% filter( problems_water_quality_other!= "")
cleaning_log <- log(var = "problems_water_quality_other", issue_name = "check")

data_filter <- data %>% filter( difficulty_accessing_health_services_other!= "")
cleaning_log <- log(var = "difficulty_accessing_health_services_other", issue_name = "check")

data_filter <- data %>% filter( resons_not_attend_other!= "")
cleaning_log <- log(var = "resons_not_attend_other", issue_name = "check")

data_filter <- data %>% filter( reason_not_returned_to_same_job_other!= "")
cleaning_log <- log(var = "reason_not_returned_to_same_job_other", issue_name = "check")

data_filter <- data %>% filter( employment_primary_barriers_other!= "")
cleaning_log <- log(var = "employment_primary_barriers_other", issue_name = "check")

data_filter <- data %>% filter(primary_livelihood_other != "")
cleaning_log <- log(var = "primary_livelihood_other", issue_name = "check")

data_filter <- data %>% filter( other_shelter_type!= "")
cleaning_log <- log(var = "", issue_name = "check")

data_filter <- data %>% filter( other_reasons_for_planning_moving!= "")
cleaning_log <- log(var = "other_reasons_for_planning_moving", issue_name = "check")

data_filter <- data %>% filter(other_safety_concerns != "")
cleaning_log <- log(var = "other_safety_concerns", issue_name = "check")

data_filter <- data %>% filter( type_of_local_peace_actors_others!= "")
cleaning_log <- log(var = "type_of_local_peace_actors_others", issue_name = "check")

# Exporting


surveys_with_duration_check <- surveys_with_duration_check %>% rename(phone.number = ben_phone_number, 
                                                                      ben.name = ben_name_eng,
                                                                      enumerator = enumerator_num,
                                                                      check.ok = Comments)
cleaning_log <- cleaning_log %>% bind_rows(surveys_with_duration_check)


write.csv(cleaning_log, "output/UNDP_checking_template.csv", row.names=FALSE, na = "")


