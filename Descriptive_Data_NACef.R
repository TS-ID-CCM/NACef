##DESCRIPTIVE NACef## 
#RStudio version used: Version 2024.12.0+467 (2024.12.0+467)

library(tidyverse)
library(psych)
library(tableone)
library(dplyr)

#Load correctly the .csv file modifying the path to the file 
NACef_2024 <- read.csv("path/to/file.csv")

#Modify non-numeric variables 
NACef_2024$age <- as.numeric(as.character(NACef_2024$age)) # Convert age to numeric 
NACef_2024[is.na(as.numeric(NACef_2024$height)), "height"] # Convert height to numeric 

#"Gender" and "BMI" modifications#
NACef_2024$bmi <- NACef_2024$weight / ((NACef_2024$height / 100) * (NACef_2024$height / 100))

head(NACef_2024)
summary(NACef_2024$bmi)

NACef_2024$gender <- ifelse(NACef_2024$gender == 0, "Female", "Male")
summary(NACef_2024$gender)

#Numeric results#
NACef_2024$age <- as.numeric(as.character(NACef_2024$age)) # Convert age to numeric 

numeric_results <- CreateTableOne(vars = c("age", "height", "weight", "bmi",
                                           "admission_sofa","admission_curb","admission_psi",
                                           "hosp_stay","days_ab","gold","prev_ab_num","prev_ab_days", 
                                           "num_prev_infec", "num_urg","sofa_72"), data = NACef_2024)
print (numeric_results, test = FALSE)

#Dicotomic results#
dicotomic <- c("gender", "health_work", "rural_work", "geriatric_home", "postramiento", 
               "living_space", "vac_influenza", "vac_neumococo", "prev_infec", "urg_12_month", 
               "ab_12month", "copd", "ant_taba", "ab_empiric", "sosp_covid", "vac_covid", 
               "ventilation", "clinical_resp", "non_pulm_infec", "cv_comp", "another_study",
               "copd","ant_taba","icu","ant_mdrd","sosp_covid","vac_covid","criteria",
               "sev_criteria","ab_empiric","ab_conjug","coinfection","etio_pneumo","2_rt_pcr_covid",
               "extub","traqueost","live_discharge","icu_death","gen_hosp_death","ventilation",
               "non_pulm_infec","cv_comp","another_study")
dicotomic
dicotomic_results <- lapply(NACef_2024[dicotomic], function(x) {
  freq_abs <- table(x)
  freq_rel <- prop.table(freq_abs)
  return(data.frame(frecuencia_absoluta = freq_abs, frecuencia_relativa = freq_rel))
})
for (i in seq_along(dicotomic_results)) {
  cat("Variable:", names(dicotomic_results)[i], "\n")
  print(dicotomic_results[[i]])
  cat("\n")
}

