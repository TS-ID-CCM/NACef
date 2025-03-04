##DESCRIPTIVE NACef## 
library(tidyverse)
library(psych)
library(tableone)
library(dplyr)
NACef_24

#"Gender" and "BMI" modifications#
NACef_24$bmi <- NACef_24$weight / ((NACef_24$height/100)*(NACef_24$height/100))
head(NACef_24)
summary(NACef_24$bmi)

NACef_24$gender <- ifelse(NACef_24$gender == 0, "Femenino", "Masculino")
summary(NACef_24$gender)

#Numeric results#
numeric_results <- CreateTableOne(vars = c("age", "height", "weight", "bmi",
                                           "admission_sofa","admission_curb","admission_psi",
                                           "hosp_stay","days_ab","gold","prev_ab_num","prev_ab_days", 
                                           "num_prev_infec", "num_urg","sofa_72"), data = NACef_24)
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
dicotomic_results <- lapply(NACef_24[dicotomic], function(x) {
  freq_abs <- table(x)
  freq_rel <- prop.table(freq_abs)
  return(data.frame(frecuencia_absoluta = freq_abs, frecuencia_relativa = freq_rel))
})
for (i in seq_along(dicotomic_results)) {
  cat("Variable:", names(dicotomic_results)[i], "\n")
  print(dicotomic_results[[i]])
  cat("\n")
}

