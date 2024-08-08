
# 01 - setting --------------------------------------------------------------

#load packages
library(pacman)
pacman::p_load(magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, extraInserts,
               ggvenn, ggplot2,knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot,
               stringr)
#font
font_add(family = "berlin_default", regular = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/font/STHeiti Light.ttc")
showtext_auto(enable = TRUE)





# 02.0 - input_name_table --------------------------------------------------------
library(googlesheets4)
gs4_auth(email = "mr.berlin.lin@gmail.com")
vars_table <- googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1T2swdx1cbfmUSUNQCQpbxa4aIJDTLD0oVKj3AuvNAuM/edit?usp=sharing', 
                                        sheet = "vars_table",
                                        col_types = "icccccc")
names(vars_table) <- c("num", "item_id", "ch", "en", "raw_en", "pub_en","field")




# 02.1 - [Data Preprocessing] 01_profile --------------------------------------------------
#***[Note:] 20230309_finish_genesis_ONLY
#input clinic_list
source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/00-read_clinic_list.R")

df01_profile <- tmp_01


#Q1-1. 初日開幕前的cofit初日班("2021-08-30"), 秀傳開幕後("2020-09-30") => topshow
df01_profile[grepl("初日班", df01_profile[["program_name"]]) & (df01_profile[["date_t0"]] < "2021-08-30") & (df01_profile[["date_t0"]] > "2020-09-30"), "org_name"] <- "topshow"
df01_profile[grepl("秀0|秀1", df01_profile[["name"]]) & (df01_profile[["date_t0"]] < "2021-08-30") & (df01_profile[["date_t0"]] > "2020-09-30"), "org_name"] <- "topshow"
#Q1-2. "program_name"初日開幕後的cofit初日班 => genesisclinic
df01_profile[Reduce(dplyr::intersect, list(grep("初日", df01_profile[["program_name"]]),
                                     which(df01_profile[["date_t0"]] >= "2021-08-30"),
                                     grep("cofit", df01_profile[["org_name"]])))
       , "org_name"] <- "genesisclinic"
#Q1-3. "name"名字有初日、初日開幕後的cofit => genesisclinic
df01_profile[Reduce(dplyr::intersect, list(grep("初日|初日001|G001", df01_profile[["name"]]),
                                     which(df01_profile[["date_t0"]] >= "2021-08-30"),
                                     grep("cofit", df01_profile[["org_name"]])))
       , "org_name"] <- "genesisclinic"
#Q1-4. 初日開幕前的"診所進階計畫"(topshow); 初日開幕後的"診所進階計畫"(genesisclinic)
#"診所進階(genesis)" AND "org_name == cofit" AND "初日開幕後" "秀傳門診結束"
df01_profile[(grepl("診所進階", df01_profile[["program_name"]])) & (grepl("cofit", df01_profile[["org_name"]])) & (df01_profile[["date_t0"]] < "2021-08-30"), "org_name"] <- "topshow"
df01_profile[(grepl("診所進階", df01_profile[["program_name"]])) & (grepl("cofit", df01_profile[["org_name"]])) & (df01_profile[["date_t0"]] >= "2021-08-30"), "org_name"] <- "genesisclinic"

#Q1-5. FLC班 => cofit
# df01_profile[(grepl("宋醫師進階", df01_profile[["program_name"]])) & (grepl("topshow|genesisclinic", df01_profile[["org_name"]])), "org_name"] <- "cofit" #wrong line: 早期秀傳/誤選都應該要記在診所=> "維持不動"
df01_profile[grepl("FLC", df01_profile[["program_name"]]) & not(grepl("秀0|秀1", df01_profile[["name"]])), "org_name"] <- "cofit"


#C1-1. class_freq by org_name
df01_profile <- df01_profile %>% full_join(df01_profile %>% group_by(id, org_name) %>% summarise(class_freq = n()), by = c("id", "org_name"))
df01_profile <- df01_profile[with(df01_profile, order(c(date_t0, id))),] %>% janitor::remove_empty("rows")
#C1-2. class_order
for (i in unique(df01_profile$id)) {
  if (i == head(unique(df01_profile$id), 1)) {
    j = 1
    df01_profile$class_order <- NA
    start_time <- Sys.time()
  }
  
  for (k in c(1:(df01_profile[which(df01_profile[["id"]] == i), "org_name"] %>% unique() %>% length()))) {
    df01_profile[(df01_profile[["id"]] == i) & (df01_profile[["org_name"]] == unique(df01_profile[which(df01_profile[["id"]] == i), "org_name"])[k]), "class_order"] <- 
      seq(1, df01_profile[(df01_profile[["id"]] == i) & (df01_profile[["org_name"]] == unique(df01_profile[which(df01_profile[["id"]] == i), "org_name"])[k]), "class_freq"] %>% unique())
    }
  
  progress(j, unique(df01_profile$id) %>% length())
  j = j + 1
  if (i == tail(unique(df01_profile$id), 1)){
    print("[Completed!]")
  }
}


#C2. age: btd - date_t0 年齡(療程起始當天計算)
df01_profile$age <- (lubridate::ymd(df01_profile$date_t0) - lubridate::ymd(df01_profile$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()

#***[初日診所]
#C3-1.非進階
a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$client_type <- NA #client_type 
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, client_type, id, clinical_list, client_type, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$program_set <- NA #program_set
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, program_set, id, clinical_list, program, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$doctor <- NA #doctor
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, doctor, id, clinical_list, doctor, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$nutritionist_major <- NA #nutritionist_major
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, nutritionist_major, id, clinical_list, nutritionist_major, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$nutritionist_online <- NA #nutritionist_online
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, nutritionist_online, id, clinical_list, nutritionist_online, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$medication <- NA #medication
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, medication, id, clinical_list, medication_note, id)

#C3-2.進階
a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, client_type, id, clinical_adv_list, client_type, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, program_set, id, clinical_adv_list, program, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, doctor, id, clinical_adv_list, doctor, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, nutritionist_major, id, clinical_adv_list, nutritionist_major, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, nutritionist_online, id, clinical_adv_list, nutritionist_online, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, medication, id, clinical_adv_list, medication_note, id)


#***[小宙診所]
#C3-1.非進階
#client_type 
a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, client_type, id, clinical_list_lumez, client_type, id)

#program_set
a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, program_set, id, clinical_list_lumez, program, id)

#doctor
a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, doctor, id, clinical_list_lumez, doctor, id)

#nutritionist_major
a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, nutritionist_major, id, clinical_list_lumez, nutritionist_major, id)

#nutritionist_online
a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, nutritionist_online, id, clinical_list_lumez, nutritionist_online, id)

#medication
a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, medication, id, clinical_list_lumez, medication_note, id)

#C3-2.進階
a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, client_type, id, clinical_adv_list_lumez, client_type, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, program_set, id, clinical_adv_list_lumez, program, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, doctor, id, clinical_adv_list_lumez, doctor, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, nutritionist_major, id, clinical_adv_list_lumez, nutritionist_major, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, nutritionist_online, id, clinical_adv_list_lumez, nutritionist_online, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "lumez"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, medication, id, clinical_adv_list_lumez, medication_note, id)


#clean by select
df01_profile <- df01_profile %>% select(c("id", "name", "gender", "age", "client_type", "program_name","date_t0","date_t1", "org_name", "class_freq", "class_order","program_set","doctor","nutritionist_major","nutritionist_online","medication", "btd", "identity_number"))



# 02.2 - [Data Preprocessing] 02_inbody --------------------------------------------------

df02_inbody <- tmp_02

#C1. format
df02_inbody[c("height","weight","bmi","body_fat_mass","body_fat_mass_percentage","weight_without_fat","muscle_mass","real_muscle_mass","vfa","vfa_level","waist_circumference","acl","cacl","total_body_water","protein_weight","mineral_weight","body_cell_mass","body_mineral","bfmi","bsmi","ffmi","systolic_blood_pressure","diastolic_blood_pressure","pulse","bmr","wepa50","algle_50_left_arm","algle_50_left_leg","algle_50_right_arm","algle_50_right_leg","algle_50_trunk","extracellular_water_ratio","extracellular_water_ratio_left_arm","extracellular_water_ratio_left_leg","extracellular_water_ratio_right_arm","extracellular_water_ratio_right_leg","extracellular_water_ratio_trunk","intracellular_weight","intracellular_weight_left_arm","intracellular_weight_left_leg","intracellular_weight_right_arm","intracellular_weight_right_leg","intracellular_weight_trunk","extracellular_weight","extracellular_weight_left_arm","extracellular_weight_left_leg","extracellular_weight_right_arm","extracellular_weight_right_leg","extracellular_weight_trunk","left_arm_fat","left_arm_fat_percentage","left_arm_muscle","left_arm_muscle_percentage","left_leg_fat","left_leg_fat_percentage","left_leg_muscle","left_leg_muscle_percentage","right_arm_fat","right_arm_fat_percentage","right_arm_muscle","right_arm_muscle_percentage","right_leg_fat","right_leg_fat_percentage","right_leg_muscle_percentage","right_leg_muscle","trunk_fat","trunk_fat_percentage","trunk_muscle","trunk_muscle_percentage","water_weight_left_arm","water_weight_left_leg","water_weight_right_arm","water_weight_right_leg","water_weight_trunk","waist_hip_ratio","tbwffm","obesity_degree","inbody_total_score")] %<>% 
  lapply(as.numeric)
df02_inbody <- df02_inbody %>% as.tibble() 
#C2. Sarcopenia Obesity(SO): "left_arm_muscle", "left_leg_muscle", "right_arm_muscle", "right_leg_muscle" #Female: < 23.4; Male: < 29.6. 
df02_inbody <- df02_inbody %>% mutate(so_score = round((left_arm_muscle+left_leg_muscle+right_arm_muscle+right_leg_muscle)*100/weight,2))
#C3. pbm
df02_inbody <- df02_inbody %>% mutate(muscle_mass_percentage = round((muscle_mass)*100/weight,2))
#C4. name_format
names(df02_inbody) <- names(df02_inbody) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
#Diagnosis: Obesity
df02_inbody$gp_bmi <- df02_inbody$bmi %>% cut(c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))
df02_inbody$gp_bmi_2 <- df02_inbody$bmi %>% cut(c(0,18.5,24,27, 30, 35 ,100), c("underweight", "normal", "overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)


#Diagnosis: HTN
df02_inbody <- df02_inbody %>% lin_diagnosis_HTN(c("sbp", "dbp"))
#C5. rm outlier
df02_inbody <- df02_inbody[-which(df02_inbody$bmi >100),]


# 02.3 - [Data Preprocessing] 03_FLC_self_report --------------------------------------------------

df03_FLC_self_report <- tmp_03 %>% select(-mobile)

#adjust
df03_FLC_self_report <- df03_FLC_self_report %>% filter(program %in% c("宋醫師專班 -FLC班","經典八週","2023 FLC-2個助教","診所八週(週一開班)-宋醫師班/初日班","宋醫師進階計畫","診所進階計畫","診所八週(週四啟動)-初日班","進階計畫","經典八週（202109新版）享瘦班"))

df03_FLC_self_report <- df03_FLC_self_report %>% select(-c(age, measurement_after_program_date, measurement_before_program_date))

#C1. col_names
names(df03_FLC_self_report) <- names(df03_FLC_self_report) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
#C1-2. filter by program not "^診所"
df03_FLC_self_report <- df03_FLC_self_report[df03_FLC_self_report[["program"]] %>% grepl("^診所",.) %>% not(),]

df03_FLC_self_report <- df03_FLC_self_report[with(df03_FLC_self_report, order(date_flc_T0, id)),]

#C2. age: btd - date_t0 年齡(療程起始當天計算)
df03_FLC_self_report$age <- (lubridate::ymd(df03_FLC_self_report$date_flc_T0) - lubridate::ymd(df03_FLC_self_report$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()

#[240315] Debug start from here


# End.


#C2-2. carb/protein/fat E%:
df03_FLC_self_report <- df03_FLC_self_report %>% mutate(carb_ep = (carbohydrate*4 / (carbohydrate*4 + protein*4 + fat*9) ))
df03_FLC_self_report <- df03_FLC_self_report %>% mutate(protein_ep = (protein*4 / (carbohydrate*4 + protein*4 + fat*9) ))
df03_FLC_self_report <- df03_FLC_self_report %>% mutate(fat_ep = (fat*9 / (carbohydrate*4 + protein*4 + fat*9) ))
#C2-3. upload_day_%:
df03_FLC_self_report <- df03_FLC_self_report %>% mutate(upload_day_p = (as.numeric(day_count) / as.numeric((lubridate::ymd(date_flc_T1) - (lubridate::ymd(date_flc_T0)) + 1))))
names(df03_FLC_self_report) <- names(df03_FLC_self_report) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
#C2-3. BMI
df03_FLC_self_report$height <- df03_FLC_self_report$height %>% as.numeric()
df03_FLC_self_report$`BMI(T0)` <- (df03_FLC_self_report$`weight(T0)`/ (df03_FLC_self_report$height/100)^2) %>% round(1)
df03_FLC_self_report$`BMI(T1)` <- (df03_FLC_self_report$`weight(T1)`/ (df03_FLC_self_report$height/100)^2) %>% round(1)
df03_FLC_self_report$`∆BMI` <- (df03_FLC_self_report$`BMI(T1)` - df03_FLC_self_report$`BMI(T0)`)
df03_FLC_self_report$`∆BMI%` <- (df03_FLC_self_report$`BMI(T1)` - df03_FLC_self_report$`BMI(T0)`)/df03_FLC_self_report$`BMI(T0)`
#C2-4. calorie
df03_FLC_self_report <- df03_FLC_self_report %>% mutate(calorie_day = ((carbohydrate*4 + protein*4 + fat*9) / day_count ))


#C3. (1.) (%) *100  (2.) numeric %>% round(2)
df03_FLC_self_report[,grep("%", names(df03_FLC_self_report))] <- df03_FLC_self_report[,grep("%", names(df03_FLC_self_report))] %>% multiply_by(100) %>% round(2)
df03_FLC_self_report[c("weight(T0)","weight(T1)","∆weight","∆weight%","BMI(T0)","BMI(T1)","∆BMI","∆BMI%","Fat(T0)","Fat(T1)","∆Fat","∆Fat%","wc(T0)","wc(T1)","∆wc","∆wc%")] %<>% round(2)

#C3-2. exclude vars
df03_FLC_self_report <- df03_FLC_self_report %>% select(-c(light_G_count, carbohydrate, protein, fat))

#C3-3. exclude missing value
df03_FLC_self_report <- df03_FLC_self_report %>% lin_exclude_NA_col(c("light_G_%", "∆weight", "carb_E%"))
#C3-4. exclude upload_day_p = 0
df03_FLC_self_report <- df03_FLC_self_report %>% filter(`upload_day_%` != 0)

#C4-1. class_freq by org_name
df03_FLC_self_report <- df03_FLC_self_report %>% full_join(df03_FLC_self_report %>% group_by(id) %>% summarise(class_freq = n()), by = c("id"))
#C4-2. class_order
for (i in unique(df03_FLC_self_report$id)) {
  if (i == head(unique(df03_FLC_self_report$id), 1)) {
    j = 1
    df03_FLC_self_report$class_order <- NA
    start_time <- Sys.time()
  }
  df03_FLC_self_report[which(df03_FLC_self_report[["id"]] == i), "class_order"] <- which(df03_FLC_self_report[["id"]] == i) %>% order()
  progress(j, unique(df03_FLC_self_report$id) %>% length())
  j = j + 1
  if (i == tail(unique(df03_FLC_self_report$id), 1)){
    print("[Completed!]")
  }
}


#C5. rm outliers
df03_FLC_self_report[["∆weight%"]] <- ifelse((df03_FLC_self_report[["∆weight%"]] < quantile(df03_FLC_self_report[["∆weight%"]], 0.01, na.rm = TRUE)) | (df03_FLC_self_report[["∆weight%"]] > quantile(df03_FLC_self_report[["∆weight%"]], 0.95, na.rm = TRUE)),
                                             df03_FLC_self_report[["∆weight%"]] %>% mean(),
                                             df03_FLC_self_report[["∆weight%"]])
# df03_FLC_self_report[["∆weight%"]] <- ifelse((df03_FLC_self_report[["∆weight%"]] < quantile(df03_FLC_self_report[["∆weight%"]], 0.01, na.rm = TRUE)) | (df03_FLC_self_report[["∆weight%"]] > quantile(df03_FLC_self_report[["∆weight%"]], 0.95, na.rm = TRUE)),
#                                              NA,
#                                              df03_FLC_self_report[["∆weight%"]])


df03_FLC_self_report <- df03_FLC_self_report %>% mutate(diet_compliance = (`upload_day_%` * `light_G_%` / 100) %>% round(2))


# 02.4 - [Data Preprocessing] 04_non_FLC_self_report --------------------------------------------------

df04_non_FLC_self_report <- tmp_04 %>% select(-mobile)

# intersect(tmp_03$id, df04_non_FLC_self_report$client_id)  #ensure no FLC client within
#C1. col_name
names(df04_non_FLC_self_report) <- names(df04_non_FLC_self_report) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
#C2-1. filter dup_id
df04_non_FLC_self_report <- df04_non_FLC_self_report %>% janitor::get_dupes(id)
#C2-2. exclude NA: id, date
df04_non_FLC_self_report <- df04_non_FLC_self_report %>% lin_exclude_NA_col(c("id", "date_time"))
#C3. order
df04_non_FLC_self_report <- df04_non_FLC_self_report[with(df04_non_FLC_self_report, order(id, date_free_version)),]

#C4. filter ∆day = 2 months(60 days + 14)

# Group the data by id and find the earliest date for each id
earliest_dates <- df04_non_FLC_self_report %>%
  group_by(id) %>%
  summarise(earliest_date = min(date_free_version))

# Join the original data frame with the earliest date to find the row with the earliest date for each id
earliest_rows <- df04_non_FLC_self_report %>%
  inner_join(earliest_dates, by = c("id", "date_free_version" = "earliest_date"))

earliest_rows$weight <- ifelse(earliest_rows$weight <= 30, NA, earliest_rows$weight)
earliest_rows$weight <- ifelse(earliest_rows$weight > 200, NA, earliest_rows$weight)
earliest_rows$bmi <- ifelse(earliest_rows$bmi <= 10, NA, earliest_rows$bmi)
earliest_rows$bmi <- ifelse(earliest_rows$bmi > 100, NA, earliest_rows$bmi)
earliest_rows$fat_mass <- ifelse(earliest_rows$fat_mass <= 5, NA, earliest_rows$fat_mass)
earliest_rows$fat_mass <- ifelse(earliest_rows$fat_mass > 100, NA, earliest_rows$fat_mass)
earliest_rows$wc <- ifelse(earliest_rows$wc <= 50, NA, earliest_rows$wc)


# Add 60 days to the earliest date for each id
second_dates <- earliest_dates %>%
  mutate(second_date = earliest_date + 60)

# Find the row with the second date for each id
second_rows <- df04_non_FLC_self_report %>%
  inner_join(second_dates, by = "id") %>%
  filter((date_free_version >= second_date) & (date_free_version <= second_date + 14)) %>% 
  distinct(id, .keep_all = TRUE)
second_rows$second_date <- second_rows$date_free_version

second_dates <- second_rows %>% select(id, earliest_date, second_date)

#period upload_counts[fixed: multi-upload in one day!]
df04_non_FLC_self_report_tmp0 <- df04_non_FLC_self_report %>%
  inner_join(second_dates, by = "id") %>%
  filter((date_free_version >= earliest_date) & (date_free_version <= second_date)) %>%
  select(-dupe_count) %>% 
  group_by(id, date_free_version) %>% 
  summarise(
    n = n()
  ) %>% janitor::get_dupes(id) %>%
  distinct(id, .keep_all = TRUE)


second_rows$weight <- ifelse(second_rows$weight <= 30, NA, second_rows$weight)
second_rows$weight <- ifelse(second_rows$weight > 200, NA, second_rows$weight)
second_rows$bmi <- ifelse(second_rows$bmi <= 10, NA, second_rows$bmi)
second_rows$bmi <- ifelse(second_rows$bmi > 100, NA, second_rows$bmi)
second_rows$fat_mass <- ifelse(second_rows$fat_mass <= 5, NA, second_rows$fat_mass)
second_rows$fat_mass <- ifelse(second_rows$fat_mass > 100, NA, second_rows$fat_mass)
second_rows$wc <- ifelse(second_rows$wc <= 50, NA, second_rows$wc)


# Combine the earliest and second rows for each id
df04_non_FLC_self_report <- earliest_rows %>%
  bind_rows(second_rows)
rm(list = c("earliest_dates", "earliest_rows", "second_dates", "second_rows"))

df04_non_FLC_self_report <- df04_non_FLC_self_report %>% filter(id %in% df04_non_FLC_self_report[!is.na(df04_non_FLC_self_report$earliest_date), "id"])
df04_non_FLC_self_report <- df04_non_FLC_self_report[with(df04_non_FLC_self_report, order(id)),]
df04_non_FLC_self_report <- df04_non_FLC_self_report %>% distinct(id, date_free_version, .keep_all = TRUE)

df04_non_FLC_self_report <- df04_non_FLC_self_report %>% select(-c("dupe_count", "date_time", "earliest_date", "second_date"))

#change colname
names(df04_non_FLC_self_report) <- names(df04_non_FLC_self_report) %>% lin_ch_en_format(format = "en", origin = "raw_en")

#clean pre/post table

df04_non_FLC_self_report_tmp <- df04_non_FLC_self_report

a <- df04_non_FLC_self_report_tmp[seq(1,nrow(df04_non_FLC_self_report_tmp), 2),] 
names(a)[-1] <- paste0(a %>% select(-c("id")) %>% names(), "_baseline")

b <- df04_non_FLC_self_report_tmp[seq(2,nrow(df04_non_FLC_self_report_tmp), 2),] 
names(b)[-1] <- paste0(b %>% select(-c("id")) %>% names(), "_endpoint")

aa <- b %>% select(-c("id", "date_free_version_endpoint", "gender_endpoint","btd_endpoint")) - a %>% select(-c("id", "date_free_version_baseline", "gender_baseline", "btd_baseline"))
aa <- cbind(a %>% select("id"), aa)
names(aa)[-1] <- paste0("∆", df04_non_FLC_self_report_tmp %>% select(-c("id","date_free_version", "gender","btd")) %>% names())

bb <- (((b %>% select(-c("id", "date_free_version_endpoint", "gender_endpoint","btd_endpoint"))) - a %>% select(-c("id", "date_free_version_baseline","gender_baseline", "btd_baseline")))*100 /   a %>% select(-c("id", "date_free_version_baseline","gender_baseline", "btd_baseline"))) %>% round(2) 
bb <- cbind(a %>% select("id"), bb)
names(bb)[-1] <- paste0("∆", df04_non_FLC_self_report_tmp %>% select(-c("id","date_free_version", "gender","btd")) %>% names(), "%")

c1 <- full_join(a, b, by = c("id"))
names(c1)[grep("date", names(c1))] <- c("date_baseline","date_endpoint")
c1 %<>% relocate(c("date_baseline","date_endpoint"), .after = "id")

c1 <- full_join(c1, aa, by = c("id"))
c1 <- full_join(c1, bb, by = c("id"))
df04_non_FLC_self_report <- c1
rm(list = c("a","aa","b","bb","c1","df04_non_FLC_self_report_tmp"))

df04_non_FLC_self_report <- df04_non_FLC_self_report %>% select(-btd_endpoint, -gender_endpoint)
df04_non_FLC_self_report <- df04_non_FLC_self_report %>% rename(btd = btd_baseline)
df04_non_FLC_self_report <- df04_non_FLC_self_report %>% rename(gender = gender_baseline)


#C5. age: b%<>% %<>% td - date_t0 年齡(療程起始當天計算)
df04_non_FLC_self_report$age <- (lubridate::ymd(df04_non_FLC_self_report$date_baseline) - lubridate::ymd(df04_non_FLC_self_report$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()

#C6. upload_count & upload_day%
df04_non_FLC_self_report <- lin_mapping(df04_non_FLC_self_report, note_counts, id, df04_non_FLC_self_report_tmp0, dupe_count, id)
rm(df04_non_FLC_self_report_tmp0)
df04_non_FLC_self_report <- df04_non_FLC_self_report %>% mutate(upload_day_p = (as.numeric(note_counts)*100 / as.numeric((lubridate::ymd(date_endpoint) - (lubridate::ymd(date_baseline)) + 1))) %>% round(2))



#rm outliers
df04_non_FLC_self_report <- df04_non_FLC_self_report %>% lin_exclude_NA_col(grep("weight",names(.), value = TRUE))

for (i in c(df04_non_FLC_self_report %>% names() %>% grep("∆", ., value = TRUE))) {
  df04_non_FLC_self_report[[i]] <- 
    ifelse(df04_non_FLC_self_report[[i]] < quantile(df04_non_FLC_self_report[[i]], 0.05, na.rm = TRUE) | df04_non_FLC_self_report[[i]] > quantile(df04_non_FLC_self_report[[i]], 0.95, na.rm = TRUE), NA, df04_non_FLC_self_report[[i]])
}

#rm commercial outliers
  # df04_non_FLC_self_report %>% select(`∆weight`) %>% summary()
df04_non_FLC_self_report <- df04_non_FLC_self_report[df04_non_FLC_self_report[["∆weight"]] >= quantile(df04_non_FLC_self_report[["∆weight"]], 0.25, na.rm = TRUE), ] %>% janitor::remove_empty("rows")

#sample size report
df04_non_FLC_self_report$id %>% unique() %>% length()


#align names with df03_FLC_self_report
names(df04_non_FLC_self_report) <- c("client_id","date_free_t0","date_free_t1", "gender", 
                                     "btd", "weight_before", "bmi_before", "fat_before", 
                                     "wc_before", "weight_after", "bmi_after", "fat_after", 
                                     "wc_after", "weight_delta", "bmi_delta", "fat_delta", 
                                     "wc_delta", "weight_delta_p", "bmi_delta_p", "fat_delta_p", 
                                     "wc_delta_p","age", "note_count", "upload_day_p")



names(df04_non_FLC_self_report) <- names(df04_non_FLC_self_report) %>% lin_ch_en_format(format = "en", origin = "raw_en")


#df04_non_FLC_self_report %>% summary()

# df04_non_FLC_self_report %>%
#   group_by(gender) %>%
#   summarize(
#     `∆weight%` = mean(`∆weight%`, na.rm = TRUE),
#     `∆fat%` = mean(`∆fat%`, na.rm = TRUE),
#     `∆wc%` = mean(`∆wc%`, na.rm = TRUE),
#     n = n()
#   )

#Output datasets

# write.table(df04_non_FLC_self_report %>% filter(date_endpoint <= "2022-09-01"), file = "datasets_non_flc")


# 02.5 - [Data Preprocessing] 05_biochem --------------------------------------------------

df05_biochem <- tmp_05

#C1. format
df05_biochem[c("glucose_ac","glucose_pc_1hr","glucose_pc_2hr","insulin","insulin_pc_1hr","insulin_pc_2hr","hba1c","homa_ir","homa_beta",
         "triglyceride","total_cholesterol","high_density_lipoprotein","low_density_lipoprotein_cholesterol","sd_ldl",
         "c_peptide","egfr","blood_creatinine","uric_acid","tsh","prolactin","fsh","lh","e2","testosterone","progesterone","dhea_s","shbg","amh","t3","t3_reverse","t4_free","psa",
         "urine_spe_gravity", "urine_ph",
         "wbc","rbc","hb","esr","mcv","mch","mchc","platelet","rdw_sd","rdw_cv","neutrophils","lymphocytes","monocytes","eosinophils","basophils","monocytes_percent","eosinophils_percent","basophils_percent","alt_gpt","ast_got","amylase","lipase","apoli_a1","apoli_b","apolib_ai_ratio",
         "glycated_albumin", "blood_vitamin_d")] %<>% 
  lapply(as.numeric)

df05_biochem <- lin_mapping(df05_biochem, gender, client_id, df01_profile, gender, id) #men

#C2. colname
names(df05_biochem) <- df05_biochem %>% names() %>% lin_ch_en_format(format = "en", origin = "raw_en")
#C3. order by date_blood
df05_biochem <- df05_biochem[with(df05_biochem, order(date_blood)),]
#(1) tAUCg, tAUCi (2) Pattern_major, Pattern_minor (3) OGIRIndex: iAUC-i(-30) - iAUC-g(+50)

df05_biochem$tAUCg <- lin_AUC_calc(df05_biochem, df05_biochem %>% names() %>% grep("^glucose", ., value = TRUE))
df05_biochem$tAUCi <- lin_AUC_calc(df05_biochem, df05_biochem %>% names() %>% grep("^insulin", ., value = TRUE))

df05_biochem <- df05_biochem %>% lin_insulin_rsp_pattern(df05_biochem %>% names() %>% grep("^insulin", ., value = TRUE), pattern = 2)
df05_biochem <- df05_biochem %>% rename(Pattern_major = I)
df05_biochem <- df05_biochem %>% lin_insulin_rsp_pattern(df05_biochem %>% names() %>% grep("^insulin", ., value = TRUE), pattern = 1)
df05_biochem <- df05_biochem %>% rename(Pattern_minor = I)

df05_biochem <- df05_biochem %>% mutate(OGIRIndex = lin_AUC_calc(df05_biochem, df05_biochem %>% names() %>% grep("^insulin", ., value = TRUE), increment_value = -30) - 
                              lin_AUC_calc(df05_biochem, df05_biochem %>% names() %>% grep("^glucose", ., value = TRUE), increment_value = 50))

#Diagnosis
df05_biochem <- df05_biochem %>% lin_diagnosis_DM(c("hba1c", "glucose_ac", "glucose_pc_1hr","glucose_pc_2hr"))
df05_biochem <- df05_biochem %>% lin_diagnosis_HLP(c("gender","tg","tc","hdl","ldl"))

#GA, eAG
# df05_biochem <- lin_conv_GA(df05_biochem, hba1c, GA)
df05_biochem <- lin_conv_eAG(df05_biochem, hba1c, eAG)


#uric acid: : ≥5.5 (mg/dL)
cutoffs_sua_0 = 5.5 #women
df05_biochem$sua_gp_0 <- df05_biochem$uric_acid %>% cut(c(-Inf, cutoffs_sua_0, Inf), c("Normal", "High"), right = FALSE)
#uric acid: men: ≥7.6 (mg/dL); women: 6.6(mg/dL)
cutoffs_sua = 6.6 #women
df05_biochem$sua_gp <- df05_biochem$uric_acid %>% cut(c(-Inf, cutoffs_sua, Inf), c("Normal", "High"), right = FALSE)
df05_biochem[(df05_biochem[["uric_acid"]] < 7.6) & (df05_biochem[["gender"]] == "male"), "sua_gp"] <- "Normal"



# 02.6 - [Data Preprocessing] 07_Diet_meal --------------------------------------------------

df07_Diet_meal <- tmp_07

df07_Diet_meal <- 
  df07_Diet_meal %>% lin_conv_food(c("milk_whole_fat","milk_low_fat","milk_skim","meat_beans_low_fat","meat_beans_medium_fat","meat_beans_high_fat","grains","vegetables","fruits","oil"))

df07_Diet_meal <- df07_Diet_meal %>% select(-c("milk_whole_fat","milk_low_fat","milk_skim","meat_beans_low_fat","meat_beans_medium_fat","meat_beans_high_fat"))

df07_Diet_meal <- df07_Diet_meal[with(df07_Diet_meal, order(client_id, date_diet)),]

#condense df by id, date
 #01.summarise by date(merge with stat_table)
df07_Diet_meal_by_meal <- 
  df07_Diet_meal %>% 
  group_by(client_id) %>% 
  summarise(
    #meal
    calorie_meal_mean = mean(calorie, na.rm = TRUE) %>% round(2),
    calorie_meal_sum = sum(calorie, na.rm = TRUE) %>% round(2),
    carb_ep_meal = (sum(carb_e, na.rm = TRUE)/sum(calorie, na.rm = TRUE)*100) %>% round(2),
    protein_ep_meal = (sum(protein_e, na.rm = TRUE)/sum(calorie, na.rm = TRUE)*100) %>% round(2),
    fat_ep_meal = (sum(fat_e, na.rm = TRUE)/sum(calorie, na.rm = TRUE)*100) %>% round(2),
    
    grains_meal = mean(grains, na.rm = TRUE) %>% round(2),
    meat_bean_meal = mean(meat_bean, na.rm = TRUE) %>% round(2),
    oil_meal = mean(oil, na.rm = TRUE) %>% round(2),
    vegetables_meal = mean(vegetables, na.rm = TRUE) %>% round(2),
    fruits_meal = mean(fruits, na.rm = TRUE) %>% round(2),
    milk_meal = mean(milk, na.rm = TRUE) %>% round(2)
  )
df07_Diet_meal_by_meal <- df07_Diet_meal_by_meal[with(df07_Diet_meal_by_meal, order(client_id)),]

  #02.summarise by day(merge with df06_Diet_day, (1)Acquire sum, (2)Merge, (3)Summarise daily results, meanwhile (4)Compared w/ target and calculate deficit)
df07_Diet_meal_by_date <- 
  df07_Diet_meal %>% 
  group_by(client_id, date_diet) %>% 
  summarise(
    #day
    calorie_day = sum(calorie, na.rm = TRUE) %>% round(2),
    carb_e_day = sum(carb_e, na.rm = TRUE) %>% round(2),
    protein_e_day = sum(protein_e, na.rm = TRUE) %>% round(2),
    fat_e_day = sum(fat_e, na.rm = TRUE) %>% round(2),
    
    grains_day = sum(grains, na.rm = TRUE) %>% round(2),
    meat_bean_day = sum(meat_bean, na.rm = TRUE) %>% round(2),
    oil_day = sum(oil, na.rm = TRUE) %>% round(2),
    vegetables_day = sum(vegetables, na.rm = TRUE) %>% round(2),
    fruits_day = sum(fruits, na.rm = TRUE) %>% round(2),
    milk_day = sum(milk, na.rm = TRUE) %>% round(2)
  )
df07_Diet_meal_by_date <- df07_Diet_meal_by_date[with(df07_Diet_meal_by_date, order(client_id, date_diet)),]


# 02.7 - [Data Preprocessing] 06_Diet_day --------------------------------------------------


df06_Diet_day <- tmp_06

df06_Diet_day <- df06_Diet_day %>% select(-c("carbohydrate","protein","fat","calorie"))

df06_Diet_day[c("note_counts","pic_counts","essay_count","light_green_count","light_yellow_count","light_red_count",
                  "grains_target","fruits_target","vegetables_target","meat_beans_low_fat_target","meat_beans_medium_fat_target","meat_beans_high_fat_target",
                  "milk_whole_fat_target","milk_low_fat_target","milk_skim_target","oil_target")] %<>% lapply(as.numeric)

#Q. id NA
df06_Diet_day <- df06_Diet_day %>% filter(!is.na(client_id))

df06_Diet_day <- df06_Diet_day %>% lin_conv_food(c("milk_whole_fat_target","milk_low_fat_target","milk_skim_target",
                                                   "meat_beans_low_fat_target","meat_beans_medium_fat_target","meat_beans_high_fat_target",
                                                   "grains_target","vegetables_target","fruits_target","oil_target"))

#merge day/meal
df06_Diet_day <- merge(df06_Diet_day, df07_Diet_meal_by_date, by.x = c("client_id", "date_diet"), all.x = TRUE)

df06_Diet_day <- df06_Diet_day %>% select(c("client_id","date_diet","note_counts","pic_counts","essay_count",
                                            "light_green_count","light_yellow_count","light_red_count",
                                            "carb_e_day","protein_e_day","fat_e_day","calorie_day",
                                            "carbohydrate_target","protein_target","fat_target","calorie_target",
                                            "grains_day","meat_bean_day","oil_day","vegetables_day","fruits_day","milk_day",
                                            "grains_target","meat_bean_target","oil_target","vegetables_target","fruits_target","milk_target"
))


# [Old]#outliers adjust:
#   #all record should be replaced by avg performance, instead of caloire only.
# id_diet <- df06_Diet_day[(df06_Diet_day[["calorie"]] < 500) | (is.na(df06_Diet_day[["calorie"]])), "client_id"] %>% unique()
# # [執行時間]
# # 使用者      系統      流逝 
# # 14342.534  5612.097 28568.632 
# for (i in c(id_diet)) {
#   if (i == c(id_diet) %>% head(1)) {
#     ptm <- proc.time()
#     j = 1
#   }
#   df06_Diet_day[(df06_Diet_day[["client_id"]] == i) & ((df06_Diet_day[["calorie"]] < 500) | (is.na(df06_Diet_day[["calorie"]]))), df06_Diet_day %>% names() %>% grep("client_id|date_diet|begin_date|end_date|target_updated_at", .,  invert = TRUE)] <- 
#     df06_Diet_day[(df06_Diet_day[["client_id"]] == i) & ((df06_Diet_day[["calorie"]] >= 500)), df06_Diet_day %>% names() %>% grep("client_id|date_diet|begin_date|end_date|target_updated_at", .,  invert = TRUE)] %>% lapply(., function(x) mean(x, na.rm =TRUE) %>% round(2))
#   
#   
#   progress(j, length(id_diet))
#   j = j + 1
#   
#   if (i == c(id_diet) %>% tail(1)) {
#     cat("\n[Completed!]\n")
#     cat("\n[執行時間]\n")
#     # (115279/115279) 09:08:45
#     print(proc.time() - ptm)
#     rm(id_diet)
#   }
# }

#[Revised: improve 99.39% efficiency]
# 使用者    系統    流逝  N = 2,266,313
# 262.179   1.250 263.339 

#[Imputation 1: Mean Imp., 假設：飲食表現良好，]
# df06_Diet_day <- df06_Diet_day %>% 
#   group_by(client_id) %>% 
#   mutate_at(vars(-c("client_id","date_diet")),
#             ~ifelse(calorie_day < 500 | is.na(calorie_day), NA, .)) %>% 
#   mutate_at(vars(-c("client_id","date_diet")),
#             ~ifelse(is.na(calorie_day), round(mean(., na.rm = TRUE), 2), .)) %>%
#   ungroup() 


#[Imputation 2: remove Imp.]
df06_Diet_day <- df06_Diet_day %>% 
  group_by(client_id) %>% 
  mutate_at(vars(-c("client_id","date_diet")),
            ~ifelse(calorie_day < 500 | is.na(calorie_day), NA, .)) %>% 
  ungroup() 

df06_Diet_day <- df06_Diet_day %>% lin_exclude_NA_col("calorie_day")

# # debug
# df06_Diet_day %>%
#   filter(client_id == 0) %>%
#   group_by(client_id) %>%
#   mutate_at(vars(-c("client_id","date_diet")),
#             ~ifelse(calorie_day < 500 | is.na(calorie_day), NA, .)) %>%
#   mutate_at(vars(-c("client_id","date_diet")),
#             ~ifelse(is.na(calorie_day), round(mean(., na.rm = TRUE), 2), .)) %>%
#   ungroup() %>% view()



#**[Trouble-shooting: many clients have long period of vacant target]
df06_Diet_day <- df06_Diet_day %>% 
  group_by(client_id) %>% 
  mutate_at(vars(grep("target", names(df06_Diet_day), value = T)),
            ~ifelse(calorie_target < 100 | is.na(calorie_target), NA, .)) %>%
  mutate_at(vars(grep("target", names(df06_Diet_day), value = T)),
            ~ifelse(is.na(eval(parse(text = tail(grep("target", names(df06_Diet_day), value = T), 1)))), round(mean(., na.rm = TRUE), 2), .)) %>%
  ungroup()







#deficit each day
  ## Actual intake - Target intake 
df06_Diet_day <- df06_Diet_day %>% mutate(grains_day_deficit = grains_day - grains_target)
df06_Diet_day <- df06_Diet_day %>% mutate(meat_bean_day_deficit = meat_bean_day - meat_bean_target)
df06_Diet_day <- df06_Diet_day %>% mutate(oil_day_deficit = oil_day - oil_target)
df06_Diet_day <- df06_Diet_day %>% mutate(vegetables_day_deficit = vegetables_day - vegetables_target)
df06_Diet_day <- df06_Diet_day %>% mutate(fruits_day_deficit = fruits_day - fruits_target)
df06_Diet_day <- df06_Diet_day %>% mutate(milk_day_deficit = milk_day - milk_target)
df06_Diet_day <- df06_Diet_day %>% mutate(carb_e_day_deficit = carb_e_day - carbohydrate_target*4)
df06_Diet_day <- df06_Diet_day %>% mutate(protein_e_day_deficit = protein_e_day - protein_target*4)
df06_Diet_day <- df06_Diet_day %>% mutate(fat_e_day_deficit = fat_e_day - fat_target*9)
df06_Diet_day <- df06_Diet_day %>% mutate(calorie_deficit = calorie_day - calorie_target)



#Sorting by Multiple Columns
df06_Diet_day <- df06_Diet_day[with(df06_Diet_day, order(client_id, date_diet)),]



# 02.8 - [Data Preprocessing] 08_3D_scanner --------------------------------------------------

df08_3D_scanner <- tmp_08
# df08_3D_scanner %>% glimpse()
df08_3D_scanner <- df08_3D_scanner %>% as_tibble()

df08_3D_scanner <- df08_3D_scanner[names(df08_3D_scanner)] %>% lapply(as.numeric)
df08_3D_scanner$client_id <- df08_3D_scanner$client_id %>% as.integer()
df08_3D_scanner <- df08_3D_scanner %>% as.tibble()


# 02.9 - [Data Preprocessing] 09_hormone --------------------------------------------------

df09_hormone <- tmp_09
df09_hormone[c("hormone_L","hormone_P","hormone_t","hormone_c","hormone_l","hormone_e","hormone_a")] %<>% lapply(as.character)







# 03.1 - Create clinic datasets:topshow, genesisclinic, lumez --------------------------------------------------------------

#C1. Select clinic clients: topshow, genesisclinic
# table((df01_profile %>% filter((org_name == "topshow") | org_name == "genesisclinic"))$org_name)
table((df01_profile %>% filter((org_name != "cofit") & (org_name != "bk2o_backup") & (org_name != "fitness_factory")))$org_name) %>% addmargins()

clinic_id_vector <- df01_profile %>% filter((org_name != "cofit") & (org_name != "bk2o_backup") & (org_name != "fitness_factory")) %>% select(id) %>% pull() %>% unique()

#C2.1.save origin version
clinic_inbody_data <- df02_inbody %>% filter(id %in% clinic_id_vector) 
clinic_blood_data <- df05_biochem %>% filter(id %in% clinic_id_vector) 

clinic_inbody_data_ori <- df02_inbody %>% filter(id %in% clinic_id_vector) 
clinic_blood_data_ori <- df05_biochem %>% filter(id %in% clinic_id_vector) 

#C2.2. med_id_pool
med_id_pool <- dplyr::intersect(clinic_inbody_data %>% select(id) %>% unique() %>% pull(),
                                clinic_blood_data %>% select(id) %>% unique() %>% pull() )

clinic_inbody_data %<>% filter(id %in% med_id_pool)
clinic_blood_data %<>% filter(id %in% med_id_pool)

#C2.3. order by id, date
clinic_inbody_data <- clinic_inbody_data[with(clinic_inbody_data, order(id, date_inbody)),] 
# clinic_inbody_data <- clinic_inbody_data[with(clinic_inbody_data, order(c(date_inbody, id))),] %>% janitor::remove_empty("rows")
clinic_blood_data <- clinic_blood_data[with(clinic_blood_data, order(id, date_blood)),] 


#C3. Map inbody & blood data to profile as as stat_table

stat_tm <- df01_profile %>% filter((org_name != "cofit") & (org_name != "bk2o_backup") & (org_name != "fitness_factory")) %>% filter(id %in% med_id_pool) %>% filter(class_order == 1)

#recored in both "topshow" and genesis: rm past record in "topshow"(w/most missing value), so that I can use unique id
stat_tm <- stat_tm %>% filter( ((id %in% stat_tm[which(duplicated(stat_tm$id)),"id"])&( org_name != "topshow")) | (id %in% stat_tm[which(duplicated(stat_tm$id)),"id"]) %>% not() )



#each id cost 0.47 sec
for (i in c(unique(stat_tm[["id"]]))) {
  #1. create dataframe
  if (i == (c(unique(stat_tm[["id"]])) %>% head(1))) {
    j = 1
    # Start the clock!
    ptm <- proc.time()
    start_time <- Sys.time()
    
    #use id = 454425, to establish table format
    #Profile
    a0 <- stat_tm[(stat_tm$id == 454425),]
    #Inbody
    #baseline (抓最近30天data)
    v_b <- (clinic_inbody_data[(clinic_inbody_data$id == 454425) , ] %>% select(date_inbody) %>% pull - stat_tm[(stat_tm$id == 454425) , "date_t0"]) %>% abs()
    v_b <- ifelse(v_b > 30, NA, v_b) %>% order(na.last = NA) %>% head(1)
    #endpoint (抓最近30天data)
    v_e <- (clinic_inbody_data[(clinic_inbody_data$id == 454425) , ] %>% select(date_inbody) %>% pull - stat_tm[(stat_tm$id == 454425) , "date_t1"]) %>% abs()
    v_e <- ifelse(v_e > 30, NA, v_e) %>% order(na.last = NA) %>% head(1)
    
    a1_b <- clinic_inbody_data[(clinic_inbody_data$id == 454425),][v_b,]
    a1_e <- clinic_inbody_data[(clinic_inbody_data$id == 454425),][v_e,]
    a1 <- merge(a1_b,
                a1_e,
                by = "id", suffixes = c("_baseline","_endpoint"))
    
    #Blood
    #baseline (抓最近30天data)
    v_b <- (clinic_blood_data[(clinic_blood_data$id == 454425) , ] %>% select(date_blood) %>% pull - stat_tm[(stat_tm$id == 454425) , "date_t0"]) %>% abs()
    v_b <- ifelse(v_b > 30, NA, v_b) %>% order(na.last = NA) %>% head(1)
    #endpoint (抓最近30天data)
    v_e <- (clinic_blood_data[(clinic_blood_data$id == 454425) , ] %>% select(date_blood) %>% pull - stat_tm[(stat_tm$id == 454425) , "date_t1"]) %>% abs()
    v_e <- ifelse(v_e > 30, NA, v_e) %>% order(na.last = NA) %>% head(1)
    
    a2_b <- clinic_blood_data[(clinic_blood_data$id == 454425),][v_b,]
    a2_e <- clinic_blood_data[(clinic_blood_data$id == 454425),][v_e,]
    a2 <- merge(a2_b,
                a2_e,
                by = "id", suffixes = c("_baseline","_endpoint"))
    
    
    
    #∆var / ∆var% 
    a3 <- merge(select_if(a1_e, is.numeric) - select_if(a1_b, is.numeric),
                ((select_if(a1_e, is.numeric) - select_if(a1_b, is.numeric))/select_if(a1_b, is.numeric)) %>% multiply_by(100) %>% round(2),
                by = "id", suffixes = c("","%"))
    names(a3) <- paste0("∆", names(a3))
    a3 <- a3 %>% dplyr::rename(id = `∆id`)
  
    a4 <- merge(select_if(a2_e, is.numeric) - select_if(a2_b, is.numeric),
                ((select_if(a2_e, is.numeric) - select_if(a2_b, is.numeric))/select_if(a2_b, is.numeric)) %>% multiply_by(100) %>% round(2),
                by = "id", suffixes = c("","%"))
    names(a4) <- paste0("∆", names(a4))
    a4 <- a4 %>% dplyr::rename(id = `∆id`)
    
    
    #establish df
    stat_table <- Reduce(function(x,y) merge(x, y, by = "id", all.x = TRUE), list(a0, a1, a2, a3, a4), accumulate =FALSE)
    stat_table <- stat_table[0,]
    
    cols_01_profile <- a0 %>% names() %>% length()
    cols_02_inbody_baseline <- a1_b %>% names() %>% length() -1 + cols_01_profile
    cols_03_inbody_endpoint <- a1_e %>% names() %>% length() -1 + cols_02_inbody_baseline
    cols_04_blood_baseline <- a2_b %>% names() %>% length() -1 + cols_03_inbody_endpoint
    cols_05_blood_endpoint <- a2_e %>% names() %>% length() -1 + cols_04_blood_baseline
    cols_06_delta_inbody<- a3 %>% names() %>% length() -1 + cols_05_blood_endpoint
    cols_07_delta_blood<- a4 %>% names() %>% length() -1 + cols_06_delta_inbody
  }
  
  #2. mapping
  #Profile
  a0 <- stat_tm[(stat_tm$id == i),]
  stat_table[j, seq(1, cols_01_profile)] <- a0
  #Inbody   
  #baseline (抓最近30天data)
  v_b <- (clinic_inbody_data[(clinic_inbody_data$id == i) , ] %>% select(date_inbody) %>% pull - stat_tm[(stat_tm$id == i) , "date_t0"]) %>% abs()
  v_b <- ifelse(v_b > 30, NA, v_b) %>% order(na.last = NA) %>% head(1)
  #endpoint (抓最近30天data)
  v_e <- (clinic_inbody_data[(clinic_inbody_data$id == i) , ] %>% select(date_inbody) %>% pull - stat_tm[(stat_tm$id == i) , "date_t1"]) %>% abs()
  v_e <- ifelse(v_e > 30, NA, v_e) %>% order(na.last = NA) %>% head(1)
  
  a1_b <- clinic_inbody_data[(clinic_inbody_data$id == i),][v_b,]
  a1_e <- clinic_inbody_data[(clinic_inbody_data$id == i),][v_e,]
  a1 <- merge(a1_b,
              a1_e,
              by = "id", suffixes = c("_baseline","_endpoint"))
  
  if (nrow(a1_b) != 0) {
    stat_table[j, seq(1 + cols_01_profile, cols_02_inbody_baseline)] <- a1_b %>% select(-id)
  }
  if (nrow(a1_e) != 0) {
    stat_table[j, seq(1 + cols_02_inbody_baseline, cols_03_inbody_endpoint)] <- a1_e %>% select(-id)
  }
  
  #Blood
  #baseline (抓最近30天data)
  v_b <- (clinic_blood_data[(clinic_blood_data$id == i) , ] %>% select(date_blood) %>% pull - stat_tm[(stat_tm$id == i) , "date_t0"]) %>% abs()
  v_b <- ifelse(v_b > 30, NA, v_b) %>% order(na.last = NA) %>% head(1)
  #endpoint (抓最近30天data)
  v_e <- (clinic_blood_data[(clinic_blood_data$id == i) , ] %>% select(date_blood) %>% pull - stat_tm[(stat_tm$id == i) , "date_t1"]) %>% abs()
  v_e <- ifelse(v_e > 30, NA, v_e) %>% order(na.last = NA) %>% head(1)
  
  a2_b <- clinic_blood_data[(clinic_blood_data$id == i),][v_b,]
  a2_e <- clinic_blood_data[(clinic_blood_data$id == i),][v_e,]
  a2 <- merge(a2_b,
              a2_e,
              by = "id", suffixes = c("_baseline","_endpoint"))
  
  if (nrow(a2_b) != 0) {
    stat_table[j, seq(1 + cols_03_inbody_endpoint, cols_04_blood_baseline)] <- a2_b %>% select(-id)
  }
  if (nrow(a2_e) != 0) {
    stat_table[j, seq(1 + cols_04_blood_baseline, cols_05_blood_endpoint)] <- a2_e %>% select(-id)
  }
  
  #∆var / ∆var% 
  if ((nrow(a1_e) != 0) & (nrow(a1_b) != 0)) {
    a3 <- merge(select_if(a1_e, is.numeric) - select_if(a1_b, is.numeric),
                ((select_if(a1_e, is.numeric) - select_if(a1_b, is.numeric))/select_if(a1_b, is.numeric)) %>% multiply_by(100) %>% round(2),
                by = "id", suffixes = c("","%"))
    names(a3) <- paste0("∆", names(a3))
    a3 <- a3 %>% dplyr::rename(id = `∆id`)
    
    stat_table[j, seq(1 + cols_05_blood_endpoint, cols_06_delta_inbody)] <- a3 %>% select(-id)
  }
  
  
  if ((nrow(a2_e) != 0) & (nrow(a2_b) != 0)) {
    a4 <- merge(select_if(a2_e, is.numeric) - select_if(a2_b, is.numeric),
                ((select_if(a2_e, is.numeric) - select_if(a2_b, is.numeric))/select_if(a2_b, is.numeric)) %>% multiply_by(100) %>% round(2),
                by = "id", suffixes = c("","%"))
    names(a4) <- paste0("∆", names(a4))
    a4 <- a4 %>% dplyr::rename(id = `∆id`)
    
    stat_table[j, seq(1 + cols_06_delta_inbody, cols_07_delta_blood)] <- a4 %>% select(-id) 
  }
  
  
  progress(j, length(unique(stat_tm[["id"]])))
  
  if (j == length(unique(stat_tm[["id"]]))) {
    cat("\n[執行時間]\n")
    print(proc.time() - ptm)
    print("[Completed!]")
    rm(list = c("med_id_pool", "a0", "a1", "a2", "a1_b", "a1_e", "a2_b", "a2_e", "a3", "a4", "v_b", "v_e",
                "cols_01_profile", "cols_02_inbody_baseline", "cols_03_inbody_endpoint", "cols_04_blood_baseline",
                "cols_05_blood_endpoint", "cols_06_delta_inbody", "cols_07_delta_blood"))
  }
  j = j + 1
}



## temp --------------------------------------------------------------------

df06_Diet_day_tmp <- df06_Diet_day[which(df06_Diet_day[["client_id"]] %in% unique(stat_table[["id"]])),]
df06_Diet_day_tmp$date_t0 <- NA
df06_Diet_day_tmp$date_t0 <- df06_Diet_day_tmp$date_t0 %>% as.Date()
df06_Diet_day_tmp <- lin_mapping(df06_Diet_day_tmp, date_t0, client_id, stat_table, date_t0, id)
df06_Diet_day_tmp$date_t1 <- NA
df06_Diet_day_tmp$date_t1 <- df06_Diet_day_tmp$date_t1 %>% as.Date()
df06_Diet_day_tmp <- lin_mapping(df06_Diet_day_tmp, date_t1, client_id, stat_table, date_t1, id)

df06_Diet_day_tmp <- df06_Diet_day_tmp %>% filter((date_diet >= date_t0) & (date_diet <= date_t1))

 
df06_Diet_day_tmp <- 
  df06_Diet_day_tmp %>% 
  group_by(client_id) %>% 
  summarise(
    upload_day_p = (n() *100 / (as.numeric((unique(date_t1) - unique(date_t0) + 1)))) %>% round(2), 
    note_counts = sum(note_counts, na.rm = TRUE),
    pic_counts = sum(pic_counts, na.rm = TRUE),
    
    light_green_p = (sum(light_green_count, na.rm = TRUE) *100 / (sum(sum(light_green_count, na.rm = TRUE),
                                                                      sum(light_yellow_count, na.rm = TRUE),
                                                                      sum(light_red_count, na.rm = TRUE)))) %>% round(2),
    light_yellow_p = (sum(light_yellow_count, na.rm = TRUE) *100 / (sum(sum(light_green_count, na.rm = TRUE),
                                                                        sum(light_yellow_count, na.rm = TRUE),
                                                                        sum(light_red_count, na.rm = TRUE)))) %>% round(2),
    light_red_p = (sum(light_red_count, na.rm = TRUE) *100 / (sum(sum(light_green_count, na.rm = TRUE),
                                                                  sum(light_yellow_count, na.rm = TRUE),
                                                                  sum(light_red_count, na.rm = TRUE)))) %>% round(2),
    
    #Actual intake
    calorie_day_tmp = (sum(calorie_day, na.rm = TRUE) / (n())) %>% round(2), 
    carb_ep = (sum(carb_e_day, na.rm = TRUE)*100 / sum(calorie_day, na.rm = TRUE)) %>% round(2),
    protein_ep = (sum(protein_e_day, na.rm = TRUE)*100 / sum(calorie_day, na.rm = TRUE)) %>% round(2),
    fat_ep = (sum(fat_e_day, na.rm = TRUE)*100 / sum(calorie_day, na.rm = TRUE)) %>% round(2),
    
    grains_day = (sum(grains_day, na.rm = TRUE) / (n())) %>% round(2),  
    meat_bean_day = (sum(meat_bean_day, na.rm = TRUE) / (n())) %>% round(2),  
    oil_day = (sum(oil_day, na.rm = TRUE) / (n())) %>% round(2),
    vegetables_day = (sum(vegetables_day, na.rm = TRUE) / (n())) %>% round(2),  
    fruits_day = (sum(fruits_day, na.rm = TRUE) / (n())) %>% round(2),  
    milk_day = (sum(milk_day, na.rm = TRUE) / (n())) %>% round(2),  
    
    #Target intake
    calorie_target_tmp = (mean(calorie_target, na.rm = TRUE)) %>% round(2), 
    carb_ep_target = (mean(carbohydrate_target, na.rm = TRUE)*4*100 / mean(calorie_target, na.rm = TRUE)) %>% round(2),
    protein_ep_target = (mean(protein_target, na.rm = TRUE)*4*100 / mean(calorie_target, na.rm = TRUE)) %>% round(2),
    fat_ep_target = (mean(fat_target, na.rm = TRUE)*9*100 / mean(calorie_target, na.rm = TRUE)) %>% round(2),
    
    grains_target = (sum(grains_target, na.rm = TRUE) / (n())) %>% round(2),  
    meat_bean_target = (sum(meat_bean_target, na.rm = TRUE) / (n())) %>% round(2),  
    oil_target = (sum(oil_target, na.rm = TRUE) / (n())) %>% round(2),
    vegetables_target = (sum(vegetables_target, na.rm = TRUE) / (n())) %>% round(2),  
    fruits_target = (sum(fruits_target, na.rm = TRUE) / (n())) %>% round(2),  
    milk_target = (sum(milk_target, na.rm = TRUE) / (n())) %>% round(2),  
    
    #Deficit (Target - Actual, make deficit as positive(+) value)
    calorie_day_deficit_tmp = (mean(calorie_deficit, na.rm = TRUE)*(-1)) %>% round(2),
    carb_e_day_deficit = (sum(carb_e_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),
    protein_e_day_deficit = (sum(protein_e_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),
    fat_e_day_deficit = (sum(fat_e_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),
    
    grains_day_deficit = (sum(grains_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),  
    meat_bean_day_deficit = (sum(meat_bean_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),  
    oil_day_deficit = (sum(oil_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),
    vegetables_day_deficit = (sum(vegetables_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),  
    fruits_day_deficit = (sum(fruits_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),  
    milk_day_deficit = (sum(milk_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2)
    
  )

colnames(df06_Diet_day_tmp) <- gsub("_tmp", colnames(df06_Diet_day_tmp), replacement = "")

names(df06_Diet_day_tmp) <- names(df06_Diet_day_tmp) %>% lin_ch_en_format(format = "en", origin = "raw_en")


df07_Diet_meal_by_meal <- df07_Diet_meal_by_meal %>% dplyr::rename(id = client_id)

df07_Diet_meal_by_meal[which(df07_Diet_meal_by_meal$id %in% df06_Diet_day_tmp$id),]


df06_Diet_day_tmp <- merge(df06_Diet_day_tmp,
                           df07_Diet_meal_by_meal[which(df07_Diet_meal_by_meal$id %in% df06_Diet_day_tmp$id),], 
                           by.x = "id", all.x = TRUE)


stat_table <- merge(stat_table,
                    df06_Diet_day_tmp, 
                    by.x = "id", all.x = TRUE)

#Diagnosis X-syndrome
stat_table <- stat_table %>% lin_diagnosis_MetaX(c("gender","wc_baseline","sbp_baseline","dbp_baseline","glucose_ac_baseline","tg_baseline","hdl_baseline")) %>%
  dplyr::rename(MetaX_baseline = MetaX)
stat_table <- stat_table %>% lin_diagnosis_MetaX(c("gender","wc_endpoint","sbp_endpoint","dbp_endpoint","glucose_ac_endpoint","tg_endpoint","hdl_endpoint")) %>%
  dplyr::rename(MetaX_endpoint = MetaX)

#Diagnosis MHO
stat_table <- stat_table %>% lin_diagnosis_MHO_MUHO(c("gender","bmi_baseline","tg_baseline","hdl_baseline","sbp_baseline","dbp_baseline","glucose_ac_baseline")) %>% 
  dplyr::rename(MHO_baseline = MHO)
stat_table <- stat_table %>% lin_diagnosis_MHO_MUHO(c("gender","bmi_endpoint","tg_endpoint","hdl_endpoint","sbp_endpoint","dbp_endpoint","glucose_ac_endpoint")) %>% 
  dplyr::rename(MHO_endpoint = MHO)

# lin_mapping(stat_table, client_type, id, df01_profile, client_type, id) %>% View()


# [Move forward] Clean client_type of df01_profile -------------------------------------------------------

#[Source 1,2:] df01_profile, client_type_is.na, look-up from clinic note
#1.not clinic
a1 <- df01_profile %>% filter((org_name == "cofit") | (org_name == "bk2o_backup") | (org_name == "fitness_factory")) 
#2.clinic, !is.na:client_type #map_ref
a2 <- df01_profile %>% filter((!is.na(client_type)) & ((org_name != "cofit") & (org_name != "bk2o_backup") & (org_name != "fitness_factory")))
#3.clinic
a3 <- df01_profile %>% filter((org_name != "cofit") & (org_name != "bk2o_backup") & (org_name != "fitness_factory"))
#4. map 2 & 3 / adv.clinic_list
a3 <- lin_mapping(a3, client_type, id, a2, client_type, id)
a3 <- lin_mapping(a3, client_type, id, clinical_adv_list, client_type, id)
#5. rbind, order
a4 <- a1 %>% rbind(a3) 
a4 <- a4[with(a4, order(date_t0, id)),]
# a4 %>% nrow()
df01_profile <- a4
rm(list = c("a1","a2","a3","a4"))

#[Source 3:]df01_profile, client_type_is.na, look-up from blood_first_record
#1.not clinic
a1 <- df01_profile %>% filter((org_name == "cofit") | (org_name == "bk2o_backup") | (org_name == "fitness_factory")) 
df01_profile_tmp <- df01_profile[is.na(df01_profile[["client_type"]]) & ((df01_profile[["org_name"]] != "cofit") & (df01_profile[["org_name"]] != "bk2o_backup") & (df01_profile[["org_name"]] != "fitness_factory")), ]
#a2.clinic, blood_first_record #map_ref
a2 <- df05_biochem[which(df05_biochem[["id"]] %in% df01_profile_tmp[["id"]]),]
a2 <- a2[with(a2, order(id, date_blood)),]
a2 <- a2 %>% filter(DM != "Unclassified")
a2 <- a2 %>% distinct(id, .keep_all = TRUE)
##if DM > client_type == "1"
a2$client_type = NA
a2$client_type <- ifelse(a2$DM == "DM", "1", "2")
# table(a2$DM, a2$client_type)
#3.clinic
a3 <- df01_profile %>% filter((org_name != "cofit") & (org_name != "bk2o_backup") & (org_name != "fitness_factory"))
#4. map 2 & 3 / blood_first_record
a3 <- lin_mapping(a3, client_type, id, a2, client_type, id)
#5. rbind, order
a4 <- a1 %>% rbind(a3) 
a4 <- a4[with(a4, order(date_t0, id)),]
# a4 %>% nrow()
df01_profile <- a4
rm(list = c("a1","a2","a3","a4"))

#[Source 4:]**temp adjustment - to be refine in the future: client_type_is.na : 2 Obesity
df01_profile[is.na(df01_profile[["client_type"]]) & ((df01_profile[["org_name"]] != "cofit") & (df01_profile[["org_name"]] != "bk2o_backup") & (df01_profile[["org_name"]] != "fitness_factory")), "client_type"] <- 2

# df01_profile[(is.na(df01_profile[["client_type"]])) & ((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow")), "id"] %>% 
#   unique() %>% length()
# df01_profile[!(is.na(df01_profile[["client_type"]])) & ((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow")), "id"] %>% 
#   unique() %>% length()


# [Move forward] Map client_type from df01_profile to stat_table -------------------------------------------------------
a3 <- df01_profile %>% filter((org_name != "cofit") & (org_name != "bk2o_backup") & (org_name != "fitness_factory"))

stat_table <- stat_table %>% as.data.frame()
a3 <- a3 %>% as.data.frame()

stat_table <- lin_mapping(stat_table, client_type, id, a3, client_type, id)
rm(a3)
stat_table$client_type <- stat_table$client_type %>% as.numeric()
#rm Inf/-Inf as NA.
df <- stat_table
df <- df %>% 
  mutate_if(is.numeric, ~ifelse(is.infinite(.), NA, .))



#Create OB. dataset
stat_table_1st_ob <- df %>% filter((client_type == 2) & 
                                         (!is.na(weight_baseline)) & (!is.na(weight_endpoint)) & 
                                         (!is.na(glucose_ac_baseline)) & (!is.na(glucose_ac_endpoint)) &
                                         (!is.na(note_count)))


#Create DM. dataset
stat_table_1st_dm <- df %>% filter((client_type == 1) & 
                                         (!is.na(weight_baseline)) & (!is.na(weight_endpoint)) & 
                                         (!is.na(glucose_ac_baseline)) & (!is.na(glucose_ac_endpoint)) &
                                         (!is.na(note_count)))


rm(list = c("stat_tm", "clinic_blood_data_ori","clinic_inbody_data_ori","df01_profile_tmp","df06_Diet_day_tmp","df", "df07_Diet_meal_by_date", "df07_Diet_meal_by_meal"))
