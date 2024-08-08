# save.image("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/WSpace_preproc.RData")

count_rds <- as.integer(seq.int(1, 470, length.out = 6))
# each one costs
# 使用者    系統    流逝 
# 208.510   1.426 210.175 
saveRDS(myplots[seq(count_rds[1],count_rds[2])], file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_1.rds")
saveRDS(myplots[seq(count_rds[2],count_rds[3])], file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_2.rds")
saveRDS(myplots[seq(count_rds[3],count_rds[4])], file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_3.rds")
saveRDS(myplots[seq(count_rds[4],count_rds[5])], file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_4.rds")
saveRDS(myplots[seq(count_rds[5],count_rds[6])], file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_5.rds")

count_rds <- as.integer(seq.int(1, 470, length.out = 6))
for (i in seq_along(count_rds[-1])) {
  saveRDS(
    myplots[seq(count_rds[i], count_rds[i + 1])],
    file = paste0("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_", i, ".rds"))
}


# saveRDS(myplots, file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots.rds")
# 使用者   系統   流逝 
# 82.361  1.326 84.892 
saveRDS(myplots_girc, file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_girc.rds") %>% system.time()
# 使用者   系統   流逝 
# 7.071  0.099  7.463 
saveRDS(myplots_plot_testosterone, file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_plot_testosterone.rds") %>% system.time()
# 使用者   系統   流逝 
# 53.779  0.658 56.215 
saveRDS(myplots_flc, file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_flc.rds") %>% system.time()

# 30 sec / each
count_rds <- as.integer(seq.int(1, 470, length.out = 6))
myplots <- list()
myplots[seq(count_rds[1],count_rds[2])] <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_1.rds")
myplots[seq(count_rds[2],count_rds[3])] <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_2.rds")
myplots[seq(count_rds[3],count_rds[4])] <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_3.rds")
myplots[seq(count_rds[4],count_rds[5])] <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_4.rds")
myplots[seq(count_rds[5],count_rds[6])] <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_5.rds")



x1 <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_1.rds")
x2 <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_2.rds")
x3 <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_3.rds")
x4 <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_4.rds")
x5 <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_5.rds")

myplots <- list()
for (i in seq_along(count_rds[-1])) {
  paste0("x",i) <- readRDS(paste0("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_", i, ".rds"))
}


# myplots <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots.rds")
# myplots_girc <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_girc.rds")
# myplots_plot_testosterone <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_plot_testosterone.rds")
# myplots_flc <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/myplots_flc.rds"))






#身體維度資料 e.g., wc 改用 3D data ##**缺date 欄位



source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/lin_function.R")


# dashboard --------------------------------------------------------------


# Pool data from cliets w/ blood data
# - weight loss (∆kg, %) total, avg (**cal. the max, and ignore rebound)
# - fat loss (∆kg, %) total, avg (**cal. the max, and ignore rebound)
# - improvement from BMI-classification
# - 減重成效，各組百分比 (**single treatment)




#1. med_id_pool
med_id_pool <- dplyr::intersect(clinic_inbody_data_ori %>% select(id) %>% unique() %>% pull(),
                                clinic_blood_data_ori %>% select(id) %>% unique() %>% pull() )
#2. produce dashborad dataset
clinic_inbody_data_dashboard <- clinic_inbody_data_ori %>% filter(id %in% med_id_pool)
clinic_blood_data_dashboard <- clinic_blood_data_ori %>% filter(id %in% med_id_pool)
rm(med_id_pool)



for (i in unique(clinic_inbody_data_dashboard$id)) {
  #loop 1st
  if (i == head(unique(clinic_inbody_data_dashboard$id),1)) {
    clinic_inbody_data_dashboard <- clinic_inbody_data_dashboard[with(clinic_inbody_data_dashboard, order(id, date_inbody)),] #order by date
    dashboard_table <- clinic_inbody_data_dashboard[0,c(-1,-2)] #create null table(w/t ID)
  }
  #main
  #every obs. - 1st obs.
  temp <- clinic_inbody_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_inbody")) - clinic_inbody_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_inbody")) %>% head(1) %>% as.vector()
  #collect data
  dashboard_table <- rbind(dashboard_table, temp %>% apply(2, min) %>% as.vector())
  #loop last
  if (i == tail(unique(clinic_inbody_data_dashboard$id),1)) {
    colnames(dashboard_table) <- names(clinic_inbody_data_dashboard[0,c(-1,-2)])
    dashboard_table <- cbind(unique(clinic_inbody_data_dashboard$id), dashboard_table)
    colnames(dashboard_table)[1] <- "id"
    cat("[Comopleted!]")
  }
} 

for (i in unique(clinic_blood_data_dashboard$id)) {
  #loop 1st
  if (i == head(unique(clinic_blood_data_dashboard$id),1)) {
    clinic_blood_data_dashboard <- clinic_blood_data_dashboard[with(clinic_blood_data_dashboard, order(id, date_blood)),] #order by date
    dashboard_table_blood <- clinic_blood_data_dashboard[0,c(-1,-2)] #create null table(w/t ID)
  }
  #main
  #every obs. - 1st obs.
  temp <- clinic_blood_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_blood")) - clinic_blood_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_blood")) %>% head(1) %>% as.vector()
  
  #collect data: min
  dashboard_table_blood <- rbind(dashboard_table_blood, temp %>% apply(2, min) %>% as.vector())
  #loop last
  if (i == tail(unique(clinic_blood_data_dashboard$id),1)) {
    colnames(dashboard_table_blood) <- names(clinic_blood_data_dashboard[0,c(-1,-2)])
    dashboard_table_blood <- cbind(unique(clinic_blood_data_dashboard$id), dashboard_table_blood)
    colnames(dashboard_table_blood)[1] <- "id"
    cat("[Comopleted!]")
  }
} 

dashboard_table <- lin_mapping(dashboard_table, client_type, id, clinical_list, client_type, id)
dashboard_table_blood <- lin_mapping(dashboard_table_blood, client_type, id, clinical_list, client_type, id)

#exclude DM duration < 30 days
dashboard_table <- dashboard_table %>% filter(client_type != 1 | is.na(client_type))
dashboard_table <- dashboard_table %>% filter((bf < -2) & (weight < -1))

dashboard_table_blood <- dashboard_table_blood %>% filter(id %in% dashboard_table$id)





#Layout
  #total client
  dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  
  #total accomplishment
  #include all: short term: finish program, long term: weight maintenance, weight rebound
  ##inbody
  dashboard_table %>% select("weight") %>% sum(na.rm = TRUE) 
  dashboard_table %>% select("bf") %>% sum(na.rm = TRUE)
  ##身體維度
  dashboard_table %>% select("wc") %>% sum(na.rm = TRUE)
  ##血生化
  # dashboard_table_blood %>% select("hba1c") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("glucose_ac") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("insulin") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("homa_ir") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("tg") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("tc") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("hdl") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("ldl") %>% sum(na.rm = TRUE)
  
  #avg
  #include all: short term: finish program, long term: weight maintenance, weight rebound
  ##inbody
  dashboard_table %>% select("weight") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  dashboard_table %>% select("bf") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  ##身體維度
  dashboard_table %>% select("wc") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  ##血生化 不適用, 嘗試換算成∆%, baseline important
  # dashboard_table_blood %>% select("hba1c") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("glucose_ac") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("insulin") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("homa_ir") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("tg") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("tc") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("hdl") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("ldl") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()

 

  

# test --------------------------------------------------------------------

  stat_table_1st_ob_lm <- stat_table_1st_ob %>% select(vars_en[vars_en != "gp"])
  names(stat_table_1st_ob_lm) <- vars_ch[vars_ch != "gp"]
  
  # Diet x Eff.
    #∆體重%
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm %>% select("∆體重%") %>% cbind(stat_table_1st_ob_lm %>% select(vars_ch[vars_ch %>% grep("baseline$|endpoint$|[∆]|id|client|gender|gp", ., invert = TRUE)]))
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm_diet %>% select(c("∆體重%","年齡","飲食紀錄完成率_%","每篇上傳照片數","綠燈率","黃燈率","紅燈率","碳水化合物_E%","蛋白質_E%","脂肪_E%","攝取熱量","水果攝取量_日","蔬菜攝取量_日","全穀雜糧攝取量_日","蛋豆魚肉攝取量_日","乳品攝取量_日","油脂攝取量_日"))
    
    model <- lm(-`∆體重%` ~ ., data = stat_table_1st_ob_lm_diet)
    model %>% summary()
    k <- olsrr::ols_step_both_p(model)
    k$model
    k$model %>% summary()
    a <- k$model %>% model_equation(digits = 3, trim = TRUE)
    a
    
    #∆體脂重%
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm %>% select("∆體脂重%") %>% cbind(stat_table_1st_ob_lm %>% select(vars_ch[vars_ch %>% grep("baseline$|endpoint$|[∆]|id|client|gender|gp", ., invert = TRUE)]))
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm_diet %>% select(c("∆體脂重%","年齡","飲食紀錄完成率_%","每篇上傳照片數","綠燈率","黃燈率","紅燈率","碳水化合物_E%","蛋白質_E%","脂肪_E%","攝取熱量","水果攝取量_日","蔬菜攝取量_日","全穀雜糧攝取量_日","蛋豆魚肉攝取量_日","乳品攝取量_日","油脂攝取量_日"))
    
    model <- lm(-`∆體脂重%` ~ ., data = stat_table_1st_ob_lm_diet)
    model %>% summary()
    k <- olsrr::ols_step_both_p(model)
    k$model
    k$model %>% summary()
    b <- k$model %>% model_equation(digits = 3, trim = TRUE)
    b

      
  

  
  #All
  # stepwise regression
  stat_table_1st_ob_lm_diet <-  stat_table_1st_ob_lm %>% select(
    (stat_table_1st_ob_lm %>% names() %>% grep("date|endpoint|id|type|∆", ., value = TRUE, invert = TRUE) %>% append(stat_table_1st_ob_lm %>% names() %>% grep("%", ., , value = TRUE)))
  )
  # stat_table_1st_ob_lm_diet <-  stat_table_1st_ob_lm %>% select(
  #    "∆體重%" %>% append(stat_table_1st_ob_lm %>% names() %>% grep("date|endpoint|id|type|∆", ., value = TRUE, invert = TRUE))
  # )
  
  model <- lm(-`∆體重%` ~ ., data = stat_table_1st_ob_lm_diet)
  model %>% summary()
  k <- olsrr::ols_step_both_p(model)
  k$model
  k$model %>% summary()
  c <- k$model %>% model_equation(digits = 3, trim = TRUE)
  c
  #plot(k)
  # final model
  k$model
  
  
  
  
  #[Prediction accuracy]
  ##1. create predict_table
  predict_table <- stat_table_1st_ob_lm_diet %>% select(k$predictors %>% gsub("`", "", .) %>% append("∆體重%"))
  ##2. create actual/forcast
  predict_table$actual <- -predict_table$`∆體重%`
  predict_table$forcast <- stats::predict(k$model, predict_table)
  #rm NA row
  predict_table <- predict_table %>% lin_exclude_NA_col(variables = names(.))
  ##3. calculate MAPE
  cat(paste("\n\n", "平均絕對百分比誤差(MAPE):", (MLmetrics::MAPE(predict_table$forcast, predict_table$actual) *100) %>% round(2), "%", "\n\n"))
  

  predict_table$residue <- abs((predict_table$actual - predict_table$forcast)/predict_table$actual *100) %>% round(2)
  predict_table$true <- predict_table$residue %>% cut(c(0, 10 , Inf), c(1, 0))
  predict_table$true %>% table() %>% prop.table() %>% multiply_by(100) %>% round(2)
  
  
  MLmetrics::Accuracy(predict_table$forcast, predict_table$true)
  MLmetrics::Precision(predict_table$forcast, predict_table$actual)
  
  
  
  
  
  
  
  #install.packages("Metrics")
  library(Metrics)

  
  set.seed(777)
  split <- caTools::sample.split(stat_table_1st_ob_lm_diet, SplitRatio = 0.8)
  
  train <- subset(stat_table_1st_ob_lm_diet, split == TRUE)
  test <- subset(stat_table_1st_ob_lm_diet, split == FALSE)
  
  
  
  


  
  

# verify the "ns" reason of testosterone gps 20230302 ---------------------

  # set.seed(333)
  # #sample size set to 100
  # a <- a %>% rbind(rbind(a[sample(which(a$gp_testosterone == "Low_testosterone"), (30 - sum(a$gp_testosterone == "Low_testosterone")), replace = TRUE),], 
  #                        a[sample(which(a$gp_testosterone != "Low_testosterone"), (30 - sum(a$gp_testosterone != "Low_testosterone")), replace = TRUE),]) )
  # table(a$gp_testosterone) %>% addmargins()
  #DONE
  
  
  

# tmp ---------------------------------------------------------------------
  # 使用者   系統   流逝 
  # 0.070  0.025  0.866 
  ptm <- proc.time()
  tmp_01 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "01_profile.sql")))
  cat("[執行時間:01_profile]\n")
  print(proc.time() - ptm)
  
  # [執行時間:02_inbody]
  # 使用者   系統   流逝 
  # 0.880  0.284 12.149 
  ptm <- proc.time()
  tmp_02 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "02_inbody.sql")))
  cat("[執行時間:02_inbody]\n")
  print(proc.time() - ptm)
  
  # [執行時間:03_FLC_self_report]
  # 使用者     系統     流逝 
  # 3.840    2.035 2762.575 
  ptm <- proc.time()
  tmp_03 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "03_FLC_self_report.sql")))
  cat("[執行時間:03_FLC_self_report]\n")
  print(proc.time() - ptm)
  
  # [執行時間:04_non_FLC_self_report]
  # 使用者   系統   流逝 
  # 0.058  0.028  6.554 
  ptm <- proc.time()
  tmp_04 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "04_non_FLC_self_report.sql")))
  cat("[執行時間:04_non_FLC_self_report]\n")
  print(proc.time() - ptm)
  
  
  # [執行時間:05_biochem]
  # 使用者   系統   流逝 
  # 0.188  0.064  5.155 
  ptm <- proc.time()
  tmp_05 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "05_biochem.sql")))
  cat("[執行時間:05_biochem]\n")
  print(proc.time() - ptm)
  
  # [執行時間:06_Diet_day]
  # 使用者   系統   流逝 
  # 16.119   2.702 238.284 
  ptm <- proc.time()
  tmp_06 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "06_Diet_day.sql")))
  cat("[執行時間:06_Diet_day]\n")
  print(proc.time() - ptm)
  
  
  # [執行時間:07_Diet_meal]
  # 使用者    系統    流逝 
  # 18.530   5.139 226.633 
  ptm <- proc.time()
  tmp_07 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "07_Diet_meal.sql")))
  cat("[執行時間:07_Diet_meal]\n")
  print(proc.time() - ptm)
  
  # [執行時間:08_3D_scanner]
  # 使用者   系統   流逝 
  # 1.119  0.181  8.010 
  ptm <- proc.time()
  tmp_08 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "08_3D_scanner.sql")))
  cat("[執行時間:08_3D_scanner]\n")
  print(proc.time() - ptm)
  
  # [執行時間:09_hormone]
  # 使用者   系統   流逝 
  # 0.039  0.013 10.025 
  ptm <- proc.time()
  tmp_09 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "09_hormone.sql")))
  cat("[執行時間:09_hormone]\n")
  print(proc.time() - ptm)
  
  
  
  

# [Data Preprocessing] 01. profile ---------------------------------------------------
  #[Note:] 20230309_finish_genesis_ONLY
  
  #input clinic_list
  source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/00-read_clinic_list.R")
  
  
  #Q1-1. 初日開幕前的cofit初日班 => topshow
  tmp_01[dplyr::intersect(grep("初日班", tmp_01[["program_name"]]), which(tmp_01[["date_t0"]] < "2021-08-30")), "org_name"] <- "topshow"
  tmp_01[dplyr::intersect(grep("秀0|秀1", tmp_01[["name"]]), which(tmp_01[["date_t0"]] < "2021-08-30")), "org_name"] <- "topshow"
  #Q1-2. "program_name"初日開幕後的cofit初日班 => genesisclinic
  tmp_01[Reduce(dplyr::intersect, list(grep("初日", tmp_01[["program_name"]]),
                                       which(tmp_01[["date_t0"]] >= "2021-08-30"),
                                       grep("cofit", tmp_01[["org_name"]])))
         , "org_name"] <- "genesisclinic"
  #Q1-3. "name"名字有初日、初日開幕後的cofit => genesisclinic
  tmp_01[Reduce(dplyr::intersect, list(grep("初日|初日001|G001", tmp_01[["name"]]),
                                       which(tmp_01[["date_t0"]] >= "2021-08-30"),
                                       grep("cofit", tmp_01[["org_name"]])))
         , "org_name"] <- "genesisclinic"
    
  #Q1-4. FLC班 => cofit
  tmp_01[grep("FLC", tmp_01[["program_name"]]), "org_name"] <- "cofit"
  
  #C1-1. class_freq by org_name
  tmp_01 <- tmp_01 %>% full_join(tmp_01 %>% group_by(id, org_name) %>% summarise(class_freq = n()), by = c("id", "org_name"))
  tmp_01 <- tmp_01[with(tmp_01, order(c(date_t0, id))),] %>% janitor::remove_empty("rows")
  #C1-2. class_order
  for (i in unique(tmp_01$id)) {
    if (i == head(unique(tmp_01$id), 1)) {
      j = 1
      tmp_01$class_order <- NA
    }
    tmp_01[which(tmp_01[["id"]] == i), "class_order"] <- which(tmp_01[["id"]] == i) %>% order()
    progress(j, unique(tmp_01$id) %>% length())
    j = j + 1
    if (i == tail(unique(tmp_01$id), 1)){
      print("[Completed!]")
    }
  }
  
  
  #C2. age: btd - date_t0 年齡(療程起始當天計算)
  tmp_01$age <- (lubridate::ymd(tmp_01$date_t0) - lubridate::ymd(tmp_01$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()
  
  #C3-1.非進階
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$client_type <- NA #client_type 
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, client_type, id, clinical_list, client_type, id)
  
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$program_set <- NA #program_set
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, program_set, id, clinical_list, program, id)
  
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$doctor <- NA #doctor
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, doctor, id, clinical_list, doctor, id)
  
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$nutritionist_major <- NA #nutritionist_major
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, nutritionist_major, id, clinical_list, nutritionist_major, id)
  
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$nutritionist_online <- NA #nutritionist_online
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, nutritionist_online, id, clinical_list, nutritionist_online, id)
  
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$medication <- NA #medication
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, medication, id, clinical_list, medication_note, id)
  
  #C3-2.進階
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),]
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, client_type, id, clinical_adv_list, client_type, id)
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, program_set, id, clinical_adv_list, program, id)
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, doctor, id, clinical_adv_list, doctor, id)
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, nutritionist_major, id, clinical_adv_list, nutritionist_major, id)
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, nutritionist_online, id, clinical_adv_list, nutritionist_online, id)
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, medication, id, clinical_adv_list, medication_note, id)
  
  
  
  #clean by select
  tmp_01 <- tmp_01 %>% select(c("id", "name", "gender", "age", "client_type", "program_name","date_t0","date_t1", "org_name", "class_freq", "class_order","program_set","doctor","nutritionist_major","nutritionist_online","medication", "btd"))
  

  
  
  
  # [Data Preprocessing] 02. profile ---------------------------------------------------
  
  #C1. format
  tmp_02[c("height","weight","bmi","body_fat_mass","body_fat_mass_percentage","weight_without_fat","muscle_mass","real_muscle_mass","vfa","vfa_level","waist_circumference","acl","cacl","total_body_water","protein_weight","mineral_weight","body_cell_mass","body_mineral","bfmi","bsmi","ffmi","systolic_blood_pressure","diastolic_blood_pressure","pulse","bmr","wepa50","algle_50_left_arm","algle_50_left_leg","algle_50_right_arm","algle_50_right_leg","algle_50_trunk","extracellular_water_ratio","extracellular_water_ratio_left_arm","extracellular_water_ratio_left_leg","extracellular_water_ratio_right_arm","extracellular_water_ratio_right_leg","extracellular_water_ratio_trunk","intracellular_weight","intracellular_weight_left_arm","intracellular_weight_left_leg","intracellular_weight_right_arm","intracellular_weight_right_leg","intracellular_weight_trunk","extracellular_weight","extracellular_weight_left_arm","extracellular_weight_left_leg","extracellular_weight_right_arm","extracellular_weight_right_leg","extracellular_weight_trunk","left_arm_fat","left_arm_fat_percentage","left_arm_muscle","left_arm_muscle_percentage","left_leg_fat","left_leg_fat_percentage","left_leg_muscle","left_leg_muscle_percentage","right_arm_fat","right_arm_fat_percentage","right_arm_muscle","right_arm_muscle_percentage","right_leg_fat","right_leg_fat_percentage","right_leg_muscle_percentage","right_leg_muscle","trunk_fat","trunk_fat_percentage","trunk_muscle","trunk_muscle_percentage","water_weight_left_arm","water_weight_left_leg","water_weight_right_arm","water_weight_right_leg","water_weight_trunk","waist_hip_ratio","tbwffm","obesity_degree","inbody_total_score")] %<>% 
    lapply(as.numeric)
  tmp_02 <- tmp_02 %>% as.tibble() 
  #C2. Sarcopenia Obesity(SO): "left_arm_muscle", "left_leg_muscle", "right_arm_muscle", "right_leg_muscle" #Female: < 23.4; Male: < 29.6. 
  tmp_02 <- tmp_02 %>% mutate(so_score = round((left_arm_muscle+left_leg_muscle+right_arm_muscle+right_leg_muscle)*100/weight,2))
  #C3. pbm
  tmp_02 <- tmp_02 %>% mutate(pbm = round((muscle_mass)*100/weight,2))
  #C4. name_format
  names(tmp_02) <- names(tmp_02) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
  #C5. rm outlier
  tmp_02 <- tmp_02[-which(tmp_02$bmi >100),]
  
  
  # 02.3 - [Data Preprocessing] 03_FLC_self_report --------------------------------------------------
  
  #C1. col_names
  names(tmp_03) <- names(tmp_03) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
  tmp_03 <- tmp_03[with(tmp_03, order(date_flc_T0)),]
  
  #C2. age: btd - date_t0 年齡(療程起始當天計算)
  tmp_03$age <- (lubridate::ymd(tmp_03$date_flc_T0) - lubridate::ymd(tmp_03$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()
  #C3. (1.) (%) *100  (2.) numeric %>% round(2)
  tmp_03[,grep("%", names(tmp_03))] %<>% multiply_by(100)
  tmp_03[c("weight(T0)","weight(T1)","∆weight","∆weight(%)","BMI(T0)","BMI(T1)","∆BMI","∆BMI(%)","Fat(T0)","Fat(T1)","∆Fat","∆Fat(%)","wc(T0)","wc(T1)","∆wc","∆wc(%)")] %<>% round(2)
  
  
  #C4-1. class_freq by org_name
  tmp_03 <- tmp_03 %>% full_join(tmp_03 %>% group_by(id) %>% summarise(class_freq = n()), by = c("id"))
  #C4-2. class_order
  for (i in unique(tmp_03$id)) {
    if (i == head(unique(tmp_03$id), 1)) {
      j = 1
      tmp_03$class_order <- NA
    }
    tmp_03[which(tmp_03[["id"]] == i), "class_order"] <- which(tmp_03[["id"]] == i) %>% order()
    progress(j, unique(tmp_03$id) %>% length())
    j = j + 1
    if (i == tail(unique(tmp_03$id), 1)){
      print("[Completed!]")
    }
  }
  
  
  # 02.4 - [Data Preprocessing] 04_non_FLC_self_report --------------------------------------------------
  
  tmp_99 <- tmp_04
  tmp_99 %>% glimpse()
  
  
  
  
  
  
  
  #cofit: 
    ##重複資料太多，不能用
  #clinic_df
    ##Done
  #diet calorie < 500/NA, summary, merge
    ##Done
  #progress, DM function
    ##Done
  #client type
    ##Done
  
  #df01_profile, client_type_is.na, look-up from clinic note
    #1.not clinic
    a1 <- df01_profile %>% filter(!((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow"))) 
    #2.clinic, !is.na:client_type #map_ref
    a2 <- df01_profile %>% filter((!is.na(client_type)) & ((org_name == "genesisclinic") | (org_name == "topshow")))
    #3.clinic
    a3 <- df01_profile %>% filter((org_name == "genesisclinic") | (org_name == "topshow"))
    #4. map 2 & 3 / adv.clinic_list
    a3 <- lin_mapping(a3, client_type, id, a2, client_type, id)
    a3 <- lin_mapping(a3, client_type, id, clinical_adv_list, client_type, id)
    #5. rbind, order
    a4 <- a1 %>% rbind(a3) 
    a4 <- a4[with(a4, order(date_t0, id)),]
    # a4 %>% nrow()
    df01_profile <- a4
    rm(list = c("a1","a2","a3","a4"))
  
  #df01_profile, client_type_is.na, look-up from blood_first_record
    #1.not clinic
    a1 <- df01_profile %>% filter(!((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow"))) 
    df01_profile_tmp <- df01_profile[is.na(df01_profile[["client_type"]]) & ((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow")), ]
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
    a3 <- df01_profile %>% filter((org_name == "genesisclinic") | (org_name == "topshow"))
    #4. map 2 & 3 / blood_first_record
    a3 <- lin_mapping(a3, client_type, id, a2, client_type, id)
    #5. rbind, order
    a4 <- a1 %>% rbind(a3) 
    a4 <- a4[with(a4, order(date_t0, id)),]
    # a4 %>% nrow()
    df01_profile <- a4
    rm(list = c("a1","a2","a3","a4"))
    
    #**temp adjustment - to be refine in the future: client_type_is.na : 2 Obesity
    df01_profile[(is.na(df01_profile[["client_type"]])) & ((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow")), "client_type"] <- 2
    
    # df01_profile[(is.na(df01_profile[["client_type"]])) & ((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow")), "id"] %>% 
    #   unique() %>% length()
    # df01_profile[!(is.na(df01_profile[["client_type"]])) & ((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow")), "id"] %>% 
    #   unique() %>% length()
  
    
  
  
    df05_biochem[(df05_biochem$id == 463448) , ] %>% select(date_blood) %>% pull
    df05_biochem[(df05_biochem$id == 463448) , "date_blood"] %>% pull
    clinic_blood_data[(clinic_blood_data$id == 463448) , "date_blood"] %>% pull
  
    
    df02_inbody[(df02_inbody$id == 463448) , ] %>% select(date_inbody) %>% pull
    clinic_inbody_data[(clinic_inbody_data$id == 463448) , "date_inbody"] %>% pull
    
    
    
    
    
    
    
    df03_FLC_self_report %>% 
      group_by(gender) %>% 
      summarise(
        weight = mean(`∆weight(%)`, na.rm = TRUE),
        fat = mean(`∆Fat(%)` , na.rm = TRUE)
      )
    df03_FLC_self_report$`∆weight(%)` %>% summary()
    df03_FLC_self_report$`∆Fat(%)` %>% summary()
    
    
    
    
    df04_non_FLC_self_report %>%  
      group_by(gender_baseline) %>% 
      summarise(
        weight = mean(`∆weight%`, na.rm = TRUE),
        fat = mean(`∆fat%` , na.rm = TRUE)
      )
    df04_non_FLC_self_report$`∆weight%` %>% summary()
    df04_non_FLC_self_report$`∆fat%` %>% summary()
  
    
    
    
    
    
    
    
    
# FLC & non-FLC -----------------------------------------------------------
    
    
    (Sys.Date() - months(6)) %>% lubridate::floor_date(unit = "month")
    df03_FLC_self_report %>% filter(date_flc_T1 <= "2022-09-01") 
    #csv
    
    

# Lee query ---------------------------------------------------------------


    #Effectiveness: age_gp < 25, clinic, FLC, non-FLC: plot: barplot, x:C/F/NF_by_gender; y = vars; facet: age_gp
    
    #01.clinic (C)
    stat_table_1st_ob$age_gp <- cut(stat_table_1st_ob$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
    stat_table_1st_ob$org_name_gp <- "Med"
    #02.FLC (F)
    df03_FLC_self_report$age_gp <- cut(df03_FLC_self_report$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
    df03_FLC_self_report$org_name_gp <- "Diet"
    
    # 03.non-FLC (NF) 
    df04_non_FLC_self_report$age_gp <- cut(df04_non_FLC_self_report$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
    df04_non_FLC_self_report$org_name_gp <- "Control"
    
    #merge Q_dr.lee_01_datasets  
    Q_dr.lee_01_datasets <- Reduce(rbind, list(stat_table_1st_ob %>% select(`∆weight`,`∆weight%`, age_gp, gender, org_name_gp), 
                                               df03_FLC_self_report %>% select(`∆weight`,`∆weight%`, age_gp, gender, org_name_gp) %>% lin_exclude_NA_col("∆weight%"),
                                               df04_non_FLC_self_report %>% select(`∆weight`,`∆weight%`, age_gp, gender, org_name_gp) %>% lin_exclude_NA_col("∆weight%")))
    Q_dr.lee_01_datasets$org_name_gp <-  Q_dr.lee_01_datasets$org_name_gp %>% factor(levels = c("Med", "Diet", "Control"))

      Q_dr.lee_01_datasets %>% 
        filter(age_gp == levels(Q_dr.lee_01_datasets$age_gp)[1]) %>% 
        select(org_name_gp, gender, `∆weight%`, age_gp) %>% rename(value = "∆weight%") %>% 
        mutate(value_adj = value %>% multiply_by(-1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "org_name_gp", alpha = 0.5, width = 0.5,
                  add = "mean", add.params = list(group = "org_name_gp"),
                  label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, 
                  position = position_dodge(0.5), 
                  xlab = "", ylab = "∆Weight Loss(%)", title = paste0("減重成效", "(Age:", levels(Q_dr.lee_01_datasets$age_gp)[1], ")"),
                  legend = "right", legend.title = "Program", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))
        
      Q_dr.lee_01_datasets %>% 
        filter(age_gp == levels(Q_dr.lee_01_datasets$age_gp)[1]) %>% 
        select(org_name_gp, gender, `∆weight`, age_gp) %>% rename(value = "∆weight") %>% 
        mutate(value_adj = value %>% multiply_by(-1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "org_name_gp", alpha = 0.5, width = 0.5,
                  add = "mean", add.params = list(group = "org_name_gp"),
                  label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, 
                  position = position_dodge(0.5), 
                  xlab = "", ylab = "∆Weight Loss(Kg)", title = paste0("減重成效", "(Age:", levels(Q_dr.lee_01_datasets$age_gp)[1], ")"),
                  legend = "right", legend.title = "Program", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))
    
    
      Q_dr.lee_01_datasets$org_name_gp %>% table() %>% addmargins()
      
      
    
    

# uric acid ---------------------------------------------------------------

    
      stat_table_1st_ob <- stat_table_1st_ob %>% mutate(delta_sua_gp = paste(sua_gp_baseline, sua_gp_endpoint, sep = ">")) 
      
      table(stat_table_1st_ob$delta_sua_gp)
      
      
      stat.test <- 
      stat_table_1st_ob %>% 
        filter(!is.na(sua_gp_baseline)) %>% 
        select(delta_sua_gp, gender, `∆weight%`) %>% rename(value = "∆weight%") %>% 
        mutate(value_adj = value %>% multiply_by(-1)) %>% 
        group_by(gender) %>% 
        rstatix::t_test(value_adj ~ delta_sua_gp) %>%
        rstatix::add_xy_position(x = "gender", fun = "mean_sd", dodge = 0.5)
      
      plot_SUA_03 <- 
      stat_table_1st_ob %>% 
        filter(!is.na(sua_gp_baseline)) %>% 
        select(delta_sua_gp, gender, `∆weight%`) %>% rename(value = "∆weight%") %>% 
        mutate(value_adj = value %>% multiply_by(-1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "delta_sua_gp", alpha = 0.5, width = 0.5,
                  add = "mean_se", add.params = list(group = "delta_sua_gp"),
                  label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, 
                  position = position_dodge(0.5), 
                  xlab = "", ylab = "∆Weight Loss(%)", title = paste0("減重成效", " x 尿酸", (" (cutoff = 5.5)")),
                  legend = "right", legend.title = "SUA Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        stat_pvalue_manual(
          stat.test, label = "p.adj.signif", tip.length = 0.01,
          bracket.nudge.y = -2, hide.ns = TRUE
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      
      
      
      table(stat_table_1st_ob$sua_gp_baseline, stat_table_1st_ob$sua_gp_endpoint)
      
      
      plot_SUA_01 <- 
      stat_table_1st_ob %>% 
        ggscatter(x = "uric_acid_baseline", y = "weight_baseline",
                  color = "black",
                  fill = "red",
                  shape = 21,
                  size = 1,
                  add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  title = "Correlation(Weight x SUA):Baseline",
                  xlab = "uric_acid(mg/dL)",
                  ylab = "Weight(kg)",
                  # xlim = c(0, 13),
                  # ylim = c(0, 180),
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        geom_vline(xintercept = c(5.5),linetype ="dashed", ) +
        annotate("text", x=5.3, y=155, label="Cutoff = 5.5 mg/dL", angle=90) +
        stat_cor(method = "pearson", size = 5, label.x = 10, label.y = 45) # Add correlation coefficient)
      
      plot_SUA_02 <- 
      stat_table_1st_ob %>% 
        ggscatter(x = "uric_acid_endpoint", y = "weight_endpoint",
                  color = "black",
                  fill = "red",
                  shape = 21,
                  size = 1,
                  add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  title = "Correlation(Weight x SUA):Endpoint",
                  xlab = "uric_acid(mg/dL)",
                  ylab = "Weight(kg)",
                  # xlim = c(0, 13),
                  # ylim = c(0, 180),
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        geom_vline(xintercept = c(5.5),linetype ="dashed", ) +
        annotate("text", x=5.3, y=150, label="Cutoff = 5.5 mg/dL", angle=90) +
        stat_cor(method = "pearson", size = 5, label.x = 7, label.y = 45) # Add correlation coefficient)
    
      
      
      ggarrange(
        ggarrange(plot_SUA_01, plot_SUA_02, ncol = 2, labels = c("A", "B")),
        ggarrange(plot_SUA_03, labels = "C"),
        nrow = 2
      )
        
      
      #corr
      
      profile_efficacy <- stat_table_1st_ob %>% 
        select(c("∆weight%","∆bf%","∆bm%","∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆homa_beta","∆tg","∆tc","∆hdl","∆ldl","∆lipase","∆uric_acid"))
      
      names(profile_efficacy) <- names(profile_efficacy) %>% lin_ch_en_format(format = "ch", origin = "en")
      
      profile_baseline <- stat_table_1st_ob %>% 
        select(c("age", "bmi_baseline","pbf_baseline","pbm_baseline",
                 "hba1c_baseline","glucose_ac_baseline","insulin_baseline","homa_ir_baseline","homa_beta_baseline", "tAUCg_baseline", "tAUCi_baseline", "OGIRIndex_baseline",
                 "tg_baseline","tc_baseline","hdl_baseline","ldl_baseline","lipase_baseline",
                 "uric_acid_baseline"))
      
      names(profile_baseline) <- names(profile_baseline) %>% lin_ch_en_format(format = "ch", origin = "en")
      
      profile_diet <- stat_table_1st_ob %>% 
        select(c("upload_day_%", "pic_counts","calorie_day","carb_E%","protein_E%","fat_E%","fruits","vegetables","grains","meat_bean","milk", "oil","light_G_%","light_Y_%","light_R_%"))
      
      names(profile_diet) <- names(profile_diet) %>% lin_ch_en_format(format = "ch", origin = "en")
      
      
      ##[Method 2] corrplot
      
      library(corrplot)
      #[Correlation r] Efficacy x Diet
      M1_sua <- cor(cbind(-profile_efficacy, profile_diet), use = "pairwise.complete.obs")
      #[2Do]change row,col names into chinese
      M_test1_sua <- cor.mtest(cbind(-profile_efficacy, profile_diet) , conf.level = .95)
      M_col_sua <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))
      
      
      # run corrplot plot
      corrplot(M1_sua,
               p.mat = M_test1_sua$p,
               type = "lower",
               insig = "label_sig",
               sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
               tl.col = "black", tl.srt = 35, tl.cex = 1.0,
               cl.ratio = 0.1,
               col = M_col_sua(200),
               title = "[Correlation] Uric acid x Diet",
               #c(bottom, left, top, right)
               mar = c(0,0,1,0))

      for (i in c(1:length(colnames(M1_sua)))) {
        if (i == 1) {
          M1_value <- M1_sua %>% round(2) 
          M1_sign <- M_test1_sua$p %>% stats::symnum(corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")) %>% as.data.frame.matrix()
          M1_df <- M1_value
          M1_df[,] <- NA
        }
        
        M1_df[,i] <- paste0(M1_value[,i], " (", M1_sign[,i], ")")
        
        if (i ==  length(colnames(M1_sua))) {
          rm(list = c("M1_value", "M1_sign"))
          M1_df <- M1_df %>% as.data.frame()
          M1_df <- M1_df %>% add_column(vars = rownames(M1_df), .before = names(M1_df)[1])
          M1_df <- M1_df %>% add_column("#" = seq(1, nrow(M1_df)), .before = names(M1_df)[1])
        }
        
      } 
      
      cor_table_01_sua <- M1_df %>% gvisTable(options=list(frozenColumns = 2,
                                                       height=300))
      
      
      M2_sua <- cor(cbind(-profile_efficacy, profile_baseline), use = "pairwise.complete.obs")
      #[2Do]change row,col names into chinese
      M_test2_sua <- cor.mtest(cbind(-profile_efficacy, profile_baseline) , conf.level = .95)
      M_col_sua <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))
      
      
      #run corrplot plot
      corrplot(M2_sua,
               p.mat = M_test2_sua$p,
               type = "lower",
               insig = "label_sig",
               sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
               tl.col = "black", tl.srt = 35, tl.cex = 1.0,
               cl.ratio = 0.1,
               col = M_col_sua(200),
               title = "[Correlation] Efficacy x Baseline",
               #c(bottom, left, top, right)
               mar = c(0,0,1,0))
      
      
      # stat_table_1st_ob$`protein_E%`
      stat_table_1st_ob %>% rename(delta_uric_acid = `∆uric_acid`) %>% rename(fat = `fat_E%`) %>% 
        ggscatter(x = "delta_uric_acid", y = "fat",
                  color = "black",
                  fill = "red",
                  shape = 21,
                  size = 1,
                  add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  title = "Correlation(Weight x SUA):Diet",
                  xlab = "∆uric_acid(mg/dL)",
                  ylab = "Intake:Protein(E%)",
                  # xlim = c(0, 13),
                  # ylim = c(0, 180),
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        stat_cor(method = "pearson", size = 5, label.x = 0, label.y = 45) # Add correlation coefficient)
      
      
      
      #
      stat_table_1st_ob$meat_bean
      
      stat.test <- 
        stat_table_1st_ob %>% 
        filter(!is.na(sua_gp_baseline)) %>% 
        select(delta_sua_gp, gender, `fat_E%`) %>% rename(value = "fat_E%") %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        group_by(gender) %>% 
        rstatix::t_test(value_adj ~ delta_sua_gp) %>%
        rstatix::add_xy_position(x = "gender", fun = "mean_sd", dodge = 0.5)
      
      stat_table_1st_ob %>% 
        filter(!is.na(sua_gp_baseline)) %>% 
        select(delta_sua_gp, gender, `fat_E%`) %>% rename(value = "fat_E%") %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "delta_sua_gp", alpha = 0.5, width = 0.5,
                  add = "mean_se", add.params = list(group = "delta_sua_gp"),
                  label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, 
                  position = position_dodge(0.5), 
                  xlab = "", ylab = "Intake: fat(E%)", title = paste0("Diet", " x 尿酸"),
                  legend = "right", legend.title = "SUA Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        stat_pvalue_manual(
          stat.test, label = "p.adj.signif", tip.length = 0.01,
          bracket.nudge.y = 1, step.increase = 0.05, hide.ns = TRUE
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      
      
      

# 星座 ----------------------------------------------------------------------
      # 生肖
      # seq(1899 + 1, 2100, by = 12)
      # as.Date(df[[variables]]) %>% format("%Y") %in% seq(1899 + 9, 2100, by = 12)
      
      
      a <- df01_profile %>% filter(org_name %in% c("topshow", "genesisclinic", "lumez")) %>% distinct(id, .keep_all = TRUE)  #Clinic
      b <- stat_table %>% rename(delta_weight_p = `∆weight%`)
      a <- lin_mapping(a, bmi_baseline, id, b, bmi_baseline, id)
      a <- lin_mapping(a, delta_weight_p, id, b, delta_weight_p, id)
      a <- lin_mapping(a, homa_ir_baseline, id, b, homa_ir_baseline, id)
      a <- lin_mapping(a, "upload_day_%", id, b, "upload_day_%", id)
      a <- lin_mapping(a, "light_G_%", id, b, "light_G_%", id)
      a <- a %>% mutate(diet_obediance = (`"upload_day_%"` * `"light_G_%"` /100) %>% round(2))
      a <- a %>% filter(!is.na(delta_weight_p))
      # a <- df01_profile %>% distinct(id, .keep_all = TRUE)  #All
      a <- lin_astrological_type(a, "btd")
      a1 <- table(a$astro) %>% addmargins() %>% as.data.frame()
      table(a$astro, a$gender) %>% addmargins()
      a1[with(a1, order(Freq, decreasing = TRUE)),]
      
      
      a %>% 
        filter(!is.na(astro)) %>% 
        select(astro, gender, `bmi_baseline`) %>% rename(value = "bmi_baseline") %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "astro", alpha = 0.5, width = 0.7,
                  add = "mean", add.params = list(group = "astro"),
                  label = TRUE, lab.nb.digits = 1, lab.pos = "out", lab.vjust = -2, lab.size = 2,
                  position = position_dodge(0.7), 
                  xlab = "", ylab = "weight(Kg)", title = paste0("BMI", "x 星座"),
                  legend = "right", legend.title = "Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      a %>% 
        filter(!is.na(astro)) %>% 
        select(astro, gender, `homa_ir_baseline`) %>% rename(value = "homa_ir_baseline") %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "astro", alpha = 0.5, width = 0.7,
                  add = "mean", add.params = list(group = "astro"),
                  label = TRUE, lab.nb.digits = 1, lab.pos = "out", lab.vjust = -2, lab.size = 2,
                  position = position_dodge(0.7), 
                  xlab = "", ylab = "HOMA-IR", title = paste0("胰島素阻抗", "x 星座"),
                  legend = "right", legend.title = "Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      a %>% 
        filter(!is.na(astro)) %>% 
        select(astro, gender, `delta_weight_p`) %>% rename(value = "delta_weight_p") %>% 
        mutate(value_adj = value %>% multiply_by(-1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "astro", alpha = 0.5, width = 0.7,
                  add = "mean", add.params = list(group = "astro"),
                  label = TRUE, lab.nb.digits = 1, lab.pos = "out", lab.vjust = -2, lab.size = 2,
                  position = position_dodge(0.7), 
                  xlab = "", ylab = "∆weight%(Kg)", title = paste0("減重成效", "x 星座"),
                  legend = "right", legend.title = "Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      a %>% 
        filter(!is.na(astro)) %>% 
        select(astro, gender, `"upload_day_%"`) %>% rename(value = `"upload_day_%"`) %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "astro", alpha = 0.5, width = 0.7,
                  add = "mean", add.params = list(group = "astro"),
                  label = TRUE, lab.nb.digits = 1, lab.pos = "out", lab.vjust = -2, lab.size = 2,
                  position = position_dodge(0.7), 
                  xlab = "", ylab = "飲食完成率(%)", title = paste0("飲食完成率", "x 星座"),
                  legend = "right", legend.title = "Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      a %>% 
        filter(!is.na(astro)) %>% 
        select(astro, gender, diet_obediance) %>% rename(value = diet_obediance) %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "astro", alpha = 0.5, width = 0.7,
                  add = "mean", add.params = list(group = "astro"),
                  label = TRUE, lab.nb.digits = 1, lab.pos = "out", lab.vjust = -2, lab.size = 2,
                  position = position_dodge(0.7), 
                  xlab = "", ylab = "飲食紀律分數", title = paste0("飲食紀律分數", "x 星座"),
                  legend = "right", legend.title = "Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      
      
      
      

# all vars analysis -------------------------------------------------------


      
      
      a <- stat_table_1st_ob %>% select_if(is.numeric) %>% 
        select(-c(id, class_freq, class_order)) %>% 
        select_if(~ mean(., na.rm = TRUE) %>% is.nan() %>% not() & is.na(mean(., na.rm = TRUE)) %>% not()) %>% 
        select_if(~ all(mean(.) != median(.)))

      
      library(corrplot)
      #[Correlation r] Efficacy x Diet
      M_all <- cor(a, use = "pairwise.complete.obs") %>% round(2)
      #[2Do]change row,col names into chinese
      M_test_all <- cor.mtest(a , conf.level = .95)
      M_col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))  
      
      
      for (i in c(1:length(colnames(M_all)))) {
        if (i == 1) {
          M_all_value <- M_all %>% round(2) 
          M_all_sign <- M_test_all$p %>% stats::symnum(corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")) %>% as.data.frame.matrix()
          M_all_df <- M_all_value
          M_all_df[,] <- NA
        }
        
        M_all_df[,i] <- paste0(M_all_value[,i], " (", M_all_sign[,i], ")")
        
        if (i ==  length(colnames(M_all))) {
          rm(list = c("M_all_value", "M_all_sign"))
          M_all_df <- M_all_df %>% as.data.frame()
          M_all_df <- M_all_df %>% add_column(vars = rownames(M_all_df), .before = names(M_all_df)[1])
          M_all_df <- M_all_df %>% add_column("#" = seq(1, nrow(M_all_df)), .before = names(M_all_df)[1])
        }
        
      } 
      
      

# uric acid ---------------------------------------------------------------


      # 尿酸：完成度*綠燈率 >80, 尿酸變化（no change, slightly increase, normal to high) 這三組減重成效是否有不同？
      # 額外篩選條件：similar  bmi pbf age
      # 尿酸變化是否影響減重效果（minor factor?)
      
      stat_table_1st_ob$`light_G_%`
      
      
      
      stat_table_1st_ob %>% 
        mutate(obediance_score = (`upload_day_%` * `light_G_%` /100 ) %>% round(2))%>% 
        filter(obediance_score >= 70) %>% 
        select(gender, delta_sua_gp) %>% table()
      
      a <- 
      stat_table_1st_ob %>% 
        mutate(obediance_score = (`upload_day_%` * `light_G_%` /100 ) %>% round(2))%>% 
        filter(obediance_score >= 80) %>% 
        filter(age >= mean(age , na.rm = T) -7 & age <= mean(age , na.rm = T) +7) %>% 
        filter(bmi_baseline >= mean(bmi_baseline , na.rm = T)  & bmi_baseline <= mean(bmi_baseline , na.rm = T) +10) %>% 
        filter(pbf_baseline >= mean(pbf_baseline , na.rm = T) *0.5 & pbf_baseline <= mean(pbf_baseline , na.rm = T) *1.5) %>% 
        filter(calorie_day >= mean(calorie_day , na.rm = T) *0.7 & calorie_day <= mean(calorie_day , na.rm = T) *1.3)
      
      # filter(var >= mean(var , na.rm = T) *0.7 & var <= mean(var , na.rm = T) *1.3)
      
      a %>% 
        group_by(gender) %>% 
        summarise(
          age = paste0(age %>% mean(na.rm = T) %>% round(1), "(", age %>% max(na.rm = T) %>% round(1),"~", age %>% min(na.rm = T) %>% round(1),")"),
          bmi = paste0(bmi_baseline %>% mean(na.rm = T) %>% round(1), "(", bmi_baseline %>% max(na.rm = T) %>% round(1),"~", bmi_baseline %>% min(na.rm = T) %>% round(1),")"),
          pbf = paste0(pbf_baseline %>% mean(na.rm = T) %>% round(1), "(", pbf_baseline %>% max(na.rm = T) %>% round(1),"~", pbf_baseline %>% min(na.rm = T) %>% round(1),")"),
          calorie_day = paste0(calorie_day %>% mean(na.rm = T) %>% round(1), "(", calorie_day %>% max(na.rm = T) %>% round(1),"~", calorie_day %>% min(na.rm = T) %>% round(1),")"),
          n = n()
        ) %>% 
        view()
      
      table(a$delta_sua_gp, a$gender)
      
      
      
      datasets_target_issue <- a %>% dplyr::rename(gp = delta_sua_gp)
      datasets_target_issue <- datasets_target_issue %>% filter(gp %in% levels(datasets_target_issue$gp))
      
      #profile
      vars_en <- c("id","client_type","age","gender","date_t0","date_t1",
                   #inbody - baseline
                   "weight_baseline","bmi_baseline","bf_baseline","pbf_baseline","bsmi_baseline","pbm_baseline","vfa_baseline","wc_baseline","ffm_baseline","bmr_baseline",
                   #blood- baseline
                   "hba1c_baseline","glucose_ac_baseline","insulin_baseline","homa_ir_baseline","homa_beta_baseline","tg_baseline","tc_baseline","hdl_baseline","ldl_baseline", "uric_acid_baseline", "amylase_baseline","lipase_baseline",
                   #inbody - endpoint
                   "weight_endpoint","bmi_endpoint","bf_endpoint","pbf_endpoint","bsmi_endpoint","pbm_endpoint","vfa_endpoint","wc_endpoint","ffm_endpoint","bmr_endpoint",
                   #blood- endpoint
                   "hba1c_endpoint","glucose_ac_endpoint","insulin_endpoint","homa_ir_endpoint","homa_beta_endpoint","tg_endpoint","tc_endpoint","hdl_endpoint","ldl_endpoint", "uric_acid_endpoint", "amylase_endpoint","lipase_endpoint",
                   #diet
                   "upload_day_%","note_count","pic_counts","carb_E%","protein_E%","fat_E%","calorie_day","light_G_%","light_Y_%","light_R_%","fruits","vegetables","grains","meat_bean","milk","oil",
                   #others
                   "gp",
                   #inbody - ∆
                   "∆weight","∆bmi","∆bf","∆pbf","∆bsmi","∆bm","∆vfa","∆wc","∆ffm","∆bmr",
                   #blood - ∆
                   "∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆homa_beta","∆tg","∆tc","∆hdl","∆ldl","∆uric_acid","∆amylase","∆lipase",
                   #inbody - ∆%
                   "∆weight%","∆bmi%","∆bf%","∆pbf%","∆bsmi%","∆bm%","∆vfa%","∆wc%","∆ffm%","∆bmr%",
                   #blood - ∆%
                   "∆hba1c%","∆glucose_ac%","∆insulin%","∆homa_ir%","∆homa_beta%","∆tg%","∆tc%","∆hdl%","∆ldl%","∆uric_acid%","∆amylase%","∆lipase%"
      )
      
      datasets_target_issue <- datasets_target_issue %>% select(vars_en)
      
      vars_en <- lin_ch_en_format(x = vars_en, format = "en", origin = "raw_en")
      names(datasets_target_issue) <- lin_ch_en_format(x = names(datasets_target_issue), format = "en", origin = "raw_en")
      
      
      #Setting improvement direction
      
      datasets_target_issue_a <- datasets_target_issue %>% select(-grep("∆", names(datasets_target_issue)))
      
      ##Improvement: Uncertain, default setting
      datasets_target_issue_b <- datasets_target_issue %>% select(grep("∆", names(datasets_target_issue), value = TRUE) %>% 
                                                                    grep(paste(c("bmr", "uric_acid", "amylase", "lipase"), collapse = "|"), ., value = TRUE))
      ##Improvement: negative (減少越多，越往上長)
      datasets_target_issue_c <- datasets_target_issue %>% select(grep("∆", names(datasets_target_issue), value = TRUE) %>% 
                                                                    grep(paste(c("weight", "bmi", "bf", "pbf", "vfa", "wc", "ffm", "hba1c", "glucose_ac", "insulin", "homa_ir", "tg", "tc", "ldl"), collapse = "|"), ., value = TRUE)) %>% multiply_by(-1)
      ##Improvement: positive
      datasets_target_issue_d <- datasets_target_issue %>% select(grep("∆", names(datasets_target_issue), value = TRUE) %>% 
                                                                    grep(paste(c("bsmi", "bm$", "bm%", "homa_beta", "hdl", "homa_beta"), collapse = "|"), ., value = TRUE)) %>% multiply_by(-1)
      
      datasets_target_issue <- Reduce(cbind,list(datasets_target_issue_a, datasets_target_issue_b, datasets_target_issue_c, datasets_target_issue_d), accumulate =FALSE) 
      
      #order again!!
      datasets_target_issue <- datasets_target_issue %>% select(vars_en)
      
      #change colname to run plot
      datasets_target_issue_for_plot <- datasets_target_issue
      
      names(datasets_target_issue_for_plot) <- gsub("∆", "delta_", names(datasets_target_issue_for_plot))
      names(datasets_target_issue_for_plot) <- gsub("%", "_percent", names(datasets_target_issue_for_plot))
      
      #set output plot order
      var_vector <- c(vars_en %>% grep("baseline$", .),
                      vars_en %>% grep("endpoint$", .),
                      vars_en %>% grep("baseline$|endpoint$|[∆]|id|client|gender|gp|date", ., invert = TRUE),
                      setdiff(vars_en %>% grep("[∆]", .), vars_en %>% grep("[%]", .)),
                      intersect(vars_en %>% grep("[∆]", .), vars_en %>% grep("[%]", .))
      )
      
      
      #Establish vars_table for visualization
      myplot_table <- data.frame(num = seq(1, length(vars_en)),
                                 vars_ch = lin_ch_en_format(x = vars_en, format = "ch", origin = "en"))
      myplot_table <- lin_mapping(myplot_table, vars_en, vars_ch, vars_table, en, ch)
      myplot_table <- lin_mapping(myplot_table, field, vars_ch, vars_table, field, ch)
      
      myplot_table <- myplot_table[var_vector,]
      myplot_table$num <- seq(1, length(myplot_table$num))
      
      i=90
      j=84
      
        a <- datasets_target_issue_for_plot %>% colnames() %>% head(i) %>% tail(1)
        a_title <- myplot_table[myplot_table$num == j, "vars_ch"]
        
        
        #p.sign?
        stat.test <- 
          datasets_target_issue_for_plot %>%
          group_by(gender) %>%
          rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ "))) 
        stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
        
        
          datasets_target_issue_for_plot %>% 
          ggbarplot(x = "gender", y = a, fill = "gp", alpha = 0.5,
                    add = "mean_se", add.params = list(group = "gp"),
                    position = position_dodge(0.8), legend = "right", legend.title = "") +
          scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
          labs(x = "", y = "Mean ± SE", title = a_title) +
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5, size = 15)
          ) +
          stat_pvalue_manual(
            stat.test, label = "p.adj.signif", tip.length = 0.0,
            bracket.nudge.y = 1, step.increase = 0.01, hide.ns = TRUE 
          )
      
      rm(list = c("datasets_target_issue_a", "datasets_target_issue_b", "datasets_target_issue_c", "datasets_target_issue_d"))
      
      
      
      
      
      

# 0415_conference_use -----------------------------------------------------


      
      #cor
      stat_table_1st_ob %>% 
        mutate(x_value = `testosterone_baseline` %>% multiply_by(1)) %>% 
        mutate(y_value = `∆bf%` %>% multiply_by(-1)) %>% 
        ggscatter(x = "x_value", y = "y_value",
                  color = "black",
                  fill = "red",
                  shape = 21,
                  size = 1, 
                  add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  title = "Correlation(Weight x SUA):Baseline",
                  xlab = "uric_acid(mg/dL)",
                  ylab = "Weight(kg)",
                  # xlim = c(0, 13),
                  # ylim = c(0, 180),
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        # geom_vline(xintercept = c(5.5),linetype ="dashed", ) +
        # annotate("text", x=5.3, y=155, label="Cutoff = 5.5 mg/dL", angle=90) +
        stat_cor(method = "pearson", size = 5, label.x = 0,
                 # label.y = 45
                 ) # Add correlation coefficient)
      
      # plot sample size
      table(QQ1_stat_table_1st$gender, SHBG = !is.na(QQ1_stat_table_1st$shbg_baseline), gp = QQ1_stat_table_1st$gp) %>% ftable()
      table_02
            
      
      #datasets range
      stat_table_1st_ob %>% 
        group_by(gender) %>% 
        summarise(
          age = paste0(age %>% mean(na.rm = T) %>% round(1), "(", age %>% max(na.rm = T) %>% round(1),"~", age %>% min(na.rm = T) %>% round(1),")"),
          bmi = paste0(bmi_baseline %>% mean(na.rm = T) %>% round(1), "(", bmi_baseline %>% max(na.rm = T) %>% round(1),"~", bmi_baseline %>% min(na.rm = T) %>% round(1),")"),
          pbf = paste0(pbf_baseline %>% mean(na.rm = T) %>% round(1), "(", pbf_baseline %>% max(na.rm = T) %>% round(1),"~", pbf_baseline %>% min(na.rm = T) %>% round(1),")"),
          calorie_day = paste0(calorie_day %>% mean(na.rm = T) %>% round(1), "(", calorie_day %>% max(na.rm = T) %>% round(1),"~", calorie_day %>% min(na.rm = T) %>% round(1),")"),
          n = n()
        ) %>% 
        view()
      
      
      #BMI_gp, gender, ∆weight_bp
      (table(stat_table_1st_ob$gender, stat_table_1st_ob$bmi_gp, stat_table_1st_ob$`∆weight_gp`) %>% addmargins() %>%  ftable()) %>% prop.table(margin = 1) %>% multiply_by(200) %>% round(2) %>%
        as.matrix() %>% 
        kable(format = "html", caption = "<b>Table: BMI x Weight Loss</b>", align = "c") %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                                  full_width = FALSE, font_size = 15) %>% 
        footnote(general_title = c(""), general = c(rbind("", c(""))),
                 footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
        )%>% 
        gsub("font-size: initial !important;", 
             "font-size: 15pt !important;", 
             .)
      
      
      

# 0512 - cofit analysis ---------------------------------------------------
      
      # Trouble shooting & Debug -------------------------------------------------------------------
      df03_FLC_mod_df %>% filter(`∆weight%` == 0) %>%  view()
      df03_FLC_mod_df %>% filter(`∆weight%` == 0) %>% select(c(2:7, diet_compliance, `∆weight%`, 13:17)) %>%  view()
      
      table(!is.na(df03_FLC_mod_df$`∆weight%`))
      
      
      
      
      
      # 00. Build Prediction-Model-Used df --------------------------------------
      
      df03_FLC_self_report <- tmp_03
      
      #adjust
      df03_FLC_self_report <- df03_FLC_self_report %>% filter(program %in% c("經典八週", "經典八週（202109新版）享瘦班", "進階計畫",
                                                                             "宋醫師專班 -FLC班", "2023 FLC-2個助教", "宋醫師進階計畫"))
      df03_FLC_self_report$program <- df03_FLC_self_report$program %>% factor(levels = c("經典八週", "經典八週（202109新版）享瘦班", "進階計畫",
                                                                                         "宋醫師專班 -FLC班", "2023 FLC-2個助教", "宋醫師進階計畫"))
      
      
      df03_FLC_self_report <- df03_FLC_self_report %>% filter(date_flc_t1 - date_flc_t0 > 50)
      
      df03_FLC_self_report <- df03_FLC_self_report %>% select(-c(age, measurement_after_program_date, measurement_before_program_date))
      
      #C1. col_names
      names(df03_FLC_self_report) <- names(df03_FLC_self_report) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
      #C1-2. filter by program not "^診所"
      df03_FLC_self_report <- df03_FLC_self_report[df03_FLC_self_report[["program"]] %>% grepl("^診所",.) %>% not(),]
      
      df03_FLC_self_report <- df03_FLC_self_report[with(df03_FLC_self_report, order(date_flc_T0, id)),]
      
      #C2. age: btd - date_t0 年齡(療程起始當天計算)
      df03_FLC_self_report$age <- (lubridate::ymd(df03_FLC_self_report$date_flc_T0) - lubridate::ymd(df03_FLC_self_report$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()
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
      
      
      #*** [Model-used df step]
      df03_FLC_mod_df <- df03_FLC_self_report
      
      
      #C5. rm outliers
      # df03_FLC_self_report[["∆weight%"]] <- ifelse((df03_FLC_self_report[["∆weight%"]] < quantile(df03_FLC_self_report[["∆weight%"]], 0.01, na.rm = TRUE)) | (df03_FLC_self_report[["∆weight%"]] > quantile(df03_FLC_self_report[["∆weight%"]], 0.95, na.rm = TRUE)),
      #                                              df03_FLC_self_report[["∆weight%"]] %>% mean(),
      #                                              df03_FLC_self_report[["∆weight%"]])
      df03_FLC_mod_df[["∆weight%"]] <- ifelse((df03_FLC_mod_df[["∆weight%"]] < quantile(df03_FLC_mod_df[["∆weight%"]], 0.01, na.rm = TRUE)) | (df03_FLC_mod_df[["∆weight%"]] > quantile(df03_FLC_mod_df[["∆weight%"]], 0.95, na.rm = TRUE)),
                                              NA,
                                              df03_FLC_mod_df[["∆weight%"]])
      
      df03_FLC_mod_df <- df03_FLC_mod_df %>% lin_exclude_NA_col(c("∆weight%"))
      
      
      df03_FLC_mod_df <- df03_FLC_mod_df %>% mutate(diet_compliance = (`upload_day_%` * `light_G_%` / 100) %>% round(2))
      
      #Adjust outliers
      ## Cut diet_compliance into groups
      df03_FLC_mod_df$gp_diet_score <- cut(df03_FLC_mod_df$diet_compliance, breaks = seq(-1, 100, 1), seq(0, 10, 0.1),include.lowest = TRUE)
      
      ## Define the code to apply to each group
      a <- df03_FLC_mod_df %>%
        group_by(gp_diet_score) %>%
        filter(gp_diet_score %in% (levels(df03_FLC_mod_df$gp_diet_score)[1:34])) %>%
        mutate(
          `∆weight%` = ifelse(
            (`∆weight%` < quantile(`∆weight%`, 0.15, na.rm = TRUE)) |
              (`∆weight%` > quantile(`∆weight%`, 0.99, na.rm = TRUE)),
            NA,
            `∆weight%`
          )
        ) %>%
        ungroup()
      
      b <- df03_FLC_mod_df %>%
        group_by(gp_diet_score) %>%
        filter(gp_diet_score %in% (levels(df03_FLC_mod_df$gp_diet_score)[35:67])) %>%
        mutate(
          `∆weight%` = ifelse(
            (`∆weight%` < quantile(`∆weight%`, 0.10, na.rm = TRUE)) |
              (`∆weight%` > quantile(`∆weight%`, 0.96, na.rm = TRUE)),
            NA,
            `∆weight%`
          )
        ) %>%
        ungroup()
      
      c <- df03_FLC_mod_df %>%
        group_by(gp_diet_score) %>%
        filter(gp_diet_score %in% (levels(df03_FLC_mod_df$gp_diet_score)[68:101])) %>%
        mutate(
          `∆weight%` = ifelse(
            (`∆weight%` < quantile(`∆weight%`, 0.01, na.rm = TRUE)) |
              (`∆weight%` > quantile(`∆weight%`, 0.90, na.rm = TRUE)),
            NA,
            `∆weight%`
          )
        ) %>%
        ungroup()
      
      
      df03_FLC_mod_df <- Reduce(rbind, list(a, b, c))
      rm(list = c("a", "b", "c"))
      
      
      # 01. Correlation --------------------------------------------------------------------
      
      
      df03_FLC_mod_df %>% 
        mutate(delta_weight_p = `∆weight%` %>% multiply_by(-1)) %>%
        ggscatter(x = "diet_compliance", y = "delta_weight_p",
                  color = "black",
                  fill = "red",
                  shape = 21,
                  size = 1,
                  add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  title = "Correlation(Weight x Compliance)",
                  xlab = "Diet Score",
                  ylab = "Weight Loss(%)",
                  # xlim = c(0, 13),
                  # ylim = c(0, 180),
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        # geom_vline(xintercept = c(5.5),linetype ="dashed", ) +
        # annotate("text", x=5.3, y=155, label="Cutoff = 5.5 mg/dL", angle=90) +
        stat_cor(method = "pearson", size = 5, label.x = 0, label.y = 10) # Add correlation coefficient)
      
      
      df03_FLC_mod_df %>% 
        mutate(delta_weight_p = `∆weight%` %>% multiply_by(-1)) %>%
        mutate(light_g = `light_G_%` %>% multiply_by(1)) %>%
        mutate(upload_day_p = `upload_day_%` %>% multiply_by(1)) %>%
        ggscatter(x = "upload_day_p", y = "light_g",
                  color = "black",
                  fill = "red",
                  shape = 21,
                  size = 1,
                  # add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  title = "Correlation(上傳完成度 x 綠燈率)",
                  xlab = "上傳完成度(%)",
                  ylab = "綠燈率(%)",
                  # xlim = c(0, 13),
                  # ylim = c(0, 180),
        ) +
        # geom_vline(xintercept = 50, lwd = 1) +
        # geom_hline(yintercept = 50, lwd = 1) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) 
        # geom_vline(xintercept = c(5.5),linetype ="dashed", ) +
        # annotate("text", x=5.3, y=155, label="Cutoff = 5.5 mg/dL", angle=90) +
        # stat_cor(method = "pearson", size = 5, label.x = 0, label.y = 10) # Add correlation coefficient)
      
      
      
      
      # 01. Setting -------------------------------------------------------------
      
      #构建模型评价函数
      ## 首先构建mpc函数，通过MSE,RMSE,Rsquare,MAE,MAPE,RASE,AIC,BIC8个指标来用来评价和筛选模型。
      library(modelr)
      mpc<-function(model,data){
        MSE<-modelr::mse(model=model,data=data)
        RMSE<-modelr::rmse(model=model,data=data)
        Rsquare<-modelr::rsquare(model=model,data=data)
        MAE<-modelr::mae(model=model,data=data)
        MAPE<-modelr::mape(model=model,data=data)
        RASE<-modelr::rsae(model=model,data=data)
        AIC=AIC(object=model)
        BIC=AIC(object=model)
        return(data.frame(MSE=MSE,RMSE=RMSE,Rsquare=Rsquare, MAE=MAE,MAPE=MAPE,RASE=RASE,
                          AIC=AIC,BIC=BIC))
      }
      
      dataset <- df03_FLC_mod_df %>% 
        mutate(delta_weight_p = `∆weight%` %>% multiply_by(-1)) %>% 
        mutate(BMI_baseline = `BMI(T0)`) %>% 
        filter(!is.na(delta_weight_p))
      
      library(caret)
      # Set train, test datasets
      set.seed(303)
      training.samples <- seq(1, nrow(dataset)) %>%
        createDataPartition(p = 0.8, list = FALSE)
      train.data  <- dataset[training.samples, ]
      test.data <- dataset[-training.samples, ]
      
      
      
      # plot
      train.data %>% 
        ggscatter(x = "diet_compliance", y = "delta_weight_p",
                  color = "black",
                  fill = "red",
                  shape = 21,
                  size = 1,
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  title = "",
                  xlab = "Diet Score",
                  ylab = "Weight Loss(%)",
                  # xlim = c(0, 13),
                  # ylim = c(0, 180),
        ) +
        stat_smooth() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        )  
      
      
      
      # 03. LM ----------------------------------------------------------------------
      
      mod.lm<-lm(delta_weight_p ~ age + BMI_baseline + diet_compliance, data = dataset)
      mod.lm %>% summary()
      dataset %>% mpc(mod.lm, .) 
      
      # 04. Non-LM --------------------------------------------------------------
      
      # library(mgcv)
      # mod.nlm <- gam(delta_weight_p ~ age + BMI_baseline + diet_compliance, data = dataset)
      # mod.nlm %>% summary()
      # dataset %>% mpc(mod.nlm, .) 
      
      
      # mod.lm<-lm(delta_weight_p ~ I(age^2) + I(BMI_baseline^1) + I(diet_compliance^2), data = dataset)
      # mod.lm<-lm(delta_weight_p ~ I(age^1) + I(BMI_baseline^1) + I(diet_compliance^2), data = dataset)
      mod.lm<-lm(delta_weight_p ~ gender + I(age^1) + I(BMI_baseline^1) + I(diet_compliance^2), data = dataset)
      mod.lm %>% summary()
      dataset %>% mpc(mod.lm, .) 
      
      library(MASS)
      model_equation(mod.lm, digits = 3, trim = TRUE)
      
      
      # 05.  Evaluation ---------------------------------------------------------
      # dataset$nutritionist_online
      
      # mod.lm<-lm(delta_weight_p ~ I(age^1) + I(BMI_baseline^1) + gender + nutritionist_online + I(diet_compliance^2), data = dataset)
      
      
      # model effectiveness
      mod.lm %>% summary()
      dataset %>% mpc(mod.lm, .) 
      
      ##* [Highlight 1: Diet score] 
      
        #01. individual input
      i=8
      look_up_profile <- data.frame(id = dataset$id[i], diet_compliance = dataset$diet_compliance[i], delta_weight_p = dataset$delta_weight_p[i],
                                    predict = mod.lm %>% predict(dataset[i,]) %>% as.numeric() %>% round(2),
                                    RMSE = dataset %>% mpc(mod.lm, .) %>% select(RMSE) %>% pull(),
                                    z_score = scale(actual, predict, RMSE) %>% round(2),
                                    PR = ((scale(actual, predict, RMSE) %>% round(2) %>% pnorm())[1]*100) %>% round(0)
                                    )
      # test.data[i,"diet_compliance"] %>% pull()
      actual <- test.data[i,"delta_weight_p"] %>% pull()
      predict <- mod.lm %>% predict(test.data[i,]) %>% as.numeric()
      
        #02. model: count PR value 
      RMSE <- dataset %>% mpc(mod.lm, .) %>% select(RMSE) %>% pull()
      # 1 x SD(σ)
      range <- (predict + c(-RMSE, +RMSE)) %>% round(2)
      
      # z score
      paste0("z score= ", scale(actual, predict, RMSE) %>% round(2))
      # PR value, rank in 100
      paste0("PR", ((scale(actual, predict, RMSE) %>% round(2) %>% pnorm())[1]*100) %>% round(0))
      # ((scale(actual, predict, RMSE) %>% round(2) %>% pnorm())[1]*100) %>% round(0)
      
      
      
      ###* [Diet score plot]
      dataset$predicted <- predict(mod.lm)
      dataset$sd <- sd(residuals(mod.lm))
      
      #1SD
      ggplot(dataset, aes(x = diet_compliance/10, y = predicted)) +
        geom_ribbon(aes(ymin = predicted - 1*sd, ymax = predicted + 1*sd, fill = "1SD"), alpha = 0.2) +
        geom_ribbon(aes(ymin = predicted - 2*sd, ymax = predicted + 2*sd, fill = "2SD"), alpha = 0.3) +
        scale_fill_manual(values = c("lightgreen", "lightpink"), labels = c("1SD", "2SD")) +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
        xlab("Diet score") +
        ylab("Weight Loss(%)") +
        labs(fill = "Area") +
        ggtitle("") + 
        annotate("text", x = min(dataset$diet_compliance/10), y = 15:14, label = c("my label", "label 2")) +
        # theme_pubr() 
        theme_bw()
       
      
      #0.5SD
      ggplot(dataset, aes(x = diet_compliance/10, y = predicted)) +
        geom_ribbon(aes(ymin = predicted - 0.5*sd, ymax = predicted + 0.5*sd, fill = "1SD"), alpha = 0.2) +
        geom_ribbon(aes(ymin = predicted - 1*sd, ymax = predicted + 1*sd, fill = "2SD"), alpha = 0.3) +
        scale_fill_manual(values = c("lightgreen", "lightpink"), labels = c("1SD", "2SD")) +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
        xlab("Diet score") +
        ylab("Weight Loss(%)") +
        labs(fill = "Area") +
        ggtitle("") + 
        annotate("text", x = min(dataset$diet_compliance/10), y = 15:14, label = c("my label", "label 2")) +
        # theme_pubr() 
        theme_bw()
      
      ###* [Diet score plot + client data]
      ggplot(dataset, aes(x = diet_compliance/10, y = predicted)) +
        geom_ribbon(aes(ymin = predicted - 1*sd, ymax = predicted + 1*sd, fill = "1SD"), alpha = 0.7) +
        geom_ribbon(aes(ymin = predicted - 2*sd, ymax = predicted + 2*sd, fill = "2SD"), alpha = 0.5) +
        scale_fill_manual(values = c("#B9D4E6", "#D5E7F0"), labels = c("1SD", "2SD")) +
        geom_line(aes(diet_compliance/10, predicted - 1*sd), color = "grey30", lwd = 0.1, alpha = 0.1) +
        geom_line(aes(diet_compliance/10, predicted + 1*sd), color = "grey30", lwd = 0.1, alpha = 0.1) +
        geom_line(aes(diet_compliance/10, predicted - 2*sd), color = "grey30", lwd = 0.1, alpha = 0.1) +
        geom_line(aes(diet_compliance/10, predicted + 2*sd), color = "grey30", lwd = 0.1, alpha = 0.1) +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "#FF6551", lwd = 1) +
        xlab("Diet score") +
        ylab("Weight Loss(%)") +
        labs(fill = "Area") +
        ggtitle("") + 
        annotate("text", x = min(dataset$diet_compliance/10), y = 15:12, hjust = 0, fontface = "bold",
                 label = c(paste0("ID: ", look_up_profile$id), 
                           paste0("Diet score: ", round(look_up_profile$diet_compliance/10, 1), " /10"),
                           paste0("Weight Loss: ", look_up_profile$delta_weight_p, " (%)"),
                           paste0("PR", look_up_profile$PR))
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
          axis.title.x = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14),
          legend.background = element_rect(fill = "white", color = "grey", linewidth = .2),
          legend.position = c(.99, .99),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)
        ) +
        # input data point
        geom_point(data = look_up_profile, aes(x = diet_compliance/10, y = delta_weight_p),
                   shape = 23, fill = "#FF6551", color = "#293172", size = 2, stroke = 1.5)
      
      
      
      ##* [Highlight 2: Program] FLC outperform Cofit
      dataset$gp_tmp <- cut(dataset$diet_compliance, breaks = 5, paste(seq(0, 8, 2), seq(2, 10, 2), sep = "-"),include.lowest = TRUE)
      dataset %>% 
        group_by(program, gp_tmp) %>% 
        summarise(
          `weight loss` = paste(mean(delta_weight_p, na.rm = T) %>% round(2),
                                (sd(delta_weight_p, na.rm = T)/sqrt(n())) %>% round(2),
                                # (sd(delta_weight_p, na.rm = T)) %>% round(2),
                                sep = " ± "),
          n = n()
        ) %>% view()
      
      dataset %>% 
        group_by(program, gp_tmp) %>% 
        summarise(
          weight_mean = mean(delta_weight_p, na.rm = T) %>% round(2),
          weight_sd = sd(delta_weight_p, na.rm = T) %>% round(2),
          weight_se = (sd(delta_weight_p, na.rm = T)/sqrt(n())) %>% round(2),
          n = n()
        ) %>% 
        ggplot(aes(x = gp_tmp, y = weight_mean, fill = program, width = 0.8)) + 
        geom_bar(stat = "identity", position = position_dodge(0.8)) +
        geom_errorbar(aes(ymin = weight_mean - weight_se, ymax = weight_mean + weight_se), width = 0.2, position = position_dodge(0.8)) +
        scale_fill_manual(values = RColorBrewer::brewer.pal(6, "RdBu")) +
        xlab("Diet score") +
        ylab("Weight Loss(%)") +
        labs(fill = "Program") +
        ggtitle("") +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
          axis.title.x = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14),
        )
      
      ##* [Highlight 2: nutritionist_online] 
      library(DT)
      rbind(cbind(nutritionist_online = "All", 
                  dataset %>% 
                    summarise(
                      weight_mean = mean(delta_weight_p, na.rm = T) %>% round(2),
                      weight_sd = sd(delta_weight_p, na.rm = T) %>% round(2),
                      weight_se = (sd(delta_weight_p, na.rm = T)/sqrt(n())) %>% round(2),
                      n = n()
                    )),
            dataset %>% 
              group_by(nutritionist_online) %>% 
              summarise(
                weight_mean = mean(delta_weight_p, na.rm = T) %>% round(2),
                weight_sd = sd(delta_weight_p, na.rm = T) %>% round(2),
                weight_se = (sd(delta_weight_p, na.rm = T)/sqrt(n())) %>% round(2),
                n = n()
              )
      ) %>%  
        # arrange(desc(weight_mean)) %>% 
        datatable(extensions = c('Buttons',"FixedColumns"),
                  options = list(fixedColumns = list(leftColumns = 1),
                                 dom = 'Blfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print'),
                                 scrollX = TRUE,
                                 # autoWidth = TRUE,
                                 lengthMenu = list(c(10,25,50,-1),
                                                   c(10,25,50,"All")),
                                 searchHighlight = TRUE),
                  filter = 'top')

      
      rbind(cbind(nutritionist_online = "All", 
                  dataset %>% 
                    summarise(
                      weight_mean = mean(delta_weight_p, na.rm = T) %>% round(2),
                      weight_sd = sd(delta_weight_p, na.rm = T) %>% round(2),
                      weight_se = (sd(delta_weight_p, na.rm = T)/sqrt(n())) %>% round(2),
                      n = n()
                    )),
            dataset %>% 
              group_by(nutritionist_online) %>% 
              summarise(
                weight_mean = mean(delta_weight_p, na.rm = T) %>% round(2),
                weight_sd = sd(delta_weight_p, na.rm = T) %>% round(2),
                weight_se = (sd(delta_weight_p, na.rm = T)/sqrt(n())) %>% round(2),
                n = n()
              )
      )  %>% 
        ggplot(aes(x = reorder(nutritionist_online, -weight_mean), y = weight_mean, width = 0.8)) + 
        geom_bar(aes(fill = nutritionist_online == "All"), stat = "identity", position = position_dodge(0.8)) +
        scale_fill_manual(guide = "none", breaks = c(FALSE, TRUE), values=c("grey30", "gold")) +  
        # geom_errorbar(aes(ymin = weight_mean - weight_se, ymax = weight_mean + weight_se), width = 0.2, position = position_dodge(0.8)) +
        xlab("營養師") +
        ylab("Weight Loss(%)") +
        # labs(fill = "") +
        ggtitle("") +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
          axis.title.x = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 7, angle = 90)
        ) + 
        coord_flip()
      

# 0519_disease improvement ------------------------------------------------

      
      library(dplyr)
      a <- stat_table_1st_ob %>% select(grep("^DM", names(.), value = TRUE))
      names(a) <- c("before", "after")
      a <- a %>% filter(across(everything(), ~ . != "Unclassified")) %>% table(exclude = "Unclassified") %>%
        ftable() %>% as.matrix() %>% as.tibble() %>% 
        cbind(baseline = levels(a$before)[-length(levels(a$before))])
      
      # library(RColorBrewer)
      # # Generate color palette
      # colors <- brewer.pal(3, "RdBu")
      # # Transform into desired format
      # colors <- paste0("'", colors, "'", collapse = ",")
      # # Add square brackets
      # colors <- paste("[", colors, "]", sep = "")
      
      
      # gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
      #              options = list(isStacked = 'percent',
      #                             bar="{groupWidth:'50%'}",
      #                             title = '減重成效-Diabetes',
      #                             legend = "{position:'right'}",
      #                             colors = "['#d9e3f0','#ffe599','#ff8080']",
      #                             backgroundColor = "#f9fffb",
      #                             width = "600",
      #                             height = "600")) %>% plot()
      
      
      #[OK]MetaX
      a <- stat_table_1st_ob %>% select(grep("^MetaX", names(.), value = TRUE))
      names(a) <- c("before", "after")
      a <- a %>% filter(across(everything(), ~ . != "Unclassified")) %>% table(exclude = "Unclassified") %>%
        ftable() %>% as.matrix() %>% as.tibble() %>% 
        cbind(baseline = levels(a$before)[-length(levels(a$before))])
      
      ggvix_plot_1 <- 
      gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
                   options = list(isStacked = 'percent',
                                  bar="{groupWidth:'50%'}",
                                  title = '減重成效-Metabolic Syndrome',
                                  legend = "{position:'right'}",
                                  colors = "['#d9e3f0','#ff8080']",
                                  backgroundColor = "#f9fffb",
                                  width = "600",
                                  height = "600")) %>% plot()
      
      
      #[ok]HTN
      a <- stat_table_1st_ob %>% select(grep("^HTN", names(.), value = TRUE))
      names(a) <- c("before", "after")
      a <- a %>% filter(across(everything(), ~ . != "Unclassified")) %>% table(exclude = "Unclassified") %>%
        ftable() %>% as.matrix() %>% as.tibble() %>% 
        cbind(baseline = levels(a$before)[-length(levels(a$before))])
      
      ggvix_plot_2 <-
      gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
                   options = list(isStacked = 'percent',
                                  bar="{groupWidth:'50%'}",
                                  title = '減重成效-Hypertension',
                                  legend = "{position:'right'}",
                                  colors = "['#0571B0','#92C5DE','#F4A582','#CA0020']",
                                  backgroundColor = "#f9fffb",
                                  width = "600",
                                  height = "600")) %>% plot()
      
      #[ok]HLP
      a <- stat_table_1st_ob %>% select(grep("^HLP", names(.), value = TRUE))
      names(a) <- c("before", "after")
      a <- a %>% filter(across(everything(), ~ . != "Unclassified")) %>% table(exclude = "Unclassified") %>%
        ftable() %>% as.matrix() %>% as.tibble() %>% 
        cbind(baseline = levels(a$before)[-length(levels(a$before))])
      
      ggvix_plot_3 <-
      gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
                   options = list(isStacked = 'percent',
                                  bar="{groupWidth:'50%'}",
                                  title = '減重成效-Hyperlipidemia',
                                  legend = "{position:'right'}",
                                  colors = "['#d9e3f0','#ff8080']",
                                  backgroundColor = "#f9fffb",
                                  width = "600",
                                  height = "600")) %>% plot()
      
      
      #[]BMI Obesity
      a <- stat_table_1st_ob %>% select(grep("gp_bmi", names(.), value = TRUE))
      names(a) <- c("before", "after")
      a <- a %>% filter(across(everything(), ~ . != "Unclassified")) %>% table(exclude = "Unclassified") %>%
        ftable() %>% as.matrix() %>% as.tibble() %>% 
        cbind(baseline = levels(a$before))
      
      
      ggvix_plot_4 <-
      gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
                   options = list(isStacked = 'percent',
                                  bar="{groupWidth:'50%'}",
                                  title = '減重成效-Obesity',
                                  legend = "{position:'right'}",
                                  colors = "['#0571B0','#92C5DE','#F4A582','#CA0020']",
                                  backgroundColor = "#f9fffb",
                                  width = "600",
                                  height = "600")) %>% plot()
      
      
      
      
      
      #[v]Insulin Response
      a <- df05_biochem %>% select(id, date_blood, Pattern_major, DM)
      # a <- left_join(df05_biochem, df01_profile %>% select(id, client_type), by = c("id"), multiple = "first")
      # a <- df05_biochem %>% filter(client_type == "2")
      a <- a %>% filter(((Pattern_major %in% c(levels(a$Pattern_major)[-6])) & (DM %in% c(levels(a$DM)[-4]))))
      a <- dplyr::add_count(a, id) %>% filter(n > 1) %>% select(-n)
      a <- a[with(a, order(id, date_blood)),]
      
      
      # Filter by id, focus on best improvement record
      a <- a %>% lin_fac_int_format(Pattern_major, to = "int")
      a <- rbind(
        a %>% distinct(id, .keep_all = T),
        a %>%
          group_by(id) %>%
          filter(Pattern_major_int == (Pattern_major_int %>% min())) %>%
          group_by(id) %>%
          slice(n())
      )
      a <- a[with(a, order(id, date_blood)),]
      a <- a %>% select(-Pattern_major_int)
        
      
      a$Pattern_major_after <- NA
      a$Pattern_major_after[1:nrow(a)-1] <- a[["Pattern_major"]][2:nrow(a)] %>% as.character()
      a$DM_after <- NA
      a$DM_after[1:nrow(a)-1] <- a[["DM"]][2:nrow(a)] %>% as.character()
      
      
      library(dplyr)
      a <- a %>% group_by(id) %>% slice(1:(n()-1))
      names(a) <- c("id", "date", "I_before", "DM_before", "I_after",  "DM_after")
      a$I_after <- factor(a$I_after, levels = levels(a$I_before))
      a$DM_after <- factor(a$DM_after, levels = levels(a$DM_before))
      
      
      #[input] table, all: addmargins(), row:addmargins(t1:2;t2:1,2),rbind RowSum
      a1 <- table(Origin = a$I_before, Change = a$I_after, exclude = "Unclassified") %>% 
        ftable() %>% as.matrix() %>% as.tibble() %>% 
        cbind(baseline = levels(a$I_before)[-length(levels(a$I_before))])
      
      
      ggvix_plot_5 <-
      gvisBarChart(a1 , xvar = "baseline", yvar = a1 %>% select(-baseline) %>% names(),
                   options = list(isStacked = 'percent',
                                  bar="{groupWidth:'50%'}",
                                  title = '治療成效-Insulin Resistance',
                                  legend = "{position:'right'}",
                                  colors="['#628bd6','#f8e05c','#ffc081','#ff834a','#ff5959']",
                                  backgroundColor = "#f9fffb",
                                  width = "600",
                                  height = "600")) %>% plot()
      
      
      
      #[v]DM
      a <- df05_biochem %>% select(id, date_blood, Pattern_major, DM)
      a <- a %>% filter(((Pattern_major %in% c(levels(a$Pattern_major)[-6])) & (DM %in% c(levels(a$DM)[-4]))))
      a <- dplyr::add_count(a, id) %>% filter(n > 1) %>% select(-n)
      a <- a[with(a, order(id, date_blood)),]
      
      
      # Filter by id, focus on best improvement record
      a <- a %>% lin_fac_int_format(DM, to = "int")
      a <- rbind(
        a %>% distinct(id, .keep_all = T),
        a %>%
          group_by(id) %>%
          filter(DM_int == (DM_int %>% min())) %>%
          group_by(id) %>%
          slice(n())
      )
      a <- a[with(a, order(id, date_blood)),]
      a <- a %>% select(-DM_int)
      
      
      a$Pattern_major_after <- NA
      a$Pattern_major_after[1:nrow(a)-1] <- a[["Pattern_major"]][2:nrow(a)] %>% as.character()
      a$DM_after <- NA
      a$DM_after[1:nrow(a)-1] <- a[["DM"]][2:nrow(a)] %>% as.character()
      
      
      library(dplyr)
      a <- a %>% group_by(id) %>% slice(1:(n()-1))
      names(a) <- c("id", "date", "I_before", "DM_before", "I_after",  "DM_after")
      a$I_after <- factor(a$I_after, levels = levels(a$I_before))
      a$DM_after <- factor(a$DM_after, levels = levels(a$DM_before))
      
      a2 <- table(Origin = a$DM_before, Change = a$DM_after, exclude = "Unclassified") %>% 
        ftable() %>% as.matrix() %>% as.tibble() %>% 
        cbind(baseline = levels(a$DM_before)[-length(levels(a$DM_before))])
      
      
      ggvix_plot_6 <-
      gvisBarChart(a2 , xvar = "baseline", yvar = a2 %>% select(-baseline) %>% names(),
                   options = list(isStacked = 'percent',
                                  bar="{groupWidth:'50%'}",
                                  title = '治療成效-Diabetes',
                                  legend = "{position:'right'}",
                                  colors="['#628bd6','#ffc081','#ff5959']",
                                  backgroundColor = "#f9fffb",
                                  width = "600",
                                  height = "600"))
      
      
    gvisMerge(ggvix_plot_5, ggvix_plot_6, horizontal = T) %>% plot()
      

# 0522: Cofit x TMU -------------------------------------------------------

      # Cofit x TMU publication
      
      
      # 01. FLC ---------------------------------------------------------------------
      #input flc data
      pub_df_cofit <- tmp_03_day %>% select(-mobile)
      pub_df_cofit <- tmp_03_day %>% mutate(course = date_flc_t1 - date_flc_t0)
      pub_df_cofit <- pub_df_cofit %>% filter(course == 56)
      
      #filter program
      pub_df_cofit <- pub_df_cofit %>% filter(program %in% c("宋醫師專班 -FLC班","經典八週","2023 FLC-2個助教","診所八週(週一開班)-宋醫師班/初日班","宋醫師進階計畫","診所進階計畫","診所八週(週四啟動)-初日班","進階計畫","經典八週（202109新版）享瘦班"))
      pub_df_cofit <- pub_df_cofit[pub_df_cofit[["program"]] %>% grepl("^診所",.) %>% not(),]
      #age calc: btd - date_t0 年齡(療程起始當天計算)
      pub_df_cofit <- pub_df_cofit %>% select(-c(age))
      pub_df_cofit$age <- (lubridate::ymd(pub_df_cofit$date_flc_t0) - lubridate::ymd(pub_df_cofit$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()
      #reorder
      pub_df_cofit <- pub_df_cofit[with(pub_df_cofit, order(client_id, date_flc_t0, date)),]
      #cut week
      pub_df_cofit$weeks <- ceiling(difftime(pub_df_cofit$date, pub_df_cofit$date_flc_t0, units = "weeks"))
      #filter last record of the week as the observation.
      pub_df_cofit <- pub_df_cofit %>%
        select(client_id, weeks, date_flc_t0, date_flc_t1, weight, age, gender) %>% 
        # group_by(client_id, weeks) %>% 
        group_by(client_id, date_flc_t0, weeks) %>%
        filter(row_number() == n()) %>% ungroup()
      
      
      pub_df_cofit <- pub_df_cofit %>% filter(weeks <= 10)
      
      # pub_df_cofit %>% filter(weeks == 0) %>% select(client_id) %>% pull() %>% unique() %>% length()
      # pub_df_cofit %>% filter(weeks %in% c(4)) %>% select(client_id) %>% pull() %>% unique() %>% length()
      # pub_df_cofit %>% filter(weeks %in% c(5, 6, 7, 8)) %>% select(client_id) %>% pull() %>% unique() %>% length()
      
      pub_df_cofit <- Reduce(rbind, list(pub_df_cofit %>% filter(weeks %in% c(0, 1)) %>% group_by(client_id, date_flc_t0) %>% 
                                           filter(row_number() == 1),
                                         pub_df_cofit %>% filter(weeks %in% c(3, 4)) %>% group_by(client_id, date_flc_t0) %>% 
                                           filter(row_number() == n()),
                                         pub_df_cofit %>% filter(weeks %in% c(5, 6, 7, 8)) %>% group_by(client_id, date_flc_t0) %>% 
                                           filter(row_number() == n()))) 
      
      pub_df_cofit$weeks <- ifelse(pub_df_cofit$weeks %in% c(0, 1), 0, pub_df_cofit$weeks)
      pub_df_cofit$weeks <- ifelse(pub_df_cofit$weeks %in% c(3, 4), 4, pub_df_cofit$weeks)
      pub_df_cofit$weeks <- ifelse(pub_df_cofit$weeks %in% c(5, 6, 7, 8), 8, pub_df_cofit$weeks)
      
      aa <- pub_df_cofit
      aa <- 
        aa %>% 
        distinct(client_id, date_flc_t0, .keep_all = TRUE)
      
      pub_df_cofit <- Reduce(function(x, y) left_join(x, y, by = c("client_id", "date_flc_t0"), multiple = "first"),
                             list(
                               aa,
                               filter(pub_df_cofit, weeks == 0) %>% select(client_id, weight) %>% dplyr::rename(weight_w0 = weight),
                               filter(pub_df_cofit, weeks == 4) %>% select(client_id, weight) %>% dplyr::rename(weight_w4 = weight),
                               filter(pub_df_cofit, weeks == 8) %>% select(client_id, weight) %>% dplyr::rename(weight_w8 = weight)
                             )
      )
      pub_df_cofit <- pub_df_cofit %>% distinct(client_id, date_flc_t0, .keep_all = TRUE)
      
      
      rm(list = c("aa"))
      
      pub_df_cofit <- 
        pub_df_cofit %>% 
        mutate(delta_weight_w0 = ((weight_w0 - weight_w0)*100/weight_w0) %>% round(2)) %>% 
        mutate(delta_weight_w4 = ((weight_w4 - weight_w0)*100/weight_w0) %>% round(2)) %>% 
        mutate(delta_weight_w8 = ((weight_w8 - weight_w0)*100/weight_w0) %>% round(2)) 
      
      pub_df_cofit <- 
        pub_df_cofit %>% reshape2::melt(id.var = c("client_id", "gender"),
                                        measure.vars = c("delta_weight_w0", "delta_weight_w4", "delta_weight_w8"),
                                        variable.name = "weeks",
                                        value.name = "weight")
      
      pub_df_cofit$weeks <- pub_df_cofit$weeks %>% gsub("delta_weight_w",., replacement = "") %>% as.numeric()
      
      pub_df_cofit$weight <- ifelse(is.nan(pub_df_cofit$weight), NA, pub_df_cofit$weight)
      pub_df_cofit$weight <- ifelse(!is.finite(pub_df_cofit$weight), NA, pub_df_cofit$weight)
      
      #***[note: gp]
      pub_df_cofit$gp <- "Cofit"
      
      
      
      # 02. non-FLC -----------------------------------------------------------------
      
      
      # input data
      pub_df_ctrl <- tmp_04 %>% filter(client_id %in% df04_non_FLC_self_report$id)
      
      #C1. age: btd - date_t0 年齡(療程起始當天計算)
      pub_df_ctrl$age <- (lubridate::ymd(pub_df_ctrl$date_free_version) - lubridate::ymd(pub_df_ctrl$birthday)) %>% as.numeric() %>% divide_by(365) %>% floor()
      pub_df_ctrl <- pub_df_ctrl[with(pub_df_ctrl, order(client_id, date_free_version)),]
      
      #C2. T0, T1
      aa <- df04_non_FLC_self_report
      aa$date_free_T0 <- aa$date_free_T0 %>% as.character()
      aa$date_free_T1 <- aa$date_free_T1 %>% as.character()
      pub_df_ctrl <- lin_mapping(pub_df_ctrl, date_free_T0, client_id, aa, date_free_T0, id)
      pub_df_ctrl <- lin_mapping(pub_df_ctrl, date_free_T1, client_id, aa, date_free_T1, id)
      
      # pub_df_ctrl$date_free_T0 <- pub_df_ctrl$date_free_T0 %>% as_date(origin = lubridate::origin)
      # pub_df_ctrl$date_free_T1 <- pub_df_ctrl$date_free_T1 %>% as_date(origin = lubridate::origin)
      
      pub_df_ctrl$date_free_T0 <- pub_df_ctrl$date_free_T0 %>% as.Date()
      pub_df_ctrl$date_free_T1 <- pub_df_ctrl$date_free_T1 %>% as.Date()
      #cut week
      pub_df_ctrl$weeks <- ceiling(difftime(pub_df_ctrl$date_free_version, pub_df_ctrl$date_free_T0, units = "weeks"))
      
      pub_df_ctrl <- pub_df_ctrl %>% filter(weeks <= 10)
      
      pub_df_ctrl <- Reduce(rbind, list(pub_df_ctrl %>% filter(weeks == 0),
                                        pub_df_ctrl %>% filter(weeks %in% c(3, 4, 5)) %>% group_by(client_id) %>% 
                                          filter(row_number() == n()),
                                        pub_df_ctrl %>% filter(weeks %in% c(8, 9, 10)) %>% group_by(client_id) %>% 
                                          filter(row_number() == n())))
      
      pub_df_ctrl$weeks <- ifelse(pub_df_ctrl$weeks %in% c(3, 4, 5), 4, pub_df_ctrl$weeks)
      pub_df_ctrl$weeks <- ifelse(pub_df_ctrl$weeks %in% c(8, 9, 10), 8, pub_df_ctrl$weeks)
      
      
      rm(list = c("aa"))
      
      aa <- pub_df_ctrl
      aa <- 
        aa %>% 
        select(client_id, date_free_T0, date_free_T1, age, gender) %>% 
        distinct(client_id, .keep_all = TRUE)
      
      pub_df_ctrl <- Reduce(function(x, y) left_join(x, y, by = "client_id", multiple = "first"),
                            list(
                              aa,
                              filter(pub_df_ctrl, weeks == 0) %>% select(client_id, weight) %>% dplyr::rename(weight_w0 = weight),
                              filter(pub_df_ctrl, weeks == 4) %>% select(client_id, weight) %>% dplyr::rename(weight_w4 = weight),
                              filter(pub_df_ctrl, weeks == 8) %>% select(client_id, weight) %>% dplyr::rename(weight_w8 = weight)
                            )
      )
      rm(list = c("aa"))
      
      pub_df_ctrl <- 
        pub_df_ctrl %>% 
        mutate(delta_weight_w0 = ((weight_w0 - weight_w0)*100/weight_w0) %>% round(2)) %>% 
        mutate(delta_weight_w4 = ((weight_w4 - weight_w0)*100/weight_w0) %>% round(2)) %>% 
        mutate(delta_weight_w8 = ((weight_w8 - weight_w0)*100/weight_w0) %>% round(2)) 
      
      pub_df_ctrl <- 
        pub_df_ctrl %>% reshape2::melt(id.var = c("client_id", "gender"),
                                       measure.vars = c("delta_weight_w0", "delta_weight_w4", "delta_weight_w8"),
                                       variable.name = "weeks",
                                       value.name = "weight")
      
      pub_df_ctrl$weeks <- pub_df_ctrl$weeks %>% gsub("delta_weight_w",., replacement = "") %>% as.numeric()
      
      #***[note: gp]
      pub_df_ctrl$gp <- "control"
      
      
      
      
      
      # 03. plot ----------------------------------------------------------------
      
      pub_df <- rbind(pub_df_cofit, pub_df_ctrl)
      
       #outliers
      pub_df$weight <- ifelse((pub_df$weight < quantile(pub_df$weight, 0.0001, na.rm = TRUE)) | (pub_df$weight > quantile(pub_df$weight, 0.99985, na.rm = TRUE)),
                              pub_df$weight %>% mean(),
                              pub_df$weight) %>% summary()
        
        
      #trouble shooting: geom_line(continues x and p-value position mismatch > transform x into factor)
      pub_df$weeks <- factor(pub_df$weeks)
      
      library(rstatix)
      library(ggpubr)
      stat.test <- pub_df %>%
        group_by(weeks) %>%
        t_test(weight ~ gp) %>%
        adjust_pvalue(method = "bonferroni") %>%
        add_significance("p.adj")
      stat.test <- stat.test %>%
        add_xy_position(x = "weeks", fun = "mean")
      
      # pub_df %>%
      #   group_by(gp, weeks, gender) %>% 
      #   summarise(
      #     var = weight %>% mean(na.rm = T),
      #     sd = weight %>% sd(na.rm = T)
      #   ) %>% 
      #   ggplot(aes(x = weeks, y = var, group = gp, color = gp)) +
      #   # for publication
      #   geom_line() +
      #   geom_point() +
      #   geom_errorbar(data = . %>% filter(weeks != 0),
      #                 aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
      #   # commercial analysis & visualization
      #   # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "firebrick", lwd = 1) +
      #   labs(x = "Weeks", y = "Weight Loss(%)", title = "Weight Loss",  color = "Program") +
      #   scale_color_brewer(palette="Paired", direction = -1) + 
      #   scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
      #   theme_bw() +
      #   theme(
      #     plot.title = element_text(hjust = 0.5, face = "bold", size = 17),
      #     axis.title.x = element_text(hjust = 0.5, face = "bold", size = 14),
      #     axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14),
      #   ) +
      #   stat_pvalue_manual(
      #     stat.test, label = "p.adj.signif", tip.length = 0.0, 
      #     bracket.size = 0, bracket.nudge.y = 2.0, step.increase = 0.05, hide.ns = FALSE 
      #   ) 
      #   
      # 
      full_join(pub_df %>%
                  group_by(gp, weeks, gender) %>% 
                  summarise(
                    var = weight %>% mean(na.rm = T),
                    sd = weight %>% sd(na.rm = T),
                    se = ((weight %>% sd(na.rm = T))/sqrt(n())) %>% round(2),
                    N = n()
                  ),
                pub_df %>%
                  group_by(gp, weeks) %>% 
                  summarise(
                    var = weight %>% mean(na.rm = T),
                    sd = weight %>% sd(na.rm = T),
                    se = ((weight %>% sd(na.rm = T))/sqrt(n())) %>% round(2),
                    N = n()
                  ) %>% add_column(gender = "All")
                ) %>% 
        filter(gender == "All") %>% 
        ggplot(aes(x = weeks, y = var, group = interaction(gender, gp), color = interaction(gender, gp))) +
        # for publication
        geom_line() +
        geom_point() +
        geom_errorbar(data = . %>% filter(weeks != 0) %>% filter(gender == "All"),
                      aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
                      # aes(ymin = var , ymax = var  ), width = .1) +
        # commercial analysis & visualization
        # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "firebrick", lwd = 1) +
        labs(x = "Weeks", y = "Weight Loss(%)", title = "Weight Loss") +
        scale_color_manual(values = 
                             #(All)Cofit x Control
                             c("#2171B5", "#252525"),
                             # Gender
                             # c("#08519C", "#2171B5", "#6BAED6",
                             #   "#252525", "#737373", "#969696"), 
                           name = "Program", 
                           labels = c("Cofit", "Control")
                           # Gender
                           # labels = c("Cofit", "Female(Cofit)","Male(Cofit)", "Control","Female(Contro)","Male(Contro)")
        )  +
        scale_y_continuous(breaks = seq(-10, 5, 5), expand = expansion(mult = c(0.2, 0.3))) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
        ) +
        stat_pvalue_manual(
          stat.test, label = "p.adj.signif", tip.length = 0.0, 
          bracket.size = 0, bracket.nudge.y = 2.0, step.increase = 0.05, hide.ns = FALSE 
        )
      
      
      
      
      full_join(pub_df %>%
                  group_by(gp, weeks, gender) %>% 
                  summarise(
                    var = weight %>% mean(na.rm = T),
                    sd = weight %>% sd(na.rm = T),
                    se = ((weight %>% sd(na.rm = T))/sqrt(n())) %>% round(2),
                    N = n()
                  ),
                pub_df %>%
                  group_by(gp, weeks) %>% 
                  summarise(
                    var = weight %>% mean(na.rm = T),
                    sd = weight %>% sd(na.rm = T),
                    se = ((weight %>% sd(na.rm = T))/sqrt(n())) %>% round(2),
                    N = n()
                  ) %>% add_column(gender = "All")
      ) %>% 
        filter(gender %in% c("female", "male")) %>% 
        ggplot(aes(x = weeks, y = var, group = interaction(gender, gp), color = interaction(gender, gp))) +
        # for publication
        geom_line() +
        geom_point() +
        # geom_errorbar(data = . %>% filter(weeks != 0) %>% filter(gender == "All"),
        #               aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
        # aes(ymin = var , ymax = var  ), width = .1) +
        # commercial analysis & visualization
        # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "firebrick", lwd = 1) +
        labs(x = "Weeks", y = "Weight Loss(%)", title = "Weight Loss") +
        scale_color_manual(values = 
                             #(All)Cofit x Control
                             # c("#2171B5", "#252525"),
                           # Gender
                           c("#2171B5", "#6BAED6",
                             "#737373", "#969696"),
                           name = "Program", 
                           # labels = c("Cofit", "Control")
                           # Gender
                           labels = c("Female(Cofit)","Male(Cofit)", "Female(Control)","Male(Control)")
        )  +
        scale_y_continuous(breaks = seq(-10, 5, 5), expand = expansion(mult = c(0.5, 0.3))) +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
        ) +
        stat_pvalue_manual(
          stat.test, label = "p.adj.signif", tip.length = 0.0, 
          bracket.size = 0, bracket.nudge.y = 2.0, step.increase = 0.05, hide.ns = FALSE 
        )
      
      table(pub_df$gp, pub_df$weeks, !is.na(pub_df$weight)) %>% ftable()
      
      
      
      

      x <- lin_conv_GA(x, hba1c_var = GA, invert = TRUE, GA_var_name = hba1c_conv)
      