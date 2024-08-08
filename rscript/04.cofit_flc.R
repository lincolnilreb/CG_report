
# df03_FLC_Cleaning -------------------------------------------------------



df03_FLC_self_report <- tmp_03 %>% select(-mobile)

df03_FLC_self_report <- df03_FLC_self_report %>% filter(program %in% c("經典八週（202109新版）享瘦班",
                                                                       "經典八週",
                                                                       "進階計畫",
                                                                       "2023 FLC-2個助教",
                                                                       "宋醫師專班 -FLC班",
                                                                       "宋醫師進階計畫"))

df03_FLC_self_report$program <-  factor(df03_FLC_self_report$program, levels = c("經典八週（202109新版）享瘦班",
                                                                                 "經典八週",
                                                                                 "進階計畫",
                                                                                 "2023 FLC-2個助教",
                                                                                 "宋醫師專班 -FLC班",
                                                                                 "宋醫師進階計畫"))

df03_FLC_self_report <- df03_FLC_self_report %>%
  mutate(program_gp = factor(ifelse(grepl("進階", program), "進階計畫", "經典八週"),
                             levels = c("經典八週", "進階計畫")))





# Overview
df03_FLC_self_report %>% 
  group_by(program) %>% 
  summarise(
    T0_start = first(date_flc_t0),
    T0_end = last(date_flc_t0),
    N = n()
  )

df03_FLC_self_report %>% 
  group_by(program_gp) %>% 
  summarise(
    weight = mean(weight_delta, na.rm = T),
    weight_p = mean(weight_delta_p, na.rm = T),
    fat = mean(fat_delta, na.rm = T),
    fat_p = mean(fat_delta_p, na.rm = T),
    N = n()
  )



df03_FLC_self_report <- df03_FLC_self_report %>% select(-c(age, measurement_after_program_date, measurement_before_program_date))

#C1. col_names
names(df03_FLC_self_report) <- names(df03_FLC_self_report) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
#C1-2. filter by program not "^診所"
df03_FLC_self_report <- df03_FLC_self_report[df03_FLC_self_report[["program"]] %>% grepl("^診所",.) %>% not(),]

df03_FLC_self_report <- df03_FLC_self_report[with(df03_FLC_self_report, order(date_flc_T0, id)),]

#C2. age: btd - date_t0 年齡(療程起始當天計算)
df03_FLC_self_report$age <- (lubridate::ymd(df03_FLC_self_report$date_flc_T0) - lubridate::ymd(df03_FLC_self_report$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()



## Data Cleaning
# - Weight record: from 10_daily_flc.sql(tmp_03_day)
# - Diet record: from 07_Deit_meal.sql > (df06_Diet_day)

a1_weight <- 
  tmp_03_day %>% 
  mutate(id = client_id) %>% 
  select(
    c(
      "id",
      "date",
      "weight",
      "bmi",
      "waist_circumference",
      "body_fat_mass"
    )
  )

# [Duplicated id, date_flc_T0, 2420320宜婷: 可能是營養師加入班級見習，兩筆都不是正式data。]
a1_weight <- a1_weight %>% distinct(id, date, .keep_all = TRUE)


a1_diet <- 
  df06_Diet_day %>%
  mutate(id = client_id) %>%
  mutate(date = date_diet) %>%
  select(
    c(
      "id",
      "date",
      "note_counts",
      "pic_counts",
      "light_green_count",
      "light_yellow_count",
      "light_red_count",
      "carb_e_day",
      "protein_e_day",
      "fat_e_day",
      "calorie_day",
      "carbohydrate_target",
      "protein_target",
      "fat_target",
      "calorie_target",
      "grains_day",
      "meat_bean_day",
      "oil_day",
      "vegetables_day",
      "fruits_day",
      "milk_day",
      "grains_target",
      "meat_bean_target",
      "oil_target",
      "vegetables_target",
      "fruits_target",
      "milk_target",
      "grains_day_deficit",
      "meat_bean_day_deficit",
      "oil_day_deficit",
      "vegetables_day_deficit",
      "fruits_day_deficit",
      "milk_day_deficit",
      "carb_e_day_deficit",
      "protein_e_day_deficit",
      "fat_e_day_deficit",
      "calorie_deficit"
    )
  )

# [Duplicated id, date_flc_T0, 2420320宜婷: 可能是營養師加入班級見習，兩筆都不是正式data。]
a1_diet <- a1_diet %>% distinct(id, date, .keep_all = TRUE)

# Merge Weight & Diet Record.
a1 <- left_join(a1_diet, a1_weight, by = c("id", "date"))


# class info
a2 <- 
df03_FLC_self_report %>% select(id, date_flc_T0, date_flc_T1) %>% 
  mutate(date_flc_w8 = lubridate::ymd(df03_FLC_self_report$date_flc_T0) + 56) %>% 
  mutate(program_duration_by_day = as.numeric((lubridate::ymd(df03_FLC_self_report$date_flc_T1) - lubridate::ymd(df03_FLC_self_report$date_flc_T0)))) %>% 
  mutate(program_duration_by_weak = as.numeric((lubridate::ymd(df03_FLC_self_report$date_flc_T1) - lubridate::ymd(df03_FLC_self_report$date_flc_T0))/7))

# Abnormal case <8 wks program
a2 <- a2 %>% filter(program_duration_by_weak >= 8)

# [Duplicated id, date_flc_T0, 2420320宜婷: 可能是營養師加入班級見習，兩筆都不是正式data。]
#[*把重複的人刪掉]
a2 <- a2 %>% filter(!(id %in% (a2[which(duplicated(a2[c("id", "date_flc_T0")]) | duplicated(a2[c("id", "date_flc_T0")], fromLast = TRUE)), "id"])))
# a2 <- a2 %>% distinct(id, date_flc_T0, .keep_all = TRUE)

# -- debug line begins

# [Duplicated id, date_flc_T0, 2420320宜婷: 可能是營養師加入班級見習，兩筆都不是正式data。]
# a2[which(duplicated(a2[c("id", "date_flc_T0")]) | duplicated(a2[c("id", "date_flc_T0")], fromLast = TRUE)),] %>% view()

# -- debug line ends

# [參與多次課程] set multiple = "all"
# a3: aggregate Weight / Diet / Program info
a3 <- left_join(a1, a2, by = "id", multiple = "all")

tmp <- df03_FLC_self_report %>% select(
  c(
    "class_id",
    "class_name",
    "program",
    "program_gp",
    "date_flc_T0",
    "date_flc_T1",
    "nutritionist_online",
    "id",
    "date_latest_update",
    "btd",
    "gender",
    "height",
    "age"
  )
)

# -- debug line begins

# [Duplicated id, date_flc_T0, 2420321宜婷: ]
# tmp[which(duplicated(tmp[c("id", "date_flc_T0")]) | duplicated(tmp[c("id", "date_flc_T0")], fromLast = TRUE)),] %>% view()

# -- debug line ends

# filter out可能是測試資料 12人共24 raws
tmp <- tmp %>% filter(!(id %in% (tmp[which(duplicated(tmp[c("id", "date_flc_T0")]) | duplicated(tmp[c("id", "date_flc_T0")], fromLast = TRUE)), "id"])))


tmp <- left_join(tmp, a3, by = c("id", "date_flc_T0", "date_flc_T1"), multiple = "all")


rm(list = c("a1", "a2", "a1_weight", "a1_diet"))
# Set start date cut-off for analysis

a3 <- tmp

a3 <-
  a3 %>% filter(!(is.na(date)) &
                  !(is.na(date_flc_T0)) & (date_flc_T0 >= "2017-01-01"))

# [random sampling-check] Good to go!
# a3[sample(a3 %>% nrow(), 10),] %>% select(id, date, calorie_day) %>% view()



# Performance of Entire Program [TBD] -------------------------------------------


a3 %>% filter( (date >= date_flc_T0) & (date <= date_flc_T1) ) %>% filter((date_flc_T1 <= Sys.Date()))

## Weight Record
# !!2Do

## Diet Record
# !!2Do






# Performance of 8-week Program -------------------------------------------
df03_FLC_self_report_w8df <- 
  a3 %>% filter( (date >= date_flc_T0) & (date <= date_flc_w8) ) %>% filter((date_flc_w8 <= Sys.Date()))

df03_FLC_self_report_w8df <- df03_FLC_self_report_w8df %>% mutate(day_no = as.numeric((lubridate::ymd(df03_FLC_self_report_w8df$date) - as.numeric((lubridate::ymd(df03_FLC_self_report_w8df$date_flc_T0)))))) %>% 
  mutate(week_no = (day_no/7) %>% ceiling())

## Weight Record

a1_profile <- 
  df03_FLC_self_report_w8df %>% select(
    c(
      "class_id",
      "class_name",
      "program",
      "program_gp",
      "date_flc_T0",
      "date_flc_T1",
      "date_flc_w8",
      "nutritionist_online",
      "id",
      "date_latest_update",
      "btd",
      "gender",
      "height",
      "age"
    )
  ) %>% distinct(id, date_flc_T0, .keep_all = TRUE)

  
a1_weight <- 
df03_FLC_self_report_w8df %>% select(
  c(
    "date_flc_T0",
    "id",
    "date",
    "weight",
    "bmi",
    "waist_circumference",
    "body_fat_mass",
    "program_duration_by_day",
    "program_duration_by_weak",
    "day_no",
    "week_no"
  )
) %>% filter(!is.na(weight)) %>%
  group_by(id, date_flc_T0) %>% 
  summarise(
    `weight(T0)` = first(weight, na_rm = TRUE),
    `weight(T1)` = last(weight, na_rm = TRUE),
    `∆weight` = `weight(T1)`-`weight(T0)`,
    `∆weight%` = ((`weight(T1)`-`weight(T0)`)/`weight(T0)`*100) %>% round(2),
    
    `BMI(T0)` = first(bmi, na_rm = TRUE),
    `BMI(T1)` = last(bmi, na_rm = TRUE),
    `∆BMI` = `BMI(T1)`-`BMI(T0)`,
    `∆BMI%` = ((`BMI(T1)`-`BMI(T0)`)/`BMI(T0)`*100) %>% round(2),

    `Fat(T0)` = first(body_fat_mass, na_rm = TRUE),
    `Fat(T1)` = last(body_fat_mass, na_rm = TRUE),
    `∆Fat` = `Fat(T1)`-`Fat(T0)`,
    `∆Fat%` = ((`Fat(T1)`-`Fat(T0)`)/`Fat(T0)`*100) %>% round(2),
    
        
    `wc(T0)` = first(waist_circumference, na_rm = TRUE),
    `wc(T1)` = last(waist_circumference, na_rm = TRUE),
    `∆wc` = `wc(T1)`-`wc(T0)`,
    `∆wc%` = ((`wc(T1)`-`wc(T0)`)/`wc(T0)`*100) %>% round(2),
    
    `day_no(T0)_weight` = first(day_no, na_rm = TRUE),
    `day_no(T1)_weight` = last(day_no, na_rm = TRUE),
    `week_no(T0)_weight` = first(week_no, na_rm = TRUE),
    `week_no(T1)_weight` = last(week_no, na_rm = TRUE),
    
  )



## Diet Record

a1_diet <- 
df03_FLC_self_report_w8df %>% select(
  c(
    "date_flc_T0",
    "id",
    "date",
    "note_counts",
    "pic_counts",
    "light_green_count",
    "light_yellow_count",
    "light_red_count",
    "carb_e_day",
    "protein_e_day",
    "fat_e_day",
    "calorie_day",
    "carbohydrate_target",
    "protein_target",
    "fat_target",
    "calorie_target",
    "grains_day",
    "meat_bean_day",
    "oil_day",
    "vegetables_day",
    "fruits_day",
    "milk_day",
    "grains_target",
    "meat_bean_target",
    "oil_target",
    "vegetables_target",
    "fruits_target",
    "milk_target",
    "grains_day_deficit",
    "meat_bean_day_deficit",
    "oil_day_deficit",
    "vegetables_day_deficit",
    "fruits_day_deficit",
    "milk_day_deficit",
    "carb_e_day_deficit",
    "protein_e_day_deficit",
    "fat_e_day_deficit",
    "calorie_deficit",
    "program_duration_by_day",
    "program_duration_by_weak",
    "day_no",
    "week_no"
  )
) %>% filter(!is.na(note_counts)) %>%
  group_by(id, date_flc_T0) %>% 
  summarise(
    upload_day_p = (n() *100 / (program_duration_by_day + 1)) %>% round(2), 
    note_counts = sum(note_counts, na.rm = TRUE),
    pic_count = sum(pic_counts, na.rm = TRUE),
    
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
    milk_day_deficit = (sum(milk_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),
    
    `day_no(T0)_diet` = first(day_no, na_rm = TRUE),
    `day_no(T1)_diet` = last(day_no, na_rm = TRUE),
    `week_no(T0)_diet` = first(week_no, na_rm = TRUE),
    `week_no(T1)_diet` = last(week_no, na_rm = TRUE),
    
  ) %>% distinct(id, .keep_all = TRUE)

colnames(a1_diet) <- gsub("_tmp", colnames(a1_diet), replacement = "")

names(a1_diet) <- names(a1_diet) %>% lin_ch_en_format(format = "en", origin = "raw_en")


## [Client w/t weight record]
# setdiff(a1_diet$id, a1_weight$id)

a1_profile <- left_join(
  a1_profile,
  a1_weight,
  by = c(
    "id",
    "date_flc_T0")
)

a1_profile <- left_join(
  a1_profile,
  a1_diet,
  by = c(
    "id",
    "date_flc_T0")
)


# !![Well-Cleaned df]
df03_FLC_self_report_w8df <- a1_profile

# [NaN -> NA Conversion]: 沒有燈號、沒有設定目標(e.g., 48361)
df03_FLC_self_report_w8df <- df03_FLC_self_report_w8df %>%
  mutate_if(is.numeric, ~ ifelse(is.nan(.), NA, .)) %>% 
  mutate_if(is.numeric, ~ ifelse(is.infinite(.), NA, .))

df03_FLC_self_report_w8df <-
df03_FLC_self_report_w8df %>% filter((`∆weight%` > quantile(df03_FLC_self_report_w8df$`∆weight%`, 0.01, na.rm = TRUE)) & (`∆weight%` < quantile(df03_FLC_self_report_w8df$`∆weight%`, 0.99, na.rm = TRUE)))




# Combination Tag of weight/diet upload types: "week_1st/week_last" 


## [經典八週] -------------------------------------------
# by week

df03_FLC_self_report_w8df_lv_entry <- df03_FLC_self_report_w8df %>% filter(program_gp == "經典八週")


tab <- table(T0 = df03_FLC_self_report_w8df_lv_entry$`week_no(T0)_weight`, T1 = df03_FLC_self_report_w8df_lv_entry$`week_no(T1)_weight`)

tab1 <- tab %>% prop.table() %>% round(4)
tab1 <- as.data.frame(tab1, stringsAsFactors = TRUE)

tab <- as.data.frame(tab, stringsAsFactors = TRUE)
tab$Percent <- tab1$Freq

tab %>% 
  filter(Freq != 0) %>% 
  ggplot(aes(T0, T1, fill = Percent)) +
  geom_tile() +
  # geom_text(aes(label = scales::percent(Percent))) +
  geom_text(aes(label = paste(Freq, scales::percent(Percent), sep = "\n") )) +
  # scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "OrRd")) +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red", midpoint = .185) +
  # scale_fill_gradient2(low = "white", mid = "yellow", high = "red", midpoint = .185) +
  # scale_fill_gradient(low = "white", high = "darkred") +
  labs(x = "1st Record", y = "Last Record", title = "Confusion Matrix of Weight Record\n經典八週",
       fill = "分佈佔比") +
  theme_classic() +
  theme(plot.title = element_text(size = 25, hjust = 0.5, 
                                  margin = margin(20, 0, 20, 0)),
        legend.title = element_text(size = 14, margin = margin(0, 20, 10, 0)),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 18),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 18))


tab <- tab %>% mutate(wk_diff = as.numeric(T1) - as.numeric(T0))
tab %>% 
  group_by(wk_diff) %>% 
  summarise(
    Freq = sum(Freq),
    Percent = sum(Percent)*100 %>% round(3)
  ) %>% 
  as.data.frame() %>% 
  filter(Freq != 0) %>% 
  mutate(output = paste0(Freq, " (", Percent, "%)"))


# by day  
tab <- table(T0 = df03_FLC_self_report_w8df_lv_entry$`day_no(T0)_weight`, T1 = df03_FLC_self_report_w8df_lv_entry$`day_no(T1)_weight`)

tab1 <- tab %>% prop.table() %>% round(4)
tab1 <- as.data.frame(tab1, stringsAsFactors = TRUE)

tab <- as.data.frame(tab, stringsAsFactors = TRUE)
tab$Percent <- tab1$Freq

tab %>% 
  filter(Freq != 0) %>% 
  ggplot(aes(T0, T1, fill = Percent)) +
  geom_tile() +
  # geom_text(aes(label = scales::percent(Percent))) +
  # geom_text(aes(label = paste(Freq, scales::percent(Percent), sep = "\n") )) +
  scale_fill_gradientn(colours = c("#BFFF70", "#FFFF40",
                                   "#FFD000", "#FFA000", "#FF7000", "#FF4000", "#FF0000")) +
  # scale_fill_gradient(low = "white", high = "#3575b5") +
  labs(x = "1st Record", y = "Last Record", title = "Confusion matrix of Weight Record\n經典八週",
       fill = "分佈佔比") +
  theme_classic() +
  theme(plot.title = element_text(size = 25, hjust = 0.5, 
                                  margin = margin(20, 0, 20, 0)),
        legend.title = element_text(size = 14, margin = margin(0, 20, 10, 0)),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 18),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 18))

tab <- tab %>% mutate(day_diff = as.numeric(T1) - as.numeric(T0))
tab %>% 
  group_by(day_diff) %>% 
  summarise(
    Freq = sum(Freq),
    Percent = sum(Percent)*100 %>% round(3)
  ) %>% 
  as.data.frame() %>% 
  filter(Freq != 0) %>% 
  mutate(output = paste0(Freq, " (", Percent, "%)"))



df03_FLC_self_report_w8df_lv_entry <- df03_FLC_self_report_w8df_lv_entry %>% mutate(wk_diff = ((as.numeric(`day_no(T1)_weight`) - as.numeric(`day_no(T0)_weight`))/7) %>% ceiling())
# df03_FLC_self_report_w8df_lv_entry$wk_diff %>% table()

df03_FLC_self_report_w8df_lv_entry %>% 
  mutate(weight_p = -`∆weight%`) %>% 
  group_by(wk_diff) %>% 
  summarise(
    weight_p = mean(weight_p, na.rm = T),
    N = n()
  )

df03_FLC_self_report_w8df_lv_entry %>% 
  mutate(weight_p = -`∆weight%`) %>% 
  ggbarplot(x = "wk_diff", y = "weight_p", fill = "wk_diff", alpha = 0.5,
            add = "mean_se", add.params = list(group = "wk_diff"),
            position = position_dodge(0.8), legend = "none", legend.title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "週數差", y = "Mean ± SE", title = "減重成效(%)\nProgram:經典八週") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15)
  ) 



### 01. Pie chart -----------------------------------------------------------
# [Issue: 有部分人只有飲食記錄，沒有體重紀錄]
df03_FLC_self_report <- df03_FLC_self_report_w8df_lv_entry %>% filter(!is.na(`weight(T0)`))

#Age
df03_FLC_self_report$age_gp <- cut(df03_FLC_self_report$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
df03_FLC_self_report <- df03_FLC_self_report %>% filter(!is.na(age_gp))
pie_flc_01_entry <- 
  df03_FLC_self_report %>% group_by(age_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Age',
                                                                                                   legend = "{position:'right'}",
                                                                                                   pieHole = 0.5,
                                                                                                   #slices = "{1:{offset:0.1}}",
                                                                                                   backgroundColor = "#f9fffb",
                                                                                                   width = "600",
                                                                                                   height = "400"))

#Gender
pie_flc_02_entry <- 
  df03_FLC_self_report %>% group_by(gender) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Gender',
                                                                                                   legend = "{position:'right'}",
                                                                                                   pieHole = 0.5,
                                                                                                   #slices = "{0:{offset:0.1}}",
                                                                                                   backgroundColor = "#f9fffb",
                                                                                                   colors = "['#DC3912', '#3366CC']",
                                                                                                   width = "600",
                                                                                                   height = "400"))



#BMI x Obesity

df03_FLC_self_report$bmi_gp <- cut(df03_FLC_self_report$`BMI(T0)`, c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))

pie_flc_03_entry <- 
  df03_FLC_self_report %>% filter(!is.na(bmi_gp)) %>% filter(gender == "male") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Male',
                                                                                                                                                           legend = "{position:'right'}",
                                                                                                                                                           pieHole = 0.5,
                                                                                                                                                           #slices = "{2:{offset:0.1}}",
                                                                                                                                                           backgroundColor = "#f9fffb",
                                                                                                                                                           width = "600",
                                                                                                                                                           height = "400"))

pie_flc_04_entry <- 
  df03_FLC_self_report %>% filter(!is.na(bmi_gp)) %>% filter(gender == "female") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Female',
                                                                                                                                                             legend = "{position:'right'}",
                                                                                                                                                             pieHole = 0.5,
                                                                                                                                                             #slices = "{1:{offset:0.1}}",
                                                                                                                                                             backgroundColor = "#f9fffb",
                                                                                                                                                             width = "600",
                                                                                                                                                             height = "400"))






### 02. Cor ---------------------------------------------------------------------


#[Create profile]  Efficacy, Baseline, Diet table
profile_efficacy <- df03_FLC_self_report %>% 
  select(c("∆weight","∆weight%","∆BMI","∆BMI%","∆Fat","∆Fat%","∆wc","∆wc%"))

names(profile_efficacy) <- names(profile_efficacy) %>% lin_ch_en_format(format = "ch", origin = "en")
# 
# profile_baseline <- df03_FLC_self_report %>% 
#   select(c("age", "weight(T0)","weight(T1)","BMI(T0)","BMI(T1)","Fat(T0)","Fat(T1)","wc(T0)","wc(T1)"))
# 
# names(profile_baseline) <- names(profile_baseline) %>% lin_ch_en_format(format = "ch", origin = "en")

profile_diet <- df03_FLC_self_report %>%
  select(c("age", "weight(T0)","weight(T1)","BMI(T0)","BMI(T1)","Fat(T0)","Fat(T1)","wc(T0)","wc(T1)","upload_day_%", "pic_count","calorie_day","carb_E%","protein_E%","fat_E%","light_G_%","light_Y_%","light_R_%"))

names(profile_diet) <- names(profile_diet) %>% lin_ch_en_format(format = "ch", origin = "en")


library(corrplot)
#[Correlation r] Efficacy x Diet
M1_flc <- cor(cbind(-profile_efficacy, profile_diet), use = "pairwise.complete.obs")
#[2Do]change row,col names into chinese
M_test1_flc <- cor.mtest(cbind(-profile_efficacy, profile_diet) , conf.level = .95)
M_col_flc <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))


#run corrplot plot
corrplot(M1_flc,
         p.mat = M_test1_flc$p,
         type = "lower",
         insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
         tl.col = "black", tl.srt = 35, tl.cex = 1.0,
         cl.ratio = 0.1,
         col = M_col_flc(200),
         title = "[Correlation] Efficacy x Diet",
         #c(bottom, left, top, right)
         mar = c(0,0,1,0))

for (i in c(1:length(colnames(M1_flc)))) {
  if (i == 1) {
    M1_flc_value <- M1_flc %>% round(2) 
    M1_flc_sign <- M_test1_flc$p %>% apply(c(1,2), function(x) {
      stats::symnum(x, corr = FALSE, na = FALSE, 
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("***", "**", "*", ".", "ns"))
    })
    
    M1_flc_df <- M1_flc_value
    M1_flc_df[,] <- NA
  }
  
  M1_flc_df[,i] <- paste0(M1_flc_value[,i], " (", M1_flc_sign[,i], ")")
  
  if (i ==  length(colnames(M1_flc))) {
    rm(list = c("M1_flc_value", "M1_flc_sign"))
    M1_flc_df <- M1_flc_df %>% as.data.frame()
    M1_flc_df <- M1_flc_df %>% add_column(vars = rownames(M1_flc_df), .before = names(M1_flc_df)[1])
    M1_flc_df <- M1_flc_df %>% add_column("#" = seq(1, nrow(M1_flc_df)), .before = names(M1_flc_df)[1])
  }
} 

cor_table_flc_01 <- M1_flc_df %>% gvisTable(options=list(frozenColumns = 2,
                                                         width="150%",height=300))



### 03.  Strat.  Weight loss% -----------------------------------------------


##T0,T1,∆ plot

df03_FLC_self_report$`∆weight%` %>% summary()

# Method 1: 按照data散佈百分比. e.g., Q1~Q4
# df03_FLC_self_report$gp <- df03_FLC_self_report$`∆weight%` %>% cut(breaks = 3, labels = c("Good","Medium","Poor"))
df03_FLC_self_report$gp <- df03_FLC_self_report$`∆weight%` %>% cut(breaks = c(-Inf, -8, -4, Inf), labels = c("Good","Medium","Poor"))
df03_FLC_self_report$gp <- factor(df03_FLC_self_report$gp, levels = (c("Good","Medium","Poor") %>% rev()) )


# df03_FLC_self_report$`∆weight%` %>% cut(breaks = 3, labels = c())
# Method 2: makes n groups with (approximately) equal numbers of observations;
# df03_FLC_self_report$gp <- df03_FLC_self_report$`∆weight%` %>% cut_number(3, labels = c("Good","Medium","Poor"))
df03_FLC_self_report$gp %>% table()

datasets_target_issue <- df03_FLC_self_report %>% dplyr::rename(gp = gp)
datasets_target_issue <- datasets_target_issue %>% filter(gp %in% levels(datasets_target_issue$gp))


#profile
vars_en <- c("id","age","gender","date_flc_T0","date_flc_T1",
             #inbody - baseline
             "weight(T0)","BMI(T0)","Fat(T0)","wc(T0)",
             #inbody - endpoint
             "weight(T1)","BMI(T1)","Fat(T1)","wc(T1)",
             #diet
             "upload_day_%","pic_count","carb_E%","protein_E%","fat_E%","calorie_day","light_G_%","light_Y_%","light_R_%",
             #others
             "gp",
             #inbody - ∆
             "∆weight","∆BMI","∆Fat","∆wc",
             #inbody - ∆%
             "∆weight%","∆BMI%","∆Fat%","∆wc%"
)

datasets_target_issue <- datasets_target_issue %>% select(vars_en)

vars_en <- lin_ch_en_format(x = vars_en, format = "en", origin = "raw_en")
names(datasets_target_issue) <- lin_ch_en_format(x = names(datasets_target_issue), format = "en", origin = "raw_en")



##Improvement: positive
datasets_target_issue_a <- datasets_target_issue %>% select(-grep("∆", names(datasets_target_issue))) 
##Improvement: negative (減少越多，越往上長)
datasets_target_issue_b <- datasets_target_issue %>% select(grep("∆", names(datasets_target_issue), value = TRUE)) %>% multiply_by(-1)

datasets_target_issue <- Reduce(cbind,list(datasets_target_issue_a, datasets_target_issue_b), accumulate =FALSE) 

#order again!!
datasets_target_issue <- datasets_target_issue %>% select(vars_en)

#change colname to run plot
datasets_target_issue_for_plot <- datasets_target_issue

names(datasets_target_issue_for_plot) <- gsub("∆", "delta_", names(datasets_target_issue_for_plot))
names(datasets_target_issue_for_plot) <- gsub("%", "_percent", names(datasets_target_issue_for_plot))
names(datasets_target_issue_for_plot) <- gsub("\\(", "_", names(datasets_target_issue_for_plot))
names(datasets_target_issue_for_plot) <- gsub("\\)", "_", names(datasets_target_issue_for_plot))

#set output plot order
var_vector <- c(vars_en %>% grep("T0)$", .),
                vars_en %>% grep("T1)", .),
                vars_en %>% grep("T0)|T1)|[∆]|id|gender|gp|date", ., invert = TRUE),
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

#[customized part!!!]å
myplots_flc_entry <- vector('list', length(var_vector))

for (i in c(var_vector)) {
  j <- match(i, var_vector)
  if (j == 1) {
    vector_pvalue <- c()
    start_time <- Sys.time()
  }
  
  a <- datasets_target_issue_for_plot %>% colnames() %>% head(i) %>% tail(1)
  a_title <- myplot_table[myplot_table$num == j, "vars_ch"]
  
  
  #p.sign?
  stat.test <- 
    datasets_target_issue_for_plot %>%
    group_by(gender) %>%
    #[customized part!!!]
    rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ "))) 
  stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
  
  #for customed summary table - part 1/4 [p value]
  vector_pvalue <- append(vector_pvalue, 
                          stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
  )
  
  #plot
  plot <- 
    datasets_target_issue_for_plot %>% 
    # ggbarplot(x = "gender", y = a, fill = "gp", palette = c("#dce5f6","#fdf7d6","#ffe6cd") %>% rev(), alpha = 1.0,
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
  
  #[customized part!!!]
  myplots_flc_entry[[j]] <- plot
  
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}

rm(list = c("datasets_target_issue_a", "datasets_target_issue_b"))

#(2.)gender x Group table
#[customized part!!!]
table_01_flc_entry <- 
  table(datasets_target_issue$gender, datasets_target_issue$gp) %>% addmargins() %>% 
  kable(format = "html", caption = "<b>Table: Population</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Note:"), general = c(rbind("\n", c("- Poor: Less than 4%", "- Medium: Between 4~8%", "- Good: More than 8%", "- Program: 經典八週(202109新版)享瘦班, 經典八週, 2023FLC-2個助教,宋醫師專班-FLC班"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)



#(3.)output statistics table
#for customed summary table [summary table]
#[customized part!!!]
summary_table_flc_entry <- 
  datasets_target_issue %>% 
  group_by(gender, gp) %>% 
  summarize_at(vars_en[var_vector],
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
  )




#rbind: summary_table, p.adj.sign, dif, improvement
summary_table_flc <- cbind(summary_table_flc_entry %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t(), as.data.frame(vector_pvalue))

colnames(summary_table_flc) <-
  c(paste0((levels(datasets_target_issue_for_plot$gp) %>% as.character())[-6],
           paste0("\n(n=",table(datasets_target_issue_for_plot$gp, datasets_target_issue_for_plot$gender) %>% as.numeric(),")")), "顯著差異")
rownames(summary_table_flc) <- myplot_table$vars_ch

#[customized part!!!]
table_02_flc_entry <- 
  summary_table_flc %>% 
  kbl(format = "html", caption = "<b>Statistics:</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1, "Female" = length(levels(datasets_target_issue$gp)), "Male" = length(levels(datasets_target_issue$gp))," " = 1)) %>% 
  footnote(general_title = c("Significance:"), general = "\n Comparison: Good vs. Poor in female population.",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%")




## [進階計畫] -------------------------------------------
# by week

df03_FLC_self_report_w8df_lv_adv <- df03_FLC_self_report_w8df %>% filter(program_gp == "進階計畫")


tab <- table(T0 = df03_FLC_self_report_w8df_lv_adv$`week_no(T0)_weight`, T1 = df03_FLC_self_report_w8df_lv_adv$`week_no(T1)_weight`)

tab1 <- tab %>% prop.table() %>% round(4)
tab1 <- as.data.frame(tab1, stringsAsFactors = TRUE)

tab <- as.data.frame(tab, stringsAsFactors = TRUE)
tab$Percent <- tab1$Freq

tab %>% 
  filter(Freq != 0) %>% 
  ggplot(aes(T0, T1, fill = Percent)) +
  geom_tile() +
  # geom_text(aes(label = scales::percent(Percent))) +
  geom_text(aes(label = paste(Freq, scales::percent(Percent), sep = "\n") )) +
  # scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "OrRd")) +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red", midpoint = .185) +
  # scale_fill_gradient2(low = "white", mid = "yellow", high = "red", midpoint = .185) +
  # scale_fill_gradient(low = "white", high = "darkred") +
  labs(x = "1st Record", y = "Last Record", title = "Confusion Matrix of Weight Record\n進階計畫",
       fill = "分佈佔比") +
  theme_classic() +
  theme(plot.title = element_text(size = 25, hjust = 0.5, 
                                  margin = margin(20, 0, 20, 0)),
        legend.title = element_text(size = 14, margin = margin(0, 20, 10, 0)),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 18),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 18))


tab <- tab %>% mutate(wk_diff = as.numeric(T1) - as.numeric(T0))
tab %>% 
  group_by(wk_diff) %>% 
  summarise(
    Freq = sum(Freq),
    Percent = sum(Percent)*100 %>% round(3)
  ) %>% 
  as.data.frame() %>% 
  filter(Freq != 0) %>% 
  mutate(output = paste0(Freq, " (", Percent, "%)"))


# by day  
tab <- table(T0 = df03_FLC_self_report_w8df_lv_adv$`day_no(T0)_weight`, T1 = df03_FLC_self_report_w8df_lv_adv$`day_no(T1)_weight`)

tab1 <- tab %>% prop.table() %>% round(4)
tab1 <- as.data.frame(tab1, stringsAsFactors = TRUE)

tab <- as.data.frame(tab, stringsAsFactors = TRUE)
tab$Percent <- tab1$Freq

tab %>% 
  filter(Freq != 0) %>% 
  ggplot(aes(T0, T1, fill = Percent)) +
  geom_tile() +
  # geom_text(aes(label = scales::percent(Percent))) +
  # geom_text(aes(label = paste(Freq, scales::percent(Percent), sep = "\n") )) +
  scale_fill_gradientn(colours = c("#BFFF70", "#FFFF40",
                                   "#FFD000", "#FFA000", "#FF7000", "#FF4000", "#FF0000")) +
  # scale_fill_gradient(low = "white", high = "#3575b5") +
  labs(x = "1st Record", y = "Last Record", title = "Confusion matrix of Weight Record\n進階計畫",
       fill = "分佈佔比") +
  theme_classic() +
  theme(plot.title = element_text(size = 25, hjust = 0.5, 
                                  margin = margin(20, 0, 20, 0)),
        legend.title = element_text(size = 14, margin = margin(0, 20, 10, 0)),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 18),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 18))

tab <- tab %>% mutate(day_diff = as.numeric(T1) - as.numeric(T0))
tab %>% 
  group_by(day_diff) %>% 
  summarise(
    Freq = sum(Freq),
    Percent = sum(Percent)*100 %>% round(3)
  ) %>% 
  as.data.frame() %>% 
  filter(Freq != 0) %>% 
  mutate(output = paste0(Freq, " (", Percent, "%)"))



df03_FLC_self_report_w8df_lv_adv <- df03_FLC_self_report_w8df_lv_adv %>% mutate(wk_diff = ((as.numeric(`day_no(T1)_weight`) - as.numeric(`day_no(T0)_weight`))/7) %>% ceiling())
# df03_FLC_self_report_w8df_lv_adv$wk_diff %>% table()

df03_FLC_self_report_w8df_lv_adv %>% 
  mutate(weight_p = -`∆weight%`) %>% 
  group_by(wk_diff) %>% 
  summarise(
    weight_p = mean(weight_p, na.rm = T),
    N = n()
  )

df03_FLC_self_report_w8df_lv_adv %>% 
  mutate(weight_p = -`∆weight%`) %>% 
  ggbarplot(x = "wk_diff", y = "weight_p", fill = "wk_diff", alpha = 0.5,
            add = "mean_se", add.params = list(group = "wk_diff"),
            position = position_dodge(0.8), legend = "none", legend.title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "週數差", y = "Mean ± SE", title = "減重成效(%)\nProgram:經典八週") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15)
  ) 



### 01. Pie chart -----------------------------------------------------------
# [Issue: 有部分人只有飲食記錄，沒有體重紀錄]
df03_FLC_self_report <- df03_FLC_self_report_w8df_lv_adv %>% filter(!is.na(`weight(T0)`))

#Age
df03_FLC_self_report$age_gp <- cut(df03_FLC_self_report$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
df03_FLC_self_report <- df03_FLC_self_report %>% filter(!is.na(age_gp))
pie_flc_01_adv <- 
  df03_FLC_self_report %>% group_by(age_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Age',
                                                                                                   legend = "{position:'right'}",
                                                                                                   pieHole = 0.5,
                                                                                                   #slices = "{1:{offset:0.1}}",
                                                                                                   backgroundColor = "#f9fffb",
                                                                                                   width = "600",
                                                                                                   height = "400"))

#Gender
pie_flc_02_adv <- 
  df03_FLC_self_report %>% group_by(gender) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Gender',
                                                                                                   legend = "{position:'right'}",
                                                                                                   pieHole = 0.5,
                                                                                                   #slices = "{0:{offset:0.1}}",
                                                                                                   backgroundColor = "#f9fffb",
                                                                                                   colors = "['#DC3912', '#3366CC']",
                                                                                                   width = "600",
                                                                                                   height = "400"))



#BMI x Obesity

df03_FLC_self_report$bmi_gp <- cut(df03_FLC_self_report$`BMI(T0)`, c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))

pie_flc_03_adv <- 
  df03_FLC_self_report %>% filter(!is.na(bmi_gp)) %>% filter(gender == "male") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Male',
                                                                                                                                                           legend = "{position:'right'}",
                                                                                                                                                           pieHole = 0.5,
                                                                                                                                                           #slices = "{2:{offset:0.1}}",
                                                                                                                                                           backgroundColor = "#f9fffb",
                                                                                                                                                           width = "600",
                                                                                                                                                           height = "400"))

pie_flc_04_adv <- 
  df03_FLC_self_report %>% filter(!is.na(bmi_gp)) %>% filter(gender == "female") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Female',
                                                                                                                                                             legend = "{position:'right'}",
                                                                                                                                                             pieHole = 0.5,
                                                                                                                                                             #slices = "{1:{offset:0.1}}",
                                                                                                                                                             backgroundColor = "#f9fffb",
                                                                                                                                                             width = "600",
                                                                                                                                                             height = "400"))






### 03.  Strat.  Weight loss% -----------------------------------------------


##T0,T1,∆ plot

df03_FLC_self_report$`∆weight%` %>% summary()

# Method 1: 按照data散佈百分比. e.g., Q1~Q4
# df03_FLC_self_report$gp <- df03_FLC_self_report$`∆weight%` %>% cut(breaks = 3, labels = c("Good","Medium","Poor"))
df03_FLC_self_report$gp <- df03_FLC_self_report$`∆weight%` %>% cut(breaks = c(-Inf, -8, -4, Inf), labels = c("Good","Medium","Poor"))
df03_FLC_self_report$gp <- factor(df03_FLC_self_report$gp, levels = (c("Good","Medium","Poor") %>% rev()) )


# df03_FLC_self_report$`∆weight%` %>% cut(breaks = 3, labels = c())
# Method 2: makes n groups with (approximately) equal numbers of observations;
# df03_FLC_self_report$gp <- df03_FLC_self_report$`∆weight%` %>% cut_number(3, labels = c("Good","Medium","Poor"))
df03_FLC_self_report$gp %>% table()

datasets_target_issue <- df03_FLC_self_report %>% dplyr::rename(gp = gp)
datasets_target_issue <- datasets_target_issue %>% filter(gp %in% levels(datasets_target_issue$gp))


#profile
vars_en <- c("id","age","gender","date_flc_T0","date_flc_T1",
             #inbody - baseline
             "weight(T0)","BMI(T0)","Fat(T0)","wc(T0)",
             #inbody - endpoint
             "weight(T1)","BMI(T1)","Fat(T1)","wc(T1)",
             #diet
             "upload_day_%","pic_count","carb_E%","protein_E%","fat_E%","calorie_day","light_G_%","light_Y_%","light_R_%",
             #others
             "gp",
             #inbody - ∆
             "∆weight","∆BMI","∆Fat","∆wc",
             #inbody - ∆%
             "∆weight%","∆BMI%","∆Fat%","∆wc%"
)

datasets_target_issue <- datasets_target_issue %>% select(vars_en)

vars_en <- lin_ch_en_format(x = vars_en, format = "en", origin = "raw_en")
names(datasets_target_issue) <- lin_ch_en_format(x = names(datasets_target_issue), format = "en", origin = "raw_en")



##Improvement: positive
datasets_target_issue_a <- datasets_target_issue %>% select(-grep("∆", names(datasets_target_issue))) 
##Improvement: negative (減少越多，越往上長)
datasets_target_issue_b <- datasets_target_issue %>% select(grep("∆", names(datasets_target_issue), value = TRUE)) %>% multiply_by(-1)

datasets_target_issue <- Reduce(cbind,list(datasets_target_issue_a, datasets_target_issue_b), accumulate =FALSE) 

#order again!!
datasets_target_issue <- datasets_target_issue %>% select(vars_en)

#change colname to run plot
datasets_target_issue_for_plot <- datasets_target_issue

names(datasets_target_issue_for_plot) <- gsub("∆", "delta_", names(datasets_target_issue_for_plot))
names(datasets_target_issue_for_plot) <- gsub("%", "_percent", names(datasets_target_issue_for_plot))
names(datasets_target_issue_for_plot) <- gsub("\\(", "_", names(datasets_target_issue_for_plot))
names(datasets_target_issue_for_plot) <- gsub("\\)", "_", names(datasets_target_issue_for_plot))

#set output plot order
var_vector <- c(vars_en %>% grep("T0)$", .),
                vars_en %>% grep("T1)", .),
                vars_en %>% grep("T0)|T1)|[∆]|id|gender|gp|date", ., invert = TRUE),
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

#[customized part!!!]å
myplots_flc_adv <- vector('list', length(var_vector))

for (i in c(var_vector)) {
  j <- match(i, var_vector)
  if (j == 1) {
    vector_pvalue <- c()
    start_time <- Sys.time()
  }
  
  a <- datasets_target_issue_for_plot %>% colnames() %>% head(i) %>% tail(1)
  a_title <- myplot_table[myplot_table$num == j, "vars_ch"]
  
  
  #p.sign?
  stat.test <- 
    datasets_target_issue_for_plot %>%
    group_by(gender) %>%
    #[customized part!!!]
    rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ "))) 
  stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
  
  #for customed summary table - part 1/4 [p value]
  vector_pvalue <- append(vector_pvalue, 
                          stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
  )
  
  #plot
  plot <- 
    datasets_target_issue_for_plot %>% 
    # ggbarplot(x = "gender", y = a, fill = "gp", palette = c("#dce5f6","#fdf7d6","#ffe6cd") %>% rev(), alpha = 1.0,
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
  
  #[customized part!!!]
  myplots_flc_adv[[j]] <- plot
  
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}

rm(list = c("datasets_target_issue_a", "datasets_target_issue_b"))

#(2.)gender x Group table
#[customized part!!!]
table_01_flc_adv <- 
  table(datasets_target_issue$gender, datasets_target_issue$gp) %>% addmargins() %>% 
  kable(format = "html", caption = "<b>Table: Population</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Note:"), general = c(rbind("\n", c("- Poor: Less than 4%", "- Medium: Between 4~8%", "- Good: More than 8%", "- Program: 進階計畫, 宋醫師進階計畫"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)



#(3.)output statistics table
#for customed summary table [summary table]
#[customized part!!!]
summary_table_flc_adv <- 
  datasets_target_issue %>% 
  group_by(gender, gp) %>% 
  summarize_at(vars_en[var_vector],
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
  )




#rbind: summary_table, p.adj.sign, dif, improvement
summary_table_flc <- cbind(summary_table_flc_adv %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t(), as.data.frame(vector_pvalue))

colnames(summary_table_flc) <-
  c(paste0((levels(datasets_target_issue_for_plot$gp) %>% as.character())[-6],
           paste0("\n(n=",table(datasets_target_issue_for_plot$gp, datasets_target_issue_for_plot$gender) %>% as.numeric(),")")), "顯著差異")
rownames(summary_table_flc) <- myplot_table$vars_ch

#[customized part!!!]
table_02_flc_adv <- 
  summary_table_flc %>% 
  kbl(format = "html", caption = "<b>Statistics:</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1, "Female" = length(levels(datasets_target_issue$gp)), "Male" = length(levels(datasets_target_issue$gp))," " = 1)) %>% 
  footnote(general_title = c("Significance:"), general = "\n Comparison: Good vs. Poor in female population.",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%")






# 04.  Importance of diet record & "Cofit Weight Loss Strategy" -----------

df03_FLC_self_report_w8df_lv_entry <- df03_FLC_self_report_w8df %>% filter(program_gp == "經典八週")

df03_FLC_self_report <- df03_FLC_self_report_w8df_lv_entry

df03_FLC_self_report <- df03_FLC_self_report %>% mutate(diet_compliance = (`light_G_%` * `upload_day_%`)/100 )

df03_FLC_self_report <- df03_FLC_self_report %>% filter((diet_compliance >= 0) & (diet_compliance <= 100))
# df03_FLC_self_report$gp_diet_compliance <- df03_FLC_self_report$diet_compliance %>% cut(breaks = 3, c("Low", "Medium", "High"))
# df03_FLC_self_report$gp_diet_compliance <- df03_FLC_self_report$`light_G_%` %>% cut(breaks = 3, c("Low", "Medium", "High"))
# df03_FLC_self_report$gp_diet_compliance <- df03_FLC_self_report$`light_G_%` %>% cut_number(n = 3, c("Low", "Medium", "High"))
# df03_FLC_self_report$gp_diet_compliance <- df03_FLC_self_report$`upload_day_%` %>% cut(breaks = 3, c("Low", "Medium", "High"))
df03_FLC_self_report$gp_diet_compliance <- df03_FLC_self_report$`upload_day_%` %>% cut_number(n = 3, c("Low", "Medium", "High"))
# df03_FLC_self_report$gp_diet_compliance <- df03_FLC_self_report$`carb_E%` %>% cut(breaks = 3, c("Low", "Medium", "High"))
# df03_FLC_self_report$gp_diet_compliance <- df03_FLC_self_report$`protein_E%` %>% cut(breaks = 3, c("Low", "Medium", "High"))
# df03_FLC_self_report$gp_diet_compliance <- df03_FLC_self_report$`fat_E%` %>% cut(breaks = 3, c("Low", "Medium", "High"))

df03_FLC_self_report %>% 
  group_by(gp_diet_compliance) %>% 
  summarise(
    weight = paste(mean(`∆weight%`, na.rm = TRUE) %>% round(1), (sd(`∆weight%`, na.rm = TRUE)/sqrt(n())) %>% round(1), sep = " ± "),
    n = n()
  )


df04_non_FLC_self_report$gp_diet_compliance <- df04_non_FLC_self_report$`upload_day_%` %>% cut_number(n = 3, c("Low", "Medium", "High"))
df04_non_FLC_self_report %>% 
  group_by(gp_diet_compliance) %>% 
  summarise(
    weight = paste(mean(`∆weight%`, na.rm = TRUE) %>% round(1), (sd(`∆weight%`, na.rm = TRUE)/sqrt(n())) %>% round(1), sep = " ± "),
    n = n()
  )


#record help weight loss w/ w/t education
a <- 
  rbind(df03_FLC_self_report %>% select(c("id","gender","weight(T0)","BMI(T0)","Fat(T0)","wc(T0)","weight(T1)","BMI(T1)","Fat(T1)","wc(T1)","∆weight","∆BMI","∆Fat","∆wc","∆weight%","∆BMI%","∆Fat%","∆wc%","age","upload_day_%")) %>%
          mutate(gp = "flc"),
        df04_non_FLC_self_report %>% select(c("id","gender","weight(T0)","BMI(T0)","Fat(T0)","wc(T0)","weight(T1)","BMI(T1)","Fat(T1)","wc(T1)","∆weight","∆BMI","∆Fat","∆wc","∆weight%","∆BMI%","∆Fat%","∆wc%","age","upload_day_%")) %>% 
          mutate(gp = "free_user")
  )
a$gp_diet_compliance <- a$`upload_day_%` %>% cut_number(n = 3, c("Low", "Medium", "High"))
# a$gp_diet_compliance <- a$`upload_day_%` %>% cut_number(n = 3, )
# a$gp_diet_compliance <- a$`upload_day_%` %>% cut_number(n = 3, )

b <- 
  a %>% 
  group_by(gp, gp_diet_compliance) %>% 
  summarise(
    weight = paste(mean(`∆weight%`, na.rm = TRUE) %>% round(1), (sd(`∆weight%`, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± "),
    n = n()
  ) %>% as.data.frame() %>% select(c(weight, n)) %>% t() %>% as.data.frame()
names(b) <- rep(levels(a$gp_diet_compliance), times = length(unique(a$gp)))

b %>% 
  kbl(format = "html", caption = "<b>Statistics:</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1, "FLC Program" = length(levels(a$gp_diet_compliance)), "Free User" = length(levels(a$gp_diet_compliance)))) %>% 
  footnote(general_title = c(""), general = "\n ",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%") 

stat.test <- 
  a %>% mutate(x = `∆weight%` %>% multiply_by(-1)) %>% 
  group_by(gp) %>%
  #[customized part!!!]
  rstatix::t_test(as.formula(paste("x", "gp_diet_compliance", sep = " ~ "))) 
stat.test <- stat.test %>% rstatix::add_xy_position(x = "gp", fun = "mean_se", dodge = 0.8)



#plot
plot_flc_ctrl_comparison <- 
  a %>% mutate(x = `∆weight%` %>% multiply_by(-1)) %>% 
  ggbarplot(x = "gp", y = "x", fill = "gp_diet_compliance", alpha = .5,
            add = "mean_se", add.params = list(group = "gp_diet_compliance"),
            position = position_dodge(0.8), legend = "right", legend.title = "Diet Score",
            label = TRUE, lab.nb.digits = 2, lab.vjust = -1.5
  ) +
  scale_x_discrete(labels = c("Cofit", "Others")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "", y = "Mean ± SE", title = "Weight Loss(%)") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15)
  ) +
  stat_pvalue_manual(
    stat.test, label = "p.adj.signif", tip.length = 0.0,
    bracket.nudge.y = 1, step.increase = 0.01, hide.ns = TRUE 
  )


rm(list = c("a", "b"))




# df03_FLC_self_report %>% 
#   mutate(delta_weight_p = `∆weight%` %>% multiply_by(-1)) %>%
#   mutate(light_G_p = `light_G_%` %>% multiply_by(1)) %>%
#   ggscatter(x = "light_G_p", y = "delta_weight_p",
#             color = "black",
#             fill = "red",
#             shape = 21,
#             size = 1, 
#             add = "reg.line",  # Add regressin line
#             add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
#             conf.int = TRUE, # Add confidence interval
#             title = "Correlation(Weight x Compliance)",
#             xlab = "Diet Compliance Score",
#             ylab = "Weight(%)",
#             # xlim = c(0, 13),
#             # ylim = c(0, 180),
#   ) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
#     axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
#     axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
#   ) +
#   # geom_vline(xintercept = c(5.5),linetype ="dashed", ) +
#   # annotate("text", x=5.3, y=155, label="Cutoff = 5.5 mg/dL", angle=90) +
#   stat_cor(method = "pearson", size = 5, label.x = 0, label.y = 10) # Add correlation coefficient)
# 


# 05. MainPage ----------------------------------------------------------------

##main page of cofit
cofit_stat_category <- factor(levels = (c("Total", "Completed", "In Progress")))

cofit_main_pagedf <- df01_profile %>% filter((org_name %in% c("cofit")))

cofit_main_pagedf <- cofit_main_pagedf %>% mutate(date_cate = date_t0 %>% lubridate::floor_date(unit = "month"))
cofit_main_pagedf <- cofit_main_pagedf %>% mutate(date_finish = date_t1 %>% lubridate::floor_date(unit = "month"))

cofit_client_stat_df_tmp <- data.frame(date = rep(seq(as.Date(cofit_main_pagedf$date_cate %>% unique() %>% min()), as.Date(cofit_main_pagedf$date_cate %>% unique() %>% max()), by = "month")))

cofit_client_stat_df <- 
  left_join(cofit_main_pagedf %>% 
              group_by(date_cate, .drop = FALSE) %>% 
              summarise(
                n = n()
              ) %>% dplyr::rename(date = date_cate),
            cofit_main_pagedf %>% 
              group_by(date_finish, .drop = FALSE) %>% 
              summarise(
                n = n()
              ) %>% dplyr::rename(date = date_finish),
            by = c("date"),
  ) %>% lin_exclude_NA_col(., variables = c("date"))

cofit_client_stat_df <- cofit_client_stat_df %>% dplyr::rename(class_buy = n.x, class_finish = n.y)

cofit_client_stat_df <- merge(cofit_client_stat_df, cofit_client_stat_df_tmp, by.y = c("date"), all.y = TRUE) 
cofit_client_stat_df[is.na(cofit_client_stat_df)] <- 0
rm(cofit_client_stat_df_tmp)



#Establish auxiliary df
cofit_client_stat_df$class_buy_cumsum_all <- cofit_client_stat_df$class_buy %>% cumsum()
cofit_client_stat_df$class_finish_cumsum_all <- cofit_client_stat_df$class_finish %>% cumsum()
cofit_client_stat_df <- cofit_client_stat_df %>% mutate(class_ongoing_all = class_buy_cumsum_all - class_finish_cumsum_all)

cofit_client_stat_df$class_buy_cumsum_sub <- c(cofit_client_stat_df %>%  select(class_buy) %>% pull() %>% cumsum())

cofit_client_stat_df$class_finish_cumsum_sub <- c(cofit_client_stat_df %>%  select(class_finish) %>% pull() %>% cumsum())
cofit_client_stat_df <- cofit_client_stat_df %>% mutate(class_ongoing_sub = class_buy_cumsum_sub - class_finish_cumsum_sub)


#Establish dashboard df
cofit_accum_client_df <- data.frame(date = rep(seq(as.Date(cofit_main_pagedf$date_cate %>% unique() %>% min()), as.Date(cofit_main_pagedf$date_cate %>% unique() %>% max()), by = "month"), each = levels(cofit_stat_category) %>% length()), 
                                    category = rep(levels(cofit_stat_category), seq(as.Date(cofit_main_pagedf$date_cate %>% unique() %>% min()), as.Date(cofit_main_pagedf$date_cate %>% unique() %>% max()), by = "month") %>% length()),
                                    value = rep(NA, (seq(as.Date(cofit_main_pagedf$date_cate %>% unique() %>% min()), as.Date(cofit_main_pagedf$date_cate %>% unique() %>% max()), by = "month") %>% length())*(levels(cofit_stat_category) %>% length())),
                                    anno_title = rep(NA, (seq(as.Date(cofit_main_pagedf$date_cate %>% unique() %>% min()), as.Date(cofit_main_pagedf$date_cate %>% unique() %>% max()), by = "month") %>% length())*(levels(cofit_stat_category) %>% length())),
                                    anno_text = rep(NA, (seq(as.Date(cofit_main_pagedf$date_cate %>% unique() %>% min()), as.Date(cofit_main_pagedf$date_cate %>% unique() %>% max()), by = "month") %>% length())*(levels(cofit_stat_category) %>% length())))

#fill in cofit_accum_client_df
cofit_accum_client_df[cofit_accum_client_df$category == "Total", "value"] <- cofit_client_stat_df[["class_buy_cumsum_all"]]
cofit_accum_client_df[cofit_accum_client_df$category == "Completed", "value"] <- cofit_client_stat_df[["class_finish_cumsum_sub"]]
cofit_accum_client_df[cofit_accum_client_df$category == "In Progress", "value"] <- cofit_client_stat_df[["class_ongoing_sub"]]


rm(list = c("cofit_stat_category", "cofit_client_stat_df"))

cofit_client_monthly_stat_report_total_client <- cofit_accum_client_df %>% filter(category == "Total") %>% select(value) %>% max(na.rm = TRUE)
cofit_client_monthly_stat_report <- googleVis::gvisAnnotationChart(cofit_accum_client_df,
                                                                   datevar = "date",
                                                                   numvar = "value",
                                                                   idvar = "category",
                                                                   titlevar = "anno_title",
                                                                   annotation = "anno_text",
                                                                   date.format = "%Y/%m/%d",
                                                                   options=list(
                                                                     displayAnnotations = TRUE,
                                                                     #chart = "{chartArea:{backgroundColor:'#003b70'}}",
                                                                     legendPosition='newRow',
                                                                     # width = 800, height = 350,
                                                                     width = "100%", height = 350,
                                                                     gvis.editor = "[選項]:圖表轉換", displayAnnotations = FALSE
                                                                   ))
#output
##cofit_client_monthly_stat_report %>% plot()



# 06.  Marketing Effectiveness Number -------------------------------------

## Establish dataset focus on ∆weight -----------------------------------------------------------------------
marketing_df <- tmp_03

#tmp
marketing_df <- marketing_df %>% select(-c(age, measurement_after_program_date, measurement_before_program_date))

#C1. col_names
names(marketing_df) <- names(marketing_df) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
#C1-2. filter by program not "^診所"
marketing_df <- marketing_df[marketing_df[["program"]] %>% grepl("^診所",.) %>% not(),]

marketing_df <- marketing_df[with(marketing_df, order(date_flc_T0, id)),]

#C2. age: btd - date_t0 年齡(療程起始當天計算)
marketing_df$age <- (lubridate::ymd(marketing_df$date_flc_T0) - lubridate::ymd(marketing_df$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()
#C2-2. carb/protein/fat E%:
marketing_df <- marketing_df %>% mutate(carb_ep = (carbohydrate*4 / (carbohydrate*4 + protein*4 + fat*9) ))
marketing_df <- marketing_df %>% mutate(protein_ep = (protein*4 / (carbohydrate*4 + protein*4 + fat*9) ))
marketing_df <- marketing_df %>% mutate(fat_ep = (fat*9 / (carbohydrate*4 + protein*4 + fat*9) ))
#C2-3. upload_day_%:
marketing_df <- marketing_df %>% mutate(upload_day_p = (as.numeric(day_count) / as.numeric((lubridate::ymd(date_flc_T1) - (lubridate::ymd(date_flc_T0)) + 1))))
names(marketing_df) <- names(marketing_df) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
#C2-3. BMI
marketing_df$height <- marketing_df$height %>% as.numeric()
marketing_df$`BMI(T0)` <- (marketing_df$`weight(T0)`/ (marketing_df$height/100)^2) %>% round(1)
marketing_df$`BMI(T1)` <- (marketing_df$`weight(T1)`/ (marketing_df$height/100)^2) %>% round(1)
marketing_df$`∆BMI` <- (marketing_df$`BMI(T1)` - marketing_df$`BMI(T0)`)
marketing_df$`∆BMI%` <- (marketing_df$`BMI(T1)` - marketing_df$`BMI(T0)`)/marketing_df$`BMI(T0)`
#C2-4. calorie
marketing_df <- marketing_df %>% mutate(calorie_day = ((carbohydrate*4 + protein*4 + fat*9) / day_count ))


#C3. (1.) (%) *100  (2.) numeric %>% round(2)
marketing_df[,grep("%", names(marketing_df))] <- marketing_df[,grep("%", names(marketing_df))] %>% multiply_by(100) %>% round(2)
marketing_df[c("weight(T0)","weight(T1)","∆weight","∆weight%","BMI(T0)","BMI(T1)","∆BMI","∆BMI%","Fat(T0)","Fat(T1)","∆Fat","∆Fat%","wc(T0)","wc(T1)","∆wc","∆wc%")] %<>% round(2)

# #C3-2. exclude vars
# marketing_df <- marketing_df %>% select(-c(light_G_count, carbohydrate, protein, fat))
# 
# #C3-3. exclude missing value
# marketing_df <- marketing_df %>% lin_exclude_NA_col(c("light_G_%", "∆weight", "carb_E%"))
# #C3-4. exclude upload_day_p = 0
# marketing_df <- marketing_df %>% filter(`upload_day_%` != 0)

#C4-1. class_freq by org_name
marketing_df <- marketing_df %>% full_join(marketing_df %>% group_by(id) %>% summarise(class_freq = n()), by = c("id"))
#C4-2. class_order
for (i in unique(marketing_df$id)) {
  if (i == head(unique(marketing_df$id), 1)) {
    j = 1
    marketing_df$class_order <- NA
    start_time <- Sys.time()
  }
  marketing_df[which(marketing_df[["id"]] == i), "class_order"] <- which(marketing_df[["id"]] == i) %>% order()
  progress(j, unique(marketing_df$id) %>% length())
  j = j + 1
  if (i == tail(unique(marketing_df$id), 1)){
    print("[Completed!]")
  }
}


#C5. rm outliers w/ mean
marketing_df[["∆weight%"]] <- ifelse((marketing_df[["∆weight%"]] < quantile(marketing_df[["∆weight%"]], 0.01, na.rm = TRUE)) | (marketing_df[["∆weight%"]] > quantile(marketing_df[["∆weight%"]], 0.95, na.rm = TRUE)),
                                     marketing_df[["∆weight%"]] %>% mean(),
                                     marketing_df[["∆weight%"]])
# marketing_df[["∆weight%"]] <- ifelse((marketing_df[["∆weight%"]] < quantile(marketing_df[["∆weight%"]], 0.01, na.rm = TRUE)) | (marketing_df[["∆weight%"]] > quantile(marketing_df[["∆weight%"]], 0.95, na.rm = TRUE)),
#                                              NA,
#                                              marketing_df[["∆weight%"]])


marketing_df <- marketing_df %>% mutate(diet_compliance = (`upload_day_%` * `light_G_%` / 100) %>% round(2))



## Calculate result --------------------------------------------------------


# 總共減多少體重(脂) -  by 人次, by 人
df03_FLC_self_report %>% 
  summarise(
    weight_total = sum(`∆weight`, na.rm = TRUE),
    weight_mean = mean(`∆weight`, na.rm = TRUE),
    weight_mean_p = mean(`∆weight%`, na.rm = TRUE),
    fat_total = sum(`∆Fat`, na.rm = TRUE),
    fat_mean = mean(`∆Fat`, na.rm = TRUE),
    n = n()
  )
marketing_df %>% 
  summarise(
    weight_total = sum(`∆weight`, na.rm = TRUE),
    weight_mean = mean(`∆weight`, na.rm = TRUE),
    weight_mean_p = mean(`∆weight%`, na.rm = TRUE),
    fat_total = sum(`∆Fat`, na.rm = TRUE),
    fat_mean = mean(`∆Fat`, na.rm = TRUE),
    n = n()
  )

marketing_stat_tbl <- marketing_df %>% 
  summarise(
    weight_total = sum(`∆weight`, na.rm = TRUE),
    weight_mean = mean(`∆weight`, na.rm = TRUE),
    weight_mean_p = mean(`∆weight%`, na.rm = TRUE),
    fat_total = sum(`∆Fat`, na.rm = TRUE),
    fat_mean = mean(`∆Fat`, na.rm = TRUE),
    n = n()
  )

















