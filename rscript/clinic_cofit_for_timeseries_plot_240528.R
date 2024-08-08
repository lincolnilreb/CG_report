# 數據需求：
# 1. 8週的減重數據 (kg & %)
# 診所
# - All 
# - T2DM
# - Overweight and Obese
# Cofit
# - All 
# - T2DM
# - Overweight and Obese
# 
# 羿清 Vanessa, 週一 下午3:09
# 診所和cofit的分開，理應All就可以看到效果，如果看不到我再分成糖尿病和過重+肥胖 就好。
# 一定要有的：八週減重數據（w0, w4, w8) 
# 有會更好，沒有就算了：w48-w52


# Datasets: stat_table, df03
# Dimension: week_number, bmi_gp(gp_bmi_baseline), gender, program(client_type, OB/DM)



# 01. CLINIC DATASETS -----------------------------------------------------


a <- stat_table %>% select(id, gender, date_t0, date_t1, client_type)
datasets_clinic <- left_join(a, df02_inbody, by = "id", multiple = "all")

names(datasets_clinic) <- names(datasets_clinic) %>% lin_ch_en_format(format = "en", origin = "raw_en")

datasets_clinic <- datasets_clinic %>% mutate(week_no = ceiling((lubridate::ymd(date_inbody) - lubridate::ymd(date_T0))/7))
datasets_clinic$week_no <- datasets_clinic$week_no %>% as.numeric()
datasets_clinic <- datasets_clinic %>% filter((week_no >= 0) & (week_no <= 52))

datasets_clinic <- datasets_clinic %>%
  group_by(id, week_no) %>%
  slice_max(order_by = date_inbody, n = 1)

datasets_clinic <- datasets_clinic %>% distinct(id, week_no, .keep_all = TRUE)

# cut interval in every 4 week

datasets_clinic <- datasets_clinic %>% mutate(week_no_interval = ceiling(week_no/4))
datasets_clinic <- datasets_clinic %>%
  group_by(id, week_no_interval) %>%
  slice_max(order_by = date_inbody, n = 1)

datasets_clinic <- datasets_clinic %>% distinct(id, week_no_interval, .keep_all = TRUE)
datasets_clinic <- datasets_clinic %>% mutate(week_no_interval = week_no_interval *4)


datasets_clinic <- 
  datasets_clinic %>%
  group_by(id) %>%
  summarise(
    gender,
    weight,
    weight_loss = weight - first(weight, na.rm = TRUE),
    weight_loss_percent = ((weight - first(weight, na.rm = TRUE)) / first(weight, na.rm = TRUE) *
                             100) %>% round(2),
    # week_no,
    week_no_interval,
    date_inbody,
    date_T0,
    date_T1,
    client_type,
    gp_bmi
  )




datasets_clinic <- datasets_clinic %>% filter(gp_bmi %in% c("overweight", "obesity"))

filter_id <-
Reduce(dplyr::intersect, list(datasets_clinic %>% filter(week_no_interval == 0) %>% filter(!is.na(week_no_interval)) %>% pull(id) %>% unique(),
                              datasets_clinic %>% filter(week_no_interval == 8) %>% filter(!is.na(week_no_interval)) %>% pull(id) %>% unique(),
                              datasets_clinic %>% filter(week_no_interval == 16) %>% filter(!is.na(week_no_interval)) %>% pull(id) %>% unique(),
                              datasets_clinic %>% filter(week_no_interval == 24) %>% filter(!is.na(week_no_interval)) %>% pull(id) %>% unique()
                              ))


a_filtered <- 
datasets_clinic %>%
  filter(id %in% filter_id) %>%
  group_by(week_no_interval) %>%
  summarise(
    mean_weight_loss = mean(weight_loss, na.rm = TRUE) %>% round(2),
    # sd_weight_loss = sd(weight_loss, na.rm = TRUE) %>% round(2),
    mean_weight_loss_percent = mean(weight_loss_percent, na.rm = TRUE) %>% round(2),
    # sd_weight_loss_percent = sd(weight_loss_percent, na.rm = TRUE) %>% round(2),
    N = n()
  ) 


writexl::write_xlsx(a_filtered, path = paste0("/Users/lincoln/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/genesisclinic/", "a_filtered", ".xlsx"))



# 02. COFIT-APP DATASETS --------------------------------------------------

a <- df03_FLC_self_report_w8df %>% select(id, gender, date_flc_T0, date_flc_T1, `BMI(T0)`)
a$gp_bmi <- a$`BMI(T0)` %>% cut(c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))
a <- a %>% select(-`BMI(T0)`)

names(tmp_03_day) <- names(tmp_03_day) %>% lin_ch_en_format(format = "en", origin = "raw_en")
b <- tmp_03_day %>% select(-c(date_flc_T0, date_flc_T1, gender))
# names(b) <- names(b) %>% lin_ch_en_format(format = "en", origin = "raw_en")

datasets_cofit <- left_join(a, b, by = "id", multiple = "all")



datasets_cofit <- datasets_cofit %>% mutate(week_no = ceiling((lubridate::ymd(date) - lubridate::ymd(date_flc_T0))/7))
datasets_cofit$week_no <- datasets_cofit$week_no %>% as.numeric()
datasets_cofit <- datasets_cofit %>% filter((week_no >= 0) & (week_no <= 52))

#Aggregate: Diet
datasets_cofit <- datasets_cofit %>% 
  group_by(id, week_no) %>% 
  mutate(
    pic_count = sum(pic_count, na.rm = TRUE),
    light_G_count = sum(light_G_count, na.rm = TRUE),
    light_y_count = sum(light_y_count, na.rm = TRUE),
    light_r_count = sum(light_r_count, na.rm = TRUE)
  ) 

#Filter NA: weight
datasets_cofit <- datasets_cofit %>% filter(!is.na(weight))

datasets_cofit <- datasets_cofit %>%
  group_by(id, week_no) %>%
  slice_max(order_by = date, n = 1)

datasets_cofit <- datasets_cofit %>% distinct(id, week_no, .keep_all = TRUE)

# cut interval in every 2 week

datasets_cofit <- datasets_cofit %>% mutate(week_no_interval = ceiling(week_no/2))
datasets_cofit <- datasets_cofit %>%
  group_by(id, week_no_interval) %>%
  slice_max(order_by = date, n = 1)

datasets_cofit <- datasets_cofit %>% distinct(id, week_no_interval, .keep_all = TRUE)
datasets_cofit <- datasets_cofit %>% mutate(week_no_interval = week_no_interval *2)


filter_id <-
  Reduce(dplyr::intersect, list(datasets_cofit %>% filter(week_no_interval == 0) %>% filter(!is.na(week_no_interval)) %>% pull(id) %>% unique(),
                                datasets_cofit %>% filter(week_no_interval == 8) %>% filter(!is.na(week_no_interval)) %>% pull(id) %>% unique(),
                                datasets_cofit %>% filter(week_no_interval == 16) %>% filter(!is.na(week_no_interval)) %>% pull(id) %>% unique(),
                                datasets_cofit %>% filter(week_no_interval == 24) %>% filter(!is.na(week_no_interval)) %>% pull(id) %>% unique()
  ))

datasets_cofit <- datasets_cofit %>% filter(id %in% filter_id)

datasets_cofit <- 
  datasets_cofit %>%
  group_by(id) %>%
  summarise(
    gender,
    weight,
    weight_loss = weight - first(weight, na_rm = T),
    weight_loss_percent = ((weight - first(weight, na_rm = TRUE)) / first(weight, na_rm = TRUE) *
                             100) %>% round(2),
    pic_count = sum(pic_count, na.rm = TRUE),
    light_G_count = sum(light_G_count, na.rm = TRUE),
    light_y_count = sum(light_y_count, na.rm = TRUE),
    light_r_count = sum(light_r_count, na.rm = TRUE),
    # week_no,
    week_no_interval,
    date,
    date_flc_T0,
    date_flc_T1,
    gp_bmi
  )


a_all <- 
  datasets_cofit %>%
  # filter(gp_bmi %in% c("overweight", "obesity")) %>% 
  group_by(week_no_interval) %>%
  summarise(
    mean_weight_loss = mean(weight_loss, na.rm = TRUE) %>% round(2),
    # sd_weight_loss = sd(weight_loss, na.rm = TRUE) %>% round(2),
    mean_weight_loss_percent = mean(weight_loss_percent, na.rm = TRUE) %>% round(2),
    # sd_weight_loss_percent = sd(weight_loss_percent, na.rm = TRUE) %>% round(2),
    N = n()
  ) 

a_filtered <- 
  datasets_cofit %>%
  filter(gp_bmi %in% c("overweight", "obesity")) %>% 
  # filter(pic_count > quantile(datasets_cofit$pic_count, probs = 0.5)) %>% 
  group_by(week_no_interval) %>%
  summarise(
    mean_weight_loss = mean(weight_loss, na.rm = TRUE) %>% round(2),
    # sd_weight_loss = sd(weight_loss, na.rm = TRUE) %>% round(2),
    mean_weight_loss_percent = mean(weight_loss_percent, na.rm = TRUE) %>% round(2),
    # sd_weight_loss_percent = sd(weight_loss_percent, na.rm = TRUE) %>% round(2),
    N = n()
  ) 


writexl::write_xlsx(a_all, path = paste0("/Users/lincoln/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/genesisclinic/", "a_all", ".xlsx"))
writexl::write_xlsx(a_filtered, path = paste0("/Users/lincoln/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/genesisclinic/", "a_filtered", ".xlsx"))
