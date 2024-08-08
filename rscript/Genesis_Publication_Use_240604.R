# 2021-01-01 ~ 2023-12-31
# 控制0, 4, 8週data pool: Inbody, Biochem 
# Education (n,%), Disease History (n,%), Occupation (n,%)


library(pacman)
pacman::p_load(
  magrittr,
  knitr,
  kableExtra,
  dplyr,
  readr,
  readxl,
  tibble,
  showtext,
  extraInserts,
  ggvenn,
  ggplot2,
  knitr,
  kableExtra,
  openxlsx,
  lubridate,
  cowplot,
  ggpubr,
  webshot,
  stringr,
  googleVis
)

# 1. Left_join profile & Questionnaire ------------------------------------


# df01_profile$org_name %>% table()  #Overview org_name
datasets_profile <- 
  df01_profile %>% 
  filter(org_name %in% c("genesisclinic", "lumez")) %>%  #診所Pool
  filter(program_name != "診所進階計畫") %>% 
  filter(date_t0 >= "2021-01-01" & date_t1 <= "2023-12-31") %>%  #Duration 2021年開始, 2023年底結束
  filter(client_type %in% c(1,2)) %>%  #Include 門診 [1:控糖 & 2:減重]
  distinct(id, .keep_all = TRUE)


a <- 
  tmp_01 %>% 
  filter(org_name %in% c("genesisclinic", "lumez")) %>%  #診所Pool
  filter(date_t0 >= "2021-01-01" & date_t1 <= "2023-12-31") %>%  #Duration 2021年開始, 2023年底結束
  distinct(id, .keep_all = TRUE) %>% 
  select(id, date_t0,q_1865_history, q_1855_occupation) #Questionnaire: q_1865_history, q_1855_occupation

datasets_profile <- 
  left_join(datasets_profile, a, by = c("id", "date_t0"))

rm(a)

# [2Do Item Clean Questionnaire answer] 
# (1) Disease History  (2) Occupation

# datasets_profile$q_1865_history %>% table() %>% view()
# datasets_profile$q_1855_occupation %>% table() %>% view()




# 2. Data Wrangling: Inbody, Biochem, Diet  -------------------------------------

Pooled_id <- 
  datasets_profile %>% pull(id)
Pooled_id %>% length()

## 2-1. Inbody -------------------------------------------------------------

datasets_inbody <- 
  df02_inbody %>% filter(id %in% Pooled_id)

a <- 
  datasets_profile %>% select(id, date_t0, date_t1)

datasets_inbody <- 
  left_join(datasets_inbody, a, by = c("id")) %>% 
  distinct(id, date_inbody, .keep_all = TRUE)

rm(a)

datasets_inbody <- 
  datasets_inbody %>% 
  mutate(day_no = as.numeric((lubridate::ymd(date_inbody) - lubridate::ymd(date_t0)))) %>% 
  filter(day_no >= 0-30 & day_no <= 56+30) %>% #先取±30天data
  mutate(week_no = as.numeric(ceiling((lubridate::ymd(date_inbody) - lubridate::ymd(date_t0))/7)))

  
datasets_inbody <- datasets_inbody[with(datasets_inbody, order(id, day_no)),]

datasets_inbody <- 
  datasets_inbody %>% 
  mutate(day_no_cal = round(day_no/28, digits = 0)*28) %>% 
  mutate(day_temp = abs(day_no_cal - day_no)) %>% 
  mutate(week_no_cal = round(week_no/4, digits = 0)*4) %>% 
  mutate(week_temp = abs(week_no_cal - week_no))
  
datasets_inbody <- 
  datasets_inbody %>%
  group_by(id, week_no_cal) %>%
  slice_min(order_by = week_temp, n = 1) %>%
  slice_min(order_by = day_temp, n = 1) %>% 
  select(-week_temp, -day_no_cal, -day_temp)  %>% 
  filter(week_no_cal >= 0 & week_no_cal <= 8)

id_pool_inbody <- 
  datasets_inbody %>% 
  group_by(id) %>%
    summarize(
      N = n()
      ) %>% filter(N == 3) %>% pull(id)

datasets_inbody <- 
  datasets_inbody %>%
  filter(id %in% id_pool_inbody)


## 2-2. Biochem -------------------------------------------------------------

datasets_biochem <- 
  df05_biochem %>% 
  filter(id %in% Pooled_id) %>% 
  filter(!is.na(glucose_ac)) 

a <- 
  datasets_profile %>% select(id, date_t0, date_t1)

datasets_biochem <- 
  left_join(datasets_biochem, a, by = c("id")) %>% 
  distinct(id, date_blood, .keep_all = TRUE)

rm(a)

datasets_biochem <- 
  datasets_biochem %>% 
  mutate(day_no = as.numeric((lubridate::ymd(date_blood) - lubridate::ymd(date_t0)))) %>% 
  filter(day_no >= 0-30 & day_no <= 56+30) %>% #先取±30天data
  mutate(week_no = as.numeric(ceiling((lubridate::ymd(date_blood) - lubridate::ymd(date_t0))/7)))

datasets_biochem <- datasets_biochem[with(datasets_biochem, order(id, day_no)),]

datasets_biochem <- 
  datasets_biochem %>% 
  mutate(day_no_cal = round(day_no/56, digits = 0)*56) %>% 
  mutate(day_temp = abs(day_no_cal - day_no)) %>% 
  mutate(week_no_cal = round(week_no/4, digits = 0)*4) %>% 
  mutate(week_temp = abs(week_no_cal - week_no))

datasets_biochem <- 
  datasets_biochem %>%
  group_by(id, week_no_cal) %>%
  slice_min(order_by = week_temp, n = 1) %>%
  slice_min(order_by = day_temp, n = 1) %>% 
  select(-week_temp, -day_no_cal, -day_temp)%>% 
  filter(week_no_cal == 0 | week_no_cal == 8)


id_pool_biochem <- 
  datasets_biochem %>% 
  group_by(id) %>%
  summarize(
    N = n()
  ) %>% filter(N == 2) %>% pull(id)


## 2-3. Diet -------------------------------------------------------------

datasets_diet <- 
  df06_Diet_day %>%
  rename(id = client_id) %>% 
  filter(id %in% Pooled_id)

a <- 
  datasets_profile %>% select(id, date_t0, date_t1)

datasets_diet <- 
  left_join(datasets_diet, a, by = c("id")) %>% 
  distinct(id, date_diet, .keep_all = TRUE)

rm(a)

datasets_diet <- datasets_diet[with(datasets_diet, order(id, "date_diet")),]






a <- 
  datasets_diet %>% 
  filter(date_diet >= date_t0 & date_diet <= date_t1) %>% 
  group_by(id) %>% 
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


colnames(a) <- gsub("_tmp", colnames(a), replacement = "")

names(a) <- names(a) %>% lin_ch_en_format(format = "en", origin = "raw_en")

datasets_diet <- a
rm(a)

id_pool_diet <- 
  datasets_diet %>% pull(id)



# 2. Venns ----------------------------------------------------------------

x <- list(
  #1
  `Clinic_clients` = Pooled_id,
  #2
  `Obesity` =  datasets_profile$id[datasets_profile[["client_type"]] == 2],
  #3
  `Diabetes` =  datasets_profile$id[datasets_profile[["client_type"]] == 1],
  #4
  `Inbody` = id_pool_inbody,
  #5
  `Blood Test` = id_pool_biochem,
  #6
  `Diet` = id_pool_diet,
  #7.OB.
  `Fit_criteria_OB` = Reduce(intersect, list(datasets_profile$id[datasets_profile[["client_type"]] == 2],
                                             id_pool_inbody,
                                             id_pool_biochem,
                                             id_pool_diet
  )), 
  #8.DM.
  `Fit_criteria_DM` = Reduce(intersect, list(datasets_profile$id[datasets_profile[["client_type"]] == 1],
                                             id_pool_inbody,
                                             id_pool_biochem,
                                             id_pool_diet
  ))
)


plot_venn_program <- 
  ggvenn(
    x, columns = names(x)[c(2,3,1)],
    fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
    stroke_size = 0.5, set_name_size = 3.5, 
  ) +
  labs(title = "Program")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
    plot.margin = unit(c(0.5,0,0,0), "cm")
  ) 



plot_venn_ob <- 
  ggvenn(
    x, columns = names(x)[c(2,4,5,6)],
    fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
    stroke_size = 0.5, set_name_size = 3.5, 
  ) +
  labs(title = "Program")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
    plot.margin = unit(c(0.5,0,0,0), "cm")
  ) 


plot_venn_dm <- 
  ggvenn(
    x, columns = names(x)[c(3,4,5,6)],
    fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
    stroke_size = 0.5, set_name_size = 3.5, 
  ) +
  labs(title = "Program")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
    plot.margin = unit(c(0.5,0,0,0), "cm")
  ) 


