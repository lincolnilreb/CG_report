

library(googlesheets4)
tmp_03 <- googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/139FLf4rO9bQRTcuffBDfFIuVMK_p6QTrZM4m5p3qLmA/edit?usp=sharing', col_types = "iccDciccDDicdddddddddddddddddddddc")

#exclude program name w/ "診所"
tmp_03 <- tmp_03 %>% filter(`program 名稱` %in% c(tmp_03$`program 名稱` %>% grep("診所", ., value = TRUE, invert = TRUE)))

#exclude 體重% is NA
tmp_03 <- tmp_03 %>% lin_exclude_NA_col("體重 %")
#exclude ∆體重 outlier
tmp_03 <- tmp_03[(tmp_03[["體重 %"]] >= quantile(tmp_03[["體重 %"]], 0.01, na.rm = TRUE)) & (tmp_03[["體重 %"]] <= quantile(tmp_03[["體重 %"]], 0.95, na.rm = TRUE)), ] %>% janitor::remove_empty("rows")

df03_FLC_self_report <- tmp_03



df03_FLC_self_report$`∆weight` %>% summary()
df03_FLC_self_report$`∆weight%` %>% summary()
df03_FLC_self_report %>% 
  group_by(`綠燈分層`) %>% 
  summarise(
    "∆weight%" = mean(`∆weight%`),
    "∆weight" = mean(`∆weight%`),
    n = n()
  )

