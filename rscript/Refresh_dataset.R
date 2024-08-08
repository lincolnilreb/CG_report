source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/lin_function.R")



# 01. SQL ---------------------------------------------------------------------


# Does source() rscript need to move to the same project dir??
lin_connect_db("connect")
path_sql <- "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/sql/"

tmp_01 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "01_profile.sql")))
# tmp_01 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "01_profile_flc_q.sql")))
tmp_02 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "02_inbody.sql")))
tmp_03 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "03_FLC_self_report.sql")))
tmp_04 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "04_non_FLC_self_report.sql")))
tmp_05 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "05_biochem.sql")))

tmp_06 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "06_Diet_day.sql")))
tmp_07 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "07_Diet_meal.sql")))
tmp_08 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "08_3D_scanner.sql")))
tmp_09 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "09_hormone.sql")))
tmp_03_day <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "10_daily_flc.sql")))
lin_connect_db("exit")
rm(path_sql)

tmp_03[tmp_03 %>% select_if(is.numeric) %>% select_if(~ !is.integer(.)) %>% names()]  %<>% lapply(as.numeric)

tmp_06[c("note_counts","pic_counts","essay_count","light_green_count","light_yellow_count","light_red_count","carbohydrate","protein","fat","calorie","grains_target","fruits_target","vegetables_target","meat_beans_low_fat_target","meat_beans_medium_fat_target","meat_beans_high_fat_target","milk_whole_fat_target","milk_low_fat_target","milk_skim_target","oil_target")] %<>% lapply(as.numeric)




# 02. Data Preprocess -----------------------------------------------------

library(pacman)
pacman::p_load(magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, extraInserts, ggvenn, ggplot2,knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot, stringr, googleVis)
#font
font_add(family = "berlin_default", regular = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/font/STHeiti Light.ttc")
showtext_auto(enable = TRUE)

source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/00-data_preprocessing.R")




