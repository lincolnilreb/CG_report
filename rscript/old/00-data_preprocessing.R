
# 01-setting --------------------------------------------------------------
  
  #load packages
  library(pacman)
  pacman::p_load(magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, extraInserts,
                 ggvenn, ggplot2,knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot,
                 stringr)
  #font
  font_add(family = "berlin_default", regular = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/font/STHeiti Light.ttc")
  showtext_auto(enable = TRUE)
  

  
  
  
# 02.0-input_name_table --------------------------------------------------------
  library(googlesheets4)
  vars_table <- googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1T2swdx1cbfmUSUNQCQpbxa4aIJDTLD0oVKj3AuvNAuM/edit?usp=sharing', 
                                          sheet = "vars_table",
                                          col_types = "iccccc")
  names(vars_table) <- c("num", "item_id", "ch", "en", "raw_en", "field")
  
  
  
  
  
  
  
# 02.1-input_blood --------------------------------------------------------
  #blood datasets
  clinic_blood_data <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/blood_data.csv")
  
  #blood data clean
  clinic_blood_data <-  clinic_blood_data %>% dplyr::select(c("member_id","date","hba1c","glucose_ac","glucose_pc_1hr","glucose_pc_2hr","insulin","insulin_pc_1hr",
                                                              "insulin_pc_2hr","homa_ir","homa_beta","triglyceride","total_cholesterol","high_density_lipoprotein",
                                                              "low_density_lipoprotein_cholesterol","e2","testosterone", "lipase"))
  colnames(clinic_blood_data)[c(1,2,12,13,14,15)] <- c("id","date_blood","tg","tc","hdl","ldl")
  
  clinic_blood_data <- clinic_blood_data[Reduce(dplyr::union,list(which(!is.na(clinic_blood_data$tg)),
                                                                  which(!is.na(clinic_blood_data$tc)),
                                                                  which(!is.na(clinic_blood_data$hdl)),
                                                                  which(!is.na(clinic_blood_data$ldl)))),]
  #variable format
  clinic_blood_data$date_blood <- as.Date(clinic_blood_data$date_blood)
  clinic_blood_data[c("id")] %<>% lapply(as.integer)
  clinic_blood_data[c("homa_ir", "homa_beta")] %<>% lapply(gsub, pattern = "[^0-9.-]", replacement = "NA")
  clinic_blood_data[c("glucose_pc_1hr","glucose_pc_2hr","insulin","insulin_pc_1hr","insulin_pc_2hr","homa_ir", "homa_beta","tg","tc","hdl","ldl","e2","testosterone", "lipase")] %<>% lapply(as.numeric)
  
  #wrong data
  clinic_blood_data <- clinic_blood_data[-which((clinic_blood_data$id == 470051) & (clinic_blood_data$date_blood == "2021-10-23")),]
  clinic_blood_data <- clinic_blood_data[-which(clinic_blood_data$id == 302),]


# 02.2-input_inbody -------------------------------------------------------
  #inbody datasets
  clinic_inbody_data <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/inbody_data.csv")
  
  #Sarcopenia Obesity(SO): "left_arm_muscle", "left_leg_muscle", "right_arm_muscle", "right_leg_muscle" #Female: < 23.4; Male: < 29.6. 
  clinic_inbody_data <- clinic_inbody_data %>% mutate(so_score = round((left_arm_muscle+left_leg_muscle+right_arm_muscle+right_leg_muscle)*100/weight,2))
  #pbm
  clinic_inbody_data <- clinic_inbody_data %>% mutate(pbm = round((muscle_mass)*100/weight,2))
  
  
  #inbody data clean
  clinic_inbody_data <- clinic_inbody_data %>% dplyr::select(c("member_id","date","weight","bmi","body_fat_mass","body_fat_mass_percentage", "bsmi", "muscle_mass", "pbm", "vfa_level","waist_circumference","weight_without_fat","extracellular_water_ratio", "wepa50","bmr", "so_score", "tbwffm"))
  colnames(clinic_inbody_data)[c(1,2,5,6,8,10,11,12)] <- c("id","date_inbody","bf","pbf","bm","vfa","wc","ffm")
  #variable format
  clinic_inbody_data$date_inbody <- as.Date(clinic_inbody_data$date_inbody)
  clinic_inbody_data[c("id","vfa")] %<>% lapply(as.integer)
  clinic_inbody_data[c("weight","bmi","bf","pbf", "bsmi","bm","wc","ffm","extracellular_water_ratio","wepa50","bmr")] %<>% lapply(as.numeric)
  #rm outlier
  clinic_inbody_data <- clinic_inbody_data[-which(clinic_inbody_data$bmi >100),]
  clinic_inbody_data <- clinic_inbody_data[-which(clinic_inbody_data$id == 302),]
  
  

  

# 02.3-input 3d -----------------------------------------------------------
  
  #3d datasets
  # clinic_3d_data <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/clinic_3d_data.csv")
  # clinic_3d_data <- clinic_3d_data %>% dplyr::select(c("member_id","date","weight","bmi","body_fat_mass","body_fat_mass_percentage", "bsmi", "muscle_mass","vfa_level","waist_circumference","weight_without_fat","extracellular_water_ratio", "wepa50","bmr"))
  # colnames(clinic_3d_data)[c(1,2,5,6,8,9,10,11)] <- c("id","date_inbody","bf","pbf","bm","vfa","wc","ffm")
  
  
  
# 02.4-input_client_info --------------------------------------------------
  #clients datasets
  clinic_client_data <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/client_info.csv")
  clinic_client_data[c("btd","date_1st","date_T0","date_T1","date_T2","date_T3","date_T4")] %<>% lapply(as.Date)
  #variable format
  clinic_client_data[c("serial_id","client_type","id","age")] %<>% lapply(as.integer)
  
  

# 02.5-input_(non)genesis_list -------------------------------------------------

  #clinic_cliet_list datasets
  clinical_list <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/genesis_list.csv")
  clinical_list <- clinical_list[-1:-5,]
  clinical_list <- clinical_list[which(!is.na(clinical_list$serial_id)),]
  clinical_list %<>% dplyr::select(c("serial_id","id","client_type","name","date","class_date_1","class_date_2","medication","doctor","nutritionist_major","nutritionist_online","program","history"))
  
  clinical_list$medication_note <- NA
  #Trulicity
  clinical_list[grep("(licity){1}",clinical_list$medication),"medication_note"] <- "Trulicity"
  #Ozempic
  clinical_list[grep("(mpic){1}",clinical_list$medication),"medication_note"] <- "Ozempic"
  #Rybelsus, rebylsus(wrong name) search list
  clinical_list[grep("(sus){1}",clinical_list$medication),"medication_note"] <- "Rybelsus"
  
  clinical_list %<>% dplyr::relocate(medication_note, .before = medication)
  
  #map age, gender, hormone
  clinical_list <- lin_mapping(clinical_list, age, id, clinic_client_data, age, id)
  clinical_list <- lin_mapping(clinical_list, gender, id, clinic_client_data, gender, id)
  clinical_list <- lin_mapping(clinical_list, hormone, id, clinic_client_data, hormone_type, id)
  
  #variable format
  clinical_list[c("serial_id","id","client_type", "age")] %<>% lapply(as.integer)
  clinical_list[c("date","class_date_1","class_date_2")] %<>% lapply(as.Date)
  colnames(clinical_list)[5] <-  "date_1st"
  
  clinical_list <- clinical_list[which(clinical_list$client_type != "0"),]  
  
  

# 03.  part 2 -------------------------------------------------------------

  
  out_of_genesis_list_age_gender <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/non_genesis_list.csv")
  out_of_genesis_list_age_gender$birthday %<>% as.Date()
  out_of_genesis_list_age_gender$age <- (lubridate::ymd("2023-01-01") -  lubridate::ymd(out_of_genesis_list_age_gender$birthday)) %>% as.numeric() %>% divide_by(365) %>% floor()
  out_of_genesis_list_age_gender[c("id", "age")] %<>% lapply(as.integer)
  
  
  
# 04.  part 3 -------------------------------------------------------------

  #save origin version
  clinic_inbody_data_ori <- clinic_inbody_data
  clinic_blood_data_ori <- clinic_blood_data
  #1. med_id_pool
  med_id_pool <- dplyr::intersect(clinic_inbody_data %>% select(id) %>% unique() %>% pull(),
                                  clinic_blood_data %>% select(id) %>% unique() %>% pull() )
  
  clinic_inbody_data %<>% filter(id %in% med_id_pool)
  clinic_blood_data %<>% filter(id %in% med_id_pool)
  
  #-------record data before screening according to blood data:
  ##(1.) 1st treatment  (2.) 2 obs. (3.) at least 1mo interval
  before_screening_data <- clinic_blood_data
  
  
  clinic_blood_data %<>% janitor::get_dupes(id)
  clinic_inbody_data %<>% filter(id %in% clinic_blood_data$id) 
  
  
  for (i in c(unique(clinic_blood_data$id))){
    if (i == unique(clinic_blood_data$id) %>% head(1)){
      #pre_setting
      clinic_inbody_data_1st_temp <- clinic_inbody_data[0,]
      clinic_blood_data_1st_temp <- clinic_blood_data[0,]
      
      clinic_inbody_data <- clinic_inbody_data[with(clinic_inbody_data, order(id, date_inbody)), ] 
      clinic_blood_data <- clinic_blood_data[with(clinic_blood_data, order(id, date_blood)), ] 
      
      # Start the clock
      ptm <- proc.time()
      j = 1
    }
    
    inbody_obs_count <- clinic_inbody_data %>% filter(id == i) %>% select("date_inbody") %>% pull() %>% length()
    blood_obs_count <- clinic_blood_data %>% filter(id == i) %>% select("date_blood") %>% pull() %>% length()
    
    if ((inbody_obs_count>1) & (blood_obs_count>1)) {
      #condition: i belongs to Genesis list w/ program T0, T1
      if (i %in% clinical_list$id) {
        if ((clinical_list %>% filter(id == i) %>% select(c("class_date_1", "class_date_2")[1])) %>% pull() %>% is.finite()) {
          #*[T0  read Genesis_list_date ; T1 read Genesis_list_date]
          #T0
          date_lower_cutoff <- (clinical_list %>% filter(id == i) %>% select(c("class_date_1", "class_date_2")[1]) - 31) %>% pull()
          #T1
          date_upper_cutoff_inbody <- (clinical_list %>% filter(id == i) %>% select(c("class_date_1", "class_date_2")[2]) + 10) %>% pull()
          date_upper_cutoff_blood <- (clinical_list %>% filter(id == i) %>% select(c("class_date_1", "class_date_2")[2]) + 21) %>% pull()
          
          #inbody T0: > T0-31, T1: T1+7 nearest date
          clinic_inbody_data_1st_temp <- rbind(clinic_inbody_data_1st_temp, clinic_inbody_data %>% filter(id == i) %>% filter(date_inbody >= date_lower_cutoff) %>% head(1) )
          index <- abs(clinic_inbody_data %>% filter(id == i) %>% select("date_inbody") %>% pull() - date_upper_cutoff_inbody) %>% which.min()
          clinic_inbody_data_1st_temp <- rbind(clinic_inbody_data_1st_temp, (clinic_inbody_data %>% filter(id == i))[index,] )
          
          clinic_blood_data_1st_temp <- rbind(clinic_blood_data_1st_temp, clinic_blood_data %>% filter(id == i) %>% filter(date_blood <= date_upper_cutoff_blood) %>% tail(2) %>% head(1) )
          index <- (clinic_inbody_data %>% filter(id == i))[index,"date_inbody"] %>% pull() + 14
          clinic_blood_data_1st_temp <- rbind(clinic_blood_data_1st_temp, clinic_blood_data %>% filter(id == i) %>% filter(date_blood <= index) %>% tail(1) )
        }else{
          #*[取時間區間 1.血檢&inbody最早時間點取較高值 2. 血檢&inbody最晚時間點取較低值 3. 前後加寬1mo]
          date_lower_cutoff <- c(clinic_inbody_data[which(clinic_inbody_data$id == i) %>% head(1), "date_inbody"] %>% pull(), clinic_blood_data[which(clinic_blood_data$id == i) %>% head(1), "date_blood"] %>% pull()) %>% max() -30
          date_upper_cutoff <- c(clinic_inbody_data[which(clinic_inbody_data$id == i) %>% tail(1), "date_inbody"] %>% pull(), clinic_blood_data[which(clinic_blood_data$id == i) %>% tail(1), "date_blood"] %>% pull()) %>% min() +30
          
          #*[T0  I0 >= (B0 - 14 days) 第一筆 ; T0 + 2mo 取最接近日期date_inbody]
          #B0
          B0_date <- clinic_blood_data %>% filter(id == i) %>% filter(date_blood >=  date_lower_cutoff) %>% select("date_blood") %>% pull() %>% head(1)
          #I0 = T0
          I0_date <- clinic_inbody_data %>% filter(id == i ) %>% filter(date_inbody >= (B0_date - 14)) %>% select("date_inbody") %>% pull() %>% head(1)
          #T1
          T1_index <- ((clinic_inbody_data %>% filter(id == i ) %>% select("date_inbody") %>% pull()) - (lubridate::ymd(I0_date) %m+% months(2)) + 7) %>% abs() %>% which.min()
          T1_index <- (clinic_inbody_data %>% filter(id == i) %>% select("date_inbody") %>% pull())[T1_index]
          T1_index <- ((clinic_blood_data %>% filter(id == i) %>% select("date_blood") %>% pull()) - T1_index) %>% abs() %>% which.min()
          date_upper_cutoff <- (clinic_blood_data %>% filter(id == i) %>% select("date_blood") %>% pull())[T1_index]
          
          
          if ((length(B0_date) != 0) & (length(I0_date) != 0) & (length(date_upper_cutoff) != 0)) {
            clinic_inbody_data_1st_temp <- rbind(clinic_inbody_data_1st_temp, clinic_inbody_data %>% filter(id == i) %>% filter(date_inbody >= I0_date) %>% head(1) )
            clinic_inbody_data_1st_temp <- rbind(clinic_inbody_data_1st_temp, clinic_inbody_data %>% filter(id == i) %>% filter(date_inbody <= date_upper_cutoff) %>% tail(1) )
            
            clinic_blood_data_1st_temp <- rbind(clinic_blood_data_1st_temp, clinic_blood_data %>% filter(id == i) %>% filter(date_blood >= B0_date) %>% head(1) )
            clinic_blood_data_1st_temp <- rbind(clinic_blood_data_1st_temp, clinic_blood_data %>% filter(id == i) %>% filter(date_blood <= date_upper_cutoff) %>% tail(1) )
          }
        }
      }else{
        #*[取時間區間 1.血檢&inbody最早時間點取較高值 2. 血檢&inbody最晚時間點取較低值 3. 前後加寬1mo]
        date_lower_cutoff <- c(clinic_inbody_data[which(clinic_inbody_data$id == i) %>% head(1), "date_inbody"] %>% pull(), clinic_blood_data[which(clinic_blood_data$id == i) %>% head(1), "date_blood"] %>% pull()) %>% max() -30
        date_upper_cutoff <- c(clinic_inbody_data[which(clinic_inbody_data$id == i) %>% tail(1), "date_inbody"] %>% pull(), clinic_blood_data[which(clinic_blood_data$id == i) %>% tail(1), "date_blood"] %>% pull()) %>% min() +30
        
        #*[T0  I0 >= (B0 - 14 days) 第一筆 ; T0 + 2mo 取最接近日期date_inbody]
        #B0
        B0_date <- clinic_blood_data %>% filter(id == i) %>% filter(date_blood >=  date_lower_cutoff) %>% select("date_blood") %>% pull() %>% head(1)
        #I0 = T0
        I0_date <- clinic_inbody_data %>% filter(id == i ) %>% filter(date_inbody >= (B0_date - 14)) %>% select("date_inbody") %>% pull() %>% head(1)
        #T1
        T1_index <- ((clinic_inbody_data %>% filter(id == i ) %>% select("date_inbody") %>% pull()) - (lubridate::ymd(I0_date) %m+% months(2)) + 7) %>% abs() %>% which.min()
        T1_index <- (clinic_inbody_data %>% filter(id == i) %>% select("date_inbody") %>% pull())[T1_index]
        T1_index <- ((clinic_blood_data %>% filter(id == i) %>% select("date_blood") %>% pull()) - T1_index) %>% abs() %>% which.min()
        date_upper_cutoff <- (clinic_blood_data %>% filter(id == i) %>% select("date_blood") %>% pull())[T1_index]
        
        
        if ((length(B0_date) != 0) & (length(I0_date) != 0) & (length(date_upper_cutoff) != 0)) {
          clinic_inbody_data_1st_temp <- rbind(clinic_inbody_data_1st_temp, clinic_inbody_data %>% filter(id == i) %>% filter(date_inbody >= I0_date) %>% head(1) )
          clinic_inbody_data_1st_temp <- rbind(clinic_inbody_data_1st_temp, clinic_inbody_data %>% filter(id == i) %>% filter(date_inbody <= date_upper_cutoff) %>% tail(1) )
          
          clinic_blood_data_1st_temp <- rbind(clinic_blood_data_1st_temp, clinic_blood_data %>% filter(id == i) %>% filter(date_blood >= B0_date) %>% head(1) )
          clinic_blood_data_1st_temp <- rbind(clinic_blood_data_1st_temp, clinic_blood_data %>% filter(id == i) %>% filter(date_blood <= date_upper_cutoff) %>% tail(1) )
        }
        
      }
    }
    
    progress(j, clinic_blood_data$id %>% unique() %>% length())
    j = j + 1
    
    if (i == unique(clinic_blood_data$id) %>% tail(1)) {
      #filter out  single record
      clinic_blood_data_1st_temp <- clinic_blood_data_1st_temp %>% janitor::get_dupes(id) %>% select(-"dupe_count")
      clinic_inbody_data_1st_temp <- clinic_inbody_data_1st_temp[which(clinic_inbody_data_1st_temp$id %in% (clinic_blood_data_1st_temp %>% janitor::get_dupes(id) %>% select(-"dupe_count") %>% select("id") %>% pull() %>% unique())), ]
      
      clinic_inbody_data <- clinic_inbody_data_1st_temp
      clinic_blood_data <- clinic_blood_data_1st_temp
      cat("\n[執行時間]\n")
      print(proc.time() - ptm)
      rm(list = c("date_lower_cutoff", "date_upper_cutoff", "clinic_blood_data_1st_temp", "clinic_inbody_data_1st_temp",
                  "B0_date", "I0_date", "T1_index", "date_upper_cutoff_blood", "date_upper_cutoff_inbody", "index", "blood_obs_count", "inbody_obs_count"))
      
      #eliminate same blood_date(the newcomimgs, those not fits criteria)
      clinic_blood_data %<>% distinct() %>% janitor::get_dupes(id) %>% select(-dupe_count)
      clinic_inbody_data %<>% filter(id %in% clinic_blood_data$id)
      
      #eliminate same inbody_date
      clinic_inbody_data %<>% distinct() %>% janitor::get_dupes(id) %>% select(-dupe_count)
      clinic_blood_data %<>% filter(id %in% clinic_inbody_data$id)
      
      
      #output filter results
      ##(1.) 1st treatment  (2.) 2 obs. (3.) at least 1mo interval
      cat("Total Clients:", before_screening_data$id %>% unique() %>% length(), "counts\n",
          "Excluded Clients:", before_screening_data$id %>% unique() %>% length() - clinic_blood_data$id %>% unique() %>% length(), "counts\n",
          "Included Clients:", clinic_blood_data$id %>% unique() %>% length(), "counts\n")
      
      ##Venn: >1 blood record
      x <- list(
        All_clients = before_screening_data %>% select(id) %>% pull() %>% unique(),
        Fit_criteria = clinic_blood_data %>% janitor::get_dupes(id) %>% select(id) %>% pull() %>% unique()
      )
      
      plot_3 <- 
      ggvenn(
        x, 
        fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
        stroke_size = 0.5, set_name_size = 4
      ) +
        labs(title = "Data Screening")+
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
          plot.margin = unit(c(0.5,0,0,0), "cm")
        )
      rm(x)
      
    }
  }
  
  
  #output table
  # setdiff(before_screening_data$id %>% unique(), clinic_blood_data$id %>% unique()) %>% as.data.frame() %>% 
  #   kable(format = "html", 
  #         caption = paste0("<b>","Excluded: id List","<br>N=", before_screening_data$id %>% unique() %>% length() - clinic_blood_data$id %>% unique() %>% length(),"</b>"), 
  #         align = "c") %>%
  #   kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
  #                             full_width = TRUE, font_size = 15) %>% 
  #   scroll_box(height = "250px", width = "200px") %>% 
  #   gsub("font-size: initial !important;", 
  #        "font-size: 12pt !important;", 
  #        .)
  
  
  
  
  rm(list = c("med_id_pool"))
  
  
  
  
  

# 05. calculate delta,  product stat_table --------------------------------

  
  clinic_inbody_data_1st <- clinic_inbody_data
  clinic_blood_data_1st <- clinic_blood_data
  
  a <- clinic_inbody_data_1st[seq(1,nrow(clinic_inbody_data_1st), 2),] 
  names(a)[-1] <- paste0(a %>% select(-c("id")) %>% names(), "_baseline")
  
  b <- clinic_inbody_data_1st[seq(2,nrow(clinic_inbody_data_1st), 2),] 
  names(b)[-1] <- paste0(b %>% select(-c("id")) %>% names(), "_endpoint")
  
  aa <- b %>% select(-c("id", "date_inbody_endpoint")) - a %>% select(-c("id", "date_inbody_baseline"))
  aa <- cbind(a %>% select("id"), aa)
  names(aa)[-1] <- paste0("∆", clinic_inbody_data_1st %>% select(-c("id","date_inbody")) %>% names())
  
  bb <- ((b %>% select(-c("id", "date_inbody_endpoint")) - a %>% select(-c("id", "date_inbody_baseline")))*100 /   a %>% select(-c("id", "date_inbody_baseline"))) %>% round(2) 
  bb <- cbind(a %>% select("id"), bb)
  names(bb)[-1] <- paste0("∆", clinic_inbody_data_1st %>% select(-c("id","date_inbody")) %>% names(), "%")
  
  c1 <- full_join(a, b, by = c("id"))
  names(c1)[grep("date", names(c1))] <- c("date_baseline","date_endpoint")
  c1 %<>% relocate(c("date_baseline","date_endpoint"), .after = "id")
  
  c1 <- full_join(c1, aa, by = c("id"))
  c1 <- full_join(c1, bb, by = c("id"))
  
  
  
  
  a <- clinic_blood_data_1st[seq(1,nrow(clinic_blood_data_1st), 2),] %>% select(-c("date_blood"))
  names(a)[-1] <- paste0(a %>% select(-c("id")) %>% names(), "_baseline")
  
  b <- clinic_blood_data_1st[seq(2,nrow(clinic_blood_data_1st), 2),] %>% select(-c("date_blood"))
  names(b)[-1] <- paste0(b %>% select(-c("id")) %>% names(), "_endpoint")
  
  aa <- b %>% select(-c("id")) - a %>% select(-c("id"))
  aa <- cbind(a %>% select("id"), aa)
  names(aa)[-1] <- paste0("∆", clinic_blood_data_1st %>% select(-c("id","date_blood")) %>% names())
  
  bb <- ((b %>% select(-c("id")) - a %>% select(-c("id"))) / a %>% select(-c("id")) *100) %>% round(2)
  bb <- cbind(a %>% select("id"), bb)
  names(bb)[-1] <- paste0("∆", clinic_blood_data_1st %>% select(-c("id","date_blood")) %>% names(), "%")
  
  
  c2 <- full_join(a, b, by = c("id"))
  c2 <- full_join(c2, aa, by = c("id"))
  c2 <- full_join(c2, bb, by = c("id"))
  
  
  stat_table_1st <- full_join(c1, c2, by = c("id"))
  
  rm(list = c("a","b", "c1", "c2", "clinic_inbody_data_1st", "clinic_blood_data_1st", "aa", "bb"))
  
  
  
  stat_table_1st <- lin_mapping(stat_table_1st, client_type, id, clinical_list, client_type, id)
  stat_table_1st <- lin_mapping(stat_table_1st, age, id, clinical_list, age, id)
  stat_table_1st <- lin_mapping(stat_table_1st, gender, id, clinical_list, gender, id)
  
  stat_table_1st <- lin_mapping(stat_table_1st, age, id, out_of_genesis_list_age_gender, age, id)
  stat_table_1st <- lin_mapping(stat_table_1st, gender, id, out_of_genesis_list_age_gender, gender, id)
  rm(out_of_genesis_list_age_gender)
  
  #With baseline OGTT data
  stat_table_1st <- stat_table_1st[Reduce(dplyr::intersect,list(which(!is.na(stat_table_1st$glucose_ac_baseline)),
                                                                which(!is.na(stat_table_1st$glucose_pc_1hr_baseline)),
                                                                which(!is.na(stat_table_1st$glucose_pc_2hr_baseline)),
                                                                which(!is.na(stat_table_1st$insulin_baseline)),
                                                                which(!is.na(stat_table_1st$insulin_pc_1hr_baseline)),
                                                                which(!is.na(stat_table_1st$insulin_pc_2hr_baseline)))),]
  
  
  ###produce stat_table
  stat_table_1st %<>% relocate(c("client_type","age","gender"), .after = "id")
  
  
  
  # 06 input_diet ---------------------------------------------------------
  
    #1-1. input diet record by day
    diet_record <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/diet_record.csv", 
                                   col_names = FALSE)
    names(diet_record) <- c("id","date_diet","note_counts","pic_counts","essay_counts","light_green",
                            "light_yellow","light_red","Cal","fat","protein","carb","x1","x2")
    #data wrangling
    diet_record <- diet_record %>% mutate(calorie_day = carb*4 + protein*4 + fat*9)
    diet_record <- diet_record %>% mutate(`carb_E%` = round((carb*4/calorie_day) *100,2))
    diet_record <- diet_record %>% mutate(`protein_E%` = round((protein*4/calorie_day) *100,2))
    diet_record <- diet_record %>% mutate(`fat_E%` = round((fat*9/calorie_day) *100,2))
  
    #1-2. input diet meal by meal
    diet_meal <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/diet_meal.csv")
    diet_meal$client_id %<>% as.integer()
    diet_meal$date %<>% as.Date()
    diet_meal[c("calorie","carbohydrate","fat","protein","fruits","vegetables","grains","meat_beans_low_fat",
                "meat_beans_medium_fat","meat_beans_high_fat","milk_whole_fat","milk_low_fat","milk_skim",
                "oil","meal_order","water_intake")] %<>% lapply(as.numeric)
    diet_meal %<>% dplyr::rename(calorie_meal = calorie)
    diet_meal %<>% dplyr::rename(id = client_id)
    diet_meal %<>% mutate(meat_bean = rowSums(select(., meat_beans_low_fat, meat_beans_medium_fat, meat_beans_high_fat), na.rm = TRUE) )
    diet_meal %<>% mutate(milk = rowSums(select(., milk_whole_fat, milk_low_fat, milk_skim), na.rm = TRUE) )
    diet_meal %<>% select(-c("meat_beans_low_fat", "meat_beans_medium_fat","meat_beans_high_fat","milk_whole_fat","milk_low_fat","milk_skim"))
    diet_meal %<>% select(c("id","date","type","calorie_meal","carbohydrate","fat","protein","fruits","vegetables","grains","meat_bean","milk", "oil","meal_order","water_intake"))
    
    diet_meal <- diet_meal[with(diet_meal, order(id, date)),]
    
    diet_meal <- diet_meal %>% select(c("id","date","fruits","vegetables","grains","meat_bean","milk","oil"))
    diet_meal %<>% dplyr::rename(date_diet = date)
    
    #condense df by id, date
    diet_meal <- 
      diet_meal %>% 
      group_by(id, date_diet) %>% 
      summarise(fruits = sum(fruits, na.rm = TRUE),
                vegetables = sum(vegetables, na.rm = TRUE),
                grains = sum(grains, na.rm = TRUE),
                meat_bean = sum(meat_bean, na.rm = TRUE),
                milk = sum(milk, na.rm = TRUE),
                oil = sum(oil, na.rm = TRUE),
      )
    
  #merge w/ diet_record
  diet_record <- merge(diet_record, diet_meal, by.x = c("id", "date_diet"), all.x = TRUE)
  
  
  
  #Sorting by Multiple Columns
  diet_record <- diet_record[with(diet_record, order(id, date_diet)),]
  
  library(dplyr)
  diet_record <- diet_record %>% select("id","date_diet","carb_E%","protein_E%","fat_E%","calorie_day","note_counts","pic_counts","essay_counts",
                                        "light_green","light_yellow","light_red",
                                        "fruits","vegetables","grains","meat_bean","milk","oil")
  
  #filter by id
  diet_record <- diet_record[which(diet_record$id %in% stat_table_1st$id),]
  
  
  #filter diet_record between baseline_date to end-point_date
  #add filter criteria
  diet_record <- lin_mapping(diet_record, date_baseline, id, stat_table_1st, date_baseline, id)
  diet_record <- lin_mapping(diet_record, date_endpoint, id, stat_table_1st, date_endpoint, id)
  #filter code
  diet_record <- diet_record %>% filter( (date_diet >= date_baseline) & (date_diet <= date_endpoint) )
  
  
  ##[20230208] adjust low calorie_day outliers
  cut_off_calorie_day <- 500
  diet_record[diet_record$calorie_day < cut_off_calorie_day, "calorie_day"] <- NA
  pool_id <- diet_record %>% filter(is.na(calorie_day)) %>% select(id) %>% unique() %>% pull()
  for (i in pool_id) {
    diet_record[(diet_record$id == i) & (is.na(diet_record$calorie_day)), "calorie_day"] <- 
      diet_record %>% filter(id == i) %>% select(calorie_day) %>% pull() %>% mean(na.rm = TRUE)
    if (i == pool_id %>% tail(1)) {
      rm(pool_id)
    }
  }
  
  
  #****1st nutritient_E% correlation with ∆HDL, ∆LDL[Q1.怎麼吃幅度最低??!]
  
  #****[Q2. Obedience with Inbody data, ∆HDL, ∆LDL?]
  #1st treatment period, upload day % 
  #input id list  
  diet_obedience <- as.data.frame(unique(diet_record$id))
  names(diet_obedience)[1] <- "id"
  
  
  for (i in c(diet_obedience$id)) {
    #create new col 
    if (i == c(diet_obedience$id) %>% head(1)) {
      diet_obedience[,c("day_count","upload_day_%","note_count","light_G","light_Y","light_R","pic_count",
                        "carb_E%","protein_E%","fat_E%", "calorie_day",
                        "fruits","vegetables","grains","meat_bean","milk","oil")] <- NA
      j = 1
      cat("\n\n[Establish diet table...]\n")
    }
    
    #inbody_T0 inbody_T1
    diet_T0 <- stat_table_1st %>% filter(id == i) %>% select("date_baseline") %>% pull()
    diet_T0 <- diet_record %>% filter(id == i) %>% filter(date_diet >= diet_T0) %>% select(date_diet) %>% pull() %>% head(1)
    diet_T1 <- stat_table_1st %>% filter(id == i) %>% select("date_endpoint") %>% pull()
    
    diet_record_temp <- diet_record %>% filter( (date_diet >= diet_T0) & (date_diet <= diet_T1) )
    
    
    #upload calorie_day
      diet_obedience[which(diet_obedience$id == i),"calorie_day"] <- (diet_record_temp %>% filter(id == i) %>% select(calorie_day) %>% sum(na.rm = TRUE)) / (diet_record_temp %>% filter(id == i) %>% nrow()) %>% round(2)
    #upload day count
      diet_obedience[which(diet_obedience$id == i),"day_count"] <- diet_record_temp %>% filter(id == i) %>% filter((date_diet >= diet_T0) & (date_diet <= diet_T1)) %>% nrow()
    #upload day count%
      diet_obedience[which(diet_obedience$id == i),"upload_day_%"] <- ((diet_record_temp %>% filter(id == i) %>% filter((date_diet >= diet_T0) & (date_diet <= diet_T1)) %>% nrow())*100/ ((diet_T1 - diet_T0 + 1) %>% as.numeric())) %>% round(2)
    #upload meal_note count
      diet_obedience[which(diet_obedience$id == i),"note_count"] <- diet_record_temp %>% filter(id == i) %>% select(note_counts) %>% sum(na.rm = TRUE)
    #Light counts: green, yellow, red
      diet_obedience[which(diet_obedience$id == i),"light_G"] <- diet_record_temp %>% filter(id == i) %>% select(light_green) %>% sum(na.rm = TRUE)
      diet_obedience[which(diet_obedience$id == i),"light_Y"] <- diet_record_temp %>% filter(id == i) %>% select(light_yellow) %>% sum(na.rm = TRUE)
      diet_obedience[which(diet_obedience$id == i),"light_R"] <- diet_record_temp %>% filter(id == i) %>% select(light_red) %>% sum(na.rm = TRUE)
    #Pic count
      diet_obedience[which(diet_obedience$id == i),"pic_count"] <- diet_record_temp %>% filter(id == i) %>% select(pic_counts) %>% sum(na.rm = TRUE)
    #3 types of nutrients calorie_day_%
      diet_obedience[which(diet_obedience$id == i),"carb_E%"] <- 
        round((sum((diet_record_temp %>% filter(id == i) %>% filter(!is.nan(`carb_E%`)) %>% select(`carb_E%`)*0.01) * diet_record_temp %>% filter(id == i) %>% filter(!is.nan(`carb_E%`)) %>% select(`calorie_day`)) / sum(diet_record_temp %>% filter(id == i) %>% select(`calorie_day`)))*100,2)
      diet_obedience[which(diet_obedience$id == i),"protein_E%"] <- 
        round((sum((diet_record_temp %>% filter(id == i) %>% filter(!is.nan(`protein_E%`)) %>% select(`protein_E%`)*0.01) * diet_record_temp %>% filter(id == i) %>% filter(!is.nan(`protein_E%`)) %>% select(`calorie_day`)) / sum(diet_record_temp %>% filter(id == i) %>% select(`calorie_day`)))*100,2)
      diet_obedience[which(diet_obedience$id == i),"fat_E%"] <- 
        round((sum((diet_record_temp %>% filter(id == i) %>% filter(!is.nan(`fat_E%`)) %>% select(`fat_E%`)*0.01) * diet_record_temp %>% filter(id == i) %>% filter(!is.nan(`fat_E%`)) %>% select(`calorie_day`)) / sum(diet_record_temp %>% filter(id == i) %>% select(`calorie_day`)))*100,2)
      
    #6 types of food
      #fruits / upload day count
      diet_obedience[which(diet_obedience$id == i),"fruits"] <- (diet_record_temp %>% filter(id == i) %>% select(fruits) %>% sum(na.rm = TRUE)) / (diet_record_temp %>% filter(id == i) %>% filter((date_diet >= diet_T0) & (date_diet <= diet_T1)) %>% nrow())
      #vegetables / upload day count
      diet_obedience[which(diet_obedience$id == i),"vegetables"] <- (diet_record_temp %>% filter(id == i) %>% select(vegetables) %>% sum(na.rm = TRUE)) / (diet_record_temp %>% filter(id == i) %>% filter((date_diet >= diet_T0) & (date_diet <= diet_T1)) %>% nrow())
      #grains / upload day count
      diet_obedience[which(diet_obedience$id == i),"grains"] <- (diet_record_temp %>% filter(id == i) %>% select(grains) %>% sum(na.rm = TRUE)) / (diet_record_temp %>% filter(id == i) %>% filter((date_diet >= diet_T0) & (date_diet <= diet_T1)) %>% nrow())
      #meat_bean / upload day count
      diet_obedience[which(diet_obedience$id == i),"meat_bean"] <- (diet_record_temp %>% filter(id == i) %>% select(meat_bean) %>% sum(na.rm = TRUE)) / (diet_record_temp %>% filter(id == i) %>% filter((date_diet >= diet_T0) & (date_diet <= diet_T1)) %>% nrow())
      #milk / upload day count
      diet_obedience[which(diet_obedience$id == i),"milk"] <- (diet_record_temp %>% filter(id == i) %>% select(milk) %>% sum(na.rm = TRUE)) / (diet_record_temp %>% filter(id == i) %>% filter((date_diet >= diet_T0) & (date_diet <= diet_T1)) %>% nrow())
      #oil / upload day count
      diet_obedience[which(diet_obedience$id == i),"oil"] <- (diet_record_temp %>% filter(id == i) %>% select(oil) %>% sum(na.rm = TRUE)) / (diet_record_temp %>% filter(id == i) %>% filter((date_diet >= diet_T0) & (date_diet <= diet_T1)) %>% nrow())
    
    
    
    if (i == c(diet_obedience$id) %>% tail(1)) {
      rm(diet_record_temp)
      cat("\n[Completed!]\n")
    }
    progress(j, length(c(diet_obedience$id)))
    j = j + 1
    
  }
  
  
  
  #mutate: pic_note
  diet_obedience <- round(diet_obedience %>% mutate(pic_per_note = pic_count / note_count),2)
  diet_obedience <- diet_obedience %>% mutate( `light_G_%`  = round(light_G/(light_G + light_Y + light_R)*100,2) )
  diet_obedience <- diet_obedience %>% mutate( `light_Y_%`  = round(light_Y/(light_G + light_Y + light_R)*100,2) )
  diet_obedience <- diet_obedience %>% mutate( `light_R_%`  = round(light_R/(light_G + light_Y + light_R)*100,2) )
  
  #map client_type
  diet_obedience <- lin_mapping(diet_obedience, client_type, id , clinical_list, client_type, id)
  
  
  ###join diet_record
  stat_table_1st <- full_join(stat_table_1st, diet_obedience, by = c("id", "client_type"))
  
  #add Doctor
  stat_table_1st$doctor <- "宋醫師"
  stat_table_1st <- lin_mapping(stat_table_1st, doctor, id, clinical_list, doctor, id, overwrite = TRUE)
  
  
  #add other vars
    ##total AUC
  stat_table_1st$tAUCg <- lin_AUC_calc(stat_table_1st, stat_table_1st %>% names() %>% grep("glucose", ., value = TRUE) %>% grep("baseline", ., value = TRUE))
  stat_table_1st$tAUCi <- lin_AUC_calc(stat_table_1st, stat_table_1st %>% names() %>% grep("insulin", ., value = TRUE) %>% grep("baseline", ., value = TRUE))
    ##OGIRIndex: iAUC-i(-30) - iAUC-g(+50)
  stat_table_1st <- stat_table_1st %>% mutate(OGIRIndex = lin_AUC_calc(stat_table_1st, stat_table_1st %>% names() %>% grep("insulin", ., value = TRUE) %>% grep("baseline", ., value = TRUE), increment_value = -30) - 
                                                lin_AUC_calc(stat_table_1st, stat_table_1st %>% names() %>% grep("glucose", ., value = TRUE) %>% grep("baseline", ., value = TRUE), increment_value = 50))
  
  
  

# Split into OB/DM --------------------------------------------------------

  #Dataset for Following analysis
  stat_table_1st_ob <- stat_table_1st %>% filter(client_type != 1) %>% filter(!is.na(insulin_baseline) & !is.na(insulin_pc_1hr_baseline) & !is.na(insulin_pc_2hr_baseline)) 
  stat_table_1st_dm <- stat_table_1st %>% filter(client_type == 1) %>% filter(!is.na(insulin_baseline) & !is.na(insulin_pc_1hr_baseline) & !is.na(insulin_pc_2hr_baseline)) 
  
  
  
  
  