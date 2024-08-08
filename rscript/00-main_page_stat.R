# main page stat. ---------------------------------------------------------
##  台北目前累積病人數? Total/OB(finished/ongoing)/DM(finished/ongoing)
### - googleVis::gvisAnnotationChart: client_monthly_stat_report
##  初日減重門診，已經幫助了多少人成功減重? 
### - [目前分析樣本]: `r success_df_freq[["Total"]] %>% max()` 
### - [成功減重人數(%)]: 
# 總共: `r success_df_freq[success_df_freq$gender == "Total", "success"]` ; `r success_df_pct[success_df_freq$gender == "Total", "success"]`
# 女性: `r success_df_freq[success_df_freq$gender == "female", "success"]` ; `r success_df_pct[success_df_freq$gender == "female", "success"]`
# 男性: `r success_df_freq[success_df_freq$gender == "male", "success"]` ; `r success_df_pct[success_df_freq$gender == "male", "success"]`
### - success_df_pct
### - success_df_freq



client_monthly_stat_report_total_id <- df01_profile %>%
  filter((org_name != "cofit") & (org_name != "bk2o_backup") & (org_name != "fitness_factory")) %>%
  select(id) %>% pull() %>% unique() %>% length()





# 2. Linechart_x_Time --------------------------------------------------------


#台北目前累積病人數? Total/OB(finished/ongoing)/DM(finished/ongoing)大概多少? (by month)
## temp: from clinical list. future: from PostgreSQL
## build OB/DM note_col
#Establish category
clinical_stat_category <- factor(levels = (c("Total", "OB", "OB(~)", "DM", "DM(~)")))

#秀傳開幕後("2020-09-30") 開始計算
main_pagedf <- df01_profile %>% filter((date_t0 >= "2020-09-30") & (org_name %in% c("genesisclinic","topshow","lumez")))

#[Problem] topshow w/t client_type : all weight loss: 2
#[Problem] lumez list? advance:client_type?
main_pagedf[(is.na(main_pagedf[["client_type"]])), "client_type"] <- 2


#floor date for monthly calculation
main_pagedf <- main_pagedf %>% mutate(date_cate = date_t0 %>% lubridate::floor_date(unit = "month"))
main_pagedf <- main_pagedf %>% mutate(date_finish = date_t1 %>% lubridate::floor_date(unit = "month"))
#clean client_type as factor
main_pagedf$client_type <- factor(main_pagedf$client_type, levels = c("1", "2"))

#thr summarise to integrate finish/ongoing df, group by date, client_type, exclude NA col(missing value: class_date)
client_stat_df_tmp <- data.frame(date = rep(seq(as.Date(main_pagedf$date_cate %>% unique() %>% min()), as.Date(main_pagedf$date_cate %>% unique() %>% max()), by = "month"), each = levels(main_pagedf$client_type) %>% length()), 
                                 client_type = rep(levels(main_pagedf$client_type), seq(as.Date(main_pagedf$date_cate %>% unique() %>% min()), as.Date(main_pagedf$date_cate %>% unique() %>% max()), by = "month") %>% length())
                                 )
client_stat_df <- 
  left_join(main_pagedf %>% 
              group_by(date_cate, client_type, .drop = FALSE) %>% 
              summarise(
                n = n()
              ) %>% dplyr::rename(date = date_cate),
            main_pagedf %>% 
              group_by(date_finish, client_type, .drop = FALSE) %>% 
              summarise(
                n = n()
              ) %>% dplyr::rename(date = date_finish),
            by = c("date", "client_type"),
  ) %>% lin_exclude_NA_col(., variables = c("date", "client_type"))

client_stat_df <- client_stat_df %>% dplyr::rename(class_buy = n.x, class_finish = n.y)

client_stat_df <- merge(client_stat_df, client_stat_df_tmp, by.y = c("date", "client_type"), all.y = TRUE) 
client_stat_df[is.na(client_stat_df)] <- 0
rm(client_stat_df_tmp)


#Establish auxiliary df
client_stat_df$class_buy_cumsum_all <- client_stat_df$class_buy %>% cumsum()
client_stat_df$class_finish_cumsum_all <- client_stat_df$class_finish %>% cumsum()
client_stat_df <- client_stat_df %>% mutate(class_ongoing_all = class_buy_cumsum_all - class_finish_cumsum_all)

client_stat_df$class_buy_cumsum_sub <- c(rbind(client_stat_df %>% filter(client_type == 1) %>% select(class_buy) %>% pull() %>% cumsum(), 
                                               client_stat_df %>% filter(client_type == 2) %>% select(class_buy) %>% pull() %>% cumsum()))

client_stat_df$class_finish_cumsum_sub <- c(rbind(client_stat_df %>% filter(client_type == 1) %>% select(class_finish) %>% pull() %>% cumsum(), 
                                                  client_stat_df %>% filter(client_type == 2) %>% select(class_finish) %>% pull() %>% cumsum()))
client_stat_df <- client_stat_df %>% mutate(class_ongoing_sub = class_buy_cumsum_sub - class_finish_cumsum_sub)


#Establish dashboard df
accum_client_df <- data.frame(date = rep(seq(as.Date(main_pagedf$date_cate %>% unique() %>% min()), as.Date(main_pagedf$date_cate %>% unique() %>% max()), by = "month"), each = levels(clinical_stat_category) %>% length()), 
                              category = rep(levels(clinical_stat_category), seq(as.Date(main_pagedf$date_cate %>% unique() %>% min()), as.Date(main_pagedf$date_cate %>% unique() %>% max()), by = "month") %>% length()),
                              value = rep(NA, (seq(as.Date(main_pagedf$date_cate %>% unique() %>% min()), as.Date(main_pagedf$date_cate %>% unique() %>% max()), by = "month") %>% length())*(levels(clinical_stat_category) %>% length())),
                              anno_title = rep(NA, (seq(as.Date(main_pagedf$date_cate %>% unique() %>% min()), as.Date(main_pagedf$date_cate %>% unique() %>% max()), by = "month") %>% length())*(levels(clinical_stat_category) %>% length())),
                              anno_text = rep(NA, (seq(as.Date(main_pagedf$date_cate %>% unique() %>% min()), as.Date(main_pagedf$date_cate %>% unique() %>% max()), by = "month") %>% length())*(levels(clinical_stat_category) %>% length())))




#fill in accum_client_df
accum_client_df[accum_client_df$category == "Total", "value"] <- client_stat_df[client_stat_df$client_type == 2, "class_buy_cumsum_all"]
accum_client_df[accum_client_df$category == "OB", "value"] <- client_stat_df[client_stat_df$client_type == 2, "class_finish_cumsum_sub"]
accum_client_df[accum_client_df$category == "OB(~)", "value"] <- client_stat_df[client_stat_df$client_type == 2, "class_ongoing_sub"]
accum_client_df[accum_client_df$category == "DM", "value"] <- client_stat_df[client_stat_df$client_type == 1, "class_finish_cumsum_sub"]
accum_client_df[accum_client_df$category == "DM(~)", "value"] <- client_stat_df[client_stat_df$client_type == 1, "class_ongoing_sub"]

# accum_client_df[(accum_client_df$date == "2021-09-01") & (accum_client_df$category == "Total") , "anno_title"] <- "Genesis Opening"

rm(list = c("clinical_stat_category", "client_stat_df"))
client_monthly_stat_report_total_client <- accum_client_df %>% filter(category == "Total") %>% select(value) %>% max(na.rm = TRUE)
client_monthly_stat_report <- googleVis::gvisAnnotationChart(accum_client_df,
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
##client_monthly_stat_report %>% plot()



# 3. success_stat ----------------------------------------------------------


# 初日減重門診，已經幫助了多少人成功減重? 
## exclude:   quit/pause/missing value, [目前分析樣本] total N = `success_df_freq[["Total"]] %>% max()` 
## successfully loss weight freq: `success_df_freq[success_df_freq$gender == "Total", "success"]` `success_df_freq[success_df_freq$gender == "female", "success"]` `success_df_freq[success_df_freq$gender == "male", "success"]`
## (%): `success_df_pct[success_df_freq$gender == "Total", "success"]` `success_df_pct[success_df_freq$gender == "female", "success"]` `success_df_pct[success_df_freq$gender == "male", "success"]`

#pie chart



#create source data: a
stat_table_1st_ob$`∆weight_gp` <- cut(stat_table_1st_ob$`∆weight%`, c(-100, -15, -8, -4, 0, 5, 15, 100), c("<-15%","-8~15%","-4~8%", "-4~0%","0~5%","5~15%",">15%"))
stat_table_1st_ob$`∆fat_gp` <- cut(stat_table_1st_ob$`∆bf%`, c(-100, -15, -8, -4, 0, 5, 15, 100), c("<-15%","-8~15%","-4~8%", "-4~0%","0~5%","5~15%",">15%"))
stat_table_1st_ob$`∆muscle_mass_gp` <- cut(stat_table_1st_ob$`∆bm%`, c(-100, -15, -8, -4, 0, 5, 15, 100), c("<-15%","-8~15%","-4~8%", "-4~0%","0~5%","5~15%",">15%"))

data <- stat_table_1st_ob %>% select(c("gender","∆weight%","∆bf%","∆bm%","∆weight_gp","∆fat_gp","∆muscle_mass_gp"))
data$gender <-  factor(data$gender, levels = c("female", "male"))
#table
#weight
a <- 
  data %>% 
  dplyr::group_by(`∆weight_gp`, `gender`, .drop = FALSE) %>%
  summarise_at(
    vars(`∆weight%`),
    funs(round(mean(., na.rm = TRUE), 2))
  )  %>% 
  inner_join(
    data %>% 
      dplyr::group_by(`∆weight_gp`, `gender`, .drop = FALSE) %>%
      summarise(
        N = n()
      ))
a <- a[with(a, order(gender)),]
a <- a %>% dplyr::group_by(gender) %>% mutate(percentage = round(N / sum(N) *100,2)) 
names(a)[c(1,3)] <- c("Group", "Value")
a$variable <-  "Weight"

#success
success_df <- a %>% filter(variable == "Weight") %>% filter(!is.nan(Value)) %>% dplyr::mutate(., success = cut(Value, c(-Inf, 0, Inf), c(1, 0)))
success_df_freq <- data.frame(gender = c("female", "male"),
                              success = success_df %>% group_by(gender, success, .drop = FALSE) %>% summarise(N = sum(N)) %>% filter(success == 1) %>% pull(),
                              failure = success_df %>% group_by(gender, success, .drop = FALSE) %>% summarise(N = sum(N)) %>% filter(success == 0) %>% pull()) %>% janitor::adorn_totals("row") %>% janitor::adorn_totals("col")
success_df_pct <- success_df_freq %>% janitor::adorn_percentages() %>% janitor::adorn_pct_formatting()
rm(success_df)
