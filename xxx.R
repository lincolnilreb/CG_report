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

pub_df_cofit <- pub_df_cofit %>% filter(client_id %in% pub_df$client_id)

#reorder
pub_df_cofit <- pub_df_cofit[with(pub_df_cofit, order(client_id, date_flc_t0, date)),]


#diet
pub_df <- 
pub_df_cofit %>% 
  group_by(client_id, date_flc_t0) %>% 
  summarise(
    age = first(age),
    gender = first(gender),
    height = first(height),
    
    upload_day_p = (n()*100/57) %>% round(2),
    pic_counts = pic_count %>% sum(na.rm = T),
    light_sum = light_g_count %>% sum(na.rm = T) + light_y_count %>% sum(na.rm = T) + light_r_count %>% sum(na.rm = T),
    light_g_p = ((light_g_count %>% sum(na.rm = T))*100/light_sum) %>% round(2),
    light_y_p = ((light_y_count %>% sum(na.rm = T))*100/light_sum) %>% round(2),
    light_r_p = ((light_r_count %>% sum(na.rm = T))*100/light_sum) %>% round(2),
    calorie_sum = ((carbohydrate %>% sum(na.rm = T))*4 + (protein %>% sum(na.rm = T))*4 + (fat %>% sum(na.rm = T))*9), 
    calorie_day = (((carbohydrate %>% sum(na.rm = T))*4 + (protein %>% sum(na.rm = T))*4 + (fat %>% sum(na.rm = T))*9)/n()) %>% round(2),
    carb_ep = ((carbohydrate %>% sum(na.rm = T))*4*100 / calorie_sum) %>% round(2),
    protein_ep = ((protein %>% sum(na.rm = T))*4*100 / calorie_sum) %>% round(2),
    fat_ep = ((fat %>% sum(na.rm = T))*9*100 / calorie_sum) %>% round(2),
    
    weight_before = weight %>% first(),
    weight_after = weight %>% last(),
    weight_delta = weight_after - weight_before %>% round(2),
    weight_delta_p = ((weight_after - weight_before)*100/weight_before) %>% round(2),
    
    bmi_before = bmi %>% first(),
    bmi_after = bmi %>% last(),
    bmi_delta = bmi_after - bmi_before %>% round(2),
    bmi_delta_p = ((bmi_after - bmi_before)*100/bmi_before) %>% round(2),
    
    wc_before = waist_circumference %>% first(),
    wc_after = waist_circumference %>% last(),
    wc_delta = wc_after - wc_before %>% round(2),
    wc_delta_p = ((wc_after - wc_before)*100/wc_before) %>% round(2),
    
    fat_before = body_fat_mass %>% first(),
    fat_after = body_fat_mass %>% last(),
    fat_delta = fat_after - fat_before %>% round(2),
    fat_delta_p = ((fat_after - fat_before)*100/fat_before) %>% round(2)
  ) %>% ungroup()

pub_df <- pub_df %>% 
  mutate_if(is.numeric, ~ifelse(is.nan(.), NA, .))
pub_df <- pub_df %>% 
  mutate_if(is.numeric, ~ifelse(is.infinite(.), NA, .))

pub_df$calorie_sum  <-  ifelse(pub_df$calorie_sum == 0, NA, pub_df$calorie_sum)
pub_df$calorie_day  <-  ifelse(pub_df$calorie_day == 0, NA, pub_df$calorie_day)

names(pub_df) <- pub_df %>% names() %>% lin_ch_en_format(., format = "en", origin = "raw_en")


#_________




# 01. Pie chart -----------------------------------------------------------


#Age
pub_df$age_gp <- cut(pub_df$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
pub_df <- pub_df %>% filter(!is.na(age_gp))
pie_flc_01 <- 
  pub_df %>% group_by(age_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Age',
                                                                                                   legend = "{position:'right'}",
                                                                                                   pieHole = 0.5,
                                                                                                   #slices = "{1:{offset:0.1}}",
                                                                                                   backgroundColor = "#f9fffb",
                                                                                                   width = "600",
                                                                                                   height = "400"))

#Gender
pie_flc_02 <- 
  pub_df %>% group_by(gender) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Gender',
                                                                                                   legend = "{position:'right'}",
                                                                                                   pieHole = 0.5,
                                                                                                   #slices = "{0:{offset:0.1}}",
                                                                                                   backgroundColor = "#f9fffb",
                                                                                                   colors = "['#DC3912', '#3366CC']",
                                                                                                   width = "600",
                                                                                                   height = "400"))



#BMI x Obesity

pub_df$bmi_gp <- cut(pub_df$`BMI(T0)`, c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))

pie_flc_03 <- 
  pub_df %>% filter(!is.na(bmi_gp)) %>% filter(gender == "male") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Male',
                                                                                                                                                           legend = "{position:'right'}",
                                                                                                                                                           pieHole = 0.5,
                                                                                                                                                           #slices = "{2:{offset:0.1}}",
                                                                                                                                                           backgroundColor = "#f9fffb",
                                                                                                                                                           width = "600",
                                                                                                                                                           height = "400"))

pie_flc_04 <- 
  pub_df %>% filter(!is.na(bmi_gp)) %>% filter(gender == "female") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Female',
                                                                                                                                                             legend = "{position:'right'}",
                                                                                                                                                             pieHole = 0.5,
                                                                                                                                                             #slices = "{1:{offset:0.1}}",
                                                                                                                                                             backgroundColor = "#f9fffb",
                                                                                                                                                             width = "600",
                                                                                                                                                             height = "400"))






# 02. Cor ---------------------------------------------------------------------


#[Create profile]  Efficacy, Baseline, Diet table
profile_efficacy <- pub_df %>% 
  select(c("∆weight","∆weight%","∆BMI","∆BMI%","∆Fat","∆Fat%","∆wc","∆wc%"))

names(profile_efficacy) <- names(profile_efficacy) %>% lin_ch_en_format(format = "ch", origin = "en")
# 
# profile_baseline <- pub_df %>% 
#   select(c("age", "weight(T0)","weight(T1)","BMI(T0)","BMI(T1)","Fat(T0)","Fat(T1)","wc(T0)","wc(T1)"))
# 
# names(profile_baseline) <- names(profile_baseline) %>% lin_ch_en_format(format = "ch", origin = "en")

profile_diet <- pub_df %>%
  select(c("age", "weight(T0)","weight(T1)","BMI(T0)","BMI(T1)","Fat(T0)","Fat(T1)","wc(T0)","wc(T1)","upload_day_%", "pic_counts","calorie_day","carb_E%","protein_E%","fat_E%","light_G_%","light_Y_%","light_R_%"))

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



# 03.  Strat.  Weight loss% -----------------------------------------------

pub_df$gender <- pub_df$gender %>% factor(levels = c("female", "male"))

##T0,T1,∆ plot

pub_df$`∆weight%` %>% summary()

# Method 1: 按照data散佈百分比. e.g., Q1~Q4
pub_df$gp <- pub_df$`∆weight%` %>% cut(breaks = c(-Inf, -8, -4, Inf), labels = c(">8%","4~8%","<4%"))
# pub_df$`∆weight%` %>% cut(breaks = 3, labels = c())
# Method 2: makes n groups with (approximately) equal numbers of observations;
# pub_df$gp <- pub_df$`∆weight%` %>% cut_number(3, labels = c("Good","Medium","Poor"))
# pub_df$gp <- pub_df$`∆weight%` %>% cut_number(3)
pub_df$gp %>% table() %>% addmargins()

datasets_target_issue <- pub_df %>% dplyr::rename(gp = gp)
datasets_target_issue <- datasets_target_issue %>% filter(gp %in% levels(datasets_target_issue$gp))


#profile
vars_en <- c("id","age","gender",
             #inbody - baseline
             "weight(T0)","BMI(T0)","Fat(T0)","wc(T0)",
             #inbody - endpoint
             "weight(T1)","BMI(T1)","Fat(T1)","wc(T1)",
             #diet
             "upload_day_%","pic_counts","carb_E%","protein_E%","fat_E%","calorie_day","light_G_%","light_Y_%","light_R_%",
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

#[customized part!!!]
myplots_flc <- vector('list', length(var_vector))

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
    rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ "))) 
  stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_sd", dodge = 0.8)
  
  #for customed summary table - part 1/4 [p value]
  vector_pvalue <- append(vector_pvalue, 
                          stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
  )
  
  #plot
  plot <- 
    datasets_target_issue_for_plot %>%
    filter(!is.na(!!as.name(a))) %>%
    ggbarplot(x = "gender", y = a, fill = "gp", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0,
              add = "mean_sd", add.params = list(group = "gp"),
              position = position_dodge(0.8), legend = "right", legend.title = "") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "", y = "Mean ± SD", title = a_title) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15)
    ) +
    stat_pvalue_manual(
      stat.test, label = "p.adj.signif", tip.length = 0.0,
      bracket.nudge.y = 1, step.increase = 0.05, hide.ns = TRUE 
    )
  
  #[customized part!!!]
  myplots_flc[[j]] <- plot
  
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}

rm(list = c("datasets_target_issue_a", "datasets_target_issue_b"))

#(2.)gender x Group table
#[customized part!!!]
table_01_flc <- 
  table(datasets_target_issue$gender, datasets_target_issue$gp) %>% addmargins() %>% 
  kable(format = "html", caption = "<b>Table: Population</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Note:"), general = c(rbind("", c(" FLC Program"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)


#(3.)output statistics table
#for customed summary table [summary table]
#[customized part!!!]
summary_table_flc <- 
  datasets_target_issue %>% 
  group_by(gender, gp) %>% 
  summarize_at(vars_en[var_vector],
               # SE
               # function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
               # SD
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)) %>% round(2), sep = " ± ")
  )




#rbind: summary_table, p.adj.sign, dif, improvement
summary_table_flc <- cbind(summary_table_flc %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t(), as.data.frame(vector_pvalue))

colnames(summary_table_flc) <-
  c(paste0((levels(datasets_target_issue_for_plot$gp) %>% as.character())[-6],
           paste0("\n(n=",table(datasets_target_issue_for_plot$gp, datasets_target_issue_for_plot$gender) %>% as.numeric(),")")), "顯著差異")
rownames(summary_table_flc) <- myplot_table$vars_ch

#[customized part!!!]
table_02_flc <- 
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


pub_df <- pub_df %>% filter((diet_compliance >= 0) & (diet_compliance <= 100))
# pub_df$gp_diet_compliance <- pub_df$diet_compliance %>% cut(breaks = 3, c("Low", "Medium", "High"))
# pub_df$gp_diet_compliance <- pub_df$`light_G_%` %>% cut(breaks = 3, c("Low", "Medium", "High"))
# pub_df$gp_diet_compliance <- pub_df$`light_G_%` %>% cut_number(n = 3, c("Low", "Medium", "High"))
# pub_df$gp_diet_compliance <- pub_df$`upload_day_%` %>% cut(breaks = 3, c("Low", "Medium", "High"))
pub_df$gp_diet_compliance <- pub_df$`upload_day_%` %>% cut_number(n = 3, c("Low", "Medium", "High"))
# pub_df$gp_diet_compliance <- pub_df$`carb_E%` %>% cut(breaks = 3, c("Low", "Medium", "High"))
# pub_df$gp_diet_compliance <- pub_df$`protein_E%` %>% cut(breaks = 3, c("Low", "Medium", "High"))
# pub_df$gp_diet_compliance <- pub_df$`fat_E%` %>% cut(breaks = 3, c("Low", "Medium", "High"))

pub_df %>% 
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
  rbind(pub_df %>% select(c("id","gender","weight(T0)","BMI(T0)","Fat(T0)","wc(T0)","weight(T1)","BMI(T1)","Fat(T1)","wc(T1)","∆weight","∆BMI","∆Fat","∆wc","∆weight%","∆BMI%","∆Fat%","∆wc%","age","upload_day_%")) %>%
          mutate(gp = "Cofit"),
        df04_non_FLC_self_report %>% select(c("id","gender","weight(T0)","BMI(T0)","Fat(T0)","wc(T0)","weight(T1)","BMI(T1)","Fat(T1)","wc(T1)","∆weight","∆BMI","∆Fat","∆wc","∆weight%","∆BMI%","∆Fat%","∆wc%","age","upload_day_%")) %>% 
          mutate(gp = "Control")
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
stat.test <- stat.test %>% rstatix::add_xy_position(x = "gp", fun = "mean_sd", dodge = 0.8)



#plot
plot_flc_ctrl_comparison <- 
  a %>% mutate(x = `∆weight%` %>% multiply_by(-1)) %>% 
  ggbarplot(x = "gp", y = "x", fill = "gp_diet_compliance", alpha = .5,
            add = "mean_sd", add.params = list(group = "gp_diet_compliance"),
            position = position_dodge(0.8), legend = "right", legend.title = "Diet Score",
            # label = TRUE, lab.nb.digits = 2, lab.vjust = -1.5
  ) +
  # scale_x_discrete(labels = c("Cofit", "Control")) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  labs(x = "", y = "Mean ± SD", title = "Weight Loss(%)") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15)
  ) +
  stat_pvalue_manual(
    stat.test, label = "p.adj.signif", tip.length = 0.0,
    bracket.nudge.y = 1, step.increase = 0.01, hide.ns = TRUE 
  )


rm(list = c("a", "b"))





