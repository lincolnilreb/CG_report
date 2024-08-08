

# 01. Pie chart -----------------------------------------------------------
# [Issue: 有部分人只有飲食記錄，沒有體重紀錄]
df03_FLC_self_report <- df03_FLC_self_report_w8df %>% filter(!is.na(`weight(T0)`))

#Age
df03_FLC_self_report$age_gp <- cut(df03_FLC_self_report$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
df03_FLC_self_report <- df03_FLC_self_report %>% filter(!is.na(age_gp))
pie_flc_01 <- 
  df03_FLC_self_report %>% group_by(age_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Age',
                                                                                                legend = "{position:'right'}",
                                                                                                pieHole = 0.5,
                                                                                                #slices = "{1:{offset:0.1}}",
                                                                                                backgroundColor = "#f9fffb",
                                                                                                width = "600",
                                                                                                height = "400"))

#Gender
pie_flc_02 <- 
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

pie_flc_03 <- 
  df03_FLC_self_report %>% filter(!is.na(bmi_gp)) %>% filter(gender == "male") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Male',
                                                                                                                             legend = "{position:'right'}",
                                                                                                                             pieHole = 0.5,
                                                                                                                             #slices = "{2:{offset:0.1}}",
                                                                                                                             backgroundColor = "#f9fffb",
                                                                                                                             width = "600",
                                                                                                                             height = "400"))

pie_flc_04 <- 
  df03_FLC_self_report %>% filter(!is.na(bmi_gp)) %>% filter(gender == "female") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Female',
                                                                                                                               legend = "{position:'right'}",
                                                                                                                               pieHole = 0.5,
                                                                                                                               #slices = "{1:{offset:0.1}}",
                                                                                                                               backgroundColor = "#f9fffb",
                                                                                                                               width = "600",
                                                                                                                               height = "400"))






# 02. Cor ---------------------------------------------------------------------


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



# 03.  Strat.  Weight loss% -----------------------------------------------


##T0,T1,∆ plot

df03_FLC_self_report$`∆weight%` %>% summary()

# Method 1: 按照data散佈百分比. e.g., Q1~Q4
# df03_FLC_self_report$gp <- df03_FLC_self_report$`∆weight%` %>% cut(breaks = 3, labels = c("Good","Medium","Poor"))
df03_FLC_self_report$gp <- df03_FLC_self_report$`∆weight%` %>% cut(breaks = c(-Inf, -8, -4, Inf), labels = c("Good","Medium","Poor"))
df03_FLC_self_report$gp <- factor(df03_FLC_self_report$gp, levels = (c("Good","Medium","Poor") %>% rev()) )


df03_FLC_self_report$`∆weight%` %>% cut(breaks = 3, labels = c())
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
  footnote(general_title = c("Note:"), general = c(rbind("\n", c("- Poor: Less than 4%", "- Medium: Between 4~8%", "- Good: More than 8%", "- Program: 經典8週-FLC, 進階計畫, 宋醫師專班-FLC, 宋醫師進階計畫"))),
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
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
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

















