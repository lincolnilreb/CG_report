
stat_table_1st_ob <- stat_table_1st_ob %>% mutate(delta_sua_gp = paste(sua_gp_baseline, sua_gp_endpoint, sep = ">")) 


# sua增加±5% Normal(no change, increase, decrease), normal -> high, high -> normal 共5組
stat_table_1st_ob <- 
stat_table_1st_ob %>% 
  mutate(delta_sua_gp = case_when(
    delta_sua_gp == "Normal>Normal" & abs((uric_acid_endpoint - uric_acid_baseline)/uric_acid_baseline) <= 0.05 ~ "No change",
    delta_sua_gp == "Normal>Normal" & (uric_acid_endpoint - uric_acid_baseline)/uric_acid_baseline > 0.05 ~ "Increase",
    delta_sua_gp == "Normal>Normal" & (uric_acid_endpoint - uric_acid_baseline)/uric_acid_baseline < -0.05 ~ "Decrease",
    TRUE ~ delta_sua_gp
  ))
stat_table_1st_ob$delta_sua_gp <- stat_table_1st_ob$delta_sua_gp %>% factor(levels = c("No change","Decrease","Increase","Normal>High","High>Normal","High>High"))

a <- stat_table_1st_ob %>% filter(!is.na(sua_gp_baseline))

a$delta_sua_gp %>% table()

#uric acid: men: ≥7.0 (mg/dL); women: 6.0(mg/dL)
table_freq_sua_ob <- table(a$gender, a$delta_sua_gp) %>% addmargins() %>% 
  kable(format = "html", caption = "<b>Table: Stuty Group</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Cutoffs:"), general = c(rbind("\n", c("Male: 7.6 (mg/dL)", "Female: 6.6(mg/dL)"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)



# stat.test <- 
#   stat_table_1st_ob %>% 
#   filter(!is.na(sua_gp_baseline)) %>% 
#   select(delta_sua_gp, gender, `∆weight%`) %>% rename(value = "∆weight%") %>% 
#   mutate(value_adj = value %>% multiply_by(-1)) %>% 
#   group_by(gender) %>% 
#   rstatix::t_test(value_adj ~ delta_sua_gp) %>%
#   rstatix::add_xy_position(x = "gender", fun = "mean_sd", dodge = 0.5)
# 
# plot_SUA_03 <- 
#   stat_table_1st_ob %>% 
#   filter(!is.na(sua_gp_baseline)) %>% 
#   select(delta_sua_gp, gender, `∆weight%`) %>% rename(value = "∆weight%") %>% 
#   mutate(value_adj = value %>% multiply_by(-1)) %>% 
#   ggbarplot(x = "gender", y = "value_adj", fill = "delta_sua_gp", alpha = 0.5, width = 0.5,
#             add = "mean_se", add.params = list(group = "delta_sua_gp"),
#             label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, 
#             position = position_dodge(0.5), 
#             xlab = "", ylab = "∆Weight Loss(%)", title = paste0("減重成效", " x 尿酸"),
#             legend = "right", legend.title = "SUA Group", ggtheme = theme_light() ) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
#     axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
#     axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
#   ) +
#   stat_pvalue_manual(
#     stat.test, label = "p.adj.signif", tip.length = 0.01,
#     bracket.nudge.y = -2, hide.ns = TRUE
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)))



# table(stat_table_1st_ob$sua_gp_baseline, stat_table_1st_ob$sua_gp_endpoint)

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
  # geom_vline(xintercept = c(5.5),linetype ="dashed", ) +
  # annotate("text", x=5.3, y=155, label="Cutoff = 5.5 mg/dL", angle=90) +
  stat_cor(method = "pearson", size = 5, label.x = 7, label.y = 45) # Add correlation coefficient)

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
  # geom_vline(xintercept = c(5.5),linetype ="dashed", ) +
  # annotate("text", x=5.3, y=150, label="Cutoff = 5.5 mg/dL", angle=90) +
  stat_cor(method = "pearson", size = 5, label.x = 7, label.y = 45) # Add correlation coefficient)


# plot_SUA_04 <- 
# ggarrange(
#   ggarrange(plot_SUA_01, plot_SUA_02, ncol = 2, labels = c("A", "B")),
#   ggarrange(plot_SUA_03, labels = "C"),
#   nrow = 2
# )


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
  select(c("upload_day_%", "pic_counts","calorie_day","carb_E%","protein_E%","fat_E%","fruits_day","vegetables_day","grains_day","meat_bean_day","milk_day", "oil_day","light_G_%","light_Y_%","light_R_%"))

names(profile_diet) <- names(profile_diet) %>% lin_ch_en_format(format = "ch", origin = "en")


##[Method 2] corrplot

library(corrplot)
#[Correlation r] Efficacy x Diet
M1_sua <- cor(cbind(-profile_efficacy, profile_diet), use = "pairwise.complete.obs")
#[2Do]change row,col names into chinese
M_test1_sua <- cor.mtest(cbind(-profile_efficacy, profile_diet) , conf.level = .95)
M_col_sua <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))


# run corrplot plot
corrplot::corrplot(M1_sua,
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
                                                     width="150%",height=300))


M2_sua <- cor(cbind(-profile_efficacy, profile_baseline), use = "pairwise.complete.obs")
#[2Do]change row,col names into chinese
M_test2_sua <- cor.mtest(cbind(-profile_efficacy, profile_baseline) , conf.level = .95)
M_col_sua <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))


#run corrplot plot
corrplot::corrplot(M2_sua,
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


for (i in c(1:length(colnames(M2_sua)))) {
  if (i == 1) {
    M2_value <- M2_sua %>% round(2) 
    M2_sign <- M_test2_sua$p %>% stats::symnum(corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")) %>% as.data.frame.matrix()
    M2_df <- M2_value
    M2_df[,] <- NA
  }
  
  M2_df[,i] <- paste0(M2_value[,i], " (", M2_sign[,i], ")")
  
  if (i ==  length(colnames(M2_sua))) {
    rm(list = c("M2_value", "M2_sign"))
    M2_df <- M2_df %>% as.data.frame()
    M2_df <- M2_df %>% add_column(vars = rownames(M2_df), .before = names(M2_df)[1])
    M2_df <- M2_df %>% add_column("#" = seq(1, nrow(M2_df)), .before = names(M2_df)[1])
  }
  
} 

cor_table_02_sua <- M2_df %>% gvisTable(options=list(frozenColumns = 2,
                                                     height=300))




# stat_table_1st_ob$`protein_E%`
# stat_table_1st_ob %>% rename(delta_uric_acid = `∆uric_acid`) %>% rename(fat = `fat_E%`) %>% 
#   ggscatter(x = "delta_uric_acid", y = "fat",
#             color = "black",
#             fill = "red",
#             shape = 21,
#             size = 1,
#             add = "reg.line",  # Add regressin line
#             add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
#             conf.int = TRUE, # Add confidence interval
#             title = "Correlation(Weight x SUA):Diet",
#             xlab = "∆uric_acid(mg/dL)",
#             ylab = "Intake:Protein(E%)",
#             # xlim = c(0, 13),
#             # ylim = c(0, 180),
#   ) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
#     axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
#     axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
#   ) +
#   stat_cor(method = "pearson", size = 5, label.x = 0, label.y = 45) # Add correlation coefficient)



#
# stat_table_1st_ob$meat_bean
# 
# stat.test <- 
#   stat_table_1st_ob %>% 
#   filter(!is.na(sua_gp_baseline)) %>% 
#   select(delta_sua_gp, gender, `fat_E%`) %>% rename(value = "fat_E%") %>% 
#   mutate(value_adj = value %>% multiply_by(1)) %>% 
#   group_by(gender) %>% 
#   rstatix::t_test(value_adj ~ delta_sua_gp) %>%
#   rstatix::add_xy_position(x = "gender", fun = "mean_sd", dodge = 0.5)
# 
# stat_table_1st_ob %>% 
#   filter(!is.na(sua_gp_baseline)) %>% 
#   select(delta_sua_gp, gender, `fat_E%`) %>% rename(value = "fat_E%") %>% 
#   mutate(value_adj = value %>% multiply_by(1)) %>% 
#   ggbarplot(x = "gender", y = "value_adj", fill = "delta_sua_gp", alpha = 0.5, width = 0.5,
#             add = "mean_se", add.params = list(group = "delta_sua_gp"),
#             label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, 
#             position = position_dodge(0.5), 
#             xlab = "", ylab = "Intake: fat(E%)", title = paste0("Diet", " x 尿酸"),
#             legend = "right", legend.title = "SUA Group", ggtheme = theme_light() ) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
#     axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
#     axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
#   ) +
#   stat_pvalue_manual(
#     stat.test, label = "p.adj.signif", tip.length = 0.01,
#     bracket.nudge.y = 1, step.increase = 0.05, hide.ns = TRUE
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)))



# Whole vars plot ---------------------------------------------------------

stat_table_1st_ob$delta_sua_gp

datasets_target_issue <- stat_table_1st_ob %>% dplyr::rename(gp = delta_sua_gp)
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
             "upload_day_%","note_count","pic_counts","carb_E%","protein_E%","fat_E%","calorie_day","light_G_%","light_Y_%","light_R_%","fruits_day","vegetables_day","grains_day","meat_bean_day","milk_day","oil_day",
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


myplots_sua <- vector('list', length(var_vector))

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
  stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
  
  #for customed summary table - part 1/4 [p value]
  vector_pvalue <- append(vector_pvalue, 
                          stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
  )
  
  #plot
  plot <- 
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
  
  myplots_sua[[j]] <- plot
  
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}

rm(list = c("datasets_target_issue_a", "datasets_target_issue_b", "datasets_target_issue_c", "datasets_target_issue_d"))

#(2.)gender x Group table
table_01_sua <- 
  table(datasets_target_issue$gender, datasets_target_issue$gp) %>% addmargins() %>% 
  kable(format = "html", caption = "<b>Table: Stuty Group</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Cutoffs:"), general = c(rbind("\n", c("Male: 7.6 (mg/dL)", "Female: 6.6(mg/dL)"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)


#(3.)output statistics table
#for customed summary table [summary table]
summary_table_sua <- 
  datasets_target_issue %>% 
  group_by(gender, gp) %>% 
  summarize_at(vars_en[var_vector],
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
  )



#rbind: summary_table, p.adj.sign, dif, improvement
# summary_table_sua <- cbind(summary_table_sua %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t(), as.data.frame(vector_pvalue)) 
summary_table_sua <- summary_table_sua %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t()

# names(summary_table_sua) <- c(rep(levels((datasets_target_issue$gp)), 2), "顯著差異")
colnames(summary_table_sua) <- c(rep(levels((datasets_target_issue$gp)), 2))
rownames(summary_table_sua) <- myplot_table$vars_ch

table_02_sua <- 
  summary_table_sua %>% 
  kbl(format = "html", caption = "<b>Statistics:</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1, "Female" = length(levels(datasets_target_issue$gp)), "Male" = length(levels(datasets_target_issue$gp)))) %>% 
  footnote(general_title = c("Significance:"), general = "\n ",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%")


