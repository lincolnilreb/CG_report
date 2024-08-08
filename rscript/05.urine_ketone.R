
source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/lin_function.R")


# a <- stat_table_1st_ob %>% filter(gender == "female")
# cutoff = 9
# 
# a <- stat_table_1st_ob %>% filter(urine_ketones_endpoint != "缺")
# a$ins_gp <- a$insulin_endpoint %>% cut(c(-Inf, cutoff, Inf), paste0(c("Ins<=", "Ins>"), cutoff))
# a$urine_ketones_endpoint %>% unique()
# a[a[["urine_ketones_endpoint"]] != "- (<5 mg/dL)", "urine_ketones_endpoint"] <- "+ (>=5 mg/dL)"
# 
# lin_chisq.test(a, urine_ketones_endpoint, ins_gp)

# 01.  Endpoint pool ------------------------------------------------------

# urine_ketones_endpoint
# insulin_endpoint # cutoff < 6
a <- stat_table_1st_ob %>% filter(urine_ketones_endpoint != "缺")
a$ins_gp <- a$insulin_endpoint %>% cut(c(-Inf, 6, Inf), c("Ins<=6", "Ins>6"))
a$urine_ketones_endpoint %>% unique()
a[a[["urine_ketones_endpoint"]] != "- (<5 mg/dL)", "urine_ketones_endpoint"] <- "+ (>=5 mg/dL)"
a$urine_ketones_endpoint <- a$urine_ketones_endpoint %>% factor(levels = c("+ (>=5 mg/dL)", "- (<5 mg/dL)"))
table(a$urine_ketones_endpoint)

#chisq 
ketone_endpoint <- a
lin_chisq.test(ketone_endpoint, ins_gp, urine_ketones_endpoint)

#line 
a <- stat_table_1st_ob %>% filter(urine_ketones_endpoint != "缺")
a[a[["urine_ketones_endpoint"]] != "- (<5 mg/dL)", "urine_ketones_endpoint"] <- "+ (>=5 mg/dL)"

table_tmp <- data.frame(matrix(ncol = 3, dimnames = list(NULL, c("cutoff", "TPR", "TNR"))))
i=1
while (i < 30) {
  # gp
  a$ins_gp <- a$insulin_endpoint %>% cut(c(-Inf, i, Inf), c("Low", "High"))
  # Accuracy
  table_tmp <- table_tmp %>% rbind(c(i,
                                     (table(a$urine_ketones_endpoint, a$ins_gp)[2,1] / table(a$urine_ketones_endpoint, a$ins_gp)[2,] %>% sum()) %>% round(2),
                                     (table(a$urine_ketones_endpoint, a$ins_gp)[1,2] / table(a$urine_ketones_endpoint, a$ins_gp)[1,] %>% sum()) %>% round(2)
  ))
  i = i + 0.1
}
table_tmp <- table_tmp[complete.cases(table_tmp), ]
table_tmp <- table_tmp %>% mutate(score = sqrt(TPR * TNR))
table(a$urine_ketones_endpoint, a$ins_gp)


# #plot
# table_tmp %>% 
#   ggplot(aes(x = cutoff)) +
#   geom_line(aes(y = TPR, colour = "TPR"), linewidth = 0.3) +
#   geom_line(aes(y = TNR, colour = "TNR"), linewidth = 0.3) +
#   geom_line(aes(y = score, colour = "GM score"), linewidth = 0.5) +
#   scale_colour_manual("", 
#                       breaks = c("TPR", "TNR", "GM score"),
#                       values = c("red", "blue", "black")) +
#   labs(x = "Cutoff: Insulin", y = "Value", title = "Optimal Cutoff: Endpoint") +
#   theme_linedraw() +
#   theme(
#     plot.title = element_text(face = "bold",
#                               hjust = 0.5),
#     axis.title.y = element_text(face = "bold", size = 15)
#   ) 


row.names(table_tmp) <- seq(nrow(table_tmp))
table_tmp$cutoff <- table_tmp$cutoff %>% round(1)
table_tmp$score <- table_tmp$score %>% round(3)
library(DT)
plot_ketone_auc_table_endpoint <- 
  table_tmp %>% datatable(extensions = c('Buttons',"FixedColumns"),
                          options = list(
                            # fixedColumns = list(leftColumns = 1),
                            dom = 'Blfrtip',
                            # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                            initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'font-size': '12px'});","}"),
                            scrollX = TRUE, 
                            # autoWidth = TRUE,
                            columnDefs = list(list(width = '10px', targets = c(1))),
                            order = list(list(4, 'desc')),
                            lengthMenu = list(c(10,25,50,-1),
                                              c(10,25,50,"All"))))



library(cutpointr)
cp <- cutpointr(a, insulin_endpoint, urine_ketones_endpoint, gender,
                method = maximize_metric, metric = sum_sens_spec, 
                pos_class = "+ (>=5 mg/dL)", neg_class = "- (<5 mg/dL)", na.rm = TRUE
)

summary(cp)
# plot_roc(cp)
# plot_metric(cp) + xlim(0,15)
# plot_sensitivity_specificity(cp) + xlim(0,15)


plot_ketone_auc_endpoint <- 
  cowplot::plot_grid(ggplot() + 
                       labs(title = "Optimal Cutoff Analysis", subtitle = paste0("Urine Ketone x Fasting Insulin - Endpoint",
                                                                                 "  (Female: ",cp$optimal_cutpoint[1], "/ Male: ",cp$optimal_cutpoint[2],")")),
                     cowplot::plot_grid(cowplot::plot_grid(plot_sensitivity_specificity(cp) + xlim(0,15), plot_metric(cp) + xlim(0,15), nrow = 2),
                                        plot_roc(cp),
                                        ncol = 2, labels = "AUTO"),
                     nrow = 2, rel_heights = c(0.13, 1))






# 02. Baseline pool ------------------------------------------------------------


# urine_ketones_baseline
# insulin_baseline # cutoff < 6
a <- stat_table_1st_ob %>% filter(urine_ketones_baseline != "缺")
a$ins_gp <- a$insulin_baseline %>% cut(c(-Inf, 6, Inf), c("Ins<=6", "Ins>6"))
a$urine_ketones_baseline %>% unique()
a[a[["urine_ketones_baseline"]] != "- (<5 mg/dL)", "urine_ketones_baseline"] <- "+ (>=5 mg/dL)"

table(a$urine_ketones_baseline)

#chisq 
ketone_baseline <- a
lin_chisq.test(a, ins_gp, urine_ketones_baseline)

#line 
a <- stat_table_1st_ob %>% filter(urine_ketones_baseline != "缺")
a[a[["urine_ketones_baseline"]] != "- (<5 mg/dL)", "urine_ketones_baseline"] <- "+ (>=5 mg/dL)"

table_tmp <- data.frame(matrix(ncol = 3, dimnames = list(NULL, c("cutoff", "TPR", "TNR"))))
i=1
while (i < 30) {
  # gp
  a$ins_gp <- a$insulin_baseline %>% cut(c(-Inf, i, Inf), c("Low", "High"))
  # Accuracy
  table_tmp <- table_tmp %>% rbind(c(i,
                                     (table(a$urine_ketones_baseline, a$ins_gp)[2,1] / table(a$urine_ketones_baseline, a$ins_gp)[2,] %>% sum()) %>% round(2),
                                     (table(a$urine_ketones_baseline, a$ins_gp)[1,2] / table(a$urine_ketones_baseline, a$ins_gp)[1,] %>% sum()) %>% round(2)
  ))
  i = i + 0.1
}
table_tmp <- table_tmp[complete.cases(table_tmp), ]
table_tmp <- table_tmp %>% mutate(score = sqrt(TPR * TNR))
table(a$urine_ketones_baseline, a$ins_gp)

#plot
# table_tmp %>% 
#   ggplot(aes(x = cutoff)) +
#   geom_line(aes(y = TPR, colour = "TPR"), linewidth = 0.3) +
#   geom_line(aes(y = TNR, colour = "TNR"), linewidth = 0.3) +
#   geom_line(aes(y = score, colour = "GM score"), linewidth = 0.5) +
#   scale_colour_manual("", 
#                       breaks = c("TPR", "TNR", "GM score"),
#                       values = c("red", "blue", "black")) +
#   labs(x = "Cutoff: Insulin", y = "Value", title = "") +
#   theme_linedraw() +
#   theme(
#     plot.title = element_text(face = "bold",
#                               hjust = 0.5),
#     axis.title.y = element_text(face = "bold", size = 15)
#   ) 


row.names(table_tmp) <- seq(nrow(table_tmp))
table_tmp$cutoff <- table_tmp$cutoff %>% round(1)
table_tmp$score <- table_tmp$score %>% round(3)

library(DT)
plot_ketone_auc_table_baseline <- 
  table_tmp %>% datatable(extensions = c('Buttons',"FixedColumns"),
                          options = list(
                            # fixedColumns = list(leftColumns = 1),
                            dom = 'Blfrtip',
                            # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                            initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'font-size': '12px'});","}"),
                            scrollX = TRUE, 
                            # autoWidth = TRUE,
                            columnDefs = list(list(width = '10px', targets = c(1))),
                            order = list(list(4, 'desc')),
                            lengthMenu = list(c(10,25,50,-1),
                                              c(10,25,50,"All"))))





library(cutpointr)
cp <- cutpointr(a, insulin_baseline, urine_ketones_baseline, gender,
                method = maximize_metric, metric = sum_sens_spec, 
                pos_class = "+ (>=5 mg/dL)", neg_class = "- (<5 mg/dL)", na.rm = TRUE
)

summary(cp)
# plot(cp)
# plot_metric(cp) + xlim(0,15)
plot_ketone_auc_baseline <- 
  cowplot::plot_grid(ggplot() + 
                       labs(title = "Optimal Cutoff Analysis", subtitle = paste0("Urine Ketone x Fasting Insulin - Baseline",
                                                                                 "  (Female: ",cp$optimal_cutpoint[1], "/ Male: ",cp$optimal_cutpoint[2],")")),
                     cowplot::plot_grid(cowplot::plot_grid(plot_sensitivity_specificity(cp) + xlim(0,15), plot_metric(cp) + xlim(0,15), nrow = 2),
                                        plot_roc(cp),
                                        ncol = 2, labels = "AUTO"),
                     nrow = 2, rel_heights = c(0.13, 1))




# Pool --------------------------------------------------------------------

a <- stat_table_1st_ob %>% filter(urine_ketones_baseline != "缺")
a$ins_gp <- a$insulin_baseline %>% cut(c(-Inf, 6, Inf), c("Ins<=6", "Ins>6"))


a <- 
  data.frame(ketone = stat_table_1st_ob %>% select(urine_ketones_baseline) %>% pull(),
             insulin = stat_table_1st_ob %>% select(insulin_baseline) %>% pull(), 
             gender = stat_table_1st_ob %>% select(gender) %>% pull(), 
             stringsAsFactors = FALSE) %>% 
  rbind(data.frame(ketone = stat_table_1st_ob %>% select(urine_ketones_endpoint) %>% pull(),
                   insulin = stat_table_1st_ob %>% select(insulin_endpoint) %>% pull(), 
                   gender = stat_table_1st_ob %>% select(gender) %>% pull(), 
                   stringsAsFactors = FALSE))

a <- a %>% filter(ketone != "缺")
a[a[["ketone"]] != "- (<5 mg/dL)", "ketone"] <- "+ (>=5 mg/dL)"

a$ins_gp <- a$insulin %>% cut(c(-Inf, 6, Inf), c("Ins<=6", "Ins>6"))
ketone_all <- a
lin_chisq.test(a, ins_gp, ketone)


table_tmp <- data.frame(matrix(ncol = 3, dimnames = list(NULL, c("cutoff", "TPR", "TNR"))))
i=1
while (i < 30) {
  # gp
  a$ins_gp <- a$insulin %>% cut(c(-Inf, i, Inf), c("Low", "High"))
  # Accuracy
  table_tmp <- table_tmp %>% rbind(c(i,
                                     (table(a$ketone, a$ins_gp)[2,1] / table(a$ketone, a$ins_gp)[2,] %>% sum()) %>% round(2),
                                     (table(a$ketone, a$ins_gp)[1,2] / table(a$ketone, a$ins_gp)[1,] %>% sum()) %>% round(2)
  ))
  i = i + 0.1
}
table_tmp <- table_tmp[complete.cases(table_tmp), ]
table_tmp <- table_tmp %>% mutate(score = sqrt(TPR * TNR))
table_tmp <- table_tmp[with(table_tmp, order(score, decreasing = T)),]
rownames(table_tmp) <- as.character(seq(nrow(table_tmp)))


# table_tmp %>% 
#   ggplot(aes(x = cutoff)) +
#   geom_line(aes(y = TPR, colour = "TPR"), linewidth = 0.3) +
#   geom_line(aes(y = TNR, colour = "TNR"), linewidth = 0.3) +
#   geom_line(aes(y = score, colour = "GM score"), linewidth = 0.5) +
#   scale_colour_manual("", 
#                       breaks = c("TPR", "TNR", "GM score"),
#                       values = c("red", "blue", "black")) +
#   labs(x = "Cutoff: Insulin", y = "Value", title = "") +
#   theme_linedraw() +
#   theme(
#     plot.title = element_text(face = "bold",
#                               hjust = 0.5),
#     axis.title.y = element_text(face = "bold", size = 15)
#   ) 


table_tmp$cutoff <- table_tmp$cutoff %>% round(1)
table_tmp$score <- table_tmp$score %>% round(3)
library(DT)
plot_ketone_auc_table_all <-
  table_tmp %>% datatable(extensions = c('Buttons',"FixedColumns"),
                          options = list(
                            # fixedColumns = list(leftColumns = 1),
                            dom = 'Blfrtip',
                            # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                            initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'font-size': '12px'});","}"),
                            scrollX = TRUE, 
                            # autoWidth = TRUE,
                            columnDefs = list(list(width = '10px', targets = c(1))),
                            order = list(list(4, 'desc')),
                            lengthMenu = list(c(10,25,50,-1),
                                              c(10,25,50,"All"))))


library(cutpointr)
cp <- cutpointr(a, insulin, ketone, 
                method = maximize_metric, metric = sum_sens_spec, gender,
                pos_class = "+ (>=5 mg/dL)", neg_class = "- (<5 mg/dL)", na.rm = TRUE
)

summary(cp)
# plot(cp)
# plot_metric(cp) + xlim(0,15)
plot_ketone_auc_all <- 
  cowplot::plot_grid(ggplot() + 
                       labs(title = "Optimal Cutoff Analysis", subtitle = paste0("Urine Ketone x Fasting Insulin - Pool",
                                                                                 "  (Female: ",cp$optimal_cutpoint[1], "/ Male: ",cp$optimal_cutpoint[2],")")),
                     cowplot::plot_grid(cowplot::plot_grid(plot_sensitivity_specificity(cp) + xlim(0,15), plot_metric(cp) + xlim(0,15), nrow = 2),
                                        plot_roc(cp),
                                        ncol = 2, labels = "AUTO"),
                     nrow = 2, rel_heights = c(0.13, 1))






# plot --------------------------------------------------------------------

a <- stat_table_1st_ob %>% filter(urine_ketones_endpoint != "缺" & urine_ketones_baseline != "缺")
# a$ins_gp <- a$insulin_endpoint %>% cut(c(-Inf, 6, Inf), c("Ins<=6", "Ins>6"))
# a$urine_ketones_endpoint %>% unique()
a[a[["urine_ketones_endpoint"]] != "- (<5 mg/dL)", "urine_ketones_endpoint"] <- "+ (>=5 mg/dL)"
a[a[["urine_ketones_baseline"]] != "- (<5 mg/dL)", "urine_ketones_baseline"] <- "+ (>=5 mg/dL)"
a$urine_ketones_baseline <-  ifelse(a$urine_ketones_baseline == "- (<5 mg/dL)", "Neg.", "Pos.")
a$urine_ketones_endpoint <-  ifelse(a$urine_ketones_endpoint == "- (<5 mg/dL)", "Neg.", "Pos.")

a$gp_kt <- paste(a$urine_ketones_baseline, a$urine_ketones_endpoint, sep = ">")

datasets_target_issue <- a %>% dplyr::rename(gp = gp_kt)
datasets_target_issue$gp <- factor(datasets_target_issue$gp, levels = c("Neg.>Neg.","Neg.>Pos.","Pos.>Neg.","Pos.>Pos."))

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
                                                              grep(paste(c("bsmi", "bm$", "bm%", "homa_beta", "hdl", "homa_beta"), collapse = "|"), ., value = TRUE)) %>% multiply_by(1)

datasets_target_issue <- Reduce(cbind,list(datasets_target_issue_a, datasets_target_issue_b, datasets_target_issue_c, datasets_target_issue_d), accumulate =FALSE)

#order again!!
datasets_target_issue <- datasets_target_issue %>% select(vars_en)

#change colname to run plot
# #**[exlude gender gp = 1, which is male & pattern V]
# datasets_target_issue_for_plot <- datasets_target_issue %>% filter(!((gender == "male") & (gp == "Pattern V")))
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

#[customized part!!!]
myplots_qqqq <- vector('list', length(var_vector))

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
    rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ "))) %>% rstatix::add_significance()
  stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
  
  #for customed summary table - part 1/4 [p value]
  vector_pvalue <- append(vector_pvalue,
                          stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
  )
  
  #plot
  plot <-
    datasets_target_issue_for_plot %>%
    ggbarplot(x = "gender", y = a, fill = "gp",  alpha = .5, 
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
  myplots_qqqq[[j]] <- plot
  
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}

rm(list = c("datasets_target_issue_a", "datasets_target_issue_b", "datasets_target_issue_c", "datasets_target_issue_d"))

#(2.)gender x Group table
#[customized part!!!]
table_01_qqqq <-
  table(datasets_target_issue$gender, datasets_target_issue$gp) %>% addmargins() %>%
  kable(format = "html", caption = "<b>Table: Population(OB.)</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>%
  footnote(general_title = c("Note:"), general = c(rbind("", c(" "))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>%
  gsub("font-size: initial !important;",
       "font-size: 15pt !important;",
       .)


#(3.)output statistics table
#for customed summary table [summary table]
#[customized part!!!]
summary_table_qqqq <-
  datasets_target_issue %>%
  group_by(gender, gp) %>%
  summarize_at(vars_en[var_vector],
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
  )



#rbind: summary_table, p.adj.sign, dif, improvement
# summary_table_sua <- cbind(summary_table_sua %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t(), as.data.frame(vector_pvalue))
# summary_table_qqqq <- summary_table_qqqq %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t() %>% cbind(as.data.frame(vector_pvalue))
summary_table_qqqq <- summary_table_qqqq %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t()

# names(summary_table_sua) <- c(rep(levels((datasets_target_issue$gp)), 2), "顯著差異")
colnames(summary_table_qqqq) <- c(rep(levels((datasets_target_issue$gp)), 2))
# colnames(summary_table_qqqq) <- c(rep(levels((datasets_target_issue$gp)), 2), "顯著差異")
rownames(summary_table_qqqq) <- myplot_table$vars_ch

#[customized part!!!]
table_02_qqqq <-
  summary_table_qqqq %>%
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
