
# Publication Genesis: 01.  -----------------------------------------------

pub1_stat_table <- rbind(stat_table_1st_ob %>% select(intersect(names(stat_table_1st_ob), names(stat_table_1st_dm))),
                         stat_table_1st_dm %>% select(intersect(names(stat_table_1st_ob), names(stat_table_1st_dm))))

Pub1_stat_table_1st <- pub1_stat_table %>% filter(!is.na(insulin_baseline) & !is.na(insulin_pc_1hr_baseline) & !is.na(insulin_pc_2hr_baseline)) 

Pub1_stat_table_1st <- Pub1_stat_table_1st %>% filter(!is.na(Pattern_major_baseline))


# Figure 2. ---------------------------------------------------------------
pub1_plot_M <- lin_insulin_rsp_pattern(Pub1_stat_table_1st, c("insulin_baseline", "insulin_pc_1hr_baseline", "insulin_pc_2hr_baseline"), plot = percentage, pattern = 2, layout = TRUE)

# Table 1 -----------------------------------------------------------------

datasets_target_issue <- pub1_stat_table %>% dplyr::rename(gp = Pattern_major_baseline)
datasets_target_issue <- datasets_target_issue %>% filter(gp %in% levels(datasets_target_issue$gp))

var_pub1 <-c("age", "bmi_baseline", "pbf_baseline", "vfa_baseline",
             "hba1c_baseline", "glucose_ac_baseline", "insulin_baseline", "homa_ir_baseline", 
             "tg_baseline","tc_baseline", "ldl_baseline", "hdl_baseline")
Pub1_stat_table_1st_tbl1 <- 
  Pub1_stat_table_1st %>%
  mutate(gp = Pattern_major_baseline) %>% 
  group_by(gender, gp) %>%
  summarize_at(var_pub1,
  # summarize_at(vars_en[var_vector],
            #SEM
               # function(x) paste(mean(x, na.rm = TRUE) %>% round(1), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(1), sep = " ± ")
            #SD
               function(x) paste(mean(x, na.rm = TRUE) %>% round(1), (sd(x, na.rm = TRUE)) %>% round(1), sep = " ± ")
  ) 


Pub1_stat_table_1st_tbl1 <- Pub1_stat_table_1st_tbl1 %>% as.data.frame() %>% select(-c("gp","gender")) %>% t() 
colnames(Pub1_stat_table_1st_tbl1) <-
  # paste0("\n(n=",table(Pub1_stat_table_1st$Pattern_major_baseline, Pub1_stat_table_1st$gender, exclude = "Unclassified") %>% as.numeric(),")")
  paste0((levels(Pub1_stat_table_1st$Pattern_major_baseline) %>% as.character())[-6],
         paste0("\n(n=",table(Pub1_stat_table_1st$Pattern_major_baseline, Pub1_stat_table_1st$gender, exclude = "Unclassified") %>% as.numeric(),")"))
rownames(Pub1_stat_table_1st_tbl1) <- c("Age", "BMI", "PBF", "Viceral Fat", "HbA1C", "Fasting Glucose", "Fasting Insulin", "HOMA-IR", "TG", "TC", "LDL", "HDL")
  
Pub1_stat_table_1st_tbl1 <- 
  Pub1_stat_table_1st_tbl1 %>% 
  kbl(format = "html", caption = "<b>Table 1.</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1, "Female" = length(levels(datasets_target_issue$gp)[1:5]), "Male" = length(levels(datasets_target_issue$gp)[1:5]))) %>% 
  footnote(general_title = c(""), general = "\n ",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%")


# Figure 3. ------------------------------------------------------------------


datasets_target_issue <- pub1_stat_table %>% dplyr::rename(gp = Pattern_major_baseline)
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
#**[exlude gender gp = 1, which is male & pattern V]
# datasets_target_issue_for_plot <- datasets_target_issue %>% filter(!((gender == "male") & (gp == "Pattern V")))
datasets_target_issue_for_plot <- datasets_target_issue 

names(datasets_target_issue_for_plot) <- gsub("∆", "delta_", names(datasets_target_issue_for_plot))
names(datasets_target_issue_for_plot) <- gsub("%", "_percent", names(datasets_target_issue_for_plot))
#adjust NA w/ mean
datasets_target_issue_for_plot <- datasets_target_issue_for_plot %>% mutate(across(var_pub1, ~if_else(is.na(.), mean(., na.rm = TRUE), .)))


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
myplots_pub1 <- vector('list', length(var_pub1))

var_vector_pub1 <- rownames(myplot_table[myplot_table$vars_en %in% var_pub1,]) %>% as.numeric()

k = 1
for (i in c(var_vector_pub1)) {
  j <- match(i, var_vector)
  if (k == 1) {
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
    ggbarplot(x = "gender", y = a, fill = "gp", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0,
              add = "mean_sd", add.params = list(group = "gp"),
              position = position_dodge(0.8), legend = "right", legend.title = "") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "", y = "Mean±SD", title = a_title) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15)
    ) +
    stat_pvalue_manual(
      stat.test, label = "p.adj.signif", tip.length = 0.0,
      bracket.nudge.y = 1, step.increase = 0.01, hide.ns = TRUE 
    )
  
  #[customized part!!!]
  myplots_pub1[[k]] <- plot
  
  progress(k, max = length(var_pub1))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
  k = k + 1
}

rm(list = c("datasets_target_issue_a", "datasets_target_issue_b", "datasets_target_issue_c", "datasets_target_issue_d"))




# Figure 4,5 --------------------------------------------------------------















