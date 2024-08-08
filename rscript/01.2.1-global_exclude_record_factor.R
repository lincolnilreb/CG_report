
# 01.飲食紀律不佳者，再做一次分析看看，能否看出其他影響因子 -----------------------------------------------------------------------


table(stat_table_1st_ob$gender, "upload_day(%) ≥80" = stat_table_1st_ob$`upload_day_%` >= 80) %>% addmargins()
table(stat_table_1st_ob$gender, "Light G(%) ≥80" = stat_table_1st_ob$`light_G_%` >= 80) %>% addmargins()

stat_table_1st_ob_filtered <- stat_table_1st_ob %>% filter(`upload_day_%` >= 80)
stat_table_1st_ob_filtered <- stat_table_1st_ob_filtered %>% filter(`light_G_%` >= 80)



# 01.1 strat -------------------------------------------------------------------


#Divide into 3 group based on ∆weight
QQ1_stat_table_1st_bad <- stat_table_1st_ob_filtered %>% filter(`∆weight%` > -3)
QQ1_stat_table_1st_bad$`∆weight%` %>%summary()
QQ1_stat_table_1st_bad$id %>% unique() %>% length()
QQ1_stat_table_1st_bad %>% summary()
QQ1_stat_table_1st_bad$gp <- "Poor"


QQ1_stat_table_1st_medium <- stat_table_1st_ob_filtered %>% filter((`∆weight%` > -10) & (`∆weight%` < -5) )
QQ1_stat_table_1st_medium$`∆weight%` %>%summary()
QQ1_stat_table_1st_medium$id %>% unique() %>% length()
QQ1_stat_table_1st_medium %>% summary()
QQ1_stat_table_1st_medium$gp <- "Medium"

QQ1_stat_table_1st_good <- stat_table_1st_ob_filtered %>% filter(`∆weight%` < -10)
QQ1_stat_table_1st_good$`∆weight%` %>%summary()
QQ1_stat_table_1st_good$id %>% unique() %>% length()
QQ1_stat_table_1st_good %>% summary()
QQ1_stat_table_1st_good$gp <- "Good"

QQ1_stat_table_1st <- rbind(QQ1_stat_table_1st_bad, QQ1_stat_table_1st_good)
QQ1_stat_table_1st <- rbind(QQ1_stat_table_1st, QQ1_stat_table_1st_medium)
QQ1_stat_table_1st$gp %<>% factor(levels = c("Poor", "Medium", "Good"))

rm(list = c("QQ1_stat_table_1st_bad","QQ1_stat_table_1st_good","QQ1_stat_table_1st_medium"))

#import "vars_en"
source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/00_vars_vector.R")

QQ1_stat_table_1st <- QQ1_stat_table_1st %>% select(vars_en)

vars_en <- lin_ch_en_format(x = vars_en, format = "en", origin = "raw_en")
names(QQ1_stat_table_1st) <- lin_ch_en_format(x = names(QQ1_stat_table_1st), format = "en", origin = "raw_en")


#Setting improvement direction
QQ1_stat_table_1st_a <- QQ1_stat_table_1st %>% select(-grep("∆", names(QQ1_stat_table_1st)))
# QQ1_stat_table_1st_a$calorie_deficit_day <- QQ1_stat_table_1st_a$calorie_deficit_day %>% multiply_by(-1)
# QQ1_stat_table_1st_a$calorie_deficit_sum <- QQ1_stat_table_1st_a$calorie_deficit_sum %>% multiply_by(-1)

##Improvement: negative (減少越多，越往上長)
QQ1_stat_table_1st_b <- QQ1_stat_table_1st %>% select(grep("∆", names(QQ1_stat_table_1st), value = TRUE) %>% 
                                                        grep(paste(c("weight", "bmi", "bf", "pbf", "vfa_level", "wc", "ffm", "hba1c", "GA", "eAG", "glucose_ac", "insulin", "homa_ir", "tg", "tc", "ldl"), collapse = "|"), ., value = TRUE)) %>% multiply_by(-1)
##Improvement: default 
QQ1_stat_table_1st_c <- QQ1_stat_table_1st %>% select(grep("∆", names(QQ1_stat_table_1st), value = TRUE) %>% 
                                                        grep(paste(c("weight", "bmi", "bf", "pbf", "vfa_level", "wc", "ffm", "hba1c", "GA", "eAG", "glucose_ac", "insulin", "homa_ir", "tg", "tc", "ldl"), collapse = "|"), ., invert = TRUE, value = TRUE)) %>% multiply_by(1)

QQ1_stat_table_1st <- Reduce(cbind,list(QQ1_stat_table_1st_a, QQ1_stat_table_1st_b,QQ1_stat_table_1st_c), accumulate =FALSE) 

rm(list = c("QQ1_stat_table_1st_a","QQ1_stat_table_1st_b","QQ1_stat_table_1st_c"))

#order again!!
QQ1_stat_table_1st <- QQ1_stat_table_1st %>% select(vars_en)

#select observation > 50!!
QQ1_stat_table_1st <- QQ1_stat_table_1st %>% select_if(~ sum(!is.na(.)) >= 50)

#change colname to run plot
QQ1_stat_table_1st_for_plot <- QQ1_stat_table_1st


names(QQ1_stat_table_1st_for_plot) <- gsub("∆", "delta_", names(QQ1_stat_table_1st_for_plot))
names(QQ1_stat_table_1st_for_plot) <- gsub("%", "_p", names(QQ1_stat_table_1st_for_plot))

vars_en_adj <- QQ1_stat_table_1st %>% names()

#set output plot order
var_vector <- c(vars_en_adj %>% grep("baseline$", .),
                vars_en_adj %>% grep("endpoint$", .),
                vars_en_adj %>% grep("baseline$|endpoint$|[∆]|id|client|gender|gp|date", ., invert = TRUE),
                setdiff(vars_en_adj %>% grep("[∆]", .), vars_en_adj %>% grep("[%]", .)),
                intersect(vars_en_adj %>% grep("[∆]", .), vars_en_adj %>% grep("[%]", .))
)


for (i in c(1:4)) {
  if (i == 1) {
    myplot_table_global_filter <- data.frame(num = seq(1, length(vars_en_adj)),
                                      vars_ch = lin_ch_en_format(x = vars_en_adj, format = "ch", origin = "en"))
    myplot_table_global_filter <- lin_mapping(myplot_table_global_filter, vars_en_adj, vars_ch, vars_table, en, ch)
    myplot_table_global_filter <- lin_mapping(myplot_table_global_filter, field, vars_ch, vars_table, field, ch)
    
    myplot_table_global_filter <- myplot_table_global_filter[var_vector,]
    myplot_table_global_filter$num <- seq(1, length(myplot_table_global_filter$num))
    
    myplot_table_global_filter$block <- NA
    x0 <- c("t0","t1","delta","delta_p")
    x1 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("^[∆]?weight",.))
    x2 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("wepa50",.))
    x3 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("ecw_ratio",.))
    x4 <- which(myplot_table_global_filter[["vars_en_adj"]] %in% (myplot_table_global_filter[["vars_en_adj"]] %>% grep("left_arm_fat",., value = TRUE) %>% grep("percent",., invert = TRUE, value = TRUE)))
    x5 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("water_weight_left_arm",.))
    x6 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("hba1c",.))
    x7 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("tg",.))
    x8 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("egfr",.))
    x9 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("tsh",.))
    x10 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("wbc",.))
    x11 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("lipase",.))
    x12 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("^age",.))
    x13 <- (myplot_table_global_filter[["vars_en_adj"]] %>% grep("oil",.))
  }
  for (j in c(1:10)) {
    if (j != 10) {
      myplot_table_global_filter$block[(eval(parse(text = paste0("x", j))))[i]:((eval(parse(text = paste0("x", j+1))))[i]-1)] <- paste(x0[i], j, sep = "_")
    }else{
      myplot_table_global_filter$block[x10[i]:x11[i]] <- paste(x0[i], j, sep = "_")
    }
  }
  
  if ((i == 4) & (j == 10)) {
    myplot_table_global_filter$block[x12:x13] <- "diet"
    rm(list = paste0("x", seq(0,13)))
  }
}



myplots_exclude_record_factor <- vector('list', length(var_vector))

for (i in c(var_vector)) {
  j <- match(i, var_vector)
  if (j == 1) {
    vector_pvalue <- c()
    start_time <- Sys.time()
    
  }
  
  
  a <- QQ1_stat_table_1st_for_plot %>% colnames() %>% head(i) %>% tail(1)
  a_title <- myplot_table_global_filter[myplot_table_global_filter$num == j, "vars_ch"]
  
  # [240409_debug]
    #Exclude gp:Poor in male, causing error in t_test Line:160 -  
  if ((QQ1_stat_table_1st_for_plot %>% filter(gender == "male" & gp == "Poor" & !is.na(eval(parse(text = a))))) %>% nrow() < 3) {
    QQ1_stat_table_1st_for_plot_run <-  QQ1_stat_table_1st_for_plot %>% filter(!(gender == "male" & gp == "Poor"))
  }else{
    QQ1_stat_table_1st_for_plot_run <- QQ1_stat_table_1st_for_plot
  }
  
  
  
  
  #observation not less than 3: count >= 3
  
  # any(combn(vec, 2, function(x) all(x))
  #any 2: male >= 3
  a1 <- ((table(Count = is.na(QQ1_stat_table_1st_for_plot_run[[a]]), QQ1_stat_table_1st_for_plot_run[["gender"]], QQ1_stat_table_1st_for_plot_run[["gp"]]) %>% ftable())[2,] >= 3)
  a1 <- any(combn(a1, 2, function(x) all(x)))
  #ALL: female >= 3
  a2 <- ((table(Count = is.na(QQ1_stat_table_1st_for_plot_run[[a]]), QQ1_stat_table_1st_for_plot_run[["gender"]], QQ1_stat_table_1st_for_plot_run[["gp"]]) %>% ftable())[1,] >= 3)
  a2 <- any(combn(a2, 2, function(x) all(x)))
  if_run <- (a1 & a2) 
  
  if (if_run) {
    #p.sign?
    stat.test <- 
      QQ1_stat_table_1st_for_plot_run %>%
      group_by(gender) %>%
      rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ "))) 
    stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
    
    #for customed summary table - part 1/4 [p value]
    vector_pvalue <- append(vector_pvalue, 
                            stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
    )
  }else{
    #p.sign?
    stat.test <- 
      QQ1_stat_table_1st_for_plot_run %>%
      rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ "))) 
    stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
    
    #for customed summary table - part 1/4 [p value]
    vector_pvalue <- append(vector_pvalue, 
                            stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
    )
  }
  
  #plot
  plot <- 
    QQ1_stat_table_1st_for_plot_run %>% 
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
      bracket.nudge.y = 1, step.increase = 0.01, hide.ns = FALSE 
    )
  
  myplots_exclude_record_factor[[j]] <- plot
  
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}



#(2.)gender x Group table
table_01_exclude_record_factor <- 
  table(QQ1_stat_table_1st$gender, QQ1_stat_table_1st$gp) %>% addmargins() %>% 
  kable(format = "html", caption = "<b>Table: Study Group</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Categorization:  ∆weight(%)"), general = c(rbind("\n", c("- Poor: Less than 3%", "- Medium: Between 5~10%", "- Good: More than 10%"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)


#(3.)output statistics table
#for customed summary table [summary table]
summary_table <- 
  QQ1_stat_table_1st %>% 
  group_by(gender, gp) %>% 
  summarize_at(vars_en_adj[var_vector],
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
  )



#rbind: summary_table, p.adj.sign, dif, improvement
summary_table <- cbind(summary_table %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t(), as.data.frame(vector_pvalue)) 

# names(summary_table) <- c(rep(c("Poor", "Medium", "Good"), 2), "顯著差異")
names(summary_table) <- c(rep(c("Poor", "Medium", "Good"), ), c("Medium", "Good"), "顯著差異")
rownames(summary_table) <- myplot_table_global_filter$vars_ch

table_02_exclude_record_factor <- 
  summary_table %>% 
  kbl(format = "html", caption = "<b>Statistics:</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1, "Female" = 3, "Male" = 2, " " = 2)) %>% 
  footnote(general_title = c("Significance:"), general = "\n Comparison: Good vs. Poor in female population.",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%")

rm(list = c("vars_en_adj", "a1","a2"))




# 01.2.ROC ------------------------------------------------------------------

library(cutpointr)

# Def: weight loss fail: > -8
cutoff <- -8
stat_table_1st_ob_filtered$gp_weight_loss <- stat_table_1st_ob_filtered$`∆weight%` %>% cut(c(-Inf, cutoff, Inf), c("success", "fail"))

# Select numeric vars
vars <- stat_table_1st_ob_filtered %>% 
  select_if(is.numeric) %>% 
  select(-c(id, class_freq, class_order, height_baseline, height_endpoint)) %>% 
  select_if(~ sum(!is.na(.)) >= 50) %>% 
  names()

# Store start time
start_time <- Sys.time()

# Apply cutpointr to each variable
table_var_cutoff <- lapply(vars, function(var) {
  a <- stat_table_1st_ob_filtered %>% select(c(var, gp_weight_loss))
  cp <- cutpointr(a, !!as.name(var), gp_weight_loss, 
                  method = maximize_metric, metric = sum_sens_spec, 
                  pos_class = "success", neg_class = "fail", na.rm = TRUE
  ) 
  tibble(var = var, cp[1:8])
})

# Combine the results into a single data frame
table_var_cutoff <- bind_rows(table_var_cutoff)

# Remove unnecessary columns
table_var_cutoff <- table_var_cutoff[-4]

# Print elapsed time
print(Sys.time() - start_time)

#data clean
table_var_cutoff[["var"]] <- table_var_cutoff[["var"]] %>% lin_ch_en_format(format = "ch", origin = "en")
table_var_cutoff$optimal_cutpoint <- table_var_cutoff$optimal_cutpoint %>% round(2)
table_var_cutoff[c("sum_sens_spec","acc","sensitivity","specificity","AUC")] <- table_var_cutoff[c("sum_sens_spec","acc","sensitivity","specificity","AUC")] %>% lapply(round, digit = 4)

#ouptut
library(DT)
table_var_cutoff <- table_var_cutoff[with(table_var_cutoff, order(AUC, decreasing = T)),]
table_global_cutoff_exclude_record_factor <-
  table_var_cutoff %>% datatable(extensions = c('Buttons',"FixedColumns"),
                                 width = '80%',
                                 options = list(
                                   # fixedColumns = list(leftColumns = 1),
                                   fixedHeader = TRUE,
                                   dom = 'Blfrtip',
                                   # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'font-size': '12px'});","}"),
                                   scrollX = TRUE, 
                                   # autoWidth = TRUE,
                                   columnDefs = list(list(width = '10px', targets = c(seq(2,8)))),
                                   columnDefs = list(list(width = '200px', targets = c(1))),
                                   order = list(list(4, 'desc')),
                                   lengthMenu = list(c(10,25,50,-1),
                                                     c(10,25,50,"All"))))

