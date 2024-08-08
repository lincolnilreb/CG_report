
# Effi.  stratification ---------------------------------------------------

QQ1_stat_table_1st <- stat_table_1st_ob %>% filter(MHO_baseline != "Others")

#MHO_baseline
QQ1_stat_table_1st$gp <- QQ1_stat_table_1st$MHO_baseline

QQ1_stat_table_1st$gp <- QQ1_stat_table_1st$gp %>% factor(levels = c("MHO", "MUHO", "MH", "MUH"))


#import "vars_en"
source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/00_vars_vector.R")

QQ1_stat_table_1st <- QQ1_stat_table_1st %>% select(all_of(vars_en))

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
    myplot_table_mho <- data.frame(num = seq(1, length(vars_en_adj)),
                               vars_ch = lin_ch_en_format(x = vars_en_adj, format = "ch", origin = "en"))
    myplot_table_mho <- lin_mapping(myplot_table_mho, vars_en_adj, vars_ch, vars_table, en, ch)
    myplot_table_mho <- lin_mapping(myplot_table_mho, field, vars_ch, vars_table, field, ch)

    myplot_table_mho <- myplot_table_mho[var_vector,]
    myplot_table_mho$num <- seq(1, length(myplot_table_mho$num))

    myplot_table_mho$block <- NA
    x0 <- c("t0","t1","delta","delta_p")
    x1 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("^[∆]?weight",.))
    x2 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("wepa50",.))
    x3 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("ecw_ratio",.))
    x4 <- which(myplot_table_mho[["vars_en_adj"]] %in% (myplot_table_mho[["vars_en_adj"]] %>% grep("left_arm_fat",., value = TRUE) %>% grep("percent",., invert = TRUE, value = TRUE)))
    x5 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("water_weight_left_arm",.))
    x6 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("hba1c",.))
    x7 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("tg",.))
    x8 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("egfr",.))
    x9 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("tsh",.))
    x10 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("wbc",.))
    x11 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("lipase",.))
    x12 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("^age",.))
    x13 <- (myplot_table_mho[["vars_en_adj"]] %>% grep("oil",.))
  }
  for (j in c(1:10)) {
    if (j != 10) {
      myplot_table_mho$block[(eval(parse(text = paste0("x", j))))[i]:((eval(parse(text = paste0("x", j+1))))[i]-1)] <- paste(x0[i], j, sep = "_")
      }else{
        myplot_table_mho$block[x10[i]:x11[i]] <- paste(x0[i], j, sep = "_")
      }
  }
  
  if ((i == 4) & (j == 10)) {
    myplot_table_mho$block[x12:x13] <- "diet"
    rm(list = paste0("x", seq(0,13)))
  }
}


#
myplots_MHO <- vector('list', length(var_vector))

for (i in c(var_vector)) {
  j <- match(i, var_vector)
  if (j == 1) {
    vector_pvalue <- c()
    start_time <- Sys.time()
  }
  
  #error skip
  skip_condition <- rownames(myplot_table_mho)[stringr::str_which(myplot_table_mho$vars_en_adj, "psa_")] %>% as.numeric()
  if (i == skip_condition) {
    vector_pvalue <- append(vector_pvalue, "ns"
    )  
    next  
  }
  
  a <- QQ1_stat_table_1st_for_plot %>% colnames() %>% head(i) %>% tail(1)
  a_title <- myplot_table_mho[myplot_table_mho$num == j, "vars_ch"]
  
  
  #ALL: female >= 3
  a1 <- (table(Count = is.na(QQ1_stat_table_1st_for_plot[[a]]), QQ1_stat_table_1st_for_plot[["gender"]], QQ1_stat_table_1st_for_plot[["gp"]]) %>% ftable(row.vars = 2))[1,  #female, !is.na()
                                                                                                                                                                  1:(QQ1_stat_table_1st_for_plot[["gp"]] %>% unique() %>% length()) #gp
                                                                                                                                                                  ]
  a1 <- a1 >= 3
  a1 <- all(combn(a1, 2, function(x) all(x)))
  #ALL: male >= 3
  a2 <- (table(Count = is.na(QQ1_stat_table_1st_for_plot[[a]]), QQ1_stat_table_1st_for_plot[["gender"]], QQ1_stat_table_1st_for_plot[["gp"]]) %>% ftable(row.vars = 2))[2,  #female, !is.na()
                                                                                                                                                                  1:(QQ1_stat_table_1st_for_plot[["gp"]] %>% unique() %>% length()) #gp
                                                                                                                                                                  ]
  a2 <- a2 >= 3
  a2 <- all(combn(a2, 2, function(x) all(x)))

  if_run <- (a1 & a2) 
  if (if_run) {
    #p.sign?
    stat.test <- 
      QQ1_stat_table_1st_for_plot %>%
      group_by(gender) %>%
      rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ "))) 
    stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
    
  }else{
    #p.sign?
    stat.test <- 
      QQ1_stat_table_1st_for_plot %>%
      rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ "))) 
    stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
    
  }
  #for customed summary table - part 1/4 [p value]
  vector_pvalue <- append(vector_pvalue, 
                          stat.test %>% select(p.adj.signif) %>% pull() %>% head(1) %>% tail(1)
  )  

  #plot
  plot <- 
    QQ1_stat_table_1st_for_plot %>% 
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
  
  myplots_MHO[[j]] <- plot
    
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}



#(2.)gender x Group table
table_MHO_01 <- 
  table(QQ1_stat_table_1st$gender, QQ1_stat_table_1st$gp, exclude = "Others") %>% addmargins() %>% 
  kable(format = "html", caption = "<b>Table: Study Group</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c(""), general = c(rbind("\n", c(""))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)


#(3.)output statistics table
#for customed summary table [summary table]
summary_table_MHO <- 
  QQ1_stat_table_1st %>% 
  group_by(gender, gp) %>% 
  summarize_at(vars_en_adj[var_vector],
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
  )



#rbind: summary_table, p.adj.sign, dif, improvement
summary_table_MHO <- cbind(summary_table_MHO %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t(), as.data.frame(vector_pvalue)) 

names(summary_table_MHO) <- c(rep(c("MHO", "MUHO", "MH", "MUH"), 2), "顯著差異")
rownames(summary_table_MHO) <- myplot_table_mho$vars_ch

table_MHO_02 <- 
  summary_table_MHO %>% 
  kbl(format = "html", caption = "<b>Statistics:</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1, "Female" = 4, "Male" = 4, " " = 1)) %>% 
  footnote(general_title = c(""), general = "\n ",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%")

rm(list = c("vars_en_adj", "a1","a2","a3"))
