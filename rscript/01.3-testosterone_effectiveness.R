# 20230214 Dr. Lee analysis request:  Testosterone x Effectiveness

# 主要想看 cutoff< 350 ng/dl 這幾個人(Male) 肥胖 胰島素阻抗 腰圍 體脂肪
# group by cutoff, gender: N, pie_chart, univariate correlation, baseline, effectiveness w/ sign.symbol

# #pie
#   googleVis::gvisMerge(pie_testosterone_01, pie_testosterone_02) %>% plot()
# #[Correlation r] Testosterone x Baseline
#   corrplot(M_testosterone,
#            p.mat = M_testosterone_test$p,
#            type = "lower",
#            insig = "label_sig",
#            sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
#            tl.col = "black", tl.srt = 35, tl.cex = 1.0,
#            cl.ratio = 0.1,
#            col = M_col(200),
#            title = "[Correlation] Efficacy x Baseline",
#            #c(bottom, left, top, right)
#            mar = c(0,0,1,0))
# #[Correlation r] table
#   cor_table_testosterone
# #>>>> Low T, High Weigt/Fat/Blood/Insulin/IR, IR improvement越差
# #[boxplot]
#   plot_grid(plotlist = myplots_plot_testosterone[84:87], ncol = 4, labels = paste0(LETTERS[6], seq(1,10)))
#   table_02_testosterone


# 0. Global setting / exploration -------------------------------------------------


cutoff_testosterone = 3.5 #ng/ml

table((stat_table_1st_ob$testosterone_baseline > cutoff_testosterone), stat_table_1st_ob$gender) %>% addmargins()
table((stat_table_1st_ob$testosterone_baseline > cutoff_testosterone), stat_table_1st_ob$gender) %>% prop.table() %>% addmargins() %>% round(2)





# 1. pie ------------------------------------------------------------------
a <- stat_table_1st_ob %>% filter(gender == "male")

a$age_gp <- cut(a$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
a$gp_testosterone <- a$testosterone_baseline %>% cut(c(-Inf, cutoff_testosterone, Inf), c("Low_testosterone", "Normal"))

a <- a[complete.cases(a$testosterone_baseline),]


pie_testosterone_01 <- 
  a %>% group_by(gp_testosterone, age_gp) %>% summarise(n = n()) %>% as_tibble() %>% filter(gp_testosterone != "Normal") %>% select(-gp_testosterone) %>% gvisPieChart(options = list(title = 'Low Testosterone',
                                                                                                                                                                                      legend = "{position:'right'}",
                                                                                                                                                                                      pieHole = 0.5,
                                                                                                                                                                                      #slices = "{0:{offset:0.1}}",
                                                                                                                                                                                      backgroundColor = "#f9fffb",
                                                                                                                                                                                      #colors = "['#DC3912', '#3366CC']",
                                                                                                                                                                                      width = "600",
                                                                                                                                                                                      height = "400"))
pie_testosterone_02 <- 
  a %>% group_by(gp_testosterone, age_gp) %>% summarise(n = n()) %>% as_tibble() %>% filter(gp_testosterone == "Normal") %>% select(-gp_testosterone) %>% gvisPieChart(options = list(title = 'Normal',
                                                                                                                                                                                      legend = "{position:'right'}",
                                                                                                                                                                                      pieHole = 0.5,
                                                                                                                                                                                      #slices = "{0:{offset:0.1}}",
                                                                                                                                                                                      backgroundColor = "#f9fffb",
                                                                                                                                                                                      #colors = "['#DC3912', '#3366CC']",
                                                                                                                                                                                      width = "600",
                                                                                                                                                                                      height = "400"))
#googleVis::gvisMerge(pie_testosterone_01, pie_testosterone_02) %>% plot()




# 2. Correlation ----------------------------------------------------------
#male


#[Create profile]  Efficacy, Baseline, Diet table
profile_efficacy <- stat_table_1st_ob %>% filter(gender == "male") %>% 
  select(c("∆weight%","∆bf%","∆bm%","∆vfa","∆wc","∆bmr","∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆homa_beta","∆tg","∆tc","∆hdl","∆ldl","∆lipase"))
names(profile_efficacy) <- names(profile_efficacy) %>% lin_ch_en_format(format = "ch", origin = "en")

##add testosterone_baseline
profile_baseline <- stat_table_1st_ob %>% filter(gender == "male") %>%  
  select(c("age", "bmi_baseline","pbf_baseline","vfa_baseline","bsmi_baseline","pbm_baseline","wc_baseline","bmr_baseline",
           "hba1c_baseline","glucose_ac_baseline","insulin_baseline","homa_ir_baseline","homa_beta_baseline", "tAUCg_baseline", "tAUCi_baseline", "OGIRIndex_baseline",
           "tg_baseline","tc_baseline","hdl_baseline","ldl_baseline","lipase_baseline",
           "testosterone_baseline"))
names(profile_baseline) <- names(profile_baseline) %>% lin_ch_en_format(format = "ch", origin = "en")



profile_diet <- stat_table_1st_ob %>% filter(gender == "male") %>%  
  select(c("upload_day_%", "pic_counts","calorie_day","carb_E%","protein_E%","fat_E%","fruits_day","vegetables_day","grains_day","meat_bean_day","milk_day", "oil_day","light_G_%","light_Y_%","light_R_%"))
names(profile_diet) <- names(profile_diet) %>% lin_ch_en_format(format = "ch", origin = "en")


a <-  cbind(-profile_efficacy, testosterone_baseline = stat_table_1st_ob[stat_table_1st_ob$gender == "male", "testosterone_baseline"] %>% as.vector())

#rm NA obs.
a <- a[complete.cases(a$testosterone_baseline),]


aa1 <- 
  a %>% dplyr::rename("weight_p" = "∆體重(%)") %>% 
  ggscatter(x = "testosterone_baseline", y = "weight_p",
            color = "black",
            fill = "red",
            shape = 21,
            size = 1,
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            xlab = "testosterone_baseline(ng/ml)",
            ylab = "∆Weight(%)",
            xlim = c(0, 7),
            ylim = c(0, 30),
  ) +
  geom_vline(xintercept = c(3.5),linetype ="dashed", ) +
  annotate("text", x=3.4, y=25, label="Cutoff = 350 ng/dl", angle=90) +
  stat_cor(method = "pearson", size = 5, label.x = 4, label.y = 25) # Add correlation coefficient



aa2 <- 
  a %>% dplyr::rename("fat_p" = "∆體脂重(%)") %>%
  ggscatter(x = "testosterone_baseline", y = "fat_p",
            color = "black",
            fill = "red",
            shape = 21,
            size = 1,
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            xlab = "testosterone_baseline(ng/ml)",
            ylab = "∆Fat(%)",
            xlim = c(0, 7),
            ylim = c(0, 30),
  ) +
  geom_vline(xintercept = c(3.5),linetype ="dashed", ) +
  annotate("text", x=3.4, y=25, label="Cutoff = 350 ng/dl", angle=90) +
  stat_cor(method = "pearson", size = 5, label.x = 4, label.y = 25) # Add correlation coefficient

#plot_grid(aa1, aa2, labels = LETTERS)




#[Correlation r] Efficacy x Baseline
library(corrplot)
M_testosterone <- cor(cbind(-profile_efficacy, profile_baseline), use = "pairwise.complete.obs")
#[2Do]change row,col names into chinese
M_testosterone_test <- cor.mtest(cbind(-profile_efficacy, profile_baseline) , conf.level = .95)
M_col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))

#run corrplot plot
# corrplot(M_testosterone,
#          p.mat = M_testosterone_test$p,
#          type = "lower",
#          insig = "label_sig",
#          sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
#          tl.col = "black", tl.srt = 35, tl.cex = 1.0,
#          cl.ratio = 0.1,
#          col = M_col(200),
#          title = "[Correlation] Efficacy x Baseline",
#          #c(bottom, left, top, right)
#          mar = c(0,0,1,0))



for (i in c(1:length(colnames(M_testosterone)))) {
  if (i == 1) {
    M2_value <- M_testosterone %>% round(2) 
    M2_sign <- M_testosterone_test$p %>% stats::symnum(corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")) %>% as.data.frame.matrix()
    M_testosterone_test_df <- M2_value
    M_testosterone_test_df[,] <- NA
  }
  
  M_testosterone_test_df[,i] <- paste0(M2_value[,i], " (", M2_sign[,i], ")")
  
  if (i ==  length(colnames(M_testosterone))) {
    rm(list = c("M2_value", "M2_sign"))
    M_testosterone_test_df <- M_testosterone_test_df %>% as.data.frame()
    M_testosterone_test_df <- M_testosterone_test_df %>% add_column(vars = rownames(M_testosterone_test_df), .before = names(M_testosterone_test_df)[1])
    M_testosterone_test_df <- M_testosterone_test_df %>% add_column("#" = seq(1, nrow(M_testosterone_test_df)), .before = names(M_testosterone_test_df)[1])
  }
  
} 
cor_table_testosterone <- M_testosterone_test_df %>% gvisTable(options=list(frozenColumns = 2, 
                                                                            width="150%",height=300))


#>>>> Low T, High Weigt/Fat/Blood/Insulin/IR, IR improvement越差

# Effectiveness --------------------------------------------------------------------

#divide gp_testosterone
a <- stat_table_1st_ob %>% filter(gender == "male")

a$gp <- a$testosterone_baseline %>% cut(c(-Inf, cutoff_testosterone, Inf), c("Low_testosterone", "Normal"))

table(a$gp) %>% addmargins()

a <- a[complete.cases(a$gp),]





#profile
vars_en <- c("id","client_type","age","gender","date_t0","date_t1",
             #inbody - baseline
             "weight_baseline","bmi_baseline","bf_baseline","pbf_baseline","bsmi_baseline","pbm_baseline","vfa_baseline","wc_baseline","ffm_baseline","bmr_baseline",
             #blood- baseline
             "hba1c_baseline","glucose_ac_baseline","insulin_baseline","homa_ir_baseline","homa_beta_baseline","tg_baseline","tc_baseline","hdl_baseline","ldl_baseline","lipase_baseline",
             #inbody - endpoint
             "weight_endpoint","bmi_endpoint","bf_endpoint","pbf_endpoint","bsmi_endpoint","pbm_endpoint","vfa_endpoint","wc_endpoint","ffm_endpoint","bmr_endpoint",
             #blood- endpoint
             "hba1c_endpoint","glucose_ac_endpoint","insulin_endpoint","homa_ir_endpoint","homa_beta_endpoint","tg_endpoint","tc_endpoint","hdl_endpoint","ldl_endpoint","lipase_endpoint",
             #diet
             "upload_day_%","note_count","pic_counts","carb_E%","protein_E%","fat_E%","calorie_day","light_G_%","light_Y_%","light_R_%","fruits_day","vegetables_day","grains_day","meat_bean_day","milk_day","oil_day",
             #others
             "gp",
             #inbody - ∆
             "∆weight","∆bmi","∆bf","∆pbf","∆bsmi","∆bm","∆vfa","∆wc","∆ffm","∆bmr",
             #blood - ∆
             "∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆homa_beta","∆tg","∆tc","∆hdl","∆ldl","∆lipase",
             #inbody - ∆%
             "∆weight%","∆bmi%","∆bf%","∆pbf%","∆bsmi%","∆bm%","∆vfa%","∆wc%","∆ffm%","∆bmr%",
             #blood - ∆%
             "∆hba1c%","∆glucose_ac%","∆insulin%","∆homa_ir%","∆homa_beta%","∆tg%","∆tc%","∆hdl%","∆ldl%","∆lipase%"
)

a %<>% select(vars_en)

vars_en <- lin_ch_en_format(x = vars_en, format = "en", origin = "raw_en")
names(a) <- lin_ch_en_format(x = names(a), format = "en", origin = "raw_en")

#turn ∆ into positve(reverse)
tmp_1 <- a %>% select(-grep("∆", names(a))) 


#Setting improvement direction
##Improvement: Uncertain, default setting
tmp_2 <- a %>% 
  select(c("∆bmr","∆bmr%", "∆lipase","∆lipase%"))

##Improvement: negative
tmp_3 <- a %>% select(c("∆weight","∆bmi","∆bf","∆pbf","∆vfa","∆wc","∆ffm","∆weight%","∆bmi%","∆bf%","∆pbf%","∆vfa%","∆wc%","∆ffm%","∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆tg","∆tc","∆ldl","∆hba1c%","∆glucose_ac%","∆insulin%","∆homa_ir%","∆tg%","∆tc%","∆ldl%")) %>% multiply_by(-1)
##Improvement: positive
tmp_4 <- a %>% select(c("∆bsmi","∆bm","∆bsmi%","∆bm%","∆homa_beta","∆hdl","∆homa_beta%","∆hdl%"))

a <- Reduce(cbind,list(tmp_1, tmp_2, tmp_3, tmp_4), accumulate =FALSE) 

rm(list = c("tmp_1","tmp_2","tmp_3","tmp_4"))

#order again!!
a <- a %>% select(vars_en)

#change colname to run plot
#table_used
c <- a
#plot_used
b <- a
names(b) <- gsub("∆", "delta_", names(b))
names(b) <- gsub("%", "_percent", names(b))


#new
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



myplots_plot_testosterone <- vector('list', length(var_vector))

for (i in c(var_vector)) {
  j <- match(i, var_vector)
  if (j == 1) {
    vector_pvalue <- c()
    start_time <- Sys.time()
  }
  
  a <- b %>% colnames() %>% head(i) %>% tail(1)
  a_title <- myplot_table[myplot_table$num == j, "vars_ch"]
  
  
  #p.sign?
  stat.test <- 
    b %>%
    rstatix::dunn_test(as.formula(paste(a, "gp", sep = " ~ ")),p.adjust.method = "bonferroni")
  stat.test <- stat.test %>% rstatix::add_y_position()
  
  
  #for customed summary table - part 1/4 [p value]
  vector_pvalue <- append(vector_pvalue, 
                          stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
  )
  
  #plot
  plot_testosterone <- 
    b %>% 
    ggboxplot(x = "gp", y = a, fill = "gp", alpha = 0.5, width = 0.5, 
              title = a_title,
              legend = "none", xlab = FALSE, ylab = FALSE) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
      axis.text.x = element_text(angle = -30, vjust = 0.9, hjust = 0.05)
    ) +
    stat_pvalue_manual(stat.test, label = "p.adj.signif")
  
  myplots_plot_testosterone[[j]] <- plot_testosterone
  
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}









#(3.)output statistics table
#for customed summary table [summary table]
summary_table_testosterone <- 
  c %>% select(var_vector, "gp") %>% 
  group_by(gp) %>% 
  summarize_at(vars_en[var_vector],
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
  )



#rbind: summary_table, p.adj.sign, dif, improvement
summary_table_testosterone <- cbind(summary_table_testosterone %>% as.data.frame() %>% select(-c("gp")) %>% t(), as.data.frame(vector_pvalue)) 

names(summary_table_testosterone) <- c(rep(c("Low", "Normal")), "顯著差異")
rownames(summary_table_testosterone) <- myplot_table$vars_ch

summary_table_testosterone <- 
  rbind("人數" = table(c$gp) %>% as.numeric() %>% append(""), summary_table_testosterone)

table_02_testosterone <- 
  summary_table_testosterone %>% 
  kbl(format = "html", caption = "<b>Statistics:</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1,"Male" = 2, " " = 1)) %>% 
  footnote(general_title = c("Significance:"), general = "\n Comparison: Testosterone in male population.",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%")










