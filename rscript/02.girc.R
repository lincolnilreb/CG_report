# #self-produced GIRC curve value, based on DATABASE avg.!
# df05_biochem %>%
#   group_by(Pattern_minor, .drop = FALSE) %>% 
#   summarise(
#     insulin_0hr = mean(insulin, rm.na = TRUE) %>% round(2),
#     insulin_1hr = mean(insulin_pc_1hr, rm.na = TRUE) %>% round(2),
#     insulin_2hr = mean(insulin_pc_2hr, rm.na = TRUE) %>% round(2)
#     
#   ) %>% view()


# Q6_stat_table_1st <- stat_table_1st %>% filter(client_type != 1) %>% filter(!is.na(insulin_baseline) & !is.na(insulin_pc_1hr_baseline) & !is.na(insulin_pc_2hr_baseline))
Q6_stat_table_1st <- stat_table_1st_ob %>% filter(!is.na(insulin_baseline) & !is.na(insulin_pc_1hr_baseline) & !is.na(insulin_pc_2hr_baseline)) 

plot_M <- lin_insulin_rsp_pattern(Q6_stat_table_1st, c("insulin_baseline", "insulin_pc_1hr_baseline", "insulin_pc_2hr_baseline"), plot = percentage, pattern = 2, layout = TRUE)
plot_m <- lin_insulin_rsp_pattern(Q6_stat_table_1st, c("insulin_baseline", "insulin_pc_1hr_baseline", "insulin_pc_2hr_baseline"), plot = percentage, pattern = 1, layout = TRUE)




#baseline, ∆%
# Q6_stat_table_1st %>% 
#   filter(!is.na(Pattern_major_baseline)) %>% 
#   select(Pattern_major_baseline, gender, `∆weight%`) %>% rename(value = `∆weight%`) %>% 
#   mutate(value_adj = value %>% multiply_by(-1)) %>% 
#   ggbarplot(x = "gender", y = "value_adj", fill = "Pattern_major_baseline", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0, width = 0.5,
#             add = "mean_se", add.params = list(group = "Pattern_major_baseline"),
#             label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, lab.size = 3,
#             position = position_dodge(0.5), 
#             xlab = "", ylab = "∆Weight Loss(%)", title = "減重成效",
#             legend = "right", legend.title = "GIRC", ggtheme = theme_light() ) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
#     axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
#     axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
#   ) 

# Q6_stat_table_1st %>% 
#   filter(!is.na(Pattern_major_baseline)) %>% 
#   select(Pattern_major_baseline, gender, `weight_baseline`) %>% rename(value = `weight_baseline`) %>% 
#   mutate(value_adj = value %>% multiply_by(1)) %>% 
#   ggbarplot(x = "gender", y = "value_adj", fill = "Pattern_major_baseline", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0, width = 0.5,
#             add = "mean_se", add.params = list(group = "Pattern_major_baseline"),
#             label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, lab.size = 3,
#             position = position_dodge(0.5), 
#             xlab = "", ylab = "Weight(Kg)", title = "減重成效",
#             legend = "right", legend.title = "GIRC", ggtheme = theme_light() ) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
#     axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
#     axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
#   ) 

# Q6_stat_table_1st %>% 
#   filter(!is.na(Pattern_major_baseline)) %>% 
#   select(Pattern_major_baseline, gender, `pbf_baseline`) %>% rename(value = `pbf_baseline`) %>% 
#   mutate(value_adj = value %>% multiply_by(1)) %>% 
#   ggbarplot(x = "gender", y = "value_adj", fill = "Pattern_major_baseline", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0, width = 0.5,
#             add = "mean_se", add.params = list(group = "Pattern_major_baseline"),
#             label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, lab.size = 3,
#             position = position_dodge(0.5), 
#             xlab = "", ylab = "PBF(%)", title = "減重成效",
#             legend = "right", legend.title = "GIRC", ggtheme = theme_light() ) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
#     axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
#     axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
#   ) 




#篩選介入前data
a <- df05_biochem %>% distinct(id, .keep_all = TRUE)

#Presume: NA -> "female"
a[is.na(a$gender), "gender"] <- "female"


#[input] table, rbind RowSum
# t <- table(a$DM, a$Pattern_major,exclude = "Unclassified", useNA = "no")
# t1 <- t %>% addmargins()
# t2 <- t %>% prop.table() %>% multiply_by(100) %>% addmargins() %>% round(2)
# 
# 
# table_girc_all <- data.frame(matrix(data = paste0((t1) %>% t(), " (", (t2) %>% t(), "%)"),
#                                     nrow = nrow(t1), ncol = ncol(t1), byrow = TRUE,
#                                     dimnames = list(t1 %>% rownames(),t1 %>% colnames()))) %>% 
#   kable(format = "html", caption = "<b>Table: Pooled Data with Blood Test</b>", align = "c") %>%
#   kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
#                             full_width = FALSE, font_size = 15) %>% 
#   footnote(general_title = c("Note:"), general = c(rbind("", c(" Before intervention"))),
#            footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
#   )%>% 
#   gsub("font-size: initial !important;", 
#        "font-size: 15pt !important;", 
#        .)
# 
# rm(list = c("t","t1", "t2"))

t <- table(a$gender,a$DM, a$Pattern_major,exclude = "Unclassified", useNA = "no")
t1 <- t %>% addmargins() %>% stats::ftable()
t2 <- t %>% prop.table() %>% multiply_by(100) %>% addmargins() %>% round(2) %>% stats::ftable()

t3 <- 
  data.frame(matrix(data = paste0((t1) %>% t(), " (", (t2) %>% t(), "%)"),
                    nrow = nrow(t1), ncol = ncol(t1), byrow = TRUE,
  ))
names(t3) <- attr(t1,"col.vars") %>% unlist()
t3$gender  <-  c(rbind(attr(t1,"row.vars")[1] %>% unlist(), "", "",""))
t3 <- t3 %>% dplyr::relocate(gender, .before = 'Pattern I')
t3$.  <-  attr(t1,"row.vars")[2] %>% unlist() %>% rep(times= length(attr(t1,"row.vars")[1] %>% unlist()))
t3 <- t3 %>% dplyr::relocate('.', .before = 'Pattern I')

table_girc_all <- 
  t3 %>% 
  kable(format = "html", caption = "<b>Table: Pooled Data with Blood Test</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Note:"), general = c(rbind("", c(" Before intervention"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)

rm(list = c("t","t1", "t2", "t3"))



#DM View
#[input] table, all: addmargins(), row:addmargins(t1:2;t2:1,2),rbind RowSum
t <- table(a$DM, a$Pattern_major, exclude = "Unclassified", useNA = "no")
t1 <- t %>% addmargins(margin = 2)
t2 <- t %>% prop.table(margin = 1) %>% multiply_by(100) %>% addmargins(margin = 2) %>% round(2)

table_DM_girc_all <- data.frame(matrix(data = paste0(t1 %>% t(), " (", t2 %>% t(), "%)"), 
                                       nrow = nrow(t1), ncol = ncol(t1), byrow = TRUE, 
                                       dimnames = list(t1 %>% rownames(),t1 %>% colnames()))) %>% 
  rbind("Total" = t1 %>% addmargins(margin = 1) %>% last() %>% as.numeric()) %>% 
  kable(format = "html", caption = "<b>Table: Pooled Data with Blood Test(DM View)</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Note:"), general = c(rbind("", c(" Before intervention"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)
rm(list = c("t","t1", "t2"))


#Insulin Pattern  View
#[input] table, all: addmargins(), row:addmargins(t1:2;t2:1,2),rbind RowSum
t <- table(a$DM, a$Pattern_major, exclude = "Unclassified", useNA = "no")
t1 <- t %>% addmargins(margin = 1)
t2 <- t %>% prop.table(margin = 2) %>% multiply_by(100) %>% addmargins(margin = 1) %>% round(2)

table_Insulin_girc_all <- data.frame(matrix(data = paste0(t1 %>% t(), " (", t2 %>% t(), "%)"), 
                                            nrow = nrow(t1), ncol = ncol(t1), byrow = TRUE, 
                                            dimnames = list(t1 %>% rownames(),t1 %>% colnames()))) %>% 
  cbind("Total" = t1 %>% addmargins(margin = 2) %>% t() %>% last() %>% as.numeric()) %>% 
  kable(format = "html", caption = "<b>Table: Pooled Data with Blood Test(IR View)</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Note:"), general = c(rbind("", c(" Before intervention", c("\n", "*IR = The \"insulin response\" during an OGTT.")))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)
rm(list = c("t","t1", "t2"))


#In obesity program
#DM & GIRC Pattern Cross_table in OB. program
Q6_stat_table_1st$DM_baseline <- Q6_stat_table_1st$DM_baseline %>% factor(levels = c("Normal", "Pre-DM", "DM"))

#[input] table, all: addmargins(), row:addmargins(t1:2;t2:1,2),rbind RowSum
# t <- table(Q6_stat_table_1st$DM_baseline, Q6_stat_table_1st$Pattern_major_baseline, exclude = "Unclassified", useNA = "no")
# t1 <- t %>% addmargins()
# t2 <- t %>% prop.table() %>% multiply_by(100) %>% addmargins() %>% round(2)
# 
# table_girc_ob <- data.frame(matrix(data = paste0(t1 %>% t(), " (", t2 %>% t(), "%)"), 
#                                    nrow = nrow(t1), ncol = ncol(t1), byrow = TRUE, 
#                                    dimnames = list(t1 %>% rownames(),t1 %>% colnames()))) %>% 
#   kable(format = "html", caption = "<b>Table: Stuty Group - Obesity Program</b>", align = "c") %>%
#   kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
#                             full_width = FALSE, font_size = 15) %>% 
#   footnote(general_title = c("Note:"), general = c(rbind("", c(" Before intervention"))),
#            footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
#   )%>% 
#   gsub("font-size: initial !important;", 
#        "font-size: 15pt !important;", 
#        .)
# rm(list = c("t","t1", "t2"))

t <- table(Q6_stat_table_1st$gender,Q6_stat_table_1st$DM_baseline, Q6_stat_table_1st$Pattern_major_baseline, exclude = "Unclassified", useNA = "no")
t1 <- t %>% addmargins() %>% stats::ftable()
t2 <- t %>% prop.table() %>% multiply_by(100) %>% addmargins() %>% round(2) %>% stats::ftable()

t3 <- 
  data.frame(matrix(data = paste0((t1) %>% t(), " (", (t2) %>% t(), "%)"),
                    nrow = nrow(t1), ncol = ncol(t1), byrow = TRUE,
  ))
names(t3) <- attr(t1,"col.vars") %>% unlist()
t3$gender  <-  c(rbind(attr(t1,"row.vars")[1] %>% unlist(), "", "",""))
t3 <- t3 %>% dplyr::relocate(gender, .before = 'Pattern I')
t3$.  <-  attr(t1,"row.vars")[2] %>% unlist() %>% rep(times= length(attr(t1,"row.vars")[1] %>% unlist()))
t3 <- t3 %>% dplyr::relocate('.', .before = 'Pattern I')

table_girc_ob <- 
  t3 %>% 
  kable(format = "html", caption = "<b>Table: Stuty Group - Obesity Program</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Note:"), general = c(rbind("", c(" Before intervention"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)

rm(list = c("t","t1", "t2", "t3"))


#DM View
#[input] table, all: addmargins(), row:addmargins(t1:2;t2:1,2),rbind RowSum
t <- table(Q6_stat_table_1st$DM_baseline, Q6_stat_table_1st$Pattern_major_baseline, exclude = "Unclassified", useNA = "no")
t1 <- t %>% addmargins(margin = 2)
t2 <- t %>% prop.table(margin = 1) %>% multiply_by(100) %>% addmargins(margin = 2) %>% round(2)

table_DM_girc_ob <- data.frame(matrix(data = paste0(t1 %>% t(), " (", t2 %>% t(), "%)"), 
                                      nrow = nrow(t1), ncol = ncol(t1), byrow = TRUE, 
                                      dimnames = list(t1 %>% rownames(),t1 %>% colnames()))) %>% 
  rbind("Total" = t1 %>% addmargins(margin = 1) %>% last() %>% as.numeric()) %>% 
  kable(format = "html", caption = "<b>Table: Stuty Group - Obesity Program(DM View)</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Note:"), general = c(rbind("", c(" Before intervention"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)
rm(list = c("t","t1", "t2"))


#Insulin Pattern  View
#[input] table, all: addmargins(), row:addmargins(t1:2;t2:1,2),rbind RowSum
t <- table(Q6_stat_table_1st$DM_baseline, Q6_stat_table_1st$Pattern_major_baseline, exclude = "Unclassified", useNA = "no")
t1 <- t %>% addmargins(margin = 1)
t2 <- t %>% prop.table(margin = 2) %>% multiply_by(100) %>% addmargins(margin = 1) %>% round(2)

table_Insulin_girc_ob <- data.frame(matrix(data = paste0(t1 %>% t(), " (", t2 %>% t(), "%)"), 
                                           nrow = nrow(t1), ncol = ncol(t1), byrow = TRUE, 
                                           dimnames = list(t1 %>% rownames(),t1 %>% colnames()))) %>% 
  cbind("Total" = t1 %>% addmargins(margin = 2) %>% t() %>% last() %>% as.numeric()) %>% 
  kable(format = "html", caption = "<b>Table: Stuty Group - Obesity Program(IR View)</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Note:"), general = c(rbind("", c(" Before intervention", c("\n", "*IR = The \"insulin response\" during an OGTT.")))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)
rm(list = c("t","t1", "t2"))




#GIRC improvement path?
##Pool improvement - fragment alignment/mapping: ncol:3(id, origin, aftermath: DM/I)
a <- df05_biochem %>% select(id, date_blood, Pattern_major, DM)
a <- a %>% filter((Pattern_major %in% c(levels(a$Pattern_major)[-6]) & (DM %in% c(levels(a$DM)[-4]))))
a <- a %>% filter(id %in% (janitor::get_dupes(a, id) %>% select(id) %>% pull))
a <- a[with(a, order(id, date_blood)),]

a$Pattern_major_after <- NA
a$Pattern_major_after[1:nrow(a)-1] <- a[["Pattern_major"]][2:nrow(a)] %>% as.character()
a$DM_after <- NA
a$DM_after[1:nrow(a)-1] <- a[["DM"]][2:nrow(a)] %>% as.character()


library(dplyr)
a <- a %>% group_by(id) %>% slice(1:(n()-1))
names(a) <- c("id", "date", "I_before", "DM_before", "I_after",  "DM_after")


# df05_biochem %>% select(id, date_blood, Pattern_major, DM) %>% view()
# exclude DM | include only OB client
a <- a[a$DM_before != "DM" & a$DM_after != "DM",]
#filter 1st intervention
# a <- a %>% distinct(id, .keep_all = T)

#[input] table, all: addmargins(), row:addmargins(t1:2;t2:1,2),rbind RowSum
t <- table(Origin = a$I_before, Change = a$I_after, exclude = "Unclassified")
t1 <- t %>% addmargins(margin = 2)
t2 <- t %>% prop.table(margin = 1) %>% multiply_by(100) %>% addmargins(margin = 2) %>% round(2)

table_improvement_girc_all <- data.frame(matrix(data = paste0(t1 %>% t(), " (", t2 %>% t(), "%)"), 
                                                nrow = nrow(t1), ncol = ncol(t1), byrow = TRUE, 
                                                dimnames = list(t1 %>% rownames(),t1 %>% colnames()))) %>% 
  rbind("Total" = t1 %>% addmargins(margin = 1) %>% last() %>% as.numeric()) %>% 
  kable(format = "html", caption = "<b>Table: Improvemnt of Insulin Response Pattern in OGTT</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Note:"), general = c(rbind("\n", c("· Row= Pre-intervention Insulin Response Pattern during an OGTT.", "· Column= Post-intervention Insulin Response Pattern during an OGTT."))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)
rm(list = c("t","t1", "t2"))




##T0,T1,∆ plot

datasets_target_issue <- Q6_stat_table_1st %>% dplyr::rename(gp = Pattern_major_baseline)
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
datasets_target_issue_for_plot <- datasets_target_issue %>% filter(!((gender == "male") & (gp == "Pattern V")))

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
myplots_girc <- vector('list', length(var_vector))

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
    ggbarplot(x = "gender", y = a, fill = "gp", palette = c("#dce5f6","#fdf7d6","#ffe6cd","#ffdac9","#ffd8d8"), alpha = 1.0,
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
  myplots_girc[[j]] <- plot
  
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}

rm(list = c("datasets_target_issue_a", "datasets_target_issue_b", "datasets_target_issue_c", "datasets_target_issue_d"))

#(2.)gender x Group table
#[customized part!!!]
table_01_girc <- 
  table(datasets_target_issue$gender, datasets_target_issue$gp) %>% addmargins() %>% 
  kable(format = "html", caption = "<b>Table: Population(OB.)</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Note:"), general = c(rbind("", c(" Obesity Program & Before intervention"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)


#(3.)output statistics table
#for customed summary table [summary table]
#[customized part!!!]
summary_table_girc <- 
  datasets_target_issue %>% 
  group_by(gender, gp) %>% 
  summarize_at(vars_en[var_vector],
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
  )



#rbind: summary_table, p.adj.sign, dif, improvement
# summary_table_sua <- cbind(summary_table_sua %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t(), as.data.frame(vector_pvalue)) 
summary_table_girc <- summary_table_girc %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t()

# names(summary_table_sua) <- c(rep(levels((datasets_target_issue$gp)), 2), "顯著差異")
# colnames(summary_table_girc) <- c(rep(levels((datasets_target_issue$gp)), 2))
colnames(summary_table_girc) <- c(levels(datasets_target_issue$gp)[1:5], levels((datasets_target_issue$gp))[1:5])
rownames(summary_table_girc) <- myplot_table$vars_ch

#[customized part!!!]
table_02_girc <- 
  summary_table_girc %>% 
  kbl(format = "html", caption = "<b>Statistics:</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1, "Female" = length(levels(datasets_target_issue$gp)[1:5]), "Male" = length(levels(datasets_target_issue$gp)[1:5]))) %>% 
  footnote(general_title = c("Significance:"), general = "\n ",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%")






# Pattern A -> Pattern B Comparison ---------------------------------------
# [Obstacle]Cannot analyze for now: do not have post-intervened OGTT in the 1st treatment,
# datasets_target_issue <- Q6_stat_table_1st %>% mutate(Pattern_change = paste(Pattern_major_baseline, Pattern_major_endpoint, sep = ">"))
# datasets_target_issue$Pattern_major_endpoint %>% table()
# datasets_target_issue$Pattern_change %>% factor(labels = )
# 
# datasets_target_issue <- Q6_stat_table_1st %>% dplyr::rename(gp = Pattern_change)
# datasets_target_issue <- datasets_target_issue %>% filter(gp %in% levels(datasets_target_issue$gp))
