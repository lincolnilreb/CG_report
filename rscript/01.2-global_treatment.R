
# ## Analysis - 1: Baseline 分布 (Pie-chart) --------------------------------

#[Pie chart] 1. cut (cluster) 2. call pie chart
#Age
stat_table_1st_ob$age_gp <- cut(stat_table_1st_ob$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
pie_01 <- 
  stat_table_1st_ob %>% group_by(age_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Age',
                                                                                                legend = "{position:'right'}",
                                                                                                pieHole = 0.5,
                                                                                                #slices = "{1:{offset:0.1}}",
                                                                                                backgroundColor = "#f9fffb",
                                                                                                width = "600",
                                                                                                height = "400"))

#Gender
pie_02 <- 
  stat_table_1st_ob %>% group_by(gender) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Gender',
                                                                                                legend = "{position:'right'}",
                                                                                                pieHole = 0.5,
                                                                                                #slices = "{0:{offset:0.1}}",
                                                                                                backgroundColor = "#f9fffb",
                                                                                                colors = "['#DC3912', '#3366CC']",
                                                                                                width = "600",
                                                                                                height = "400"))



#BMI x Obesity

stat_table_1st_ob$bmi_gp <- cut(stat_table_1st_ob$bmi_baseline, c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))

stat_table_1st_ob$bmi_gp_2 <- stat_table_1st_ob$bmi_baseline %>% cut(c(0,18.5,24,27, 30, 35 ,100), c("underweight", "normal", "overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)

pie_03 <- 
  stat_table_1st_ob %>% filter(gender == "male") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Male',
                                                                                                                             legend = "{position:'right'}",
                                                                                                                             pieHole = 0.5,
                                                                                                                             #slices = "{2:{offset:0.1}}",
                                                                                                                             backgroundColor = "#f9fffb",
                                                                                                                             width = "600",
                                                                                                                             height = "400"))

pie_04 <- 
  stat_table_1st_ob %>% filter(gender == "female") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Female',
                                                                                                                               legend = "{position:'right'}",
                                                                                                                               pieHole = 0.5,
                                                                                                                               #slices = "{1:{offset:0.1}}",
                                                                                                                               backgroundColor = "#f9fffb",
                                                                                                                               width = "600",
                                                                                                                               height = "400"))

pie_03_2 <- 
  stat_table_1st_ob %>% filter(gender == "male") %>% group_by(bmi_gp_2) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Male',
                                                                                                                             legend = "{position:'right'}",
                                                                                                                             pieHole = 0.5,
                                                                                                                             #slices = "{2:{offset:0.1}}",
                                                                                                                             backgroundColor = "#f9fffb",
                                                                                                                             width = "600",
                                                                                                                             height = "400"))

pie_04_2 <- 
  stat_table_1st_ob %>% filter(gender == "female") %>% filter(bmi_gp_2 != "underweight") %>% group_by(bmi_gp_2) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Female',
                                                                                                                               legend = "{position:'right'}",
                                                                                                                               pieHole = 0.5,
                                                                                                                               #slices = "{1:{offset:0.1}}",
                                                                                                                               backgroundColor = "#f9fffb",
                                                                                                                               width = "600",
                                                                                                                               height = "400"))
# Disease
  #DM
pie_05_DM <- 
  stat_table_1st_ob %>% group_by(DM_baseline) %>%
  filter(DM_baseline != "Unclassified") %>%
  summarise(n = n()) %>% gvisPieChart(options = list(title = 'Diabetes',
                                                     legend = "{position:'right'}",
                                                     pieHole = 0.5,
                                                     #slices = "{1:{offset:0.1}}",
                                                     backgroundColor = "#f9fffb",
                                                     width = "600",
                                                     height = "400"))


  #HTN
pie_05_HTN <-
  stat_table_1st_ob %>% group_by(HTN_baseline) %>%
  filter(HTN_baseline != "Unclassified") %>%
  summarise(n = n()) %>% gvisPieChart(options = list(title = 'Hypertension',
                                                     legend = "{position:'right'}",
                                                     pieHole = 0.5,
                                                     #slices = "{1:{offset:0.1}}",
                                                     backgroundColor = "#f9fffb",
                                                     width = "600",
                                                     height = "400"))

  #HLP
pie_05_HLP <-
  stat_table_1st_ob %>% group_by(HLP_baseline) %>%
  filter(HLP_baseline != "Unclassified") %>%
  summarise(n = n()) %>% gvisPieChart(options = list(title = 'Hyperlipidemia',
                                                     legend = "{position:'right'}",
                                                     pieHole = 0.5,
                                                     #slices = "{1:{offset:0.1}}",
                                                     backgroundColor = "#f9fffb",
                                                     width = "600",
                                                     height = "400"))

  #Metabolic Syndrome
pie_05_MetX <-
  stat_table_1st_ob %>% group_by(MetaX_baseline) %>%
  filter(MetaX_baseline != "Unclassified") %>%
  summarise(n = n()) %>% gvisPieChart(options = list(title = 'Metabolic Syndrome',
                                                     legend = "{position:'right'}",
                                                     pieHole = 0.5,
                                                     #slices = "{1:{offset:0.1}}",
                                                     backgroundColor = "#f9fffb",
                                                     width = "600",
                                                     height = "400"))

  #Insulin Pattern
pie_05_Ins <-
  stat_table_1st_ob %>% group_by(Pattern_major_baseline) %>%
  filter(Pattern_major_baseline != "Unclassified" | !is.na(Pattern_major_baseline)) %>%
  summarise(n = n()) %>% gvisPieChart(options = list(title = 'Insulin Response Pattern',
                                                     legend = "{position:'right'}",
                                                     pieHole = 0.5,
                                                     #slices = "{1:{offset:0.1}}",
                                                     colors="['#628bd6','#f8e05c','#ffc081','#ff834a','#ff5959']",
                                                     backgroundColor = "#f9fffb",
                                                     width = "600",
                                                     height = "400"))




# Efficacy_Inbody (weight,  fat,  muscle) ----------------------------------

####控糖減重成效-身體組成


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

#fat
b <- 
  data %>% 
  dplyr::group_by(`∆fat_gp`, `gender`, .drop = FALSE) %>%
  summarise_at(
    vars(`∆bf%`),
    funs(round(mean(., na.rm = TRUE), 2))
  )  %>% 
  inner_join(
    data %>% 
      dplyr::group_by(`∆fat_gp`, `gender`, .drop = FALSE) %>%
      summarise(
        N = n()
      ))
b <- b[with(b, order(gender)),]
b <- b %>% dplyr::group_by(gender) %>% mutate(percentage = round(N / sum(N) *100,2)) 
names(b)[c(1,3)] <- c("Group", "Value")
b$variable <-  "Fat"
a <- rbind(a,b)
#muscle
b <- 
  data %>% 
  dplyr::group_by(`∆muscle_mass_gp`, `gender`, .drop = FALSE) %>%
  summarise_at(
    vars(`∆bm%`),
    funs(round(mean(., na.rm = TRUE), 2))
  )  %>% 
  inner_join(
    data %>% 
      dplyr::group_by(`∆muscle_mass_gp`, `gender`, .drop = FALSE) %>%
      summarise(
        N = n()
      ))
b <- b[with(b, order(gender)),]
b <- b %>% dplyr::group_by(gender) %>% mutate(percentage = round(N / sum(N) *100,2)) 
names(b)[c(1,3)] <- c("Group", "Value")
b$variable <-  "Muscle"
a <- rbind(a,b)



#output thr .kable
# b <- a
# b <- b %>% dplyr::rename(., "Mean ± SE" = "Value") 
#   




#output plot
a$Group <-  factor(a$Group, levels = c("<-15%","-8~15%","-4~8%", "-4~0%","0~5%","5~15%",">15%"))
a$variable <-  factor(a$variable, levels = c("Weight","Fat","Muscle"))


#*** for Index.Rmd highlight
success_df <- a %>% filter(variable == "Weight") %>% filter(!is.nan(Value)) %>% dplyr::mutate(., success = cut(Value, c(-Inf, 0, Inf), c(1, 0)))
success_df_freq <- data.frame(gender = c("female", "male"),
                              success = success_df %>% group_by(gender, success, .drop = FALSE) %>% summarise(N = sum(N)) %>% filter(success == 1) %>% pull(),
                              failure = success_df %>% group_by(gender, success, .drop = FALSE) %>% summarise(N = sum(N)) %>% filter(success == 0) %>% pull()) %>% janitor::adorn_totals("row") %>% janitor::adorn_totals("col")
success_df_pct <- success_df_freq %>% janitor::adorn_percentages() %>% janitor::adorn_pct_formatting()
rm(success_df)

# Stacked + percent
library(ggplot2)
# plot_stack_col <- 
# ggplot(a) +
#   aes(x = variable, y = percentage, fill = Group) +
#   geom_col() +
#   scale_fill_brewer(palette = "RdYlBu", 
#                     direction = 1) +
#   labs(x = " ", y = "Percentage(%)", title = "控糖減重成效-身體組成(OB)", fill = "Interval") +
#   geom_text(data = subset(a, percentage > 5), aes(label = paste0(percentage,"%")), size = 5, position = position_stack(vjust = 0.8)) +
#   theme_minimal() +
#   theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20), 
#         axis.text.x = element_text(size = 17,face = "bold"),
#         axis.text.y = element_text(size = 12),
#         axis.title.y = element_text(size = 17, face = "bold"),
#         legend.text = element_text(size = 15)) +
#   facet_wrap(vars(gender))


#ggviz_bar_stack
global_eff_bar_df <- function(data, gender){
  sex <- deparse(substitute(gender))
  data <- data %>% as.tibble()
  #create df
  b <- data %>% filter((gender == sex) & (variable == levels(data[[deparse(substitute(variable))]])[1])) %>% select(c("Group", "N")) %>% as.data.frame() %>% t()
  colnames(b) <- b["Group",]
  b <- b %>%  as.data.frame()
  b <- b["N",]
  #transform ori table
  b <-
    Reduce(rbind,list(b,
                      data %>% filter((gender == sex) & (variable == levels(data$variable)[2])) %>% select(c("Group", "N")) %>% as.data.frame() %>% select("N") %>% t() %>%  as.vector(),
                      data %>% filter((gender == sex) & (variable == levels(data$variable)[3])) %>% select(c("Group", "N")) %>% as.data.frame() %>% select("N") %>% t() %>%  as.vector()
    ))
  #adjust df
  b <- b %>% lapply(as.numeric) %>% as.tibble()
  b$var <- levels(a$variable)
  return(b)
}

b <- global_eff_bar_df(data = a, female)


#plot
col_color <- 
  paste0("[", RColorBrewer::brewer.pal(11, 'RdYlBu')[c(2,3,4,5,7,9,10)] %>% rev() %>% 
           stringr::str_c('\'', ., '\'') %>% 
           stringr::str_c(collapse = ","), "]")

plot_stack_col_female <-
  gvisColumnChart(b , xvar = "var", yvar = b %>% select(-var) %>% names() %>% rev(),
                  options = list(isStacked = 'percent',
                                 bar="{groupWidth:'50%'}",
                                 title = '控糖減重成效-身體組成(Female)',
                                 legend = "{position:'right'}",
                                 colors = col_color,
                                 backgroundColor = "#f9fffb",
                                 width = "600",
                                 height = "600"))

b <- global_eff_bar_df(data = a, male)

col_color <- 
  paste0("[", RColorBrewer::brewer.pal(11, 'RdYlBu')[c(2,3,4,5,7,9,10)] %>% rev() %>% 
           stringr::str_c('\'', ., '\'') %>% 
           stringr::str_c(collapse = ","), "]")
plot_stack_col_male <-
  gvisColumnChart(b , xvar = "var", yvar = b %>% select(-var) %>% names() %>% rev(),
                  options = list(isStacked = 'percent',
                                 bar="{groupWidth:'50%'}",
                                 title = '控糖減重成效-身體組成(Male)',
                                 legend = "{position:'right'}",
                                 colors = col_color,
                                 backgroundColor = "#f9fffb",
                                 width = "600",
                                 height = "600"))


rm(list = c("a", "b", "data"))

# Line plot ---------------------------------------------------------------
#Establish summary table
a <- stat_table_1st_ob %>% select(grep("baseline$", stat_table_1st_ob %>% names())) %>% select_if(is.numeric)
b <- stat_table_1st_ob %>% select(grep("endpoint$", stat_table_1st_ob %>% names())) %>% select_if(is.numeric)
var_ch <- c("weight","bmi","bf", "pbf","bsmi",  "bm", "vfa_level","wc", "ffm","bmr","hba1c", "glucose_ac", "insulin","homa_ir", "homa_beta", "tg", "tc", "hdl", "ldl", "uric_acid", "amylase","lipase")

a <- a %>% select_if(is.numeric) %>%
  select(grep(paste0("^",paste(var_ch, collapse = "|")), names(.)))
b <- b %>% select_if(is.numeric) %>%
  select(grep(paste0("^",paste(var_ch, collapse = "|")), names(.)))

a <- a %>% select(grep("bfmi|ffmi|tbwffm|insulin_pc_1hr|insulin_pc_2hr|sd_ldl|pbm",
                       names(a), invert = T)) 
b <- b %>% select(grep("bfmi|ffmi|tbwffm|insulin_pc_1hr|insulin_pc_2hr|sd_ldl|pbm",
                       names(b), invert = T)) 



#[D0] mean
stat_table_1st_ob_temp <- 
  a %>%
  apply(2, function(x) round(mean(x, na.rm = TRUE ), 2))

#[D0] SEM
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        a %>%
          apply(2, function(x) round(sd(x, na.rm = TRUE ), 2))
  )

#[D60] mean
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        b %>%
          apply(2, function(x) round(mean(x, na.rm = TRUE ), 2))
  )

#[D60] SEM
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        b %>%
          apply(2, function(x) round(sd(x, na.rm = TRUE ), 2))
  )

stat_table_1st_ob_temp <- stat_table_1st_ob_temp %>% as.data.frame()
names(stat_table_1st_ob_temp) <-  c("D0_mean","D0_SEM","D60_mean","D60_SEM")

#[∆] mean
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        ((b) - (a)) %>% 
          apply(2, function(x) round(mean(x, na.rm = TRUE ), 2))
  )

#[∆] SEM
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        ((b) - (a)) %>%
          apply(2, function(x) round(sd(x, na.rm = TRUE )/sqrt(length(x)), 2))
  )

#[∆%] mean
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        (
          ((b) - (a)) / a *100) %>% 
          apply(2, function(x) round(mean(x, na.rm = TRUE ), 2))
  )

#[∆%] SEM
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        (
          ((b) - (a)) / a *100) %>% 
          apply(2, function(x) round(sd(x, na.rm = TRUE )/sqrt(length(x)), 2))
  )

for (i in c(1:((stat_table_1st_ob_temp %>% nrow())))) {
  if (!("p-value" %in% names(stat_table_1st_ob_temp))) {
    stat_table_1st_ob_temp$`p-value` <- NA
  }
  stat_table_1st_ob_temp$`p-value`[i] <- t.test(a[i] %>% pull(),
                                                b[i] %>% pull(),
                                                paired = TRUE,
                                                na.rm = TRUE,
                                                alternative = c("two.sided", "less", "greater"))$p.value
  if ( i == ((stat_table_1st_ob_temp %>% nrow()))) {
    print("[completed!]")
  }
}
#n()
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        a %>% apply(2, function(x) length(x))
  )


#df cleaning
names(stat_table_1st_ob_temp) <-  c("D0_mean","D0_SEM","D60_mean","D60_SEM", "∆_mean", "∆_SEM", "∆%_mean", "∆%_SEM", "p-value", "N")
stat_table_1st_ob_temp$variable <- gsub("_baseline$","", row.names(stat_table_1st_ob_temp))

row.names(stat_table_1st_ob_temp) <- seq(1,nrow(stat_table_1st_ob_temp))
library(dplyr)
stat_table_1st_ob_temp <- stat_table_1st_ob_temp %>% relocate(variable)

#before/after bar chart 
library(reshape)
stat_table_1st_ob_temp_barchart <- stat_table_1st_ob_temp %>% select("variable","∆%_mean", "∆%_SEM")
stat_table_1st_ob_temp_barchart <- melt(stat_table_1st_ob_temp_barchart, c("variable"))
colnames(stat_table_1st_ob_temp_barchart) <-  c("variable", "pre_post", "value")
stat_table_1st_ob_temp_barchart <- cbind(stat_table_1st_ob_temp_barchart[grep("mean$", stat_table_1st_ob_temp_barchart$pre_post),],
                                         stat_table_1st_ob_temp_barchart[grep("SEM$", stat_table_1st_ob_temp_barchart$pre_post),]["value"]
)
colnames(stat_table_1st_ob_temp_barchart) <-c("variable", "pre_post", "mean", "sem")

#create baseline 0,  w/ rbind
a <- stat_table_1st_ob_temp_barchart
a$mean <- a$sem <- 0
a$pre_post <- "Before"
stat_table_1st_ob_temp_barchart <- rbind(a, stat_table_1st_ob_temp_barchart)
rm(a)
stat_table_1st_ob_temp_barchart$pre_post[grep("^∆%", stat_table_1st_ob_temp_barchart$pre_post)] <- "After"
stat_table_1st_ob_temp_barchart$pre_post <-  stat_table_1st_ob_temp_barchart$pre_post %>% factor(levels = c("Before", "After"))
stat_table_1st_ob_temp_barchart <- stat_table_1st_ob_temp_barchart[with(stat_table_1st_ob_temp_barchart, order(variable, pre_post)),]
row.names(stat_table_1st_ob_temp_barchart) <- seq(1, nrow(stat_table_1st_ob_temp_barchart))
stat_table_1st_ob_temp_barchart <- stat_table_1st_ob_temp_barchart %>% mutate(sd = round(sem * sqrt(stat_table_1st_ob %>% nrow()), 2)) 


#line chart
var_ch <- c("weight","bmi","bf", "pbf","bsmi",  "bm", "vfa_level","wc", "ffm","bmr","hba1c", "glucose_ac", "insulin","homa_ir", "homa_beta", "tg", "tc", "hdl", "ldl", "uric_acid", "amylase","lipase")
line_plot_df <- stat_table_1st_ob_temp_barchart %>% filter(variable %in% var_ch)
var_ch <- var_ch %>% lin_ch_en_format(format = "ch", origin = "en")

line_plot_df$variable <- var_ch %>% rep(each = 2)
line_plot_df$variable <- line_plot_df$variable %>% factor(levels = var_ch)


line_plot <- 
  line_plot_df %>% 
  ggplot( aes(x = pre_post, group = 1)) + 
  geom_point(aes(y = mean), size = 1.5, color = "red3",) +
  geom_line(aes(y = mean), size = 0.3, color = "red3") +
  geom_text(data = . %>% filter(pre_post == "After"),
            aes(y = mean, label = paste0(round(mean, 1),"%")), 
            nudge_x = -0.7, size = 3,
            show.legend = FALSE) +
  labs(x = "", y = "成效(%)", title = "")+
  xlim("Before", "After") +
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.3))) +
  facet_wrap(vars(variable), scales = "free") +
  theme_linedraw() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 15)
  ) 


#google viz
# a <- stat_table_1st_ob_temp_barchart[!is.element(stat_table_1st_ob_temp_barchart$variable, c("glucose_pc_1hr", "glucose_pc_2hr", "insulin_pc_1hr", "insulin_pc_2hr")),] %>% filter(pre_post == "After") %>% select(c("variable", "mean"))
# a <- a %>% add_column(before = 0)
# names(a) <- c("var", "post", "pre")
# a <- a %>% select(c("var", "pre", "post")) %>% t()
# colnames(a) <- a["var",]
# a <- a[2:3,] %>% as.tibble()  %>% lapply(as.numeric) %>% as.tibble()
# a$pre_post <- c("pre", "post")
# 
# #line_plot <- 
# gvisLineChart(a, xvar = "pre_post", yvar = c("weight", "bf")) %>% plot()





# cor_plot ----------------------------------------------------------------





#[Create profile]  Efficacy, Baseline, Diet table
profile_efficacy <- stat_table_1st_ob %>% 
  select(c("∆weight%","∆bf%","∆bm%","∆vfa","∆wc","∆bmr","∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆homa_beta","∆tg","∆tc","∆hdl","∆ldl","∆lipase"))

names(profile_efficacy) <- names(profile_efficacy) %>% lin_ch_en_format(format = "ch", origin = "en")

profile_baseline <- stat_table_1st_ob %>% 
  select(c("age", "bmi_baseline","pbf_baseline","vfa_baseline","bsmi_baseline","pbm_baseline","wc_baseline","bmr_baseline",
           "hba1c_baseline","glucose_ac_baseline","insulin_baseline","homa_ir_baseline","homa_beta_baseline", "tAUCg_baseline", "tAUCi_baseline", "OGIRIndex_baseline",
           "tg_baseline","tc_baseline","hdl_baseline","ldl_baseline","lipase_baseline",
           "testosterone_baseline"))

names(profile_baseline) <- names(profile_baseline) %>% lin_ch_en_format(format = "ch", origin = "en")

profile_diet <- stat_table_1st_ob %>% 
  select(c("upload_day_%", "pic_counts", "light_G_%","light_Y_%","light_R_%",
           "calorie_day","carb_E%","protein_E%","fat_E%",
           "calorie_target","carb_ep_target","protein_ep_target","fat_ep_target",
           "calorie_day_deficit","carb_e_day_deficit","protein_e_day_deficit","fat_e_day_deficit",
           "calorie_meal_mean","carb_ep_meal","protein_ep_meal","fat_ep_meal",
           "fruits_day","vegetables_day","grains_day","meat_bean_day","milk_day", "oil_day"))



names(profile_diet) <- names(profile_diet) %>% lin_ch_en_format(format = "ch", origin = "en")


##[Method 2] corrplot

library(corrplot)
#[Correlation r] Efficacy x Diet
M1 <- cor(cbind(-profile_efficacy, profile_diet), use = "pairwise.complete.obs")
#[2Do]change row,col names into chinese
M_test1 <- cor.mtest(cbind(-profile_efficacy, profile_diet) , conf.level = .95)
M_col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))

# colnames(M1) <- row.names(M1) <- c("∆體重(%)", "∆體脂(%)","∆肌肉(%)","∆內臟脂肪","∆腰圍", "∆BMR", #6
#                                    "∆糖化血色素","∆空服血糖","∆空腹胰島素","∆Homa_IR","∆Homa_ß","∆三酸甘油脂","∆總膽固醇","∆HDL","∆LDL", "∆解脂酶", #10
#                                    "上傳天數%","上傳照片數", "總攝取卡路里","總碳水比_E%","總蛋白比_E%","總脂肪比_E%",
#                                    "水果(日)","蔬菜(日)","全穀雜糧(日)","蛋豆魚肉(日)","乳品(日)","油脂(日)",
#                                    "綠燈比_%","黃燈比_%","紅燈比_%") #15

#run corrplot plot
# corrplot(M1,
#          p.mat = M_test1$p,
#          type = "lower",
#          insig = "label_sig",
#          sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
#          tl.col = "black", tl.srt = 35, tl.cex = 1.0,
#          cl.ratio = 0.1,
#          col = M_col(200),
#          title = "[Correlation] Efficacy x Diet",
#          #c(bottom, left, top, right)
#          mar = c(0,0,1,0))


for (i in c(1:length(colnames(M1)))) {
  if (i == 1) {
    M1_value <- M1 %>% round(2) 
    M1_sign <- M_test1$p %>% stats::symnum(corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")) %>% as.data.frame.matrix()
    M1_df <- M1_value
    M1_df[,] <- NA
  }
  
  M1_df[,i] <- paste0(M1_value[,i], " (", M1_sign[,i], ")")
  
  if (i ==  length(colnames(M1))) {
    rm(list = c("M1_value", "M1_sign"))
    M1_df <- M1_df %>% as.data.frame()
    M1_df <- M1_df %>% add_column(vars = rownames(M1_df), .before = names(M1_df)[1])
    M1_df <- M1_df %>% add_column("#" = seq(1, nrow(M1_df)), .before = names(M1_df)[1])
  }
  
} 

cor_table_01 <- M1_df %>% gvisTable(options=list(frozenColumns = 2,
                                                 width="150%",height=300
                                                 ))




#[Correlation r] Efficacy x Baseline
M2 <- cor(cbind(-profile_efficacy, profile_baseline), use = "pairwise.complete.obs")
#[2Do]change row,col names into chinese
M_test2 <- cor.mtest(cbind(-profile_efficacy, profile_baseline) , conf.level = .95)
M_col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))

# colnames(M2) <- row.names(M2) <- c("∆體重(%)", "∆體脂(%)","∆肌肉(%)","∆內臟脂肪","∆腰圍", "∆BMR", #6
#                                    "∆糖化血色素","∆空服血糖","∆空腹胰島素","∆Homa_IR","∆Homa_ß","∆三酸甘油脂","∆總膽固醇","∆HDL","∆LDL", "解脂酶", #10
#                                    "BMI(T0)", "體脂率(T0)", "內臟脂肪(T0)", "BSMI(T0)", "肌肉重(T0)", "腰圍(T0)", "BMR(T0)",
#                                    "糖化血色素(T0)", "空服血糖(T0)", "空腹胰島素(T0)", "HOMA_IR(T0)", "HOMA_ß(T0)", "三酸甘油脂(T0)", "總膽固醇(T0)", #17
#                                    "HDL(T0)", "LDL(T0)", "解脂酶(T0)") #

#run corrplot plot
# corrplot(M2,
#          p.mat = M_test2$p,
#          type = "lower",
#          insig = "label_sig",
#          sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
#          tl.col = "black", tl.srt = 35, tl.cex = 1.0,
#          cl.ratio = 0.1,
#          col = M_col(200),
#          title = "[Correlation] Efficacy x Baseline",
#          #c(bottom, left, top, right)
#          mar = c(0,0,1,0))

for (i in c(1:length(colnames(M2)))) {
  if (i == 1) {
    M2_value <- M2 %>% round(2) 
    M2_sign <- M_test2$p %>% stats::symnum(corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")) %>% as.data.frame.matrix()
    M2_df <- M2_value
    M2_df[,] <- NA
  }
  
  M2_df[,i] <- paste0(M2_value[,i], " (", M2_sign[,i], ")")
  
  if (i ==  length(colnames(M2))) {
    rm(list = c("M2_value", "M2_sign"))
    M2_df <- M2_df %>% as.data.frame()
    M2_df <- M2_df %>% add_column(vars = rownames(M2_df), .before = names(M2_df)[1])
    M2_df <- M2_df %>% add_column("#" = seq(1, nrow(M2_df)), .before = names(M2_df)[1])
  }

} 
cor_table_02 <- M2_df %>% gvisTable(options=list(frozenColumns = 2,
                                                 height=300))



#[Cor: All numeric vars]

a <- stat_table_1st_ob %>% select_if(is.numeric) %>% 
  select(-c(id, class_freq, class_order, client_type)) %>% 
  select_if(~ sd(., na.rm = TRUE) %>% is.nan() %>% not() & is.na(sd(., na.rm = TRUE)) %>% not()) %>% 
  select_if(~ sum(!is.na(.)) >= 60)
 
#SELECT 770 in order to analyze
# a <- a %>% filter(!is.na(wepa50_baseline))

library(corrplot)
#[Correlation r] 
M_all <- cor(a, use = "pairwise.complete.obs") %>% round(2)
M_test_all <- cor.mtest(a , conf.level = .95)
M_col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))  


for (i in c(1:length(colnames(M_all)))) {
  if (i == 1) {
    M_all_value <- M_all %>% round(2) 
    M_all_sign <- M_test_all$p %>% stats::symnum(corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")) %>% as.data.frame.matrix()
    M_all_df <- M_all_value
    M_all_df[,] <- NA
  }
  
  M_all_df[,i] <- paste0(M_all_value[,i], " (", M_all_sign[,i], ")")
  
  if (i ==  length(colnames(M_all))) {
    rm(list = c("M_all_value", "M_all_sign"))
    M_all_df <- M_all_df %>% as.data.frame()
    M_all_df <- M_all_df %>% add_column(vars = rownames(M_all_df), .before = names(M_all_df)[1])
    M_all_df <- M_all_df %>% add_column("#" = seq(1, nrow(M_all_df)), .before = names(M_all_df)[1])
  }
  
} 

names(M_all_df) <- names(M_all_df) %>% lin_ch_en_format(format = "ch", origin = "en")
rownames(M_all_df) <- rownames(M_all_df) %>% lin_ch_en_format(format = "ch", origin = "en")

library(tidyverse)
library(DT)
cor_table_all_01 <- 
  M_all_df %>% select(-c('#', 'vars')) %>% 
  datatable(extensions = c('Buttons',"FixedColumns"),
            options = list(fixedColumns = list(leftColumns = 1),
                           dom = 'Blfrtip',
                           # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'font-size': '12px'});","}"),
                           scrollX = TRUE,
                           autoWidth = TRUE,
                           lengthMenu = list(c(10,25,50,-1),
                                             c(10,25,50,"All"))))

rm(list = c("a", "M_all", "M_test_all", "M_all_df"))


# Effi.  stratification ---------------------------------------------------

#Divide into 3 group based on ∆weight
QQ1_stat_table_1st_bad <- stat_table_1st_ob %>% filter(`∆weight%` > -3)
QQ1_stat_table_1st_bad$`∆weight%` %>%summary()
QQ1_stat_table_1st_bad$id %>% unique() %>% length()
QQ1_stat_table_1st_bad %>% summary()
QQ1_stat_table_1st_bad$gp <- "Poor"


QQ1_stat_table_1st_medium <- stat_table_1st_ob %>% filter((`∆weight%` > -10) & (`∆weight%` < -5) )
QQ1_stat_table_1st_medium$`∆weight%` %>%summary()
QQ1_stat_table_1st_medium$id %>% unique() %>% length()
QQ1_stat_table_1st_medium %>% summary()
QQ1_stat_table_1st_medium$gp <- "Medium"

QQ1_stat_table_1st_good <- stat_table_1st_ob %>% filter(`∆weight%` < -10)
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
    myplot_table_global <- data.frame(num = seq(1, length(vars_en_adj)),
                               vars_ch = lin_ch_en_format(x = vars_en_adj, format = "ch", origin = "en"))
    myplot_table_global <- lin_mapping(myplot_table_global, vars_en_adj, vars_ch, vars_table, en, ch)
    myplot_table_global <- lin_mapping(myplot_table_global, field, vars_ch, vars_table, field, ch)

    myplot_table_global <- myplot_table_global[var_vector,]
    myplot_table_global$num <- seq(1, length(myplot_table_global$num))

    myplot_table_global$block <- NA
    x0 <- c("t0","t1","delta","delta_p")
    x1 <- (myplot_table_global[["vars_en_adj"]] %>% grep("^[∆]?weight",.))
    x2 <- (myplot_table_global[["vars_en_adj"]] %>% grep("wepa50",.))
    x3 <- (myplot_table_global[["vars_en_adj"]] %>% grep("ecw_ratio",.))
    x4 <- which(myplot_table_global[["vars_en_adj"]] %in% (myplot_table_global[["vars_en_adj"]] %>% grep("left_arm_fat",., value = TRUE) %>% grep("percent",., invert = TRUE, value = TRUE)))
    x5 <- (myplot_table_global[["vars_en_adj"]] %>% grep("water_weight_left_arm",.))
    x6 <- (myplot_table_global[["vars_en_adj"]] %>% grep("hba1c",.))
    x7 <- (myplot_table_global[["vars_en_adj"]] %>% grep("tg",.))
    x8 <- (myplot_table_global[["vars_en_adj"]] %>% grep("egfr",.))
    x9 <- (myplot_table_global[["vars_en_adj"]] %>% grep("tsh",.))
    x10 <- (myplot_table_global[["vars_en_adj"]] %>% grep("wbc",.))
    x11 <- (myplot_table_global[["vars_en_adj"]] %>% grep("lipase",.))
    x12 <- (myplot_table_global[["vars_en_adj"]] %>% grep("^age",.))
    x13 <- (myplot_table_global[["vars_en_adj"]] %>% grep("oil",.))
  }
  for (j in c(1:10)) {
    if (j != 10) {
      myplot_table_global$block[(eval(parse(text = paste0("x", j))))[i]:((eval(parse(text = paste0("x", j+1))))[i]-1)] <- paste(x0[i], j, sep = "_")
      }else{
        myplot_table_global$block[x10[i]:x11[i]] <- paste(x0[i], j, sep = "_")
      }
  }
  
  if ((i == 4) & (j == 10)) {
    myplot_table_global$block[x12:x13] <- "diet"
    rm(list = paste0("x", seq(0,13)))
  }
}



myplots <- vector('list', length(var_vector))

for (i in c(var_vector)) {
  j <- match(i, var_vector)
  if (j == 1) {
    vector_pvalue <- c()
    start_time <- Sys.time()
  }
  
  a <- QQ1_stat_table_1st_for_plot %>% colnames() %>% head(i) %>% tail(1)
  a_title <- myplot_table_global[myplot_table_global$num == j, "vars_ch"]
  #observation not less than 3: count >= 3
  
  #Poor: male >= 3
  a1 <- (table(Count = is.na(QQ1_stat_table_1st_for_plot[[a]]), QQ1_stat_table_1st_for_plot[["gender"]], QQ1_stat_table_1st_for_plot[["gp"]]) %>% ftable())[2,1]
  #Medium/Good: male >= 3 任2
  a2 <- (table(Count = is.na(QQ1_stat_table_1st_for_plot[[a]]), QQ1_stat_table_1st_for_plot[["gender"]], QQ1_stat_table_1st_for_plot[["gp"]]) %>% ftable())[2,2]
  a3 <- (table(Count = is.na(QQ1_stat_table_1st_for_plot[[a]]), QQ1_stat_table_1st_for_plot[["gender"]], QQ1_stat_table_1st_for_plot[["gp"]]) %>% ftable())[2,3]
  if_run <- (a1 >= 3 & (a2 >= 3 | a3 >= 3)) 
  
  if (if_run) {
    #p.sign?
    stat.test <- 
      QQ1_stat_table_1st_for_plot %>%
      group_by(gender) %>%
      rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ ")), ref.group = "Poor") 
    stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
    
    #for customed summary table - part 1/4 [p value]
    vector_pvalue <- append(vector_pvalue, 
                            stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
    )
  }else{
    #p.sign?
    stat.test <- 
      QQ1_stat_table_1st_for_plot %>%
      rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ ")), ref.group = "Poor") 
    stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
    
    #for customed summary table - part 1/4 [p value]
    vector_pvalue <- append(vector_pvalue, 
                            stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
    )
  }

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
  
  myplots[[j]] <- plot
    
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}



#(2.)gender x Group table
table_01 <- 
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

names(summary_table) <- c(rep(c("Poor", "Medium", "Good"), 2), "顯著差異")
rownames(summary_table) <- myplot_table_global$vars_ch

table_02 <- 
  summary_table %>% 
  kbl(format = "html", caption = "<b>Statistics:</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1, "Female" = 3, "Male" = 3, " " = 1)) %>% 
  footnote(general_title = c("Significance:"), general = "\n Comparison: Good vs. Poor in female population.",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%")

rm(list = c("vars_en_adj", "a1","a2","a3"))
