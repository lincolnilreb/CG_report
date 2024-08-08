#**[FLC Program Optimization Insight]


# 01. Dataset -------------------------------------------------------------

df03_FLC_mod_df <- readRDS(file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/TMU_Cofit/pub_df.rds")
df03_FLC_mod_df <- df03_FLC_mod_df %>% filter(org == "Cofit")

a <- df01_profile %>% select(id, program_name, date_t0) %>% rename(date_start = date_t0)
df03_FLC_mod_df <- left_join(df03_FLC_mod_df, a, by = c("id", "date_start"), multiple = "first")
a <- df03_FLC_self_report %>% select(id, date_flc_T0, nutritionist_online) %>% rename(date_start = date_flc_T0)
df03_FLC_mod_df <- left_join(df03_FLC_mod_df, a, by = c("id", "date_start"), multiple = "first")



# 02. Client Profile -----------------------------------------------------------------
flc_client_profile <- df01_profile

flc_client_profile$program_name <- flc_client_profile$program_name %>% factor(levels = c("經典八週", "經典八週（202109新版）享瘦班", "進階計畫",
                                                                                   "宋醫師專班 -FLC班", "2023 FLC-2個助教", "宋醫師進階計畫"))

flc_client_profile <- flc_client_profile %>% filter(program_name %in% levels(program_name))

a <- tmp_03_day %>% 
  lin_exclude_NA_col("weight") %>% 
  group_by(client_id, date_flc_t0) %>% 
  summarise(
    weight_w0 = first(weight),
    bmi_w0 = first(bmi)
  ) %>% ungroup() %>% rename(id = client_id, date_t0 = date_flc_t0)

flc_client_profile <- left_join(ad, a, by = c("id", "date_t0"), multiple = "first")

# table(!is.na(flc_client_profile$bmi_w0)) %>% addmargins()

# flc_client_profile$program_name %>% table() %>% addmargins()
flc_client_profile$gender <- flc_client_profile$gender %>% factor(levels = c("female", "male"))
flc_client_profile$gp_age <- cut(flc_client_profile$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
flc_client_profile$gp_bmi <- cut(flc_client_profile$bmi_w0, c(0,18.5,24,27, 30, 35 ,100), c("underweight", "normal", "overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)


#gender
flc_pie_gender <-
flc_client_profile %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  gvisPieChart(options = list(title = "Gender",
                              legend = "{position:'right'}",
                              pieHole = 0.5,
                              #slices = "{1:{offset:0.1}}",
                              backgroundColor = "#f9fffb",
                              width = "600",
                              height = "400"))


#age
flc_pie_age <-
flc_client_profile %>% 
  filter(gp_age %in% levels(gp_age)) %>% 
  group_by(gp_age) %>% 
  summarise(n = n()) %>% 
  gvisPieChart(options = list(title = "Age",
                              legend = "{position:'right'}",
                              pieHole = 0.5,
                              #slices = "{1:{offset:0.1}}",
                              backgroundColor = "#f9fffb",
                              width = "600",
                              height = "400"))

gvisMerge(flc_pie_gender, flc_pie_age,
          horizontal = TRUE) %>% plot()



a <- 
  table(flc_client_profile$gender, flc_client_profile$gp_age) %>% 
  ftable() %>% as.matrix() %>% as.tibble() %>% 
  cbind(xvar = levels(flc_client_profile$gender))

# flc_bar_gender_age <- 
gvisBarChart(a , xvar = "xvar", yvar = a %>% select(-xvar) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'50%'}",
                            title = '性別x年齡分佈',
                            legend="bottom",
                            # colors = "['#d9e3f0','#ff8080']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "300")) %>% plot()



a <- 
  table(flc_client_profile$gender, flc_client_profile$program_name) %>% 
  ftable() %>% as.matrix() %>% as.tibble() %>% 
  cbind(xvar = levels(flc_client_profile$gender))


# flc_bar_gender_program <- 
gvisBarChart(a , xvar = "xvar", yvar = a %>% select(-xvar) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'50%'}",
                            title = '購買課程x性別分佈',
                            legend="right",
                            # colors = "['#d9e3f0','#ff8080']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "300",
                            chartArea = "{left: 50, width: '70%'}")) %>% plot()


a <- 
  table(flc_client_profile$gp_age, flc_client_profile$program_name) %>% 
  ftable() %>% as.matrix() %>% as.tibble() %>% 
  cbind(xvar = levels(flc_client_profile$gp_age))


# flc_bar_age_program <- 
gvisBarChart(a , xvar = "xvar", yvar = a %>% select(-xvar) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'50%'}",
                            title = '年齡x購買課程分佈',
                            legend="bottom",
                            # colors = "['#d9e3f0','#ff8080']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "300",
                            chartArea = "{width: '80%'}")) %>% plot()

a <- 
  table(flc_client_profile$program_name, flc_client_profile$gp_age) %>% 
  ftable() %>% as.matrix() %>% as.tibble() %>% 
  cbind(xvar = levels(flc_client_profile$program_name))

# flc_bar_program_age <- 
gvisBarChart(a , xvar = "xvar", yvar = a %>% select(-xvar) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'50%'}",
                            title = '課程x年齡分佈',
                            legend="bottom",
                            # colors = "['#d9e3f0','#ff8080']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "300",
                            chartArea = "{left:200, width: '80%'}")) %>% plot()






a <- 
  table(flc_client_profile$gender,flc_client_profile$gp_bmi) %>% 
  ftable() %>% as.matrix() %>% as.tibble() %>% 
  cbind(xvar = levels(flc_client_profile$gender))

# flc_bar_gender_ob <- 
gvisBarChart(a , xvar = "xvar", yvar = a %>% select(-xvar) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'50%'}",
                            title = '性別 x Obeisty',
                            legend="bottom",
                            colors = "['#2166AC','#67A9CF','#D1E5F0','#FDDBC7','#EF8A62','#B2182B']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "300",
                            chartArea = "{width: '80%'}")) %>% plot()

b <- flc_client_profile %>% filter(gender == "female")
# b <- flc_client_profile %>% filter(gender == "male")

a <- 
  table(b$gp_age,b$gp_bmi) %>% 
  ftable() %>% as.matrix() %>% as.tibble() %>% 
  cbind(xvar = levels(flc_client_profile$gp_age))

# flc_bar_age_ob <- 
gvisBarChart(a , xvar = "xvar", yvar = a %>% select(-xvar) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'50%'}",
                            title = '(女性)年齡 x Obeisty',
                            legend="bottom",
                            colors = "['#2166AC','#67A9CF','#D1E5F0','#FDDBC7','#EF8A62','#B2182B']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "300",
                            chartArea = "{width: '80%'}")) %>% plot()

# b <- flc_client_profile %>% filter(gender == "female")
b <- flc_client_profile %>% filter(gender == "male")

a <- 
  table(b$gp_age,b$gp_bmi) %>% 
  ftable() %>% as.matrix() %>% as.tibble() %>% 
  cbind(xvar = levels(flc_client_profile$gp_age))

# flc_bar_age_ob <- 
gvisBarChart(a , xvar = "xvar", yvar = a %>% select(-xvar) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'50%'}",
                            title = '(男性)年齡 x Obeisty',
                            legend="bottom",
                            colors = "['#2166AC','#67A9CF','#D1E5F0','#FDDBC7','#EF8A62','#B2182B']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "300",
                            chartArea = "{width: '80%'}")) %>% plot()








# flc_client_profile$gp_bmi <- cut(flc_client_profile$`BMI(T0)`, c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"), right = FALSE)
# 2023成人肥胖防治實證指引 - 衛生福利部國民健康署
  # 過重：24 ≦ BMI ＜ 27
  # 輕度肥胖：27 ≦ BMI ＜ 30
  # 中度肥胖：30 ≦ BMI ＜ 35
  # 重度肥胖： BMI ≧ 35
df03_FLC_mod_df$gp_bmi <- cut(df03_FLC_mod_df$`BMI(T0)`, c(0,18.5,24,27, 30, 35 ,100), c("underweight", "normal", "overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)

df03_FLC_mod_df %>%
  filter(gender == "male") %>%
  group_by(gp_bmi) %>%
  summarise(n = n()) %>%
  gvisPieChart(options = list(title = "Male",
                              legend = "{position:'right'}",
                              pieHole = 0.5,
                              #slices = "{1:{offset:0.1}}",
                              backgroundColor = "#f9fffb",
                              width = "600",
                              height = "400")) %>% plot


df03_FLC_mod_df %>%
  filter(gender == "female") %>%
  group_by(gp_bmi) %>%
  summarise(n = n()) %>%
  gvisPieChart(options = list(title = "Female",
                              legend = "{position:'right'}",
                              pieHole = 0.5,
                              #slices = "{1:{offset:0.1}}",
                              backgroundColor = "#f9fffb",
                              width = "600",
                              height = "400")) %>% plot


a <- 
  table(df03_FLC_mod_df$gender,df03_FLC_mod_df$gp_bmi) %>% 
  ftable() %>% as.matrix() %>% as.tibble() %>% 
  cbind(xvar = levels(flc_client_profile$gender))

# flc_bar_gender_ob <- 
gvisBarChart(a , xvar = "xvar", yvar = a %>% select(-xvar) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'50%'}",
                            title = '性別 x Obeisty',
                            legend="bottom",
                            colors = "['#2166AC','#67A9CF','#D1E5F0','#FDDBC7','#EF8A62','#B2182B']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "300",
                            chartArea = "{width: '80%'}")) %>% plot()

b <- df03_FLC_mod_df %>% filter(gender == "female")
# b <- df03_FLC_mod_df %>% filter(gender == "male")

a <- 
  table(b$gp_age,b$gp_bmi) %>% 
  ftable() %>% as.matrix() %>% as.tibble() %>% 
  cbind(xvar = levels(flc_client_profile$gp_age))

# flc_bar_age_ob <- 
gvisBarChart(a , xvar = "xvar", yvar = a %>% select(-xvar) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'50%'}",
                            title = '(女性)年齡 x Obeisty',
                            legend="bottom",
                            colors = "['#2166AC','#67A9CF','#D1E5F0','#FDDBC7','#EF8A62','#B2182B']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "300",
                            chartArea = "{width: '80%'}")) %>% plot()

# b <- df03_FLC_mod_df %>% filter(gender == "female")
b <- df03_FLC_mod_df %>% filter(gender == "male")

a <- 
  table(b$gp_age,b$gp_bmi) %>% 
  ftable() %>% as.matrix() %>% as.tibble() %>% 
  cbind(xvar = levels(flc_client_profile$gp_age))

# flc_bar_age_ob <- 
gvisBarChart(a , xvar = "xvar", yvar = a %>% select(-xvar) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'50%'}",
                            title = '(男性)年齡 x Obeisty',
                            legend="bottom",
                            colors = "['#2166AC','#67A9CF','#D1E5F0','#FDDBC7','#EF8A62','#B2182B']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "300",
                            chartArea = "{width: '80%'}")) %>% plot()



# 03. Effectiveness ---------------------------------------------------------------------

# refer to pub_01_R



# 04. Record Behavior Analysis --------------------------------------------


# tmp_03_day
tmp_03_day$height <- tmp_03_day$height %>% as.integer()
# no weight but having bmi
tmp_03_day[is.na(tmp_03_day$weight) & !is.na(tmp_03_day$bmi),] <- tmp_03_day[is.na(tmp_03_day$weight) & !is.na(tmp_03_day$bmi),] %>% mutate(weight = (bmi * (height/100)^2) %>% round(1))

# Cofit Program - Loss Connect Analysis

test <- tmp_03_day %>% select(-mobile)

test <- test %>% mutate(course = date_flc_t1 - date_flc_t0)
test <- test %>% filter(course == 56)

# test <- test %>% filter(date_flc_t0 >= "2020-01-01" & date_flc_t1 < today())
test <- test %>% filter(date_flc_t1 < today())

#adjust
test <- test %>% filter(program %in% c("宋醫師專班 -FLC班","經典八週","2023 FLC-2個助教","診所八週(週一開班)-宋醫師班/初日班","宋醫師進階計畫","診所進階計畫","診所八週(週四啟動)-初日班","進階計畫","經典八週（202109新版）享瘦班"))

test <- test %>% select(-c(age))

#C1. filter by program not "^診所"
test <- test[test[["program"]] %>% grepl("^診所",.) %>% not(),]
#C2. age: btd - date_t0 年齡(療程起始當天計算)
test$age <- (lubridate::ymd(test$date_flc_t0) - lubridate::ymd(test$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()

test <- test[with(test, order(client_id, date)),]

#align id list
test <- test %>% filter(client_id %in% flc_client_profile$id)

#cut week
test$weeks <- ceiling(difftime(test$date, test$date_flc_t0, units = "weeks"))
#cut days
test$days <- ceiling(difftime(test$date, test$date_flc_t0, units = "days"))


# survival plot

library(survival)
library(survminer)

#last upload day

a <- rbind(test %>%      #From D0: Never upload weight data
             filter(is.na(weight) & days == 0),
           test %>%      #From Day ??: Stop uploading weight data
             filter(!is.na(weight)) %>%
             group_by(client_id) %>%
             filter(row_number() == n())
)


a <- a %>% select(client_id, days, program, gender, age)
a$status <- ifelse(a$days < 56, 2, 1) #1: alive, 2: event

# plot
fit<- survfit(Surv(days, status) ~ gender, data = a)
surv <- 
ggsurvplot(fit, 
           pval = FALSE, conf.int = TRUE,
           risk.table = TRUE, risk.table.y.text.col = FALSE,
           surv.median.line = "hv",
           break.time.by = 7,
           fontsize = 3, 
           font.main = 12,
           # font.tickslab = 10,
           legend.title = "Gender",
           legend.labs = c("Women", "Men"),
           linetype = "strata",
           title="FLC Program Adhesion: Weight Upload",
           xlab="Days", ylab="", 
           ggtheme = theme_survminer() +
             theme(plot.title = element_text(hjust = 0.5)),
           # legend =  "none",
           # tables.theme = clean_theme()
             )

surv

left_join(test %>%
            filter(!is.na(weight)) %>% 
            group_by(days) %>% 
            summarise(
              weight_record = (n()*100/(.$client_id %>% unique() %>% length())) %>% round(2),
            ),
          test %>%
            filter(pic_count > 0) %>% 
            group_by(days) %>% 
            summarise(
              diet_record = (n()*100/(.$client_id %>% unique() %>% length())) %>% round(2)
            )
) %>% reshape2::melt(id.var = c("days"),
                     measure.vars = c("weight_record", "diet_record"),
                     variable.name = "gp",
                     value.name = "count") %>% 
  ggplot(aes(x = days, y = count, group = gp, color = factor(gp, levels = c("diet_record", "weight_record")))) +
  geom_point(size = 1) +
  geom_line(size = 1, lwd = 0.5) +
  # scale_color_manual(values = RColorBrewer::brewer.pal(4, "RdBu")) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired"), 
                     name = "Data Type", 
                     labels = c("Diet", "Weight"),
                     ) +
  scale_x_continuous(breaks = seq(0,56,7))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Days", y = "Upload Client Proportion(%)", title = "FLC Program Adhersion: Weight & Diet Record") +
  ylim(0, 100) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
  ) 



# 05. 2mo Weight loss Journal -------------------------------------------------


## 5-1 Weight ------------------------------------------------------------------


# a <- tmp_03_day[tmp_03_day$class_id %in% df03_FLC_mod_df$class_id, ]
a <- tmp_03_day %>% filter(program %in% c("經典八週", "經典八週（202109新版）享瘦班", "進階計畫",
                                          "宋醫師專班 -FLC班", "2023 FLC-2個助教", "宋醫師進階計畫"))


#age calc: btd - date_t0 年齡(療程起始當天計算)
a <- a %>% select(-c(age))
a$age <- (lubridate::ymd(a$date_flc_t0) - lubridate::ymd(a$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()
#reorder
a <- a[with(a, order(client_id, date_flc_t0, date)),]
#cut week
a$weeks <- ceiling(difftime(a$date, a$date_flc_t0, units = "weeks"))
#filter last record of the week as the observation.
a <- a %>%
  select(client_id, program, weeks, date_flc_t0, date_flc_t1, height, weight, bmi, age, gender) %>% 
  # group_by(client_id, weeks) %>% 
  group_by(client_id, date_flc_t0, weeks) %>%
  filter(row_number() == n()) %>% ungroup()


a <- a %>% filter(weeks <= 10)

# a <- a %>% na.omit(weight)
a <- a %>% lin_exclude_NA_col(c("weight"))
a <- a %>% filter(weight > 0)

#impute bmi
a$bmi <- ifelse(is.na(a$bmi), (a$weight/(a$height/100)^2) %>% round(1), a$bmi)


a <- a %>% filter(client_id %in% (a[a$weeks == 0, "client_id"] %>% pull() %>% unique()))

a <- a %>%
  group_by(client_id, date_flc_t0) %>%
  mutate(delta_weight = weight - first(weight))
a <- a %>%
  group_by(client_id, date_flc_t0) %>%
  mutate(delta_weight_p = ((weight - first(weight))*100/first(weight)) %>% round(2))

a <- a %>% rename(id = client_id)

b <- a %>% 
  group_by(id, date_flc_t0) %>% 
  summarise(
    bmi_w0 = first(bmi),
    delta_weight_p = ((last(weight) - first(weight))*100/first(weight)) %>% round(2)
  ) %>% ungroup()
b$gp <- b$delta_weight_p %>% cut(breaks = c(-Inf, -8, -4, Inf), labels = c(">8%","4~8%","<4%"))
b$gp_bmi <- cut(b$bmi_w0, c(0,18.5,24,27, 30, 35 ,100), c("underweight", "normal", "overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)

b <- b %>% select(-delta_weight_p, -bmi_w0)

a <- left_join(a, b, by = c("id", "date_flc_t0"))
rm(b)

#Table
table_cofit_001 <-
  a %>%
  filter(weeks <= 8) %>% 
  group_by(weeks) %>%
  # group_by(gp, weeks) %>%
  summarise(
    delta_weight = delta_weight %>% mean(na.rm = T) %>% round(2),
    delta_weight_p = delta_weight_p %>% mean(na.rm = T) %>% round(2),
    N = n()
  ) %>%
  rename(Week = weeks, `Weight(kg)` = delta_weight, `Weight(%)` = delta_weight_p, N = N) %>% 
  kable(format = "html", caption = "<b>FLC Effectiveness by Weeks</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c(""), general = c(rbind("", c(""))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  ) %>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)

plot_cofit_001 <- 
  a %>%
  filter(weeks <= 8) %>% 
  group_by(gp, weeks) %>%
  summarise(
    delta_weight_mean = delta_weight %>% mean(na.rm = T) %>% round(2),
    delta_weight_sd = delta_weight %>% sd(na.rm = T) %>% round(2),
    delta_weight_p_mean = delta_weight_p %>% mean(na.rm = T) %>% round(2),
    delta_weight_p_sd = delta_weight_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_weight_mean, sd = delta_weight_sd) %>% 
  ggplot(aes(x = weeks, y = var, color = gp)) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, lwd = 0.3) +
  geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(kg)", title = "Weight Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-12, 1) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
  )

plot_cofit_002 <- 
  a %>%
  filter(weeks <= 8) %>% 
  group_by(gp, weeks) %>%
  summarise(
    delta_weight_mean = delta_weight %>% mean(na.rm = T) %>% round(2),
    delta_weight_sd = delta_weight %>% sd(na.rm = T) %>% round(2),
    delta_weight_p_mean = delta_weight_p %>% mean(na.rm = T) %>% round(2),
    delta_weight_p_sd = delta_weight_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_weight_p_mean, sd = delta_weight_p_sd) %>% 
  ggplot(aes(x = weeks, y = var, color = gp)) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(%)", title = "Weight Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-12, 1) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
  )


cowplot::plot_grid(plot_cofit_001, plot_cofit_002, nrow = 2)



#by Program
a %>%
  filter(weeks <= 8) %>% 
  group_by(gp, program, weeks) %>%
  summarise(
    delta_weight_mean = delta_weight %>% mean(na.rm = T) %>% round(2),
    delta_weight_sd = delta_weight %>% sd(na.rm = T) %>% round(2),
    delta_weight_p_mean = delta_weight_p %>% mean(na.rm = T) %>% round(2),
    delta_weight_p_sd = delta_weight_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_weight_p_mean, sd = delta_weight_p_sd) %>% 
  ggplot(aes(x = weeks, y = var, color = gp)) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(%)", title = "Weight Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-12, 1) +
  facet_grid(program ~ .) +  # Facet by gender
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
  )








plot_cofit_001a <- 
  a %>%
  filter(weeks <= 8) %>% 
  group_by(gp, gender, gp_bmi, weeks) %>%
  summarise(
    delta_weight_mean = delta_weight %>% mean(na.rm = T) %>% round(2),
    delta_weight_sd = delta_weight %>% sd(na.rm = T) %>% round(2),
    delta_weight_p_mean = delta_weight_p %>% mean(na.rm = T) %>% round(2),
    delta_weight_p_sd = delta_weight_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_weight_mean, sd = delta_weight_sd) %>% 
  ggplot(aes(x = weeks, y = var, color = gp)) +
  geom_rect(aes(fill = gender),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.005, show.legend = F) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, lwd = 0.3) +
  geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(kg)", title = "Weight Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-15, 1) +
  facet_grid(gp_bmi ~ gender) +  # Facet by gender
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
    strip.background = element_rect(colour="black", fill="white", size = 1.0, linetype = "solid"),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 10),
  )

plot_cofit_002a <- 
  a %>%
  filter(weeks <= 8) %>% 
  group_by(gp, gender, gp_bmi, weeks) %>%
  summarise(
    delta_weight_mean = delta_weight %>% mean(na.rm = T) %>% round(2),
    delta_weight_sd = delta_weight %>% sd(na.rm = T) %>% round(2),
    delta_weight_p_mean = delta_weight_p %>% mean(na.rm = T) %>% round(2),
    delta_weight_p_sd = delta_weight_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_weight_p_mean, sd = delta_weight_p_sd) %>% 
  ggplot(aes(x = weeks, y = var, color = gp)) +
  geom_rect(aes(fill = gender),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.005, show.legend = F) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(%)", title = "Weight Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-12, 1) +
  facet_grid(gp_bmi ~ gender) +  # Facet by gender
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
    strip.background = element_rect(colour="black", fill="white", size = 1.0, linetype = "solid"),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 10),
  )


cowplot::plot_grid(plot_cofit_001a, plot_cofit_002a, nrow = 2)


# 二、8週課程整體分析(性別xBMI分組)
plot_cofit_003 <- 
  a %>%
  filter(weeks <= 8) %>% 
  group_by(gender, gp_bmi, weeks) %>%
  summarise(
    delta_weight_mean = delta_weight %>% mean(na.rm = T) %>% round(2),
    delta_weight_sd = delta_weight %>% sd(na.rm = T) %>% round(2),
    delta_weight_p_mean = delta_weight_p %>% mean(na.rm = T) %>% round(2),
    delta_weight_p_sd = delta_weight_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_weight_mean, sd = delta_weight_sd) %>% 
  ggplot(aes(x = weeks, y = var)) +
  geom_rect(aes(fill = gender),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.01, show.legend = F) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, lwd = 0.3) +
  # geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(kg)", title = "Weight Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-12, 2) +
  facet_grid(gender ~ gp_bmi) +  # Facet by gender
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
    strip.background = element_rect(colour="black", fill="white", size = 1.0, linetype = "solid"),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 10),
  )


plot_cofit_004 <- 
  a %>%
  filter(weeks <= 8) %>% 
  group_by(gender, gp_bmi, weeks) %>%
  summarise(
    delta_weight_mean = delta_weight %>% mean(na.rm = T) %>% round(2),
    delta_weight_sd = delta_weight %>% sd(na.rm = T) %>% round(2),
    delta_weight_p_mean = delta_weight_p %>% mean(na.rm = T) %>% round(2),
    delta_weight_p_sd = delta_weight_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_weight_p_mean, sd = delta_weight_p_sd) %>% 
  ggplot(aes(x = weeks, y = var)) +
  geom_rect(aes(fill = gender),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.01, show.legend = F) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  # geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(%)", title = "Weight Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-12, 2) +
  facet_grid(gender ~ gp_bmi) +  # Facet by gender
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
    strip.background = element_rect(colour="black", fill="white", size = 1.0, linetype = "solid"),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 10),
  )


cowplot::plot_grid(plot_cofit_003, plot_cofit_004, nrow = 2)


## 5-2 fat (not ok: %) ------------------------------------------------------------------


# a <- tmp_03_day[tmp_03_day$class_id %in% df03_FLC_mod_df$class_id, ]
a <- tmp_03_day %>% filter(program %in% c("宋醫師專班 -FLC班","經典八週","2023 FLC-2個助教","診所八週(週一開班)-宋醫師班/初日班","宋醫師進階計畫","診所進階計畫","診所八週(週四啟動)-初日班","進階計畫","經典八週（202109新版）享瘦班"))


#age calc: btd - date_t0 年齡(療程起始當天計算)
a <- a %>% select(-c(age))
a$age <- (lubridate::ymd(a$date_flc_t0) - lubridate::ymd(a$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()
#reorder
a <- a[with(a, order(client_id, date_flc_t0, date)),]
#cut week
a$weeks <- ceiling(difftime(a$date, a$date_flc_t0, units = "weeks"))
#filter last record of the week as the observation.
a <- a %>%
  select(client_id, weeks, date_flc_t0, date_flc_t1, height, weight, bmi, body_fat_mass, age, gender) %>% 
  # group_by(client_id, weeks) %>% 
  group_by(client_id, date_flc_t0, weeks) %>%
  filter(row_number() == n()) %>% ungroup()


a <- a %>% filter(weeks <= 10)

a <- a %>% rename(fat = body_fat_mass)

#Outliers
a$fat <- ifelse((a$fat < quantile(a$fat, 0.03, na.rm = TRUE)) | (a$fat > quantile(a$fat, 0.999, na.rm = TRUE)), NA, a$fat)

a <- a %>%
  group_by(client_id) %>%
  mutate(fat = ifelse(!is.na(abs(fat - lead(fat))) & (abs(fat - lead(fat)) > 10), lead(fat), fat)) %>%
  ungroup() 



# a <- a %>% na.omit(weight)
a <- a %>% lin_exclude_NA_col(c("fat"))
a <- a %>% filter(fat > 0)

#impute bmi
a$bmi <- ifelse(is.na(a$bmi), (a$weight/(a$height/100)^2) %>% round(1), a$bmi)


a <- a %>% filter(client_id %in% (a[a$weeks == 0, "client_id"] %>% pull() %>% unique()))

a <- a %>%
  group_by(client_id, date_flc_t0) %>%
  mutate(delta_fat = fat - first(fat))
a <- a %>%
  group_by(client_id, date_flc_t0) %>%
  mutate(delta_fat_p = ((fat - first(fat))*100/first(fat)) %>% round(2))

a <- a %>% rename(id = client_id)

b <- a %>% 
  group_by(id, date_flc_t0) %>% 
  summarise(
    bmi_w0 = first(bmi),
    delta_fat_p = ((last(fat) - first(fat))*100/first(fat)) %>% round(2)
  ) %>% ungroup()
b$gp <- b$delta_fat_p %>% cut(breaks = c(-Inf, -8, -4, Inf), labels = c(">8%","4~8%","<4%"))
b$gp_bmi <- cut(b$bmi_w0, c(0,18.5,24,27, 30, 35 ,100), c("underweight", "normal", "overfat", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)

b <- b %>% select(-delta_fat_p, -bmi_w0)

a <- left_join(a, b, by = c("id", "date_flc_t0"))
rm(b)

#Outliers
a$delta_fat <- ifelse((a$delta_fat < quantile(a$delta_fat, 0.02, na.rm = TRUE)) | (a$delta_fat > quantile(a$delta_fat, 0.97, na.rm = TRUE)), NA, a$delta_fat)
a$delta_fat_p <- ifelse((a$delta_fat_p < quantile(a$delta_fat_p, 0.02, na.rm = TRUE)) | (a$delta_fat_p > quantile(a$delta_fat_p, 0.97, na.rm = TRUE)), NA, a$delta_fat_p)


#Table
table_cofit_001 <-
  a %>%
  filter(weeks <= 8) %>% 
  group_by(weeks) %>%
  # group_by(gp, weeks) %>%
  summarise(
    delta_fat = delta_fat %>% mean(na.rm = T) %>% round(2),
    delta_fat_p = delta_fat_p %>% mean(na.rm = T) %>% round(2),
    N = n()
  ) %>%
  rename(Week = weeks, `fat(kg)` = delta_fat, `fat(%)` = delta_fat_p, N = N) %>% 
  kable(format = "html", caption = "<b>FLC Effectiveness by Weeks</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c(""), general = c(rbind("", c(""))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  ) %>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)

plot_cofit_001 <- 
  a %>%
  filter(weeks <= 8) %>% 
  group_by(weeks) %>%
  # group_by(gp, weeks) %>%
  summarise(
    delta_fat_mean = delta_fat %>% mean(na.rm = T) %>% round(2),
    delta_fat_sd = delta_fat %>% sd(na.rm = T) %>% round(2),
    delta_fat_p_mean = delta_fat_p %>% mean(na.rm = T) %>% round(2),
    delta_fat_p_sd = delta_fat_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_fat_mean, sd = delta_fat_sd) %>% 
  ggplot(aes(x = weeks, y = var)) +
  geom_line(alpha = 0.3) +
  # geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, lwd = 0.3) +
  # geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(kg)", title = "Fat Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-12, 2) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
  )

# plot_cofit_002 <-
#   a %>%
#   filter(weeks <= 8) %>%
#   group_by(weeks) %>%
#   # group_by(gp, weeks) %>%
#   summarise(
#     delta_fat_mean = delta_fat %>% mean(na.rm = T) %>% round(2),
#     delta_fat_sd = delta_fat %>% sd(na.rm = T) %>% round(2),
#     delta_fat_p_mean = delta_fat_p %>% mean(na.rm = T) %>% round(2),
#     delta_fat_p_sd = delta_fat_p %>% sd(na.rm = T) %>% round(2),
#     N = n()
#   ) %>%
#   rename(var = delta_fat_p_mean, sd = delta_fat_p_sd) %>%
#   ggplot(aes(x = weeks, y = var)) +
#   geom_line(alpha = 0.3) +
#   geom_point(size = 1, alpha = 1.0) +
#   geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
#   # geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
#   labs(x = "Weeks", y = "Difference(%)", title = "fat Loss Trend", color = "") +
#   # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
#   ylim(-12, 2) +
#   theme_bw() +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
#     axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
#     axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
#   )


cowplot::plot_grid(plot_cofit_001, plot_cofit_002, nrow = 2)



plot_cofit_001a <- 
  a %>%
  filter(weeks <= 8) %>% 
  group_by(gp, gender, gp_bmi, weeks) %>%
  summarise(
    delta_fat_mean = delta_fat %>% mean(na.rm = T) %>% round(2),
    delta_fat_sd = delta_fat %>% sd(na.rm = T) %>% round(2),
    delta_fat_p_mean = delta_fat_p %>% mean(na.rm = T) %>% round(2),
    delta_fat_p_sd = delta_fat_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_fat_mean, sd = delta_fat_sd) %>% 
  ggplot(aes(x = weeks, y = var, color = gp)) +
  geom_rect(aes(fill = gender),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.005, show.legend = F) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, lwd = 0.3) +
  geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(kg)", title = "fat Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-15, 1) +
  facet_grid(gp_bmi ~ gender) +  # Facet by gender
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
    strip.background = element_rect(colour="black", fill="white", size = 1.0, linetype = "solid"),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 10),
  )

plot_cofit_002a <- 
  a %>%
  filter(weeks <= 8) %>% 
  group_by(gp, gender, gp_bmi, weeks) %>%
  summarise(
    delta_fat_mean = delta_fat %>% mean(na.rm = T) %>% round(2),
    delta_fat_sd = delta_fat %>% sd(na.rm = T) %>% round(2),
    delta_fat_p_mean = delta_fat_p %>% mean(na.rm = T) %>% round(2),
    delta_fat_p_sd = delta_fat_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_fat_p_mean, sd = delta_fat_p_sd) %>% 
  ggplot(aes(x = weeks, y = var, color = gp)) +
  geom_rect(aes(fill = gender),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.005, show.legend = F) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(%)", title = "fat Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-12, 1) +
  facet_grid(gp_bmi ~ gender) +  # Facet by gender
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
    strip.background = element_rect(colour="black", fill="white", size = 1.0, linetype = "solid"),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 10),
  )


cowplot::plot_grid(plot_cofit_001a, plot_cofit_002a, nrow = 2)


# 二、8週課程整體分析(性別xBMI分組)
plot_cofit_003 <- 
  a %>%
  filter(weeks <= 8) %>% 
  filter(gp_bmi %in% levels(gp_bmi)) %>% 
  group_by(gender, gp_bmi, weeks) %>%
  summarise(
    delta_fat_mean = delta_fat %>% mean(na.rm = T) %>% round(2),
    delta_fat_sd = delta_fat %>% sd(na.rm = T) %>% round(2),
    delta_fat_p_mean = delta_fat_p %>% mean(na.rm = T) %>% round(2),
    delta_fat_p_sd = delta_fat_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_fat_mean, sd = delta_fat_sd) %>% 
  ggplot(aes(x = weeks, y = var)) +
  geom_rect(aes(fill = gender),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.01, show.legend = F) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, lwd = 0.3) +
  # geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(kg)", title = "Fat Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-12, 2) +
  facet_grid(gender ~ gp_bmi) +  # Facet by gender
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
    strip.background = element_rect(colour="black", fill="white", size = 1.0, linetype = "solid"),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 10),
  )


plot_cofit_004 <- 
  a %>%
  filter(weeks <= 8) %>% 
  filter(gp_bmi %in% levels(gp_bmi)) %>% 
  group_by(gender, gp_bmi, weeks) %>%
  summarise(
    delta_fat_mean = delta_fat %>% mean(na.rm = T) %>% round(2),
    delta_fat_sd = delta_fat %>% sd(na.rm = T) %>% round(2),
    delta_fat_p_mean = delta_fat_p %>% mean(na.rm = T) %>% round(2),
    delta_fat_p_sd = delta_fat_p %>% sd(na.rm = T) %>% round(2),
    N = n()
  ) %>% 
  rename(var = delta_fat_p_mean, sd = delta_fat_p_sd) %>% 
  ggplot(aes(x = weeks, y = var)) +
  geom_rect(aes(fill = gender),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf, alpha = 0.01, show.legend = F) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 1.0) +
  geom_errorbar(aes(ymin = var - 1.0*sd, ymax = var + 1.0*sd), width = .1) +
  # geom_text(data = . %>% filter(var != 0), aes(label = paste0(var)), size = 3, nudge_x = 0.3, nudge_y = -0.6) +
  labs(x = "Weeks", y = "Difference(%)", title = "Fat Loss Trend", color = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  ylim(-12, 2) +
  facet_grid(gender ~ gp_bmi) +  # Facet by gender
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.x = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 10),
    strip.background = element_rect(colour="black", fill="white", size = 1.0, linetype = "solid"),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 10),
  )


cowplot::plot_grid(plot_cofit_003, plot_cofit_004, nrow = 2)



# 2Do ---------------------------------------------------------------------

# Q. Determine the types of clients purchasing various Cofit programs.(Await SQL raw data from Lei.)


# nutritionist_online
library(modelr)
mpc<-function(model,data){
  MSE<-modelr::mse(model=model,data=data)
  RMSE<-modelr::rmse(model=model,data=data)
  Rsquare<-modelr::rsquare(model=model,data=data)
  MAE<-modelr::mae(model=model,data=data)
  MAPE<-modelr::mape(model=model,data=data)
  RASE<-modelr::rsae(model=model,data=data)
  AIC=AIC(object=model)
  BIC=AIC(object=model)
  return(data.frame(MSE=MSE,RMSE=RMSE,Rsquare=Rsquare, MAE=MAE,MAPE=MAPE,RASE=RASE,
                    AIC=AIC,BIC=BIC))
}

dataset <- a %>% 
  filter(!is.na(delta_weight_p))
# issue: nutritionist_online(full name), diet_compliance
mod.lm<-lm(delta_weight_p ~ I(age^1) + I(bmi^1) + gender + nutritionist_online + I(diet_compliance^2), data = dataset)
# algo
# PR
# KNN 全名 K Nearest Neighbor https://openhome.cc/Gossip/DCHardWay/KNN.html
