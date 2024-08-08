# DM/OB
# 2mo: 參加1次 (endpoint - baseline < 3mo)
# 4mo: 連續參加2次 (endpoint - baseline < 5mo)
# 6mo: 連續參加3次 (endpoint - baseline < 7mo)

# 慢性病：Obesity, MetS, DM, IR, HTN, HLN
# Baseline(same in 3 groups)
# Endpoint(varies in 3 groups)


# Steps
# 1. Acquire ID list
# 2. Extract Baseline data
# 3. Extract Endpoint data
# 4. Establish tbl(col: id, client_type, [2mo]Obesity, MetS, DM, IR, HTN, HLN, [4mo]Obesity, MetS, DM, IR, HTN, HLN, [6mo]Obesity, MetS, DM, IR, HTN, HLN)
# 5. gvisBarChart





# ID:參加at least1次
x <- df01_profile %>% filter(!(org_name %in% c("cofit", "bk2o_backup", "fitness_factory"))) %>% filter(client_type %in% c(1,2)) %>% distinct(id, .keep_all = TRUE)

# table(x$org_name)
improvement_tbl <- 
data.frame(id = x$id,
           client_type = x$client_type,
           gender = x$gender,
           
           `Obesity_0mo` = "",
           `MetS_0mo` = "",
           `DM_0mo` = "",
           `IR_0mo` = "",
           `HTN_0mo` = "",
           `HLP_0mo` = "",
   
           `Obesity_2mo` = "",
           `MetS_2mo` = "",
           `DM_2mo` = "",
           `IR_2mo` = "",
           `HTN_2mo` = "",
           `HLP_2mo` = "",
           
           `Obesity_4mo` = "",
           `MetS_4mo` = "",
           `DM_4mo` = "",
           `IR_4mo` = "",
           `HTN_4mo` = "",
           `HLP_4mo` = "",
           
           `Obesity_6mo` = "",
           `MetS_6mo` = "",
           `DM_6mo` = "",
           `IR_6mo` = "",
           `HTN_6mo` = "",
           `HLP_6mo` = ""
           )
 
# 0mo ---------------------------------------------------------------------


#inbody
x1 <- left_join(df02_inbody %>% filter(id %in% improvement_tbl$id), 
                # 2mo: 參加1次
                df01_profile %>% filter(id %in% improvement_tbl$id) %>% filter(class_order == 1),
                multiple = "all", by = "id")
#[issue]部分人有inbody data 沒有01.profile對應「療程期間」
x1 <- x1 %>% filter((date_inbody > date_t0 -14) & (date_inbody < date_t1 +14))

x1 <- x1[with(x1, order(date_inbody)),]

x_source <- 
x1 %>%
  group_by(id) %>%
  summarise(
    bmi = first(bmi, na_rm = TRUE),
    wc = first(wc, na_rm = TRUE),
    sbp = first(sbp, na_rm = TRUE),
    dbp = first(dbp, na_rm = TRUE)
  ) %>% ungroup()

improvement_tbl <- merge(improvement_tbl, x_source[, c("id", "bmi", "wc", "sbp", "dbp")], by = "id", all.x = TRUE)


#blood
x1 <- left_join(df05_biochem %>% filter(id %in% improvement_tbl$id), 
                # 2mo: 參加1次
                df01_profile %>% filter(id %in% improvement_tbl$id) %>% filter(class_order == 1),
                multiple = "all", by = "id")
x1 <- x1 %>% filter((date_blood > date_t0 -15) & (date_blood < date_t1 + 15))

x1 <- x1[with(x1, order(date_blood)),]

x_source <- 
  x1 %>%
  group_by(id) %>%
  summarise(
    hba1c = first(hba1c, na_rm = TRUE),
    glucose_ac = first(glucose_ac, na_rm = TRUE),
    glucose_pc_1hr = first(glucose_pc_1hr, na_rm = TRUE),
    glucose_pc_2hr = first(glucose_pc_2hr, na_rm = TRUE),
    insulin = first(insulin, na_rm = TRUE),
    insulin_pc_1hr = first(insulin_pc_1hr, na_rm = TRUE),
    insulin_pc_2hr = first(insulin_pc_2hr, na_rm = TRUE),
    tg = first(tg, na_rm = TRUE),
    tc = first(tc, na_rm = TRUE),
    ldl = first(ldl, na_rm = TRUE),
    hdl = first(hdl, na_rm = TRUE)
  ) %>% ungroup()

improvement_tbl <- merge(improvement_tbl, x_source[, c("id", "hba1c", "glucose_ac", "glucose_pc_1hr", "glucose_pc_2hr", "insulin", "insulin_pc_1hr", "insulin_pc_2hr", "tg", "tc", "ldl", "hdl")], by = "id", all.x = TRUE)

#preprocess
improvement_tbl <- improvement_tbl %>% as.data.table()

#Obesity
# improvement_tbl$gp_bmi <- improvement_tbl$bmi %>% cut(c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))
improvement_tbl$gp <- improvement_tbl$bmi %>% cut(c(0,18.5,24,27, 30, 35 ,100), c("underweight", "normal", "overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)
improvement_tbl$Obesity_0mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#MetS
improvement_tbl <- lin_diagnosis_MetaX(improvement_tbl, c("gender","wc","sbp","dbp","glucose_ac","tg","hdl"))
improvement_tbl$MetS_0mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#DM
improvement_tbl <- lin_diagnosis_DM_x_OGTT(improvement_tbl, c("hba1c","glucose_ac","glucose_pc_1hr","glucose_pc_2hr"))
improvement_tbl$DM_0mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#IR
improvement_tbl <- improvement_tbl %>% lin_insulin_rsp_pattern(improvement_tbl %>% names() %>% grep("^insulin", ., value = TRUE), pattern = 2)
improvement_tbl$IR_0mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#HTN
improvement_tbl <- improvement_tbl %>% lin_diagnosis_HTN(c("sbp", "dbp"))
improvement_tbl$HTN_0mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#HLP
improvement_tbl <- improvement_tbl %>% lin_diagnosis_HLP(c("gender","tg","tc","hdl","ldl"))
improvement_tbl$HLP_0mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

improvement_tbl <- improvement_tbl %>% select(-c("bmi","wc","sbp","dbp","hba1c","glucose_ac","glucose_pc_1hr","glucose_pc_2hr","insulin","insulin_pc_1hr","insulin_pc_2hr","tg","tc","ldl","hdl"))

# 2mo ---------------------------------------------------------------------


#inbody
x1 <- left_join(df02_inbody %>% filter(id %in% improvement_tbl$id), 
                # 2mo: 參加1次
                df01_profile %>% filter(id %in% improvement_tbl$id) %>% filter(class_order == 1),
                multiple = "all", by = "id")
#[issue]部分人有inbody data 沒有01.profile對應「療程期間」
x1 <- x1 %>% filter((date_inbody > date_t0 -14) & (date_inbody < date_t1 +14))

x1 <- x1[with(x1, order(date_inbody)),]

x_source <- 
  x1 %>%
  group_by(id) %>%
  summarise(
    bmi = last(bmi, na_rm = TRUE),
    wc = last(wc, na_rm = TRUE),
    sbp = last(sbp, na_rm = TRUE),
    dbp = last(dbp, na_rm = TRUE)
  ) %>% ungroup()

improvement_tbl <- merge(improvement_tbl, x_source[, c("id", "bmi", "wc", "sbp", "dbp")], by = "id", all.x = TRUE)


#blood
x1 <- left_join(df05_biochem %>% filter(id %in% improvement_tbl$id), 
                # 2mo: 參加1次
                df01_profile %>% filter(id %in% improvement_tbl$id) %>% filter(class_order == 1),
                multiple = "all", by = "id")
x1 <- x1 %>% filter((date_blood > date_t0 -15) & (date_blood < date_t1 + 15))

x1 <- x1[with(x1, order(date_blood)),]

x_source <- 
  x1 %>%
  group_by(id) %>%
  summarise(
    hba1c = last(hba1c, na_rm = TRUE),
    glucose_ac = last(glucose_ac, na_rm = TRUE),
    glucose_pc_1hr = nth(glucose_pc_1hr, 2, na_rm = TRUE),
    glucose_pc_2hr = nth(glucose_pc_2hr, 2, na_rm = TRUE),
    insulin = last(insulin, na_rm = TRUE),
    insulin_pc_1hr = nth(insulin_pc_1hr, 2, na_rm = TRUE),
    insulin_pc_2hr = nth(insulin_pc_2hr, 2, na_rm = TRUE),
    tg = last(tg, na_rm = TRUE),
    tc = last(tc, na_rm = TRUE),
    ldl = last(ldl, na_rm = TRUE),
    hdl = last(hdl, na_rm = TRUE)
  ) %>% ungroup()

improvement_tbl <- merge(improvement_tbl, x_source[, c("id", "hba1c", "glucose_ac", "glucose_pc_1hr", "glucose_pc_2hr", "insulin", "insulin_pc_1hr", "insulin_pc_2hr", "tg", "tc", "ldl", "hdl")], by = "id", all.x = TRUE)

#preprocess
improvement_tbl <- improvement_tbl %>% as.data.table()

#Obesity
# improvement_tbl$gp_bmi <- improvement_tbl$bmi %>% cut(c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))
improvement_tbl$gp <- improvement_tbl$bmi %>% cut(c(0,18.5,24,27, 30, 35 ,100), c("underweight", "normal", "overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)
improvement_tbl$Obesity_2mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#MetS
improvement_tbl <- lin_diagnosis_MetaX(improvement_tbl, c("gender","wc","sbp","dbp","glucose_ac","tg","hdl"))
improvement_tbl$MetS_2mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#DM
improvement_tbl <- lin_diagnosis_DM_x_OGTT(improvement_tbl, c("hba1c","glucose_ac","glucose_pc_1hr","glucose_pc_2hr"))
improvement_tbl$DM_2mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#IR
improvement_tbl <- improvement_tbl %>% lin_insulin_rsp_pattern(improvement_tbl %>% names() %>% grep("^insulin", ., value = TRUE), pattern = 2)
improvement_tbl$IR_2mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#HTN
improvement_tbl <- improvement_tbl %>% lin_diagnosis_HTN(c("sbp", "dbp"))
improvement_tbl$HTN_2mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#HLP
improvement_tbl <- improvement_tbl %>% lin_diagnosis_HLP(c("gender","tg","tc","hdl","ldl"))
improvement_tbl$HLP_2mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]


improvement_tbl <- improvement_tbl %>% select(-c("bmi","wc","sbp","dbp","hba1c","glucose_ac","glucose_pc_1hr","glucose_pc_2hr","insulin","insulin_pc_1hr","insulin_pc_2hr","tg","tc","ldl","hdl"))




# 4mo ---------------------------------------------------------------------

x1 <- left_join(df02_inbody %>% filter(id %in% improvement_tbl$id), 
                # 4mo: 參加2次
                df01_profile %>% filter(id %in% improvement_tbl$id) %>% filter(class_order == 2),
                multiple = "all", by = "id")
#[issue]部分人有inbody data 沒有01.profile對應「療程期間」
x1 <- x1 %>% filter((date_inbody > date_t0 -14) & (date_inbody < date_t1 +14))

x1 <- x1[with(x1, order(date_inbody)),]

x_source <- 
  x1 %>%
  group_by(id) %>%
  summarise(
    bmi = last(bmi, na_rm = TRUE),
    wc = last(wc, na_rm = TRUE),
    sbp = last(sbp, na_rm = TRUE),
    dbp = last(dbp, na_rm = TRUE)
  ) %>% ungroup()

improvement_tbl <- merge(improvement_tbl, x_source[, c("id", "bmi", "wc", "sbp", "dbp")], by = "id", all.x = TRUE)


#blood
x1 <- left_join(df05_biochem %>% filter(id %in% improvement_tbl$id), 
                # 4mo: 參加2次
                df01_profile %>% filter(id %in% improvement_tbl$id) %>% filter(class_order == 2),
                multiple = "all", by = "id")
x1 <- x1 %>% filter((date_blood > date_t0 -15) & (date_blood < date_t1 + 15))

x1 <- x1[with(x1, order(date_blood)),]

x_source <- 
  x1 %>%
  group_by(id) %>%
  summarise(
    hba1c = last(hba1c, na_rm = TRUE),
    glucose_ac = last(glucose_ac, na_rm = TRUE),
    glucose_pc_1hr = last(glucose_pc_1hr, na_rm = TRUE),
    glucose_pc_2hr = last(glucose_pc_2hr, na_rm = TRUE),
    insulin = last(insulin, na_rm = TRUE),
    insulin_pc_1hr = last(insulin_pc_1hr, na_rm = TRUE),
    insulin_pc_2hr = last(insulin_pc_2hr, na_rm = TRUE),
    tg = last(tg, na_rm = TRUE),
    tc = last(tc, na_rm = TRUE),
    ldl = last(ldl, na_rm = TRUE),
    hdl = last(hdl, na_rm = TRUE)
  ) %>% ungroup()

improvement_tbl <- merge(improvement_tbl, x_source[, c("id", "hba1c", "glucose_ac", "glucose_pc_1hr", "glucose_pc_2hr", "insulin", "insulin_pc_1hr", "insulin_pc_2hr", "tg", "tc", "ldl", "hdl")], by = "id", all.x = TRUE)

#preprocess
improvement_tbl <- improvement_tbl %>% as.data.table()

#Obesity
# improvement_tbl$gp_bmi <- improvement_tbl$bmi %>% cut(c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))
improvement_tbl$gp <- improvement_tbl$bmi %>% cut(c(0,18.5,24,27, 30, 35 ,100), c("underweight", "normal", "overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)
improvement_tbl$Obesity_4mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#MetS
improvement_tbl <- lin_diagnosis_MetaX(improvement_tbl, c("gender","wc","sbp","dbp","glucose_ac","tg","hdl"))
improvement_tbl$MetS_4mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#DM
improvement_tbl <- lin_diagnosis_DM_x_OGTT(improvement_tbl, c("hba1c","glucose_ac","glucose_pc_1hr","glucose_pc_2hr"))
improvement_tbl$DM_4mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#IR
improvement_tbl <- improvement_tbl %>% lin_insulin_rsp_pattern(improvement_tbl %>% names() %>% grep("^insulin", ., value = TRUE), pattern = 2)
improvement_tbl$IR_4mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#HTN
improvement_tbl <- improvement_tbl %>% lin_diagnosis_HTN(c("sbp", "dbp"))
improvement_tbl$HTN_4mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#HLP
improvement_tbl <- improvement_tbl %>% lin_diagnosis_HLP(c("gender","tg","tc","hdl","ldl"))
improvement_tbl$HLP_4mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]


improvement_tbl <- improvement_tbl %>% select(-c("bmi","wc","sbp","dbp","hba1c","glucose_ac","glucose_pc_1hr","glucose_pc_2hr","insulin","insulin_pc_1hr","insulin_pc_2hr","tg","tc","ldl","hdl"))





# 6mo ---------------------------------------------------------------------


x1 <- left_join(df02_inbody %>% filter(id %in% improvement_tbl$id), 
                # 6mo: 參加3次
                df01_profile %>% filter(id %in% improvement_tbl$id) %>% filter(class_order == 3),
                multiple = "all", by = "id")
#[issue]部分人有inbody data 沒有01.profile對應「療程期間」
x1 <- x1 %>% filter((date_inbody > date_t0 -14) & (date_inbody < date_t1 +14))

x1 <- x1[with(x1, order(date_inbody)),]

x_source <- 
  x1 %>%
  group_by(id) %>%
  summarise(
    bmi = last(bmi, na_rm = TRUE),
    wc = last(wc, na_rm = TRUE),
    sbp = last(sbp, na_rm = TRUE),
    dbp = last(dbp, na_rm = TRUE)
  ) %>% ungroup()

improvement_tbl <- merge(improvement_tbl, x_source[, c("id", "bmi", "wc", "sbp", "dbp")], by = "id", all.x = TRUE)


#blood
x1 <- left_join(df05_biochem %>% filter(id %in% improvement_tbl$id), 
                # 6mo: 參加2次
                df01_profile %>% filter(id %in% improvement_tbl$id) %>% filter(class_order == 3),
                multiple = "all", by = "id")
x1 <- x1 %>% filter((date_blood > date_t0 -15) & (date_blood < date_t1 + 15))

x1 <- x1[with(x1, order(date_blood)),]

x_source <- 
  x1 %>%
  group_by(id) %>%
  summarise(
    hba1c = last(hba1c, na_rm = TRUE),
    glucose_ac = last(glucose_ac, na_rm = TRUE),
    glucose_pc_1hr = last(glucose_pc_1hr, na_rm = TRUE),
    glucose_pc_2hr = last(glucose_pc_2hr, na_rm = TRUE),
    insulin = last(insulin, na_rm = TRUE),
    insulin_pc_1hr = last(insulin_pc_1hr, na_rm = TRUE),
    insulin_pc_2hr = last(insulin_pc_2hr, na_rm = TRUE),
    tg = last(tg, na_rm = TRUE),
    tc = last(tc, na_rm = TRUE),
    ldl = last(ldl, na_rm = TRUE),
    hdl = last(hdl, na_rm = TRUE)
  ) %>% ungroup()

improvement_tbl <- merge(improvement_tbl, x_source[, c("id", "hba1c", "glucose_ac", "glucose_pc_1hr", "glucose_pc_2hr", "insulin", "insulin_pc_1hr", "insulin_pc_2hr", "tg", "tc", "ldl", "hdl")], by = "id", all.x = TRUE)

#preprocess
improvement_tbl <- improvement_tbl %>% as.data.table()

#Obesity
# improvement_tbl$gp_bmi <- improvement_tbl$bmi %>% cut(c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))
improvement_tbl$gp <- improvement_tbl$bmi %>% cut(c(0,18.5,24,27, 30, 35 ,100), c("underweight", "normal", "overweight", "Mild obesity", "Moderate obesity", "Morbid obesity"), right = FALSE)
improvement_tbl$Obesity_6mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#MetS
improvement_tbl <- lin_diagnosis_MetaX(improvement_tbl, c("gender","wc","sbp","dbp","glucose_ac","tg","hdl"))
improvement_tbl$MetS_6mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#DM
improvement_tbl <- lin_diagnosis_DM_x_OGTT(improvement_tbl, c("hba1c","glucose_ac","glucose_pc_1hr","glucose_pc_2hr"))
improvement_tbl$DM_6mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#IR
improvement_tbl <- improvement_tbl %>% lin_insulin_rsp_pattern(improvement_tbl %>% names() %>% grep("^insulin", ., value = TRUE), pattern = 2)
improvement_tbl$IR_6mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#HTN
improvement_tbl <- improvement_tbl %>% lin_diagnosis_HTN(c("sbp", "dbp"))
improvement_tbl$HTN_6mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]

#HLP
improvement_tbl <- improvement_tbl %>% lin_diagnosis_HLP(c("gender","tg","tc","hdl","ldl"))
improvement_tbl$HLP_6mo <- improvement_tbl[,ncol(improvement_tbl), with = FALSE]
improvement_tbl <- improvement_tbl[,-ncol(improvement_tbl), with = FALSE]


improvement_tbl <- improvement_tbl %>% select(-c("bmi","wc","sbp","dbp","hba1c","glucose_ac","glucose_pc_1hr","glucose_pc_2hr","insulin","insulin_pc_1hr","insulin_pc_2hr","tg","tc","ldl","hdl"))



# Visualization -----------------------------------------------------------

# Preprocess 連續6mo?
x1 <- left_join(df01_profile %>% filter(id %in% improvement_tbl$id) %>% filter(class_order == 1) %>% select(id, date_t0),
                df01_profile %>% filter(id %in% improvement_tbl$id) %>% filter(class_order == 2) %>% select(id, date_t1),
                multiple = "all", by = "id") %>% dplyr::rename(date_t2 = date_t1)
x1 <- left_join(x1,
                df01_profile %>% filter(id %in% improvement_tbl$id) %>% filter(class_order == 3) %>% select(id, date_t1),
                multiple = "all", by = "id") %>% dplyr::rename(date_t3 = date_t1)
x1 <- x1 %>% mutate(continued_2t = as.numeric((x1$date_t2 - x1$date_t0) < (5 * 31)))
x1 <- x1 %>% mutate(continued_3t = as.numeric((x1$date_t3 - x1$date_t0) < (7 * 31)))
# table(x1$continued_2t)
# table(x1$continued_3t)

improvement_tbl <- merge(improvement_tbl, x1[, c("id", "continued_2t", "continued_3t")], by = "id", all.x = TRUE)

## OB Patients ----------------------------------------------------------------------

### ob ----------------------------------------------------------------------


#1:DM 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 2)


#Baseline vs. 2/4/6mo
# table(improve_data$Obesity_0mo, improve_data$Obesity_2mo)

a <- 
cbind(
  baseline = improve_data$Obesity_0mo %>% levels(),
  #var_2/4/6mo
  table(improve_data$Obesity_0mo, improve_data$Obesity_2mo) %>% 
    ftable() %>% as.matrix() %>% as.tibble()
)


improve_data <- improvement_tbl %>% filter(client_type == 2) %>% filter(continued_2t == 1)

a1 <- 
  cbind(
    baseline = improve_data$Obesity_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$Obesity_0mo, improve_data$Obesity_4mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)


improve_data <- improvement_tbl %>% filter(client_type == 2) %>% filter(continued_3t == 1)

a1 <- 
  cbind(
    baseline = improve_data$Obesity_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$Obesity_0mo, improve_data$Obesity_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)

a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- 
a %>%
  group_by(baseline) %>%
  mutate(baseline = ifelse(row_number() == 3, "6mo", baseline)) %>% 
  mutate(baseline = ifelse(baseline != "6mo" & row_number() == 2, "4mo", baseline))

a <- a %>% as.data.frame()

# - underweight
a <- a[-1:-3,]

imp_ob_obeisty <- 
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '2/4/6mo減重成效-Obesity',
                            legend = "{position:'right'}",
                            colors = "['#2166AC','#67A9CF','#D1E5F0','#FDDBC7','#EF8A62','#B2182B']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
                            )) 




### MetS --------------------------------------------------------------------

#1:DM 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 2)


#Baseline vs. 2/4/6mo
# table(improve_data$MetS_0mo, improve_data$MetS_2mo)

a <- 
  cbind(
    baseline = improve_data$MetS_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$MetS_0mo, improve_data$MetS_2mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )


improve_data <- improvement_tbl %>% filter(client_type == 2) %>% filter(continued_2t == 1)

a1 <- 
  cbind(
    baseline = improve_data$MetS_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$MetS_0mo, improve_data$MetS_4mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)


improve_data <- improvement_tbl %>% filter(client_type == 2) %>% filter(continued_3t == 1)

a1 <- 
  cbind(
    baseline = improve_data$MetS_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$MetS_0mo, improve_data$MetS_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)

a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- 
  a %>%
  group_by(baseline) %>%
  mutate(baseline = ifelse(row_number() == 3, "6mo", baseline)) %>% 
  mutate(baseline = ifelse(baseline != "6mo" & row_number() == 2, "4mo", baseline))

a <- a %>% as.data.frame()

a <- a %>% select(-Unclassified)
a <- a[1:6,]

#
a <- a[-1:-3,]

imp_ob_mets <- 
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '2/4/6mo減重成效-MetS',
                            legend = "{position:'right'}",
                            colors = "['#d9e3f0','#ff8080']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
             )) 



### DM ----------------------------------------------------------------------


#1:DM 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 2)


#Baseline vs. 2/4/6mo
# table(improve_data$DM_0mo, improve_data$DM_2mo)

a <- 
  cbind(
    baseline = improve_data$DM_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$DM_0mo, improve_data$DM_2mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )


improve_data <- improvement_tbl %>% filter(client_type == 2) %>% filter(continued_2t == 1)

a1 <- 
  cbind(
    baseline = improve_data$DM_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$DM_0mo, improve_data$DM_4mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)


improve_data <- improvement_tbl %>% filter(client_type == 2) %>% filter(continued_3t == 1)

a1 <- 
  cbind(
    baseline = improve_data$DM_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$DM_0mo, improve_data$DM_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)

a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- 
  a %>%
  group_by(baseline) %>%
  mutate(baseline = ifelse(row_number() == 3, "6mo", baseline)) %>% 
  mutate(baseline = ifelse(baseline != "6mo" & row_number() == 2, "4mo", baseline))

a <- a %>% as.data.frame()

a <- a %>% select(-Unclassified)
a <- a[1:(nrow(a)-3),]


#
a <- a[-1:-3,]

imp_ob_dm <- 
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '2/4/6mo減重成效-DM',
                            legend = "{position:'right'}",
                            colors="['#628bd6','#ffc081','#ff5959']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
             )) 





### IR ----------------------------------------------------------------------



#1:IR 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 2) %>% filter(continued_3t == 1)


#Baseline vs. 2/4/6mo
# table(improve_data$IR_0mo, improve_data$IR_2mo)

a <- 
  cbind(
    baseline = improve_data$IR_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$IR_0mo, improve_data$IR_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )


a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- a %>% as.data.frame()

a <- a %>% select(-Unclassified)
a <- a[1:(nrow(a)-1),]


imp_ob_ir <-
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '6mo成效-IR',
                            legend = "{position:'right'}",
                            colors="['#628bd6','#f8e05c','#ffc081','#ff834a','#ff5959']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
             )) 



### HTN ---------------------------------------------------------------------



#1:HTN 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 2)


#Baseline vs. 2/4/6mo
# table(improve_data$HTN_0mo, improve_data$HTN_2mo)

a <- 
  cbind(
    baseline = improve_data$HTN_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HTN_0mo, improve_data$HTN_2mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )


improve_data <- improvement_tbl %>% filter(client_type == 2) %>% filter(continued_2t == 1)

a1 <- 
  cbind(
    baseline = improve_data$HTN_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HTN_0mo, improve_data$HTN_4mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)


improve_data <- improvement_tbl %>% filter(client_type == 2) %>% filter(continued_3t == 1)

a1 <- 
  cbind(
    baseline = improve_data$HTN_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HTN_0mo, improve_data$HTN_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)

a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- 
  a %>%
  group_by(baseline) %>%
  mutate(baseline = ifelse(row_number() == 3, "6mo", baseline)) %>% 
  mutate(baseline = ifelse(baseline != "6mo" & row_number() == 2, "4mo", baseline))

a <- a %>% as.data.frame()

a <- a %>% select(-Unclassified)
a <- a[1:(nrow(a)-3),]


#
a <- a[-1:-3,]

imp_ob_htn <-
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '2/4/6mo減重成效-HTN',
                            legend = "{position:'right'}",
                            colors = "['#0571B0','#92C5DE','#F4A582','#CA0020']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
             )) 



### HLP ---------------------------------------------------------------------



#1:HLP 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 2)


#Baseline vs. 2/4/6mo
# table(improve_data$HLP_0mo, improve_data$HLP_2mo)

a <- 
  cbind(
    baseline = improve_data$HLP_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HLP_0mo, improve_data$HLP_2mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )


improve_data <- improvement_tbl %>% filter(client_type == 2) %>% filter(continued_2t == 1)

a1 <- 
  cbind(
    baseline = improve_data$HLP_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HLP_0mo, improve_data$HLP_4mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)


improve_data <- improvement_tbl %>% filter(client_type == 2) %>% filter(continued_3t == 1)

a1 <- 
  cbind(
    baseline = improve_data$HLP_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HLP_0mo, improve_data$HLP_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)

a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- 
  a %>%
  group_by(baseline) %>%
  mutate(baseline = ifelse(row_number() == 3, "6mo", baseline)) %>% 
  mutate(baseline = ifelse(baseline != "6mo" & row_number() == 2, "4mo", baseline))

a <- a %>% as.data.frame()

a <- a %>% select(-Unclassified)
a <- a[1:(nrow(a)-3),]


#
a <- a[-1:-3,]

imp_ob_hlp <-
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '2/4/6mo減重成效-HLP',
                            legend = "{position:'right'}",
                            colors = "['#d9e3f0','#ff8080']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
             )) 




## DM Patients ----------------------------------------------------------------------

### ob ----------------------------------------------------------------------
#1:DM 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 1)


#Baseline vs. 2/4/6mo
# table(improve_data$Obesity_0mo, improve_data$Obesity_2mo)

a <- 
  cbind(
    baseline = improve_data$Obesity_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$Obesity_0mo, improve_data$Obesity_2mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )


improve_data <- improvement_tbl %>% filter(client_type == 1) %>% filter(continued_2t == 1)

a1 <- 
  cbind(
    baseline = improve_data$Obesity_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$Obesity_0mo, improve_data$Obesity_4mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)


improve_data <- improvement_tbl %>% filter(client_type == 1) %>% filter(continued_3t == 1)

a1 <- 
  cbind(
    baseline = improve_data$Obesity_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$Obesity_0mo, improve_data$Obesity_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)

a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- 
  a %>%
  group_by(baseline) %>%
  mutate(baseline = ifelse(row_number() == 3, "6mo", baseline)) %>% 
  mutate(baseline = ifelse(baseline != "6mo" & row_number() == 2, "4mo", baseline))

a <- a %>% as.data.frame()

# - underweight
a <- a[-1:-3,]

imp_dm_obeisty <- 
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '2/4/6mo減重成效-Obesity',
                            legend = "{position:'right'}",
                            colors = "['#2166AC','#67A9CF','#D1E5F0','#FDDBC7','#EF8A62','#B2182B']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
             )) 


### MetS --------------------------------------------------------------------

#1:DM 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 1)


#Baseline vs. 2/4/6mo
# table(improve_data$MetS_0mo, improve_data$MetS_2mo)

a <- 
  cbind(
    baseline = improve_data$MetS_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$MetS_0mo, improve_data$MetS_2mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )


improve_data <- improvement_tbl %>% filter(client_type == 1) %>% filter(continued_2t == 1)

a1 <- 
  cbind(
    baseline = improve_data$MetS_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$MetS_0mo, improve_data$MetS_4mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)


improve_data <- improvement_tbl %>% filter(client_type == 1) %>% filter(continued_3t == 1)

a1 <- 
  cbind(
    baseline = improve_data$MetS_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$MetS_0mo, improve_data$MetS_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)

a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- 
  a %>%
  group_by(baseline) %>%
  mutate(baseline = ifelse(row_number() == 3, "6mo", baseline)) %>% 
  mutate(baseline = ifelse(baseline != "6mo" & row_number() == 2, "4mo", baseline))

a <- a %>% as.data.frame()

a <- a %>% select(-Unclassified)
a <- a[1:6,]

#
a <- a[-1:-3,]

imp_dm_mets <-
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '2/4/6mo減重成效-MetS',
                            legend = "{position:'right'}",
                            colors = "['#d9e3f0','#ff8080']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
             )) 



### DM ----------------------------------------------------------------------


#1:DM 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 1)


#Baseline vs. 2/4/6mo
# table(improve_data$DM_0mo, improve_data$DM_2mo)

a <- 
  cbind(
    baseline = improve_data$DM_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$DM_0mo, improve_data$DM_2mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )


improve_data <- improvement_tbl %>% filter(client_type == 1) %>% filter(continued_2t == 1)

a1 <- 
  cbind(
    baseline = improve_data$DM_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$DM_0mo, improve_data$DM_4mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)


improve_data <- improvement_tbl %>% filter(client_type == 1) %>% filter(continued_3t == 1)

a1 <- 
  cbind(
    baseline = improve_data$DM_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$DM_0mo, improve_data$DM_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)

a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- 
  a %>%
  group_by(baseline) %>%
  mutate(baseline = ifelse(row_number() == 3, "6mo", baseline)) %>% 
  mutate(baseline = ifelse(baseline != "6mo" & row_number() == 2, "4mo", baseline))

a <- a %>% as.data.frame()

a <- a %>% select(-Unclassified)
a <- a[1:(nrow(a)-3),]


#
a <- a[-1:-3,]

imp_dm_dm <-
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '2/4/6mo減重成效-DM',
                            legend = "{position:'right'}",
                            colors="['#628bd6','#ffc081','#ff5959']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
             )) 





### IR ----------------------------------------------------------------------


#1:IR 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 1) %>% filter(continued_3t == 1)


#Baseline vs. 2/4/6mo
# table(improve_data$IR_0mo, improve_data$IR_2mo)

a <- 
  cbind(
    baseline = improve_data$IR_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$IR_0mo, improve_data$IR_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )


a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- a %>% as.data.frame()

a <- a %>% select(-Unclassified)
a <- a[1:(nrow(a)-1),]

imp_dm_ir <-
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '6mo成效-IR',
                            legend = "{position:'right'}",
                            colors="['#628bd6','#f8e05c','#ffc081','#ff834a','#ff5959']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
             )) 


### HTN ---------------------------------------------------------------------



#1:HTN 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 1)


#Baseline vs. 2/4/6mo
# table(improve_data$HTN_0mo, improve_data$HTN_2mo)

a <- 
  cbind(
    baseline = improve_data$HTN_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HTN_0mo, improve_data$HTN_2mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )


improve_data <- improvement_tbl %>% filter(client_type == 1) %>% filter(continued_2t == 1)

a1 <- 
  cbind(
    baseline = improve_data$HTN_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HTN_0mo, improve_data$HTN_4mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)


improve_data <- improvement_tbl %>% filter(client_type == 1) %>% filter(continued_3t == 1)

a1 <- 
  cbind(
    baseline = improve_data$HTN_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HTN_0mo, improve_data$HTN_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)

a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- 
  a %>%
  group_by(baseline) %>%
  mutate(baseline = ifelse(row_number() == 3, "6mo", baseline)) %>% 
  mutate(baseline = ifelse(baseline != "6mo" & row_number() == 2, "4mo", baseline))

a <- a %>% as.data.frame()

a <- a %>% select(-Unclassified)
a <- a[1:(nrow(a)-3),]


#
a <- a[-1:-3,]

imp_dm_htn <-
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '2/4/6mo減重成效-HTN',
                            legend = "{position:'right'}",
                            colors = "['#0571B0','#92C5DE','#F4A582','#CA0020']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
             )) 



### HLP ---------------------------------------------------------------------



#1:HLP 2:OB
improve_data <- improvement_tbl %>% filter(client_type == 1)


#Baseline vs. 2/4/6mo
# table(improve_data$HLP_0mo, improve_data$HLP_2mo)

a <- 
  cbind(
    baseline = improve_data$HLP_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HLP_0mo, improve_data$HLP_2mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )


improve_data <- improvement_tbl %>% filter(client_type == 1) %>% filter(continued_2t == 1)

a1 <- 
  cbind(
    baseline = improve_data$HLP_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HLP_0mo, improve_data$HLP_4mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)


improve_data <- improvement_tbl %>% filter(client_type == 1) %>% filter(continued_3t == 1)

a1 <- 
  cbind(
    baseline = improve_data$HLP_0mo %>% levels(),
    #var_2/4/6mo
    table(improve_data$HLP_0mo, improve_data$HLP_6mo) %>% 
      ftable() %>% as.matrix() %>% as.tibble()
  )
a <- rbind(a, a1)

a <- a[order(factor(a$baseline, levels = a$baseline %>% unique())), ]

a <- 
  a %>%
  group_by(baseline) %>%
  mutate(baseline = ifelse(row_number() == 3, "6mo", baseline)) %>% 
  mutate(baseline = ifelse(baseline != "6mo" & row_number() == 2, "4mo", baseline))

a <- a %>% as.data.frame()

a <- a %>% select(-Unclassified)
a <- a[1:(nrow(a)-3),]


#
a <- a[-1:-3,]

imp_dm_hlp <-
gvisBarChart(a , xvar = "baseline", yvar = a %>% select(-baseline) %>% names(),
             options = list(isStacked = 'percent',
                            bar="{groupWidth:'70%'}",
                            title = '2/4/6mo減重成效-HLP',
                            legend = "{position:'right'}",
                            colors = "['#d9e3f0','#ff8080']",
                            backgroundColor = "#f9fffb",
                            width = "800",
                            height = "400",
                            chartArea = "{left: 100, width: '65%'}",
                            vAxis = "{textStyle : {fontSize: 10}}"
             )) 


