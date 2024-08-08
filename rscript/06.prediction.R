
# 01.Global vars optimal cutoff analysis ----------------------------------------------------------------
library(cutpointr)

# Def: weight loss fail: > -8
cutoff <- -8
stat_table_1st_ob$gp_weight_loss <- stat_table_1st_ob$`∆weight%` %>% cut(c(-Inf, cutoff, Inf), c("success", "fail"))

# Select numeric vars
vars <- stat_table_1st_ob %>% 
  select_if(is.numeric) %>% 
  select(-c(id, class_freq, class_order, height_baseline, height_endpoint)) %>% 
  select_if(~ sum(!is.na(.)) >= 50) %>% 
  names()

# Store start time
start_time <- Sys.time()

# Apply cutpointr to each variable
table_var_cutoff <- lapply(vars, function(var) {
  a <- stat_table_1st_ob %>% select(c(var, gp_weight_loss))
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
table_global_cutoff <-
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

