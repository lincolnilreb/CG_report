library(pacman)
pacman::p_load(magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, extraInserts, ggvenn, ggplot2,knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot)


#Venn Graph
x <- list(
  #1
  `Clinic_clients` = stat_table[["id"]] %>% unique(),
  #2
  `Obesity` =  stat_table$id[stat_table[["client_type"]] == 2],
  #3
  `Diabetes` =  stat_table$id[stat_table[["client_type"]] == 1],
  #4
  `Inbody` = stat_table$id[((!is.na(stat_table[["weight_baseline"]])) & (!is.na(stat_table[["weight_endpoint"]])))],
  #5
  `Blood` = stat_table$id[((!is.na(stat_table[["glucose_ac_baseline"]])) & (!is.na(stat_table[["glucose_ac_endpoint"]])))],
  #6
  `Diet` = stat_table$id[!is.na(stat_table[["note_count"]])],
  #7.OB.
  `Fit_criteria_OB` = Reduce(intersect, list(stat_table$id[stat_table[["client_type"]] == 2],
                                             stat_table$id[((!is.na(stat_table[["weight_baseline"]])) & (!is.na(stat_table[["weight_endpoint"]])))],
                                             stat_table$id[((!is.na(stat_table[["glucose_ac_baseline"]])) & (!is.na(stat_table[["glucose_ac_endpoint"]])))],
                                             stat_table$id[!is.na(stat_table[["note_count"]])]
  )), 
  #8.DM.
  `Fit_criteria_DM` = Reduce(intersect, list(stat_table$id[stat_table[["client_type"]] == 1],
                                             stat_table$id[((!is.na(stat_table[["weight_baseline"]])) & (!is.na(stat_table[["weight_endpoint"]])))],
                                             stat_table$id[((!is.na(stat_table[["glucose_ac_baseline"]])) & (!is.na(stat_table[["glucose_ac_endpoint"]])))],
                                             stat_table$id[!is.na(stat_table[["note_count"]])]
  )),
  #9
  `Quit` =  stat_table$id[(stat_table[["client_type"]] != 1) & (stat_table[["client_type"]] != 2)]
)


plot_venn_0 <- 
  ggvenn(
    x, columns = names(x)[c(2,3,9)],
    fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
    stroke_size = 0.5, set_name_size = 3.5, 
  ) +
  labs(title = "首次療程客戶")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
    plot.margin = unit(c(0.5,0,0,0), "cm")
  ) 

plot_venn_1 <- 
  ggvenn(
    x, columns = names(x)[c(2,7,1)],
    fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
    stroke_size = 0.5, set_name_size = 3.5, 
  ) +
  labs(title = "Data Screening")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
    plot.margin = unit(c(0.5,0,0,0), "cm")
  ) 

plot_venn_2 <- 
  ggvenn(
    x, columns = names(x)[c(2,4,5,6)],
    fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
    stroke_size = 0.5, set_name_size = 3.5, 
  ) +
  labs(title = "Data Screening")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
    plot.margin = unit(c(0.5,0,0,0), "cm")
  ) 

plot_venn_3 <- 
  ggvenn(
    x, columns = names(x)[c(3,8,1)],
    fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
    stroke_size = 0.5, set_name_size = 3.5, 
  ) +
  labs(title = "Data Screening")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
    plot.margin = unit(c(0.5,0,0,0), "cm")
  ) 

plot_venn_4 <- 
  ggvenn(
    x, columns = names(x)[c(3,4,5,6)],
    fill_color = c("#0073C2", "#CD534C", "#00FA9A", "#EFC000", "#868686"),
    stroke_size = 0.5, set_name_size = 3.5, 
  ) +
  labs(title = "Data Screening")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2.0),
    plot.margin = unit(c(0.5,0,0,0), "cm")
  ) 



rm(x)

