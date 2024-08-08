#Global vars vector: en








var_profile <- c("id","client_type","age","gender","date_t0","date_t1", "gp")
var_inbody <- c(#inbody
                "weight","bmi",
                "bf","pbf","vfa_level",
                "ffm","bm","real_muscle_mass","pbm","so_score",
                "wc","acl","cacl","waist_hip_ratio",
                "total_body_water","protein_weight","mineral_weight","body_cell_mass","body_mineral",
                "bfmi","bsmi","ffmi",
                "sbp","dbp","pulse","bmr",
                #相位角
                "wepa50","algle_50_left_arm","algle_50_left_leg","algle_50_right_arm","algle_50_right_leg","algle_50_trunk",
                #體水份
                "ecw_ratio","extracellular_water_ratio_left_arm","extracellular_water_ratio_left_leg","extracellular_water_ratio_right_arm","extracellular_water_ratio_right_leg","extracellular_water_ratio_trunk",
                "icw","intracellular_weight_left_arm","intracellular_weight_left_leg","intracellular_weight_right_arm","intracellular_weight_right_leg","intracellular_weight_trunk",
                "ecw","extracellular_weight_left_arm","extracellular_weight_left_leg","extracellular_weight_right_arm","extracellular_weight_right_leg","extracellular_weight_trunk",
                #inbody - 部位別
                "left_arm_fat","left_arm_fat_percentage","left_arm_muscle","left_arm_muscle_percentage",
                "left_leg_fat","left_leg_fat_percentage","left_leg_muscle","left_leg_muscle_percentage",
                "right_arm_fat","right_arm_fat_percentage","right_arm_muscle","right_arm_muscle_percentage",
                "right_leg_fat","right_leg_fat_percentage","right_leg_muscle_percentage","right_leg_muscle",
                "trunk_fat","trunk_fat_percentage","trunk_muscle","trunk_muscle_percentage",
                #體水份 - 部位別
                "water_weight_left_arm","water_weight_left_leg",
                "water_weight_right_arm","water_weight_right_leg",
                "water_weight_trunk","tbwffm"
                )
var_blood <- c(#Glucose/Insulin
               "hba1c", "GA", "eAG", "glucose_ac","glucose_pc_1hr","glucose_pc_2hr","insulin","insulin_pc_1hr","insulin_pc_2hr","homa_ir","homa_beta","c_peptide",
               "tAUCg","tAUCi","OGIRIndex",
               #Lipid
               "tg","tc","hdl","ldl","sd_ldl",
               #Kidney
               "egfr","blood_creatinine","uric_acid",
               #Hormone
               "tsh","prolactin","fsh","lh","e2","testosterone","progesterone","dhea_s","shbg","amh","t3","t3_reverse","t4_free","psa",
               # "urine_appearance","urine_spe_gravity","urine_ph","urine_protein","urine_glucose","urine_bilirubin","urobilinogen","urine_nitrite","urine_ketones","urine_clarity","urine_blood","urine_leu","unine_sediments","urine_wbc","urine_rbc","urine_ep","urine_cas","urine_cry","urine_bac",
               #Hematology
               "wbc","rbc","hb","esr","mcv","mch","mchc","platelet","rdw_sd","rdw_cv","neutrophils","lymphocytes","monocytes","eosinophils","basophils","monocytes_percent","eosinophils_percent","basophils_percent",
               "alt_gpt","ast_got",
               "amylase","lipase","apoli_a1","apoli_b","apolib_ai_ratio"
               )
var_diet <- c(#Obedience
              "upload_day_%","note_count","pic_counts",
              "light_G_%","light_Y_%","light_R_%",
              #3/6 Macronutrition
              "calorie_day","carb_E%","protein_E%","fat_E%",
              "calorie_target","carb_ep_target","protein_ep_target","fat_ep_target",
              "calorie_day_deficit","carb_e_day_deficit","protein_e_day_deficit","fat_e_day_deficit",
              "calorie_meal_mean","carb_ep_meal","protein_ep_meal","fat_ep_meal",
              "fruits_day","vegetables_day","grains_day","meat_bean_day","milk_day", "oil_day")
 

vars_en <- 
Reduce(append, list(var_profile,
                    paste0(var_inbody, "_baseline"),
                    paste0(var_blood, "_baseline"),
                    paste0(var_inbody, "_endpoint"),
                    paste0(var_blood, "_endpoint"),
                    var_diet,
                    paste0("∆",var_inbody),
                    paste0("∆",var_blood),
                    paste0("∆",var_inbody, "%"),
                    paste0("∆",var_blood, "%")
                    ))

rm(list = c("var_profile","var_inbody","var_blood","var_diet"))








