t <- Sys.time()
source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/genesisclinic/rscript/Refresh_dataset.R")


ver_info <- paste0("Elapsed Time: ", hms::as_hms(difftime(Sys.time(),t, units = "secs") %>% floor()))
ver_info <- paste0(ver_info, "\n", "Time version: ", t, "\n")
cat(ver_info)


save.image("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/WSpace_preproc.RData")
cat(ver_info, file = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/log.txt", append = TRUE)
# rm(list = objects())