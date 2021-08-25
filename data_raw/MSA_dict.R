#MSA_dict_raw <- readRDS("data_raw/MSA_dict.RDS")
MSA_dict_raw <- readxl::read_xlsx("data_raw/MSA_dict.xlsx")
#names(MSA_dict_raw) <- c("key", "DE", "EN")
MSA_dict_raw <- MSA_dict_raw[,c("key", "EN", "DE")]
MSA_dict <- psychTestR::i18n_dict$new(MSA_dict_raw)
usethis::use_data(MSA_dict, overwrite = TRUE)
