#MSA_dict_raw <- readRDS("data_raw/MSA_dict.RDS")
MSA_dict_raw <- readxl::read_xlsx("data_raw/MSA_dict.xlsx")
#names(MSA_dict_raw) <- c("key", "DE", "EN")
MSA_dict_raw <- MSA_dict_raw[,c("key", "en", "de","de_inf","de_f", "fr")]
MSA_dict <- psychTestR::i18n_dict$new(MSA_dict_raw)
usethis::use_data(MSA_dict, overwrite = TRUE)
MSA::MSA_languages()
#to update documentation
#devtools::document()
