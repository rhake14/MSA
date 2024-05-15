# set WD beforehand
MSA_item_bank <- readr::read_csv("data_raw/MSA_item_bank.csv")
# MSA_item_bank <- readRDS("data_raw/MSA_item_bank.RDS")
usethis::use_data(MSA_item_bank, overwrite = TRUE)

# # adaptive stuff
# # maybe unnecessary
# MSA2_item_bank <- read.csv("data_raw/MSA2_item_bank.csv", stringsAsFactors = F)
# # might be necessarry
# usethis::use_data(MSA2_item_bank, overwrite = TRUE)

# adaptive stuff
# maybe unnecessary
MSA_item_bank <- read.csv("data_raw/MSA_item_bank.csv", stringsAsFactors = F)
# might be necessarry
usethis::use_data(MSA_item_bank, overwrite = TRUE)


# MSA short
MSA_itembank_training_only <- read.csv("data_raw/MSA_items_training_only.csv", stringsAsFactors = F)
# might be necessarry
usethis::use_data(MSA_itembank_training_only, overwrite = TRUE)

MSA_itembank_training_only <- readr::read_csv("data_raw/MSA_items_training_only.csv")
# MSA_item_bank <- readRDS("data_raw/MSA_item_bank.RDS")
usethis::use_data(MSA_itembank_training_only, overwrite = TRUE)
# overwrite by
devtools::document()
