# set WD beforehand
MSA_item_bank <- readr::read_csv("data_raw/MSA_item_bank.csv")
# MSA_item_bank <- readRDS("data_raw/MSA_item_bank.RDS")
usethis::use_data(MSA_item_bank, overwrite = TRUE)

# adaptive stuff
# maybe unnecessary
# MSA2_item_bank <- read.csv("data_raw/MSA2_item_bank.csv", stringsAsFactors = F)

# might be necessarry
# usethis::use_data(MSA2_item_bank, overwrite = TRUE)
