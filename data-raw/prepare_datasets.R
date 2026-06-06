# This script downloads and prepares the default datasets for XploreQuran
# Run this script manually using: source("data-raw/prepare_datasets.R")

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(quRan)
source("R/pull_translation.r")

# Ensure data directory exists
if(!dir.exists("data")) dir.create("data")

cat("Downloading translations...\n")

trans_en_yusufali <- tanzil_translation("https://tanzil.net/trans/en.yusufali")
cat("Downloaded Yusuf Ali\n")

trans_en_sahih <- tanzil_translation("https://tanzil.net/trans/en.sahih")
cat("Downloaded Sahih International\n")

trans_ms_basmeih <- tanzil_translation("https://tanzil.net/trans/ms.basmeih")
cat("Downloaded Basmeih (Malay)\n")

trans_en_pickthall <- tanzil_translation("https://tanzil.net/trans/en.pickthall")
cat("Downloaded Pickthall\n")

trans_id_indonesian <- tanzil_translation("https://tanzil.net/trans/id.indonesian")
cat("Downloaded Indonesian\n")

cat("Saving data objects to data/ folder...\n")

save(trans_en_yusufali, file = "data/trans_en_yusufali.rda")
save(trans_en_sahih, file = "data/trans_en_sahih.rda")
save(trans_ms_basmeih, file = "data/trans_ms_basmeih.rda")
save(trans_en_pickthall, file = "data/trans_en_pickthall.rda")
save(trans_id_indonesian, file = "data/trans_id_indonesian.rda")

cat("Datasets successfully prepared and saved!\n")
