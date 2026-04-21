# 01_generate_handcoding.R


# Inputs:
#   data/processed/uni_reports_snippets_updated.csv
# Outputs:
#   data/processed/unireports_clean.rds
#   output/tables/ForHandcoding_2601.csv
#   output/tables/ForHandcoding_2601.xlsx
# ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(writexl)

# Paths (assumes working directory is the project root)
snippets_path   <- "data/processed/uni_reports_snippets_updated.csv"
clean_out       <- "data/processed/unireports_clean.rds"
handcoding_csv  <- "output/tables/ForHandcoding_2601.csv"
handcoding_xlsx <- "output/tables/ForHandcoding_2601.xlsx"

# Load snippet data
setwd("~/Documents/CSS/POLI176/unireports_project")
unireports <- read_csv(snippets_path,
                       col_types = cols(doc_id = col_character()))

# Quick NA audit then drop
unireports %>%
  summarise(across(everything(), ~ sum(is.na(.))))
unireports <- drop_na(unireports)

# Descriptives
unireports %>% count(region)
unireports %>% count(tier)

# Binary tier variable for "key" universities (Project 985, 211, or 2first)
unireports <- unireports %>%
  mutate(key_uni = ifelse(proj985 == 1 | proj211 == 1 | `2first` == 1, 1, 0))
unireports %>% count(key_uni)

# Save cleaned data for downstream scripts
saveRDS(unireports, clean_out)

# Draw a 200-snippet sample for hand coding
set.seed(4869)
handcoding <- unireports[sample(1:nrow(unireports), 200), ]

write_excel_csv(handcoding, handcoding_csv)
write_xlsx(handcoding, handcoding_xlsx)

message("01_generate_handcoding.R complete.")
