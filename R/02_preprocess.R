# 02_preprocess.R
# ---------------------------------------------------------------
# Merge hand codes from two coders, compute intercoder reliability,
# resolve disagreements, and build the quanteda corpus + DFM used
# for all downstream modeling.
#
# Inputs:
#   data/processed/unireports_clean.rds
#   data/processed/ForHandcoding_2601_coder1.csv
#   data/processed/ForHandcoding_2601_coder2.csv
# Outputs:
#   data/processed/combined.rds       (merged, labels resolved)
#   data/processed/dfm.rds            (trimmed DFM)
#   output/tables/kripp_alpha.txt
# ---------------------------------------------------------------

library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(irr)

unireports <- readRDS("data/processed/unireports_clean.rds")

# Read hand coding
handcoded <- read_csv("data/processed/ForHandcoding_2601_coder1.csv",
                      col_types = cols(doc_id = col_character()))
coder2    <- read_csv("data/processed/ForHandcoding_2601_coder2.csv",
                      col_types = cols(doc_id = col_character()))
handcoded$party_2 <- coder2$party_2

# Merge hand codes back into the full dataset
combined <- unireports %>%
  left_join(
    handcoded %>% dplyr::select(doc_id, uni_code, text, party_1, party_2),
    by = c("doc_id", "uni_code", "text")
  )
dim(combined)

# Intercoder reliability
conf_hand <- table(combined$party_1, combined$party_2)
print(conf_hand)

kripp <- kripp.alpha(t(combined[, c("party_1", "party_2")]))
print(kripp)

# Save alpha to a text file for the repo
sink("output/tables/kripp_alpha.txt")
cat("Krippendorff's alpha between coder 1 and coder 2\n")
cat("-------------------------------------------------\n")
print(kripp)
cat("\nConfusion table (coder 1 rows, coder 2 columns):\n")
print(conf_hand)
sink()

# Disagreements
handcoded[handcoded$party_1 != handcoded$party_2,
          c("doc_id", "text", "party_1", "party_2")]

# Resolve disagreements (manually adjudicated)
combined$party <- combined$party_1
combined$party[combined$doc_id == "923_04"]  <- 0
combined$party[combined$doc_id == "950_04"]  <- 0
combined$party[combined$doc_id == "1668_10"] <- 0
combined$party[combined$doc_id == "634_04"]  <- 0

# Preprocessing into a DFM
corpus_uni <- corpus(combined, text_field = "text")
docvars(corpus_uni, "doc_id") <- combined$doc_id

toks <- tokens(corpus_uni,
               what = "word",
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE)

sw_zh <- stopwords("zh", source = "stopwords-iso")
toks  <- tokens_select(toks, sw_zh, selection = "remove")

dfm <- dfm(toks)

# Trim rare and overly common words (light trimming for Chinese text)
dfm <- dfm_trim(dfm,
                min_docfreq  = 0.02,
                max_docfreq  = 0.95,
                docfreq_type = "prop")

# Save outputs
saveRDS(combined, "data/processed/combined.rds")
saveRDS(dfm,      "data/processed/dfm.rds")

message("02_preprocess.R complete.")
