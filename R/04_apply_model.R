# 04_apply_model.R
# ---------------------------------------------------------------
# Apply the fitted decision tree to label every snippet in the
# full corpus, then inspect prediction rates and false positives.
#
# Inputs:
#   data/processed/combined.rds
#   data/processed/dfmmat.rds
#   data/processed/tree_model.rds
# Outputs:
#   data/processed/combined_labeled.rds
#   output/tables/prediction_rates.txt
# ---------------------------------------------------------------

library(tidyverse)

combined   <- readRDS("data/processed/combined.rds")
dfmmat     <- readRDS("data/processed/dfmmat.rds")
tree.party <- readRDS("data/processed/tree_model.rds")

# Predict on the full dfmmat (same feature space the tree was trained on)
combined$predict.party <- as.numeric(as.character(
  predict(tree.party, dfmmat, type = "class")
))

# Prediction rates
pred_rate <- prop.table(table(combined$predict.party))
hand_rate <- prop.table(table(combined$party))

sink("output/tables/prediction_rates.txt")
cat("Predicted party-ideology rate (whole corpus):\n")
print(pred_rate)
cat("\nHand-coded party rate (labeled subset only):\n")
print(hand_rate)
sink()

# Inspect false positives on the labeled subset
false_positives <- combined %>%
  filter(predict.party == 1, party == 0)

false_positives %>%
  select(doc_id, predict.party, party, text) %>%
  head(10)

saveRDS(combined, "data/processed/combined_labeled.rds")

message("04_apply_model.R complete.")
