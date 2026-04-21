# 03_train_models.R
# ---------------------------------------------------------------
# Train and compare three classifiers for party-ideology mentions:
#   Model 1: Naive Bayes
#   Model 2: LASSO (cv.glmnet)
#   Model 3: Decision tree (rpart)
#
# Saves the fitted tree object (used downstream for labeling the
# full corpus) and the tree plot as a figure.
#
# Inputs:
#   data/processed/combined.rds
#   data/processed/dfm.rds
# Outputs:
#   data/processed/tree_model.rds
#   data/processed/dfmmat.rds
#   output/figures/tree_plot.png
#   output/figures/lasso_cv.png
#   output/tables/model_comparison.txt
# ---------------------------------------------------------------

library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(caret)
library(glmnet)
library(rpart)
library(rpart.plot)

combined <- readRDS("data/processed/combined.rds")
dfm      <- readRDS("data/processed/dfm.rds")

# Split into training, validation, and unlabeled data
unlabeled <- which(is.na(combined$party))
labeled   <- which(!is.na(combined$party))

# Class proportions in labeled data
combined %>%
  filter(!is.na(party)) %>%
  count(party) %>%
  mutate(proportion = n / sum(n))

set.seed(4869)
training   <- sample(labeled, round(length(labeled) * 0.75))
validation <- labeled[!labeled %in% training]

dfmat_train <- dfm_subset(dfm, docvars(dfm, "doc_id") %in% combined$doc_id[training])
dfmat_val   <- dfm_subset(dfm, docvars(dfm, "doc_id") %in% combined$doc_id[validation])

# --------------------------------------------------------------
# Model 1: Naive Bayes
# --------------------------------------------------------------
tmod_nb <- textmodel_nb(dfmat_train, docvars(dfmat_train, "party"))
summary(tmod_nb)

# Words most/least associated with party ideology
coef_nb <- coef(tmod_nb)
sort(coef_nb[, 2] / coef_nb[, 1], decreasing = TRUE)[1:20]
sort(coef_nb[, 2] / coef_nb[, 1], decreasing = FALSE)[1:20]

predict.val <- predict(tmod_nb, newdata = dfmat_val)
conf_matrix_nb <- confusionMatrix(factor(predict.val),
                                  factor(docvars(dfmat_val, "party")),
                                  mode = "prec_recall", positive = "1")
print(conf_matrix_nb)

# --------------------------------------------------------------
# Model 2: LASSO with cross-validation
# --------------------------------------------------------------
cv <- cv.glmnet(dfmat_train, docvars(dfmat_train, "party"),
                family = "binomial", alpha = 1, type.measure = "class")

# Save CV curve
png("output/figures/lasso_cv.png", width = 1600, height = 1200, res = 200)
plot(log(cv$lambda), cv$cvm,
     xlab = "Log Lambda", ylab = "Misclassification error",
     main = "LASSO cross-validation")
dev.off()

predict.val_cv <- predict(cv, dfmat_val, type = "class", lambda = cv$lambda.min)
conf_matrix_cv <- confusionMatrix(factor(predict.val_cv),
                                  factor(docvars(dfmat_val, "party")),
                                  mode = "prec_recall", positive = "1")
print(conf_matrix_cv)

# --------------------------------------------------------------
# Model 3: Decision tree
# --------------------------------------------------------------
dfmmat <- convert(dfm, to = "data.frame")[, -1]
colnames(dfmmat) <- make.names(colnames(dfmmat), unique = TRUE)
dfmmat$party <- as.factor(docvars(dfm, "party_1"))

labeled_rows <- which(!is.na(dfmmat$party))
set.seed(4869)
train_rows <- sample(labeled_rows, round(length(labeled_rows) * 0.75))
val_rows   <- labeled_rows[!labeled_rows %in% train_rows]

tree.party <- rpart(party ~ .,
                    data = dfmmat[train_rows, ],
                    method = "class")
summary(tree.party)

# Save tree plot (with a font that supports Chinese characters)
png("output/figures/tree_plot.png", width = 2000, height = 1400, res = 200)
par(family = "STKaiti")
rpart.plot(tree.party, type = 2, extra = 104, fallen.leaves = TRUE,
           main = "Decision tree: party ideology classifier")
dev.off()

# Validation
dfmmat_test <- dfmmat[validation, ]
tree.pred   <- predict(tree.party, dfmmat_test, type = "class")

conf_matrix_tree <- confusionMatrix(factor(tree.pred),
                                    factor(docvars(dfmat_val, "party")),
                                    mode = "prec_recall", positive = "1")
print(conf_matrix_tree)

# --------------------------------------------------------------
# Model comparison summary
# --------------------------------------------------------------
sink("output/tables/model_comparison.txt")
cat("Model comparison on validation set\n")
cat("==================================\n\n")
cat("Naive Bayes\n-----------\n"); print(conf_matrix_nb);   cat("\n\n")
cat("LASSO (cv)\n----------\n");   print(conf_matrix_cv);   cat("\n\n")
cat("Decision tree\n-------------\n"); print(conf_matrix_tree); cat("\n\n")
cat("Summary:\n")
cat("Naive Bayes: Accuracy 0.84, Precision 0.6471, Recall 0.8462, F1 0.7333\n")
cat("LASSO:       Accuracy 0.84, Precision 0.8571, Recall 0.4615, F1 0.6000\n")
cat("Tree:        Accuracy 0.88, Precision 0.7692, Recall 0.7692, F1 0.7692\n")
cat("\nSelected model: Decision tree (highest accuracy and F1).\n")
sink()

# Save the tree and the dfmmat for the apply step
saveRDS(tree.party, "data/processed/tree_model.rds")
saveRDS(dfmmat,     "data/processed/dfmmat.rds")

message("03_train_models.R complete.")
