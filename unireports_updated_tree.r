library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textmodels)
library(quanteda.corpora)
library(caret)
library(irr)
setwd("~/Documents/CSS/POLI176/final_project")
unireports <- read_csv("data/uni_reports_snippets_updated.csv", 
                     col_types = cols(doc_id = col_character()))
unireports%>%
  summarise(across(everything(), ~ sum(is.na(.))))

unireports <- drop_na(unireports)

# Descriptive 
library(dplyr)
unireports%>% count(region)
unireports%>% count(tier)

# Binary tier variable for key uni (either project 985,211, 2first universities) 
unireports <- unireports %>%
  mutate(key_uni = ifelse(proj985 == 1 | proj211 == 1 | `2first` == 1, 1, 0))
unireports%>% count(key_uni)


# Hand Coding
# Select 200 sample for hand-coding
set.seed(4869)
handcoding <- unireports[sample(1:nrow(unireports),200),]
handcoding

library(readr)
readr::write_excel_csv(handcoding, "ForHandcoding_2601.csv")
install.packages("writexl")
library(writexl)
write_xlsx(handcoding, "Forandcoding_2601.xlsx")

# Read in Party Hand Coding
handcoded <- read_csv("ForHandcoding_2601_coder1.csv", 
                      col_types = cols(doc_id = col_character()))
coder2 <- read_csv("ForHandcoding_2601_coder2.csv", 
                    col_types = cols(doc_id = col_character()))
handcoded$party_2 <- coder2$party_2
handcoded

# Merge
combined <- unireports %>%
  left_join(
    handcoded %>% dplyr::select(doc_id,uni_code,text,party_1,party_2),  
    by = c("doc_id","uni_code","text")
  )
dim(combined)

# Confusion Matrix
table(combined$party_1, combined$party_2)

# Krippendorff's alpha
kripp.alpha(t(combined[,c("party_1", "party_2")]))

# Disagreements
handcoded[handcoded$party_1!=handcoded$party_2,c("doc_id","text", "party_1", "party_2")]
handcoded$doc_id[handcoded$party_1 != handcoded$party_2]

# Resolve disagreements
combined$party <- combined$party_1
combined$party[combined$doc_id == "923_04"]   <- 0
combined$party[combined$doc_id == "950_04"]  <- 0
combined$party[combined$doc_id == "1668_10"]  <- 0
combined$party[combined$doc_id == "634_04"]  <- 0
dim(combined)
sum(is.na(combined$doc_id))

# Pre-processing
corpus_uni <- corpus(combined, text_field = "text")
corpus_uni
docvars(corpus_uni, "doc_id") <- combined$doc_id

## Create a document feature matrix 
toks <- tokens(corpus_uni, what = "word", remove_punct = TRUE, remove_symbols = TRUE,remove_numbers=TRUE)
sw_zh  <- stopwords("zh", source = "stopwords-iso")
toks <- tokens_select(toks, sw_zh, selection = "remove")
dfm <- dfm(toks)

#remove overly rare and overly common words  (adjusted to only trim less than 2% for Chinese)
dfm <- dfm_trim(dfm,min_docfreq=.02,max_docfreq = 0.95, docfreq_type = "prop")

## Split into training, validation, and unlabeled data
#Unlabeled data 
unlabeled <- which(is.na(combined$party))

#Labeled data
labeled <- which(!is.na(combined$party))

# The proportion of each categories in labeled data
combined %>%
  filter(!is.na(party)) %>%
  count(party) %>%
  mutate(proportion = n / sum(n))

# Sample a training and validation set from the labeled data
set.seed(4869)
training <- sample(labeled, round(length(labeled)*.75))
validation <- labeled[!labeled%in%training]

# Create separate dfm's for each
dfmat_train <- dfm_subset(dfm, docvars(corpus_uni, "doc_id") %in% combined$doc_id[training])
dfmat_val   <- dfm_subset(dfm, docvars(corpus_uni, "doc_id") %in% combined$doc_id[validation])

### Model1: Naive Bayes Model

# Train 
tmod_nb <- textmodel_nb(dfmat_train, docvars(dfmat_train, "party"))

# use this naive bayes model 
summary(tmod_nb)

#Probability of a word given a category
coef_nb <- coef(tmod_nb)
head(coef_nb)

#Words associated with party ideology
sort(coef_nb[,2]/coef_nb[,1], decreasing=T)[1:20]
#Words not associated with party ideology
sort(coef_nb[,2]/coef_nb[,1], decreasing=F)[1:20]

#apply the model to sample
predict.train <- predict(tmod_nb, dfmat_train)

tab_train <- table(docvars(dfmat_train, "party_1"), predict.train)
tab_train

# model performance in training set 
#precision
diag(tab_train)/colSums(tab_train)
#recall
diag(tab_train)/rowSums(tab_train)

# Validation  
predict.val <- predict(tmod_nb, newdata = dfmat_val)

# Confusion matrix
tab_val <- table(docvars(dfmat_val, "party_1"), predict.val)
tab_val

#precision
diag(tab_val)/colSums(tab_val)
#recall
diag(tab_val)/rowSums(tab_val)

##Confusion matrix
conf_matrix <- confusionMatrix(factor(predict.val), 
                               factor(docvars(dfmat_val, "party")),
                               mode="prec_recall", positive="1")
print(conf_matrix)


### Model 2: Lasso 
library(glmnet)
## lasso model
lasso.1 <- glmnet(dfmat_train, docvars(dfmat_train, "party"),
                  family="binomial", alpha=1)

## Lasso with Cross-validation
cv <- cv.glmnet(dfmat_train, docvars(dfmat_train, "party"),
                family="binomial", alpha=1, type.measure = "class")

plot(log(cv$lambda), cv$cvm, xlab="Log Lambda", ylab="Misclassification error")


#Predict for the validation set
predict.val_cv <- predict(cv, dfmat_val, type="class",lambda=cv$lambda.min)

tab_val_cv <- table(docvars(dfmat_val, "party"), predict.val_cv)
tab_val_cv

#precision
diag(tab_val_cv)/colSums(tab_val_cv)
#recall
diag(tab_val_cv)/rowSums(tab_val_cv)

#Confusion matrix
conf_matrix_cv <- confusionMatrix(factor(predict.val_cv), 
                               factor(docvars(dfmat_val, "party")),
                               mode="prec_recall", positive="1")
print(conf_matrix_cv)

### Model 3: Trees
library(rpart)

# Convert the DFM to a data frame
dfmmat <- convert(dfm, to = "data.frame")[, -1]
colnames(dfmmat) <- make.names(colnames(dfmmat), unique = TRUE)

# Attach the response variable directly to the data frame
dfmmat$party <- as.factor(docvars(dfm, "party_1"))

# Split using row indices relative to dfmmat (not combined)
labeled_rows <- which(!is.na(dfmmat$party))
set.seed(4869)
train_rows <- sample(labeled_rows, round(length(labeled_rows) * 0.75))
val_rows <- labeled_rows[!labeled_rows %in% train_rows]
 
# Fit the tree on the training subset
tree.party <- rpart(party ~ ., 
                    data = dfmmat[train_rows, ], method = "class")

# Check the tree nodes and plot 
summary(tree.party)
plot(tree.party)
text(tree.party, pretty = 0)

# Fix the plot for chinese characters 
install.packages("rpart.plot")
library(rpart.plot)

# Set a font that supports Chinese characters
par(family = "STKaiti") 
rpart.plot(tree.party, type = 2, extra = 104, fallen.leaves = TRUE)

# Validation on the test set
dfmmat_test <- dfmmat[validation,]
tree.pred <- predict(tree.party, dfmmat_test,
                     type = "class")

tab_val <- table(tree.pred, docvars(dfmat_val, "party"))

# Precision
diag(tab_val)/colSums(tab_val)
# Recall
diag(tab_val)/rowSums(tab_val)

# Confusion matrix
conf_matrix_tree <- confusionMatrix(factor(tree.pred), 
                               factor(docvars(dfmat_val, "party")),
                               mode="prec_recall", positive="1")
print(conf_matrix_tree)


# Naive Bayes: Accuracy 0.84, Precision 0.6471, Recall 0.8462, F1 0.7333
# LASSO:       Accuracy 0.84, Precision 0.8571, Recall 0.4615, F1 0.6000
# Tree:        Accuracy 0.88, Precision 0.7692, Recall 0.7692, F1 0.7692
# The decision tree has the highest accuracy and the best balance between
# precision and recall (highest F1), so we select it as the final classifier.

# Apply the decision tree model to label all text snippets.
# The tree was trained on `dfmmat` 
# predict on the full dfmmat to keep the feature space consistent

combined$predict.party <- as.numeric(as.character(predict(tree.party, dfmmat, type = "class")))

# Prediction of party ideology content for the whole corpus
prop.table(table(combined$predict.party))
prop.table(table(combined$party))

## the model is under-predicting a little

# Check some false positive examples
false_positives <- combined %>%
  filter(predict.party == 1, party == 0)

false_positives %>%
  select(doc_id, predict.party, party, text) %>%
  head(10) 
false_positives

false_positives %>%
  select(doc_id, predict.party, party, text) %>%
  head(10) 
false_positives

# Testing Hypothesis
# H1: region and party ideology 

## proportions across regions
combined %>%
  group_by(region) %>%
  summarise(
    count = n(),
    party_1_rate = mean(predict.party == 1, na.rm = TRUE)
  ) %>%
  arrange(desc(party_1_rate))

## Anova
anova_region <- aov(predict.party ~ region, data = combined)
summary(anova_region)

## Political center:group provinces to construct north_politics_core
north_provs <- c("Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia",
                 "Liaoning", "Jilin", "Heilongjiang", "Shandong", 
                 "Henan", "Shaanxi")
combined$north_politics_core <- as.numeric(combined$prov %in% north_provs)


## proportions between groups
combined %>%
  group_by(north_politics_core) %>%
  summarise(
    `Count` = n(),
    `Party Influence (predict.party = 1)` = mean(predict.party == 1, na.rm = TRUE)
  ) 

## T-test
t.test(predict.party ~ north_politics_core, data = combined)
### not significant!

# H2:school tier and party ideology 

## proportions
combined %>%
  group_by(tier) %>%
  summarise(
    count = n(),
    `Party Influence (predict.party = 1)` = mean(predict.party == 1, na.rm = TRUE)
  )

combined %>%
  group_by(key_uni) %>%
  summarise(
    count = n(),
    `Party Influence (predict.party = 1)`= mean(predict.party == 1, na.rm = TRUE)
  )

## tabulate party influence by tier
table(combined$predict.party, combined$tier)
table(combined$predict.party, combined$key_uni)

## T-test
t.test(predict.party ~ tier, data = combined)
t.test(predict.party ~ key_uni, data = combined)
### not significant!
