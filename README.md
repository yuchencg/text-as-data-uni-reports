# Text-as-Data-Uni-Reports
 Text classification of party-aligned ideological content in Chinese university annual information disclosure reports (2022–2024). Trained Naive Bayes, LASSO, and tree-based classifiers using hand-labeled sample of snippets. The decision tree is selected and applied to the full corpus. 
Hypothesis tests then examine whether predicted party ideology content vary by region and school tier.


## Required packages

tidyverse, readr, writexl, quanteda, quanteda.textplots, quanteda.textmodels, irr, caret, glmnet, rpart, rpart.plot

## Model comparison

| Model        | Accuracy | Precision | Recall | F1     |
|--------------|----------|-----------|--------|--------|
| Naive Bayes  | 0.84     | 0.6471    | 0.8462 | 0.7333 |
| LASSO        | 0.84     | 0.8571    | 0.4615 | 0.6000 |
| **Tree**     | **0.88** | 0.7692    | 0.7692 | **0.7692** |

The decision tree is selected as the final classifier because it has the highest accuracy and the best balance between precision and recall.
