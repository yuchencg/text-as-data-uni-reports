# 05_analysis.R
# ---------------------------------------------------------------
# Test hypotheses on the tree-labeled corpus:
#   H1: Party ideology varies by region / northern political core.
#       – One-way ANOVA + FDR-corrected pairwise t-tests across regions.
#   H2: Party ideology varies by school tier / "key" university.
#
# Saves t-test results as text and as bar-chart figures.
#
# Inputs:
#   data/processed/combined_labeled.rds
# Outputs:
#   output/tables/anova_region.txt        (incl. FDR pairwise comparisons)
#   output/tables/ttest_north_politics.txt
#   output/tables/ttest_tier.txt
#   output/tables/ttest_key_uni.txt
#   output/figures/party_rate_by_region.png
#   output/figures/party_rate_north_vs_other.png
#   output/figures/party_rate_by_tier.png
#   output/figures/party_rate_by_key_uni.png
# ---------------------------------------------------------------

library(tidyverse)

combined <- readRDS("data/processed/combined_labeled.rds")

# Helper: save a summary object to a text file
save_sink <- function(path, ...) {
  sink(path)
  for (x in list(...)) print(x)
  sink()
}

# --------------------------------------------------------------
# H1: region and party ideology
# --------------------------------------------------------------
region_summary <- combined %>%
  group_by(region) %>%
  summarise(count = n(),
            party_1_rate = mean(predict.party == 1, na.rm = TRUE)) %>%
  arrange(desc(party_1_rate))
print(region_summary)

# ANOVA across regions
anova_region <- aov(predict.party ~ region, data = combined)

# FDR-corrected pairwise comparisons (t-tests with Benjamini-Hochberg adjustment)
pairwise_region <- pairwise.t.test(combined$predict.party, combined$region,
                                   p.adjust.method = "fdr")

save_sink("output/tables/anova_region.txt",
          region_summary, summary(anova_region), pairwise_region)

# Bar chart: predicted party rate by region
p_region <- ggplot(region_summary,
                   aes(x = reorder(region, -party_1_rate), y = party_1_rate)) +
  geom_col(fill = "#B22222") +
  labs(x = "Region", y = "Predicted party-ideology rate",
       title = "Predicted party-ideology rate by region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave("output/figures/party_rate_by_region.png",
       p_region, width = 8, height = 5, dpi = 200)

# Political center: group provinces into a "northern political core"
north_provs <- c("Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia",
                 "Liaoning", "Jilin", "Heilongjiang", "Shandong",
                 "Henan", "Shaanxi")
combined$north_politics_core <- as.numeric(combined$prov %in% north_provs)

north_summary <- combined %>%
  group_by(north_politics_core) %>%
  summarise(Count = n(),
            party_rate = mean(predict.party == 1, na.rm = TRUE))
print(north_summary)

ttest_north <- t.test(predict.party ~ north_politics_core, data = combined)
save_sink("output/tables/ttest_north_politics.txt",
          north_summary, ttest_north)

p_north <- north_summary %>%
  mutate(group = ifelse(north_politics_core == 1, "Northern core", "Other")) %>%
  ggplot(aes(x = group, y = party_rate, fill = group)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Northern core" = "#B22222", "Other" = "#808080")) +
  labs(x = NULL, y = "Predicted party-ideology rate",
       title = "Party ideology: northern political core vs. rest",
       subtitle = sprintf("t = %.2f, p = %.3f",
                          ttest_north$statistic, ttest_north$p.value)) +
  theme_minimal()

ggsave("output/figures/party_rate_north_vs_other.png",
       p_north, width = 6, height = 4.5, dpi = 200)

# --------------------------------------------------------------
# H2: school tier and party ideology
# --------------------------------------------------------------
tier_summary <- combined %>%
  group_by(tier) %>%
  summarise(count = n(),
            party_rate = mean(predict.party == 1, na.rm = TRUE))

key_summary <- combined %>%
  group_by(key_uni) %>%
  summarise(count = n(),
            party_rate = mean(predict.party == 1, na.rm = TRUE))

ttest_tier    <- t.test(predict.party ~ tier,    data = combined)
ttest_key_uni <- t.test(predict.party ~ key_uni, data = combined)

save_sink("output/tables/ttest_tier.txt",    tier_summary, ttest_tier)
save_sink("output/tables/ttest_key_uni.txt", key_summary,  ttest_key_uni)

p_tier <- tier_summary %>%
  mutate(tier = factor(tier)) %>%
  ggplot(aes(x = tier, y = party_rate, fill = tier)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Tier", y = "Predicted party-ideology rate",
       title = "Party ideology by school tier",
       subtitle = sprintf("t = %.2f, p = %.3f",
                          ttest_tier$statistic, ttest_tier$p.value)) +
  theme_minimal()

ggsave("output/figures/party_rate_by_tier.png",
       p_tier, width = 6, height = 4.5, dpi = 200)

p_key <- key_summary %>%
  mutate(group = ifelse(key_uni == 1, "Key university", "Non-key")) %>%
  ggplot(aes(x = group, y = party_rate, fill = group)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("Key university" = "#B22222", "Non-key" = "#808080")) +
  labs(x = NULL, y = "Predicted party-ideology rate",
       title = "Party ideology: key vs. non-key universities",
       subtitle = sprintf("t = %.2f, p = %.3f",
                          ttest_key_uni$statistic, ttest_key_uni$p.value)) +
  theme_minimal()

ggsave("output/figures/party_rate_by_key_uni.png",
       p_key, width = 6, height = 4.5, dpi = 200)

message("05_analysis.R complete.")
