library(knitr)
library(dplyr)
install.packages("kableExtra")
library(kableExtra)
# Perform pairwise Wilcoxon signed-rank test for columns W4 to W24
wilcox_results <- combn(colnames(e)[4:ncol(e)], 2, function(pair) {
  x <- e[, pair[1]]
  y <- e[, pair[2]]
  if(all(x == y)) {
    return(data.frame(Column1 = pair[1], Column2 = pair[2], P_Value = "> 0.05", Significance_Difference = "No"))
  } else {
    test_result <- wilcox.test(x, y, paired = TRUE, exact = FALSE, correct = TRUE)
    return(data.frame(Column1 = pair[1], Column2 = pair[2], P_Value = test_result$p.value, Significance_Difference = ifelse(test_result$p.value < 0.05, "Yes", "No")))
  }
}, simplify = FALSE)
# Combine the results into a single dataframe
wilcox_results_df <- do.call(rbind, wilcox_results)
# Format the p-values and add color highlighting for significant differences
wilcox_results_df <- wilcox_results_df %>%
  mutate(P_Value = ifelse(is.nan(P_Value), "", formatC(P_Value, format = "e", digits = 2)),
         Significance_Difference = ifelse(Significance_Difference == "Yes", "Yes", "No"))
# Print the formatted dataframe in a table format
kable(wilcox_results_df, format = "html", escape = FALSE, caption = "Pairwise Wilcoxon Signed-Rank Test Results for Columns W4 to W24") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(4, color = "white", background = ifelse(wilcox_results_df$Significance_Difference == "Yes", "green", "red"), bold = TRUE)
