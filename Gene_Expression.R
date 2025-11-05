#Libraries required 
library(tibble)
library(dplyr)
library(ggplot2)

#Set seed
set.seed(123)

#Gene expression data
data <- tibble(
  gene = rep(paste0("Gene_", 1:10), each = 6),
  condition = rep(c("Control", "Treatment"), each = 3, times = 10),
  expression_level = rnorm(60, mean = 10, sd = 2)
)

#Summarize data by condition 
summary_data <- data %>%
  group_by(condition) %>%
  summarise(
    mean_expression = mean(expression_level),
    sd_expression = sd(expression_level)
  )

#Print summary table
print(summary_data)

#Plot: Average Expression per Condition
ggplot(summary_data, aes(x = condition, y = mean_expression, fill = condition)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = mean_expression - sd_expression,
                    ymax = mean_expression + sd_expression),
                width = 0.2) +
  labs(title = "Average Gene Expression per Condition",
       x = "Condition", y = "Mean Expression Level") +
  theme_minimal() +
  theme(legend.position = "none")

#Save Plot to File
ggsave("gene_expression_plot.png", width = 6, height = 4)

#Faceted Plot: Expression by Gene
ggplot(data, aes(x = condition, y = expression_level, fill = condition)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  facet_wrap(~ gene) +
  labs(title = "Expression Levels by Gene and Condition",
       x = "Condition", y = "Expression Level") +
  theme_minimal()
