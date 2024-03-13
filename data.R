library(tidyverse)

telecom_data <- read.csv('./telecom.csv')

cat("Initial Data Summary:\n")
str(telecom_data)
summary(telecom_data)

cat("Missing Values:\n")
sum(is.na(telecom_data))

internet_service_plot <- ggplot(telecom_data, aes(x = InternetService)) +
  geom_bar() +
  labs(title = "Distribution of Internet Service Type",
       x = "Internet Service Type",
       y = "Count")
ggsave("internet_service_distribution.png", plot = internet_service_plot)

monthly_charges_plot <- ggplot(telecom_data, aes(y = MonthlyCharges)) +
  geom_boxplot() +
  labs(title = "Distribution of Monthly Charges",
       y = "Monthly Charges")
ggsave("monthly_charges_distribution.png", plot = monthly_charges_plot)

churn_status_plot <- ggplot(telecom_data, aes(x = Churn, y = MonthlyCharges, fill = Churn)) +
  geom_boxplot() +
  labs(title = "Monthly Charges by Churn Status",
       x = "Churn Status",
       y = "Monthly Charges")
ggsave("monthly_charges_by_churn_status.png", plot = churn_status_plot)