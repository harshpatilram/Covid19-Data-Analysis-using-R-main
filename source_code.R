# Problem Statement: Analyse the real-world COVID-19 dataset 
# and find countries have the highest rates of positive COVID-19 tests relative to their testing numbers. 

# Dataset: https://www.kaggle.com/datasets/lin0li/covid19testing

rm(list = ls())

# importing...
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)

# Loading and validating the dataset
covid_df <- read_csv("tested_worldwide.csv")

cat("Dataset dimensions:", dim(covid_df), "\n")
str(covid_df)
cat("\nNumber of unique countries:", length(unique(covid_df$Country_Region)))
cat("\nDate range:", range(covid_df$Date), "\n")

#checking missing values
colSums(is.na(covid_df))

# checking negative values in numeric columns
negative_counts <- covid_df %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~sum(.x < 0, na.rm = TRUE))) %>%
  unlist()

negative_counts


# Cleaning the data
# 1. date column <- date format
# 2. cleaning negative, missing and infinite values if any

covid_df <- covid_df %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(across(where(is.numeric), ~abs(.x))) %>%
  mutate(across(where(is.numeric), ~replace(.x, is.infinite(.x), NA)))

covid_df_clean <- covid_df %>%
  filter(!is.na(daily_tested), !is.na(daily_positive)) %>%
  filter(Province_State == "All States") %>%
  select(-Province_State,-total_tested)

glimpse(covid_df_clean)

# Calculate positivity rates by country
country_summary <- covid_df_clean %>%
  group_by(Country_Region) %>%
  summarise(
    total_tested = sum(daily_tested),
    total_positive = sum(daily_positive),
    positivity_rate = case_when(
      total_tested == 0 ~ 0,
      TRUE ~ total_positive / total_tested
    )) %>%
  arrange(desc(positivity_rate))

glimpse(country_summary)

# Get top 10 countries
top_10_countries <- head(country_summary, 10)
top_10_countries

# visualizing positivity rate for top 10 countries
top_10_plot <- ggplot(top_10_countries, 
  aes(x = reorder(Country_Region, -positivity_rate), 
  y = positivity_rate)) +
  
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_bw() +
  labs(title = "Most Affected Countries by Covid-19",
       x = "Country",
       y = "Positivity Rate")+
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = rel(1.0)))

top_10_plot

# summary statistics for top 10 countries
top_10_stats <- top_10_countries %>%
  summarise(
    avg_positivity = mean(positivity_rate),
    max_positivity = max(positivity_rate),
    min_positivity = min(positivity_rate)
  )
top_10_stats

# correlation matrix
correlation_matrix <- country_summary %>%
  select(total_tested, total_positive) %>%
  cor(use = "complete.obs")
correlation_matrix

# output list
results <- list(
  top_10_countries = top_10_countries,
  summary_statistics = top_10_stats,
  visualization = top_10_plot
)

cat("\nKey Findings:\n")

cat("\nTop 5 Countries by Positivity Rate:\n")
print(head(top_10_countries %>%
             select(Country_Region, positivity_rate) %>%
             mutate(positivity_rate = scales::percent(positivity_rate, accuracy = 0.01)), 5))

cat("\nSummary Statistics for Top 10 Countries:\n")
print(top_10_stats %>%
        mutate(across(everything(), ~scales::percent(., accuracy = 0.01))))

