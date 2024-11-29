library(naniar) # https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
library(tidyverse)
library(plotly)


data <- readRDS("data/nda_clean.rds")


# Summarize missingness
miss_var_summary(data)

# Plot the number of missings for each variable
gg_miss_var(data)

# Plot the pct of missings for each variable
gg_miss_var(data, show_pct = TRUE)

# Plot the pct of missings for each variable grouped by another variable
gg_miss_var(data, show_pct = TRUE, facet = gender)

gg_miss_var(data, show_pct = TRUE, facet = diabetes_type)

gg_miss_var(data, show_pct = TRUE, facet = bmi_category)

# Plot the distribution of missing values per row (case)
gg_miss_case(data)

# Missingness across patient id
gg_miss_fct(data %>% 
              slice_sample(n = 10000), fct = patient_id) +
  theme(
    axis.text.x = element_blank(),  # Remove X-axis labels
    axis.ticks.x = element_blank()  # Remove X-axis tick marks
  ) +
  labs(title = "Missing Data Across Variables and Patients")

# Check which variables are missing together - indicative of systematic missingness
gg_miss_upset(data)

