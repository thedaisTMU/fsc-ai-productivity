# Load necessary libraries
library(data.table)
library(did)
library(ggplot2)

# Load the synthetic dataset
data <- fread('C:/Users/graham.dobbs/OneDrive - Toronto Metropolitan University (RU)/Documents/GitHub/fsc-ai-productivity/synthetic_brm_data.csv')
# Create additional variables if needed
data[, time := as.integer(format(as.Date(FiscalEndDate), "%Y"))]

# Ensure we have some pre-treatment periods
data[, treat := ifelse(time >= 2005, 1, 0)]
data[, first_treat := ifelse(treat == 1, min(time[treat == 1]), 0), by = Entid]

# Create a mix of pre-treatment and post-treatment periods
set.seed(123)
data[, first_treat := ifelse(first_treat == 0, sample(2003:2004, .N, replace = TRUE), first_treat)]

# Adjust control group and ensure validity
data[, first_treat := ifelse(first_treat == 0, max(time) + 1, first_treat)]

# Run Difference-in-Differences analysis using the 'did' package
results <- att_gt(
  yname = "total_revenue",
  tname = "time",
  idname = "Entid",
  gname = "first_treat",
  xformla = NULL,
  data = data,
  control_group = "notyettreated",
  panel =  FALSE
)

# Summarize results
summary_results <- summary(results)
print(summary_results)

# Plot results using ggdid
ggdid(results)

write.csv(data,'C:/Users/graham.dobbs/OneDrive - Toronto Metropolitan University (RU)/Documents/GitHub/fsc-ai-productivity/synthetic_brm_data.csv')
