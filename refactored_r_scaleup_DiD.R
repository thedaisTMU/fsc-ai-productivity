# Load necessary libraries
library(data.table)
library(did)
library(ggplot2)

# Load the synthetic dataset
data <- fread('C:/Users/graham.dobbs/OneDrive - Toronto Metropolitan University (RU)/Documents/GitHub/fsc-ai-productivity/synthetic_brm_data.csv')

# Create additional variables if needed
# Assuming creation of new variables based on operations in the STATA file

# Example: Create a treatment indicator and time variable (modify as per .do file)
data[, treat := ifelse(FiscalEndDate >= '2005-01-01', 1, 0)]
data[, time := as.integer(format(as.Date(FiscalEndDate), "%Y"))]

# Assuming outcome variable is 'total_revenue' and id variable is 'Entid'
# Modify these variable names as per the .do file specifics

# Run Difference-in-Differences analysis using the 'did' package
results <- att_gt(
  yname = "total_revenue",
  tname = "time",
  idname = "Entid",
  gname = "treat",
  xformla = NULL,
  data = data,
  panel = FALSE
)

# Summarize results
summary(results)

# Plot results using ggdid
ggdid(results)


# Load necessary libraries
library(data.table)
library(did)

# Load the synthetic dataset
data <- fread('synthetic_brm_data_v2.csv')

# Create additional variables if needed
# Assuming creation of new variables based on operations in the STATA file

# Example: Create a treatment indicator and time variable (modify as per .do file)
data[, treat := ifelse(FiscalEndDate >= '2005-01-01', 1, 0)]
data[, time := as.integer(format(as.Date(FiscalEndDate), "%Y"))]

# Assuming outcome variable is 'total_revenue' and id variable is 'Entid'
# Modify these variable names as per the .do file specifics

# Run Difference-in-Differences analysis using the 'did' package
results <- att_gt(
  yname = "total_revenue",
  tname = "time",
  idname = "Entid",
  gname = "treat",
  xformla = NULL,
  data = data
)

# Summarize results
summary(results)

# Plot results using ggdid
ggdid(results)

# Installing necessary packages if not already installed
if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}
if (!requireNamespace("did", quietly = TRUE)) {
  install.packages("did")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Load necessary libraries
library(data.table)
library(did)
library(ggplot2)

# Load the synthetic dataset
data <- fread('synthetic_brm_data_v2.csv')

# Create additional variables if needed
data[, treat := ifelse(FiscalEndDate >= '2005-01-01', 1, 0)]
data[, time := as.integer(format(as.Date(FiscalEndDate), "%Y"))]

# Run Difference-in-Differences analysis using the 'did' package
results <- att_gt(
  yname = "total_revenue",
  tname = "time",
  idname = "Entid",
  gname = "treat",
  xformla = NULL,
  data = data
)

# Summarize results
summary(results)

# Plot results using ggdid
ggdid(results)

