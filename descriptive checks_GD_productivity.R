
# Load necessary libraries
library(data.table)
library(lubridate)
library(ggplot2)

data <- fread("C:/Users/GCD/OneDrive/Documents/GitHub/fsc-ai-productivity/synthetic_brm_data.csv")

##################################################################################

# Create a random sample
# Extract the year from the FiscalEndDate column to use for stratified sampling
data[, year := as.integer(format(as.Date(FiscalEndDate), "%Y"))]



# Perform stratified random sampling with equal weight for each year
sampled_data <- data[, .SD[sample(.N, round(0.10 * .N))], by = year]


# View the sampled data
print(sampled_data)


##################################################################################
##################################################################################
# Identify numeric columns only
numeric_cols <- names(sampled_data)[sapply(sampled_data, is.numeric)]

# Summarize only the numeric columns
summary_stats <- sampled_data[, lapply(.SD, function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}), .SDcols = numeric_cols]

# Transpose the summary statistics
summary_stats_t <- transpose(summary_stats)

# Add column names for easier interpretation
setnames(summary_stats_t, c("mean", "median", "sd", "min", "max"))

# Add row names for variables
summary_stats_t[, Variable := numeric_cols]

# Reorder columns to have "Variable" first
setcolorder(summary_stats_t, c("Variable", "mean", "median", "sd", "min", "max"))

# View summary statistics
print(summary_stats_t)
##################################################################################

# Check for duplicate column names
duplicate_cols <- duplicated(names(sampled_data))
if (any(duplicate_cols)) {
  print("Removing duplicate columns")
  sampled_data <- sampled_data[, !duplicate_cols, with = FALSE]
}

# List of selected variables for which we want to see the distribution
selected_variables <- c("T4_Payroll", "total_assets", "total_liabilities", "total_revenue", "PD7_AvgEmp_12")

# Create a histogram for each selected variable
for (var in selected_variables) {
  p <- ggplot(sampled_data, aes(x = !!sym(var))) +
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal()
  
  # Save the plot as a PNG file
  ggsave(paste0(var, "_histogram.png"), plot = p)
}


##################################################################################
# Identify numeric columns only (excluding total_revenue)
numeric_cols <- names(sampled_data)[sapply(sampled_data, is.numeric) & names(sampled_data) != "total_revenue"]

# Check for duplicate column names
duplicate_cols <- duplicated(names(sampled_data))
if (any(duplicate_cols)) {
  print("Removing duplicate columns")
  sampled_data <- sampled_data[, !duplicate_cols, with = FALSE]
}

# Create cross-tabulations and scatter plots
for (var in selected_variables) {
  # Cross-tabulation
  cross_tab <- table(cut(data[[var]], breaks = 5), cut(data$total_revenue, breaks = 5))
  print(paste("Cross-tabulation of total_revenue and", var))
  print(cross_tab)
  
  # Scatter plot
  p <- ggplot(sampled_data, aes(x = !!sym(var), y = total_revenue)) +
    geom_point(alpha = 0.5, color = "blue") +
    labs(title = paste("Scatter plot of total_revenue vs", var), x = var, y = "Total Revenue") +
    theme_minimal()
  
  # Save the plot as a PNG file
  ggsave(paste0("scatter_total_revenue_vs_", var, ".png"), plot = p)
}
##################################################################################
##################################################################################
##################################################################################
#run DiD
results <- att_gt(
  yname = "total_revenue",
  tname = "time",
  idname = "Entid",
  gname = "first_treat",
  xformla = NULL,
  sampled_data = sampled_data,
  control_group = "notyettreated",
  panel =  FALSE
)

##################################################################################
# Summarize results
summary_results <- summary(results)
print(summary_results)

##################################################################################
# Plot results using ggdid
ggdid(results)
