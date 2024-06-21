# Install necessary packages if not already installed
if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

# Load necessary libraries
library(data.table)
library(lubridate)

# Define the number of records
num_records <- 1000
unique_ids <- 200
TS <- num_records / unique_ids
years <- 2016:2020

# Set seed for reproducibility
set.seed(123)

# Generate the synthetic dataset
data <- data.table(
  Entid = rep(1:unique_ids, each = TS),
  time =  rep(years, times = unique_ids),
  T4_Payroll = sample(50000:5000000, num_records, replace = TRUE),
  PD7_AvgEmp_12 = sample(1:1000, num_records, replace = TRUE),
  PD7_AvgEmp_NonZero = sample(1:1000, num_records, replace = TRUE),
  total_assets = sample(100000:10000000, num_records, replace = TRUE),
  total_liabilities = sample(50000:5000000, num_records, replace = TRUE),
  total_shareholder_equity = sample(50000:5000000, num_records, replace = TRUE),
  total_current_assets = sample(50000:5000000, num_records, replace = TRUE),
  total_tangible_assets = sample(50000:5000000, num_records, replace = TRUE),
  tot_acum_amort_tangible_assets = sample(1000:500000, num_records, replace = TRUE),
  total_intangible_assets = sample(1000:500000, num_records, replace = TRUE),
  tot_acum_amort_intang_assets = sample(1000:500000, num_records, replace = TRUE),
  total_current_liabilities = sample(50000:5000000, num_records, replace = TRUE),
  land = sample(1000:500000, num_records, replace = TRUE),
  buildings = sample(1000:500000, num_records, replace = TRUE),
  machinery_and_equipment = sample(1000:500000, num_records, replace = TRUE),
  total_revenue = sample(100000:10000000, num_records, replace = TRUE),
  total_expenses = sample(50000:5000000, num_records, replace = TRUE),
  farm_total_revenue = sample(1000:500000, num_records, replace = TRUE),
  farm_total_expenses = sample(1000:500000, num_records, replace = TRUE),
  farm_net_income = sample(1000:500000, num_records, replace = TRUE),
  total_cost_of_sales = sample(1000:500000, num_records, replace = TRUE),
  gross_profits = sample(1000:500000, num_records, replace = TRUE),
  net_income_befor_taxextraitems = sample(1000:500000, num_records, replace = TRUE),
  sales_goods_and_services = sample(1000:500000, num_records, replace = TRUE),
  net_income_after_taxextraitems = sample(1000:500000, num_records, replace = TRUE),
  opening_inventory = sample(1000:500000, num_records, replace = TRUE),
  closing_inventory = sample(1000:500000, num_records, replace = TRUE),
  total_operating_expenses = sample(1000:500000, num_records, replace = TRUE),
  amortization_tangible_assets = sample(1000:500000, num_records, replace = TRUE),
  amortization_intangible_assets = sample(1000:500000, num_records, replace = TRUE),
  SRED_Expenditures = sample(1000:500000, num_records, replace = TRUE),
  SRED_ITC_Earned = sample(1000:500000, num_records, replace = TRUE),
  SRED_ITC_Current_at_35Percent = sample(1000:500000, num_records, replace = TRUE),
  SRED_ITC_Capital_at_35Percent = sample(1000:500000, num_records, replace = TRUE),
  SRED_ITC_Current_at_20Percent = sample(1000:500000, num_records, replace = TRUE),
  SRED_ITC_Capital_at_20Percent = sample(1000:500000, num_records, replace = TRUE),
  SRED_Deducted_PartI = sample(1000:500000, num_records, replace = TRUE),
  SRED_from_partnership = sample(1000:500000, num_records, replace = TRUE),
  SRED_refunded = sample(1000:500000, num_records, replace = TRUE),
  SRED_carried_back_1year = sample(1000:500000, num_records, replace = TRUE),
  SRED_carried_back_2years = sample(1000:500000, num_records, replace = TRUE),
  SRED_carried_back_3years = sample(1000:500000, num_records, replace = TRUE),
  OPAddressProvince = sample(c('ON', 'QC', 'BC', 'AB', 'MB', 'SK', 'NS', 'NB', 'NL', 'PE', 'NT', 'YT', 'NU'), num_records, replace = TRUE),
  LegalTypeCode = sample(c(1, 2, 3, 4, 5, 6, 9), num_records, replace = TRUE),
  NonProfitCode = sample(c(0, 1, 2), num_records, replace = TRUE),
  NAICS = sample(11:99, num_records, replace = TRUE),
  EntMultiEstablishmentFlag = sample(c(0, -1), num_records, replace = TRUE),
  EntMultiLocationFlag = sample(c(0, -1), num_records, replace = TRUE),
  EntMultiProvinceFlag = sample(c(0, -1), num_records, replace = TRUE),
  EntMultiActivityFlag = sample(c(0, -1), num_records, replace = TRUE),
  FiscalStartDate = sample(seq(as.Date('2012-01-01'), as.Date('2024-12-31'), by="day"), num_records, replace = TRUE),
  FiscalEndDate = sample(seq(as.Date('2012-01-01'), as.Date('2024-12-31'), by="day"), num_records, replace = TRUE),
  BirthDate = sample(seq(as.Date('1970-01-01'), as.Date('2000-12-31'), by="day"), num_records, replace = TRUE),
  BusinessStatusCode = sample(0:7, num_records, replace = TRUE),
  IncorporationDate = sample(seq(as.Date('1970-01-01'), as.Date('2000-12-31'), by="day"), num_records, replace = TRUE),
  Purchases_cost_of_materials = sample(1000:500000, num_records, replace = TRUE),
  capital_cost_allowance = sample(1000:500000, num_records, replace = TRUE),
  NbBN_filedT4 = sample(1:10, num_records, replace = TRUE),
  NbBN_filedPD7 = sample(1:10, num_records, replace = TRUE),
  NbBN_filedT2 = sample(1:10, num_records, replace = TRUE),
  CCPC = sample(c(0, 1), num_records, replace = TRUE)
)

# assign half of sample to treatment group
data[, treat := ifelse(Entid %% 2 == 1, 1, 0)]

# first treat: random assignment between two years for treatment group
data[, first_treat := ifelse(treat == 1, sample(2018:2019, length(unique_ids), replace = TRUE), 0), by = Entid]

# Save to CSV
fwrite(data,'new_synthetic_brm_data.csv')

#run DiD
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



