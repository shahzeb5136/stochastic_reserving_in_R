# Load the libraries
library(dplyr)
library(readr)
library(openxlsx)

# Set the number of rows
num_rows <- 500000

# --- Generate Random Data ---

# Generate random OriginDates and PaymentDates between 2015 and 2020
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2020-12-31")
date_range_days <- as.numeric(end_date - start_date)

OriginDate <- start_date + sample(0:date_range_days, num_rows, replace = TRUE)
PaymentDate <- start_date + sample(0:date_range_days, num_rows, replace = TRUE)

# Generate random ClaimAmount from a Gamma distribution
# Adjust shape and scale parameters to get a distribution roughly between 0 and 100k
# The mean of a Gamma distribution is shape * scale
# Let's aim for a mean around 20k-30k and scale it up
gamma_shape <- 2 # Adjust shape for desired distribution shape
gamma_scale <- 10000 # Adjust scale to get values in the desired range
ClaimAmount <- rgamma(num_rows, shape = gamma_shape, scale = gamma_scale)

# Ensure ClaimAmount is positive (Gamma distribution naturally produces positive values)
# If you need to strictly cap at 100k, you could add a filter or transformation,
# but rgamma naturally produces values above the mean, so some will exceed 100k.
# If a strict cap is needed: ClaimAmount <- pmin(ClaimAmount, 100000)

# Generate random SubCat (A, B, or C)
SubCat <- sample(c("A", "B", "C"), num_rows, replace = TRUE)

# Create the data frame
random_data <- data.frame(
  OriginDate = OriginDate,
  PaymentDate = PaymentDate,
  ClaimAmount = ClaimAmount,
  SubCat = SubCat
)

# Display the first few rows of the generated data
print("Generated Random Data (first 6 rows):")
print(head(random_data))

# --- Aggregate the Data ---

# Aggregate ClaimAmount by OriginDate, PaymentDate, and SubCat
aggregated_data <- random_data %>%
  group_by(OriginDate, PaymentDate, SubCat) %>%
  summarise(TotalClaimAmount = sum(ClaimAmount), .groups = 'drop') # .groups = 'drop' removes grouping after summarizing

# Display the first few rows of the aggregated data
print("Aggregated Data (first 6 rows):")
print(head(aggregated_data))

# --- Save to Excel File ---

# Define the file path
excel_file_path <- "C:\\Users\\user\\Desktop\\Model\\02. Demos\\xx. Reserving Assiatant R\\aggregated_claims.xlsx"

# Write the aggregated data to an Excel file
tryCatch({
  write.xlsx(aggregated_data, excel_file_path)
  print(paste("Aggregated data successfully saved to", excel_file_path))
}, error = function(e) {
  print(paste("Error saving data to Excel:", e$message))
  print("Please ensure you have the 'openxlsx' package installed and write permissions for the directory.")
})

