# stochastic_reserving_in_R
A short and sweet R Shiny application to carry out stochastic reserving using Mack Chain Ladder methods.

This application is built to allow actuaries, analysts, and insurance professionals to:
  Upload and validate claims data.
  Construct incremental and cumulative triangles.
  Apply the Mack Chain Ladder method.
  Generate reserve estimates with uncertainty measures.
  Visualize diagnostic plots.
  Download all results in an Excel-friendly format.

-- Required R packages
install.packages(c(
  "shiny", "readxl", "dplyr", "lubridate", "tidyr",
  "ChainLadder", "DT", "writexl", "ggplot2", "patchwork"
))

-- How it works
Upload your claims data: The file must contain the columns OriginDate, PaymentDate, ClaimAmount, and SubCat in that order and column name.
Select a subcategory or run analysis across all categories.
Run the Mack Chain Ladder Analysis with the click of a button.
Review the results in organized tabs:
  Data Preview
  Cumulative Triangle
  Incremental Triangle
  Reserve Estimates
  Mack Chain Ladder Plots
  Download results into a clean Excel workbook for further analysis.

