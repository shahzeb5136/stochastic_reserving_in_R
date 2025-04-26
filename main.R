#Needed Packages
#install.packages(c("shiny", "readxl", "dplyr", "lubridate", "tidyr", "ChainLadder", "DT", "writexl", "ggplot2"))

# Load necessary libraries
library(shiny)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ChainLadder) # Core package for reserving methods
library(DT)        # For interactive tables
library(writexl)   # For exporting to Excel
library(ggplot2)   # For plots (used by ChainLadder plots)
library(patchwork)

# Increase maximum upload size to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

# Define UI
ui <- fluidPage(
  titlePanel("Medical LOB Actuarial Reserving Tool (Mack Chain Ladder)"),
  
  sidebarLayout(
    sidebarPanel(
      # --- Added width argument here ---
      # You can adjust this value (e.g., 2, 3, 4). Default is 4.
      # The mainPanel will automatically use the remaining width (12 - sidebar width).
      width = 2, 
      
      fileInput("data_file", "Upload Claims Data (.xlsx)",
                accept = c(".xlsx")),
      
      # Dynamic UI for subcategory selection
      uiOutput("subcat_selector_ui"),
      
      # Button to trigger analysis
      actionButton("run_analysis", "Run Analysis", icon = icon("cogs")),
      
      # Horizontal line for separation
      hr(),
      
      # Download button - initially hidden or disabled until results are ready
      uiOutput("download_button_ui")
    ),
    
    mainPanel(
      # The mainPanel automatically gets width = 12 - sidebarPanel width
      # So if sidebarPanel width = 3, mainPanel width = 9
      tabsetPanel(id = "main_tabs",
                  tabPanel("1. Data Preview and Checks", DTOutput("raw_data_table")),
                  tabPanel("2. Cumulative Triangle", DTOutput("cumulative_triangle_table")),
                  tabPanel("3. Incremental Triangle", DTOutput("incremental_triangle_table")),
                  #tabPanel("3.5. Mack CL Summary", verbatimTextOutput("mack_summary")),
                  tabPanel("4. Reserve Estimates", DTOutput("reserve_estimates_table")),
                  tabPanel(
                    "5. Mack CL Plots", 
                    plotOutput("mack_plot1"),
                    plotOutput("mack_plot2")
                  ),
                  tabPanel("6. Recast Analysis"),
                  tabPanel("7. Seasonality Analysis")
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # --- 1. Data Input and Reactive Values ---
  
  # Reactive expression to read and initially process the uploaded data
  raw_data_reactive <- reactive({
    req(input$data_file) # Require a file to be uploaded
    
    # Validate file type (though 'accept' in fileInput helps)
    ext <- tools::file_ext(input$data_file$name)
    validate(need(ext == "xlsx", "Invalid file type. Please upload an .xlsx file."))
    
    # Read the excel file (assuming data is on the first sheet)
    df <- read_excel(input$data_file$datapath, sheet = 1)
    
    # --- Basic Validation ---
    required_cols <- c("OriginDate", "PaymentDate", "ClaimAmount", "SubCat")
    validate(need(all(required_cols %in% names(df)),
                  paste("Missing required columns. Ensure file contains:",
                        paste(required_cols, collapse = ", "))))
    
    # --- Data Type Conversion and Cleaning ---
    tryCatch({
      df <- df %>%
        # Convert potential character dates to Date objects
        # Adjust formats as necessary based on your Excel file
        mutate(
          OriginDate = as.Date(OriginDate),
          PaymentDate = as.Date(PaymentDate),
          ClaimAmount = as.numeric(ClaimAmount),
          SubCat = as.factor(SubCat) # Treat SubCat as a factor
        ) %>%
        
        # Remove rows with NA dates or claim amounts as they can't be processed
        filter(!is.na(OriginDate), !is.na(PaymentDate), !is.na(ClaimAmount)) %>%
        
        # Ensure payments don't precede origin
        filter(PaymentDate >= OriginDate)
      
      #Check dataaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      validate(
        need(nrow(df) > 0, paste("No data available for the selected subcategory")),
        need(
          nrow(df) == nrow(unique(df[, c("OriginDate", "PaymentDate", "SubCat")])),
          "Error: There are multiple entries for the same Origin Date, Payment Date, and Subcategory combination. The tool expects aggregated data."
        )
      )
      
      return(df)
      
    }, error = function(e) {
      # If conversion fails, show an error
      validate(paste("Error processing data:", e$message))
      return(NULL)
    })
  })
  
  # --- 2. Dynamic UI Elements ---
  
  # Render the SubCat selector based on the uploaded data
  output$subcat_selector_ui <- renderUI({
    df <- raw_data_reactive()
    req(df) # Require data before rendering
    
    sub_categories <- levels(df$SubCat) # Get unique subcategories
    choices <- c("All Categories", sort(sub_categories))
    
    selectInput("subcat_filter", "Select Subcategory:", choices = choices)
  })
  
  # Render the download button conditionally
  output$download_button_ui <- renderUI({
    # Only show download button after analysis results are available
    req(analysis_results())
    downloadButton("download_results", "Download Results (.xlsx)", icon = icon("download"))
  })
  
  
  # --- 3. Core Analysis Logic (Triggered by Button) ---
  
  # Reactive expression holding the results of the analysis
  # eventReactive waits for input$run_analysis button press
  analysis_results <- eventReactive(input$run_analysis, {
    
    # Require necessary inputs before proceeding
    req(raw_data_reactive(), input$subcat_filter)
    showNotification("Running analysis...", type = "message", duration = 2)
    
    df_raw <- raw_data_reactive()
    selected_subcat <- input$subcat_filter
    
    # Filter data based on selection
    if (selected_subcat != "All Categories") {
      df_filtered <- df_raw %>% filter(SubCat == selected_subcat)
    } else {
      df_filtered <- df_raw # Use all data
    }
    
    validate(need(nrow(df_filtered) > 0, paste("No data available for the selected subcategory:", selected_subcat)))
    
    # --- Data Preparation for Triangles (Monthly) ---
    df_processed <- df_filtered %>%
      mutate(
        OriginMonth = floor_date(OriginDate, "month"), # Start of the origin month
        PaymentMonth = floor_date(PaymentDate, "month"), # Start of the payment month
        # Calculate Development Month Lag (integer)
        # Add 1 because the first month is DevMonth 1
        DevMonth = interval(OriginMonth, PaymentMonth) %/% months(1) + 1
      ) %>%
      # Group by origin and development month to get incremental payments
      group_by(OriginMonth, DevMonth) %>%
      summarise(IncrementalClaims = sum(ClaimAmount, na.rm = TRUE), .groups = 'drop') %>%
      # Ensure sorted correctly for cumulative calculation
      arrange(OriginMonth, DevMonth)
    
    # --- Create Incremental Triangle ---
    # Using pivot_wider to create the wide format
    incr_triangle_df <- df_processed %>%
      pivot_wider(names_from = DevMonth,
                  values_from = IncrementalClaims,
                  names_prefix = "Dev_") # Add prefix for clarity
    
    # Convert to matrix suitable for ChainLadder package
    # Ensure OriginMonth is the first column for later use
    origin_months <- incr_triangle_df$OriginMonth
    incr_triangle_matrix <- as.matrix(select(incr_triangle_df, -OriginMonth))
    rownames(incr_triangle_matrix) <- as.character(origin_months) # Set rownames
    
    # --- Create Cumulative Triangle ---
    # Calculate cumulative claims within each origin period
    df_cumulative <- df_processed %>%
      group_by(OriginMonth) %>%
      mutate(CumulativeClaims = cumsum(IncrementalClaims)) %>%
      ungroup()
    
    cum_triangle_df <- df_cumulative %>%
      select(OriginMonth, DevMonth, CumulativeClaims) %>%
      pivot_wider(names_from = DevMonth,
                  values_from = CumulativeClaims,
                  names_prefix = "Dev_") # Add prefix
    
    # Convert to matrix suitable for ChainLadder package
    cum_triangle_matrix <- as.matrix(select(cum_triangle_df, -OriginMonth))
    rownames(cum_triangle_matrix) <- as.character(origin_months) # Ensure rownames match
    
    # Handle potential single-row triangles (may cause issues with CL methods)
    validate(need(nrow(cum_triangle_matrix) > 1, "Chain Ladder methods require at least 2 origin periods."))
    validate(need(ncol(cum_triangle_matrix) > 1, "Chain Ladder methods require at least 2 development periods."))
    
    # --- Perform Mack Chain Ladder Analysis ---
    # Use the *cumulative* triangle for MackChainLadder
    # Set est.sigma to "Mack" for Mack's estimation; others exist (e.g., "log-linear")
    mack_model <- tryCatch({
      MackChainLadder(Triangle = cum_triangle_matrix, est.sigma = "Mack")
    }, error = function(e){
      validate(paste("Error during MackChainLadder calculation:", e$message,
                     "\nCheck triangle structure (e.g., sufficient data, no zeros on diagonal if using certain methods)."))
      return(NULL) # Return NULL if error occurs
    })
    
    req(mack_model) # Require the model to be successfully created
    
    
    # --- Extract Results ---
    mack_summary_obj <- summary(mack_model)
    mack_summary_text <- capture.output(mack_summary_obj) # Capture text summary
    
    # Create a more structured data frame for reserve estimates
    reserve_estimates_df <- data.frame(
      Origin = rownames(mack_summary_obj$ByOrigin),
      Latest = mack_summary_obj$ByOrigin[, "Latest"],
      Dev.To.Date = mack_summary_obj$ByOrigin[, "Dev.To.Date"],
      Ultimate = mack_summary_obj$ByOrigin[, "Ultimate"],
      IBNR = mack_summary_obj$ByOrigin[, "IBNR"],
      Std.Err = mack_summary_obj$ByOrigin[, "Mack.S.E"],
      CV = mack_summary_obj$ByOrigin[, "CV(IBNR)"]
    )
    # Add Totals row
    totals <- data.frame(
      Origin = "Totals",
      Latest = mack_summary_obj$Totals["Latest",],
      Dev.To.Date = NA, # Not applicable for total
      Ultimate = mack_summary_obj$Totals["Ultimate",],
      IBNR = mack_summary_obj$Totals["IBNR",],
      Std.Err = mack_summary_obj$Totals["Mack.S.E",],
      CV = mack_summary_obj$Totals["CV(IBNR)",]
    )
    reserve_estimates_df <- bind_rows(reserve_estimates_df, totals)
    
    # Prepare Incremental Triangle for Output/Download (convert back to df)
    incr_triangle_out_df <- as.data.frame(incr_triangle_matrix)
    incr_triangle_out_df <- tibble::rownames_to_column(incr_triangle_out_df, "OriginMonth")
    
    # Prepare Cumulative Triangle for Output/Download
    cum_triangle_out_df <- as.data.frame(mack_model$Triangle) # Use triangle from model object
    cum_triangle_out_df <- tibble::rownames_to_column(cum_triangle_out_df, "OriginMonth")
    
    
    # Return results as a list
    return(list(
      selected_subcat = selected_subcat,
      raw_data_filtered = df_filtered,
      incremental_triangle_df = incr_triangle_out_df,
      cumulative_triangle_df = cum_triangle_out_df,
      mack_model = mack_model,
      mack_summary_text = paste(mack_summary_text, collapse = "\n"), # Combine lines
      reserve_estimates_df = reserve_estimates_df,
      full_cumulative_triangle = as.data.frame(mack_model$FullTriangle) %>% # For potential later use
        tibble::rownames_to_column("OriginMonth")
      
    ))
  })
  
  # --- 4. Render Outputs ---
  
  # Display Raw Data Table
  output$raw_data_table <- renderDT({
    req(raw_data_reactive())
    datatable(raw_data_reactive(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Display Cumulative Triangle
  output$cumulative_triangle_table <- renderDT({
    results <- analysis_results() # Triggered by button
    req(results)
    datatable(results$cumulative_triangle_df,
              rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = nrow(results$cumulative_triangle_df), dom = 't'), # Show all rows, no filter/pages
              caption = paste("Cumulative Claims Triangle - Subcategory:", results$selected_subcat)) %>%
      formatRound(columns = which(sapply(results$cumulative_triangle_df, is.numeric)), digits = 0) # Format numbers
  })
  
  # Display Incremental Triangle
  output$incremental_triangle_table <- renderDT({
    results <- analysis_results()
    req(results)
    datatable(results$incremental_triangle_df,
              rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = nrow(results$incremental_triangle_df), dom = 't'),
              caption = paste("Incremental Claims Triangle - Subcategory:", results$selected_subcat)) %>%
      formatRound(columns = which(sapply(results$incremental_triangle_df, is.numeric)), digits = 0)
  })
  
  # Display Mack Model Summary Text
  output$mack_summary <- renderText({
    results <- analysis_results()
    req(results)
    results$mack_summary_text
  })
  
  # Display Reserve Estimates Table
  output$reserve_estimates_table <- renderDT({
    results <- analysis_results()
    req(results)
    datatable(results$reserve_estimates_df,
              rownames = FALSE,
              options = list(
                scrollX = TRUE,
                pageLength = nrow(results$reserve_estimates_df), # Show all rows
                dom = 't',
                # Assuming 'origindate' is the first column (index 0)
                order = list(list(0, 'desc'))
              ),
              caption = paste("Mack Chain Ladder Reserve Estimates - Subcategory:", results$selected_subcat)) %>%
      formatRound(columns = c('Latest', 'Ultimate', 'IBNR', 'Std.Err'), digits = 0) %>%
      formatPercentage(columns = c('CV'), digits = 2) %>%
      formatRound(columns = c('Dev.To.Date'), digits = 3)
    
  })
  
  # Display Mack Model Plots
  output$mack_plot1 <- renderPlot({
    results <- analysis_results()
    req(results, results$mack_model)
    
    # --- Temporarily disable scientific notation for this plot ---
    # 1. Store the current 'scipen' option value
    old_scipen <- getOption("scipen")
    
    # 2. Ensure the original option is restored when renderPlot finishes or errors
    # 'add = TRUE' is important if other on.exit calls exist or might be added
    on.exit(options(scipen = old_scipen), add = TRUE)
    
    # 3. Set 'scipen' to a large positive value to strongly prefer fixed notation
    options(scipen = 999)
    # -------------------------------------------------------------
    
    # Generate the plot 
    plot(results$mack_model, which = 1)
  })
  
  output$mack_plot2 <- renderPlot({
    results <- analysis_results()
    req(results, results$mack_model)
    
    # --- Temporarily disable scientific notation for this plot ---
    # 1. Store the current 'scipen' option value
    old_scipen <- getOption("scipen")
    
    # 2. Ensure the original option is restored when renderPlot finishes or errors
    # 'add = TRUE' is important if other on.exit calls exist or might be added
    on.exit(options(scipen = old_scipen), add = TRUE)
    
    # 3. Set 'scipen' to a large positive value to strongly prefer fixed notation
    options(scipen = 999)
    # -------------------------------------------------------------
    
    # Generate the plot
    plot(results$mack_model, which = 2)
  })
  
  # --- 5. Download Handler ---
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("Mack_CL_Results_", input$subcat_filter, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      results <- analysis_results()
      req(results) # Ensure results exist
      
      # Prepare summary text for export (optional, can be long)
      # Convert the single string back into lines for a cleaner look in Excel
      summary_df <- data.frame(Summary = strsplit(results$mack_summary_text, "\n")[[1]])
      
      
      # List of data frames to write to Excel sheets
      sheets_list <- list(
        "Cumulative Triangle" = results$cumulative_triangle_df,
        "Incremental Triangle" = results$incremental_triangle_df,
        "Reserve Estimates" = results$reserve_estimates_df
        # "Full Cumulative Triangle" = results$full_cumulative_triangle # Optional: export completed triangle
      )
      
      # Write to Excel
      write_xlsx(sheets_list, path = file)
      
      showNotification("Results successfully downloaded.", type = "message", duration = 5)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)