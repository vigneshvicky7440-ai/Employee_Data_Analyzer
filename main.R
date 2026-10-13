library(shiny)
library(DT)

# Define User Interface
ui <- fluidPage(
  titlePanel("Enhanced Employee Data Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload Input CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      tags$hr(),
      numericInput("targetSalary", "Baseline Salary for Comparison:", value = 30000, step = 1000),
      tags$hr(),
      helpBox = tags$div(
        tags$b("Note:"), 
        "Your CSV file must contain columns named ", 
        tags$code("Name"), " and ", tags$code("Salary"), "."
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Analyzed Data", 
                 tags$br(),
                 DTOutput("contents"),
                 tags$br(),
                 downloadButton("downloadData", "Download Results as CSV", class = "btn-primary")
        ),
        tabPanel("Summary & Visualization",
                 tags$br(),
                 verbatimTextOutput("summaryText"),
                 tags$hr(),
                 plotOutput("salaryPlot")
        )
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Reactive expression to read and process uploaded data
  employeeData <- reactive({
    req(input$file1)
    df <- tryCatch({
      read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error reading CSV file.", type = "error")
      return(null)
    })
    
    # Validate column names
    if (!all(c("Name", "Salary") %in% colnames(df))) {
      showNotification("Error: CSV must contain 'Name' and 'Salary' columns.", type = "error")
      return(NULL)
    }
    
    # Perform calculations
    df$Salary_Sqrt <- round(sqrt(df$Salary), 2)
    df$Abs_Difference <- abs(df$Salary - input$targetSalary)
    
    return(df)
  })
  
  # Render Interactive Table
  output$contents <- renderDT({
    df <- employeeData()
    req(df)
    datatable(df, options = list(pageLength = 5, dom = 'Bfrtip'))
  })
  
  # Render Text Summary
  output$summaryText <- renderPrint({
    df <- employeeData()
    req(df)
    cat("=== Summary Statistics ===\n")
    cat("Total Employees :", nrow(df), "\n")
    cat("Average Salary  :", round(mean(df$Salary, na.rm = TRUE), 2), "\n")
    cat("Highest Salary  :", max(df$Salary, na.rm = TRUE), "\n")
    cat("Lowest Salary   :", min(df$Salary, na.rm = TRUE), "\n")
  })
  
  # Render Salary Bar Chart
  output$salaryPlot <- renderPlot({
    df <- employeeData()
    req(df)
    barplot(df$Salary, names.arg = df$Name, col = "#2c3e50", 
            main = "Employee Salaries Overview", ylab = "Salary", 
            las = 2, cex.names = 0.9)
  })
  
  # Download Handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("employee_analysis_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(employeeData(), file, row.names = FALSE)
    }
  )
}

# Run the Application
shinyApp(ui = ui, server = server)