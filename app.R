library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)
library(tidyr)
library(scales)
library(readr)
library(plotly)
library(ggiraph)

# Custom CSS for better styling
custom_css <- "
  .content-wrapper {
    background-color: #f8f9fa;
  }
  .box {
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    margin-bottom: 20px;
  }
  .box-header {
    border-bottom: 1px solid #f0f0f0;
    padding: 15px;
  }
  .box-title {
    font-weight: 600;
    color: #2c3e50;
  }
  .value-box {
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  .value-box .value {
    font-size: 24px;
    font-weight: 600;
  }
  .value-box .caption {
    font-size: 14px;
    font-weight: 500;
  }
  .sidebar-menu > li > a {
    padding: 12px 15px;
    font-weight: 500;
  }
  .sidebar-menu > li.active > a {
    background-color: #2c3e50;
    border-left: 4px solid #3498db;
  }
  .selectize-input {
    border-radius: 4px;
    border: 1px solid #ddd;
    padding: 8px;
  }
  .selectize-input:focus {
    border-color: #3498db;
    box-shadow: 0 0 0 2px rgba(52,152,219,0.2);
  }
  .btn {
    border-radius: 4px;
    font-weight: 500;
  }
  .btn-primary {
    background-color: #3498db;
    border-color: #3498db;
  }
  .btn-primary:hover {
    background-color: #2980b9;
    border-color: #2980b9;
  }
  .dataTables_wrapper .dataTables_length,
  .dataTables_wrapper .dataTables_filter {
    margin-bottom: 15px;
  }
  .dataTables_wrapper .dataTables_length select,
  .dataTables_wrapper .dataTables_filter input {
    border-radius: 4px;
    border: 1px solid #ddd;
    padding: 5px;
  }
  .dataTables_wrapper .dataTables_info {
    padding-top: 15px;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button {
    border-radius: 4px;
    margin: 0 2px;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button.current {
    background: #3498db;
    border-color: #3498db;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
    background: #2980b9;
    border-color: #2980b9;
  }
"

# UI definition
ui <- dashboardPage(
  dashboardHeader(
    title = "IBKR Portfolio Analysis",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    # Account selector
    div(
      style = "padding: 15px;",
      selectInput("account", "Select Account",
                  choices = NULL,
                  selected = NULL)
    ),
    # Main menu items
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Transactions", tabName = "transactions", icon = icon("table")),
      menuItem("Asset Allocation", tabName = "portfolio", icon = icon("chart-pie")),
      menuItem("Glossary", tabName = "glossary", icon = icon("book"))
    ),
    # Data Upload
    tags$div(
      style = "margin-top: 20px; border-top: 1px solid #ddd; padding: 15px;",
      fileInput("file1", "Upload IBKR data",
                accept = c("text/plain", ".txt"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"),
      
    )
  ),
  
  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$style(HTML(custom_css))
    ),
    
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = div(icon("calendar"), " Time Period", style = "display: flex; align-items: center; gap: 10px;"),
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("time_period", "Select Time Period",
                              choices = c("This Week" = "week",
                                          "This Month" = "month",
                                          "Year to Date" = "ytd",
                                          "This Year" = "year",
                                          "All Time" = "all"),
                              selected = "all"),
                  width = 12
                )
              ),
              fluidRow(
                valueBoxOutput("total_deposits", width = 3),
                valueBoxOutput("total_dividends", width = 3),
                valueBoxOutput("total_investment_tax", width = 3),
                valueBoxOutput("total_interest_tax", width = 3)
              ),
              fluidRow(
                box(
                  title = div(icon("money-bill-wave"), " Cumulative Deposits Over Time", style = "display: flex; align-items: center; gap: 10px;"),
                  status = "success",
                  solidHeader = TRUE,
                  girafeOutput("cumulative_deposits"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = div(icon("hand-holding-usd"), " Cumulative Dividends Over Time", style = "display: flex; align-items: center; gap: 10px;"),
                  status = "info",
                  solidHeader = TRUE,
                  girafeOutput("cumulative_dividends"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = div(icon("shopping-cart"), " Cumulative Purchases Over Time", style = "display: flex; align-items: center; gap: 10px;"),
                  status = "warning",
                  solidHeader = TRUE,
                  girafeOutput("cumulative_purchases"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = div(icon("landmark"), " Cumulative Investment Tax Withholding", style = "display: flex; align-items: center; gap: 10px;"),
                  status = "primary",
                  solidHeader = TRUE,
                  girafeOutput("cumulative_investment_tax"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = div(icon("percent"), " Cumulative Interest Tax Withholding", style = "display: flex; align-items: center; gap: 10px;"),
                  status = "warning",
                  solidHeader = TRUE,
                  girafeOutput("cumulative_interest_tax"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = div(icon("money-bill"), " Cumulative Sales Over Time", style = "display: flex; align-items: center; gap: 10px;"),
                  status = "success",
                  solidHeader = TRUE,
                  girafeOutput("cumulative_sales"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = div(icon("chart-bar"), " Monthly Transaction Summary", style = "display: flex; align-items: center; gap: 10px;"),
                  status = "info",
                  solidHeader = TRUE,
                  plotOutput("monthly_summary"),
                  width = 12
                )
              )
      ),
      
      # Transactions tab with improved styling
      tabItem(tabName = "transactions",
              fluidRow(
                box(
                  title = div(
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    div(icon("table"), " Transaction History", style = "display: flex; align-items: center; gap: 10px;"),
                    actionButton("help_transactions", "", icon = icon("question-circle"),
                               style = "background: none; border: none; color: #2c3e50;")
                  ),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("transaction_table")
                )
              )
      ),
      
      # Portfolio Analysis tab
      tabItem(tabName = "portfolio",
              fluidRow(
                box(
                  title = div(icon("chart-pie"), " Asset Class Distribution", style = "display: flex; align-items: center; gap: 10px;"),
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("asset_allocation"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = div(icon("table"), " Asset Details", style = "display: flex; align-items: center; gap: 10px;"),
                  status = "info",
                  solidHeader = TRUE,
                  DTOutput("asset_details"),
                  width = 12
                )
              )
      ),
      
      # Glossary tab with improved styling
      tabItem(tabName = "glossary",
              fluidRow(
                box(
                  title = div(icon("book"), " Transaction Types Glossary", style = "display: flex; align-items: center; gap: 10px;"),
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    style = "padding: 20px;",
                    # Symbol Lookup Section
                    div(
                      style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
                      h3(icon("search"), " Symbol Lookup", style = "color: #2c3e50; margin-bottom: 15px;"),
                      div(
                        style = "display: grid; grid-template-columns: 1fr 2fr; gap: 20px;",
                        # Left side - Search input
                        div(
                          style = "background-color: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          selectizeInput("symbol_search", "Search Symbol",
                                       choices = c(
                                         "AEEM" = "AEEM",
                                         "AUUSIUSD" = "AUUSIUSD",
                                         "EXSA" = "EXSA",
                                         "REZ" = "REZ",
                                         "SPICHA" = "SPICHA",
                                         "US-T Govt Bills" = "US-T Govt Bills",
                                         "VOO" = "VOO",
                                         "ZSIL" = "ZSIL"
                                       ),
                                       selected = NULL,
                                       options = list(
                                         placeholder = "Type or select a symbol",
                                         onInitialize = I('function() { this.setValue(""); }')
                                       )),
                          div(
                            style = "margin-top: 20px;",
                            h4(icon("list"), " Common Symbols", style = "color: #2c3e50; margin-bottom: 15px; font-size: 16px; font-weight: 600;"),
                            div(
                              style = "max-height: 300px; overflow-y: auto; padding-right: 10px;",
                              tags$ul(
                                style = "list-style-type: none; padding: 0; margin: 0;",
                                tags$li(
                                  style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin-bottom: 8px; border-left: 4px solid #3498db;",
                                  tags$strong("AEEM", style = "color: #2c3e50;"),
                                  tags$p("Amundi MSCI Emerging Markets ETF", style = "margin: 5px 0 0 0; font-size: 13px; color: #666;")
                                ),
                                tags$li(
                                  style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin-bottom: 8px; border-left: 4px solid #e74c3c;",
                                  tags$strong("AUUSIUSD", style = "color: #2c3e50;"),
                                  tags$p("UBS ETF Gold", style = "margin: 5px 0 0 0; font-size: 13px; color: #666;")
                                ),
                                tags$li(
                                  style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin-bottom: 8px; border-left: 4px solid #2ecc71;",
                                  tags$strong("EXSA", style = "color: #2c3e50;"),
                                  tags$p("iShares STOXX Europe 600 ETF", style = "margin: 5px 0 0 0; font-size: 13px; color: #666;")
                                ),
                                tags$li(
                                  style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin-bottom: 8px; border-left: 4px solid #9b59b6;",
                                  tags$strong("REZ", style = "color: #2c3e50;"),
                                  tags$p("iShares Residential & Multifamily Real Estate ETF", style = "margin: 5px 0 0 0; font-size: 13px; color: #666;")
                                ),
                                tags$li(
                                  style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin-bottom: 8px; border-left: 4px solid #f1c40f;",
                                  tags$strong("SPICHA", style = "color: #2c3e50;"),
                                  tags$p("UBS ETF SPI (Swiss Market)", style = "margin: 5px 0 0 0; font-size: 13px; color: #666;")
                                ),
                                tags$li(
                                  style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin-bottom: 8px; border-left: 4px solid #1abc9c;",
                                  tags$strong("US-T Govt Bills", style = "color: #2c3e50;"),
                                  tags$p("U.S. Government Short-Term Bonds", style = "margin: 5px 0 0 0; font-size: 13px; color: #666;")
                                ),
                                tags$li(
                                  style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin-bottom: 8px; border-left: 4px solid #e67e22;",
                                  tags$strong("VOO", style = "color: #2c3e50;"),
                                  tags$p("Vanguard S&P 500 ETF", style = "margin: 5px 0 0 0; font-size: 13px; color: #666;")
                                ),
                                tags$li(
                                  style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin-bottom: 8px; border-left: 4px solid #34495e;",
                                  tags$strong("ZSIL", style = "color: #2c3e50;"),
                                  tags$p("Swisscanto CH Silver ETF", style = "margin: 5px 0 0 0; font-size: 13px; color: #666;")
                                )
                              )
                            )
                          )
                        ),
                        # Right side - Symbol details
                        div(
                          style = "background-color: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          uiOutput("symbol_details")
                        )
                      )
                    ),
                    # Common Transaction Types Section
                    div(
                      style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
                      h3(icon("info-circle"), " Common Transaction Types", style = "color: #2c3e50; margin-bottom: 15px;"),
                      div(
                        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 15px;",
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("shopping-cart"), " Achat (Buy)", style = "color: #2c3e50;"),
                          p("Purchase of securities (stocks, bonds, ETFs, etc.)")
                        ),
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("money-bill-wave"), " Vente (Sell)", style = "color: #2c3e50;"),
                          p("Sale of securities")
                        ),
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("hand-holding-usd"), " Dividende (Dividend)", style = "color: #2c3e50;"),
                          p("Distribution of company profits to shareholders")
                        ),
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("arrow-down"), " Dépôt (Deposit)", style = "color: #2c3e50;"),
                          p("Cash deposit into the account")
                        ),
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("arrow-up"), " Retrait (Withdrawal)", style = "color: #2c3e50;"),
                          p("Cash withdrawal from the account")
                        ),
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("landmark"), " Foreign Tax Withholding", style = "color: #2c3e50;"),
                          p("Tax withheld on foreign dividends or interest")
                        ),
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("exchange-alt"), " FX Translations P&L", style = "color: #2c3e50;"),
                          p("Profit & Loss from currency exchange rate fluctuations")
                        ),
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("balance-scale"), " Ajustement (Adjustment)", style = "color: #2c3e50;"),
                          p("Adjustment due to revaluation of non-USD holdings")
                        )
                      )
                    ),
                    # Transaction Components Section
                    div(
                      style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
                      h3(icon("calculator"), " Transaction Components", style = "color: #2c3e50; margin-bottom: 15px;"),
                      div(
                        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 15px;",
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("dollar-sign"), " Montant brut (Gross Amount)", style = "color: #2c3e50;"),
                          p("Total transaction amount before fees")
                        ),
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("dollar-sign"), " Montant net (Net Amount)", style = "color: #2c3e50;"),
                          p("Final transaction amount after all fees")
                        ),
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("hashtag"), " Quantité (Quantity)", style = "color: #2c3e50;"),
                          p("Number of shares or units traded")
                        ),
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("tag"), " Prix (Price)", style = "color: #2c3e50;"),
                          p("Price per share or unit")
                        ),
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4(icon("times"), " Multiplicateur (Multiplier)", style = "color: #2c3e50;"),
                          p("Contract multiplier for derivatives")
                        )
                      )
                    ),
                    # Additional Information Section
                    div(
                      style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
                      h3(icon("lightbulb"), " Additional Information", style = "color: #2c3e50; margin-bottom: 15px;"),
                      div(
                        style = "background-color: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                        p(icon("exchange-alt"), " All amounts are displayed in the account's base currency. For international transactions, amounts are converted using the exchange rate at the time of the transaction."),
                        p(icon("plus-minus"), " Negative amounts typically indicate money leaving the account (purchases, fees, taxes), while positive amounts indicate money entering the account (sales, dividends, deposits).")
                      )
                    )
                  )
                )
              )
      )
    )
  )
)

# Server definition
server <- function(input, output, session) {
  
  # Reactive value to store processed data
  processed_data <- reactiveVal(NULL)
  
  # Process data when file is uploaded
  observeEvent(input$file1, {
    req(input$file1)
    
    # Read the data
    data <- read.delim(input$file1$datapath, sep="\t", header=TRUE, stringsAsFactors=FALSE)
    
    # Clean and format the data
    data_cleaned <- data %>%
      # Convert date to proper date format
      mutate(Date = as.Date(Date)) %>%
      # Clean numeric columns by removing currency symbols and converting to numeric
      mutate(
        Quantité = as.numeric(gsub("[^0-9.-]", "", Quantité)),
        Prix = as.numeric(gsub("[^0-9.-]", "", Prix)),
        `Montant.brut` = as.numeric(gsub("[^0-9.-]", "", `Montant.brut`)),
        Commission = as.numeric(gsub("[^0-9.-]", "", Commission)),
        `Montant.net` = as.numeric(gsub("[^0-9.-]", "", `Montant.net`)),
        `Taux.de.change` = as.numeric(gsub("[^0-9.-]", "", `Taux.de.change`)),
        `Frais.de.transaction` = as.numeric(gsub("[^0-9.-]", "", `Frais.de.transaction`)),
        Multiplicateur = as.numeric(gsub("[^0-9.-]", "", Multiplicateur))
      )
    
    # Store processed data
    processed_data(data_cleaned)
    
    # Update account selector choices
    updateSelectInput(session, "account",
                      choices = c("All Accounts", unique(data_cleaned$Compte)),
                      selected = "All Accounts")
  })
  
  # Reactive filtered data based on selected account
  filtered_data <- reactive({
    req(processed_data(), input$account)
    if (input$account == "All Accounts") {
      processed_data()
    } else {
      processed_data() %>% filter(Compte == input$account)
    }
  })
  
  # Function to filter data by time period
  filtered_data_by_period <- reactive({
    req(filtered_data(), input$time_period)
    
    data <- filtered_data()
    today <- Sys.Date()
    
    filtered <- switch(input$time_period,
                       "week" = {
                         start_date <- today - as.numeric(format(today, "%u")) + 1
                         data %>% filter(Date >= start_date)
                       },
                       "month" = {
                         start_date <- as.Date(format(today, "%Y-%m-01"))
                         data %>% filter(Date >= start_date)
                       },
                       "ytd" = {
                         start_date <- as.Date(format(today, "%Y-01-01"))
                         data %>% filter(Date >= start_date)
                       },
                       "year" = {
                         start_date <- as.Date(format(today, "%Y-01-01"))
                         end_date <- as.Date(format(today, "%Y-12-31"))
                         data %>% filter(Date >= start_date, Date <= end_date)
                       },
                       "all" = data
    )
    
    return(filtered)
  })
  
  # Update value boxes
  output$total_deposits <- renderValueBox({
    req(filtered_data_by_period())
    valueBox(
      sum(filtered_data_by_period()$Montant.net[filtered_data_by_period()$Type.de.transaction == "Dépôt"], na.rm = TRUE),
      "Total Deposits",
      icon = icon("money-bill"),
      color = "green"
    )
  })
  
  output$total_dividends <- renderValueBox({
    req(filtered_data_by_period())
    valueBox(
      sum(filtered_data_by_period()$Montant.net[filtered_data_by_period()$Type.de.transaction == "Dividende"], na.rm = TRUE),
      "Total Dividends",
      icon = icon("money-bill"),
      color = "blue"
    )
  })
  
  output$total_investment_tax <- renderValueBox({
    req(filtered_data_by_period())
    valueBox(
      sum(filtered_data_by_period()$Montant.net[filtered_data_by_period()$Type.de.transaction == "Foreign Tax Withholding" & 
                                                grepl("Dividend|ETF|Stock", filtered_data_by_period()$Description, ignore.case = TRUE)], na.rm = TRUE),
      "Investment Tax Withholding",
      icon = icon("money-bill-wave"),
      color = "purple"
    )
  })
  
  output$total_interest_tax <- renderValueBox({
    req(filtered_data_by_period())
    valueBox(
      sum(filtered_data_by_period()$Montant.net[filtered_data_by_period()$Type.de.transaction == "Intérêts créditeurs"], na.rm = TRUE),
      "Interest on Cash",
      icon = icon("money-bill-wave"),
      color = "orange"
    )
  })
  
  # Update monthly summary plot
  output$monthly_summary <- renderPlot({
    req(filtered_data_by_period())
    monthly_data <- filtered_data_by_period() %>%
      group_by(month = floor_date(Date, "month"), Type.de.transaction) %>%
      summarise(Amount = sum(Montant.net, na.rm = TRUE)) %>%
      filter(Type.de.transaction %in% c("Achat", "Dividende", "Dépôt"))
    
    ggplot(monthly_data, aes(x = month, y = Amount, fill = Type.de.transaction)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_text(aes(label = format(round(Amount), big.mark = "'"),
                    vjust = ifelse(Amount < 0, 1.5, -0.5)),
                position = position_dodge(width = 0.9),
                size = 4) +
      theme_minimal() +
      labs(x = "Month", y = "Amount", fill = "Transaction Type") +
      scale_fill_brewer(palette = "Set2") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
  })
  
  # Transaction help modal
  observeEvent(input$help_transactions, {
    showModal(modalDialog(
      title = "Understanding Transaction Types",
      div(
        style = "padding: 20px;",
        h4("Common Transaction Types:"),
        tags$ul(
          tags$li(tags$strong("Achat (Purchase):"), " Purchase of securities (stocks, bonds, ETFs, etc.)"),
          tags$li(tags$strong("Vente (Sale):"), " Sale of securities"),
          tags$li(tags$strong("Dividende (Dividend):"), " Distribution of company profits to shareholders"),
          tags$li(tags$strong("Dépôt (Deposit):"), " Cash deposit into the account"),
          tags$li(tags$strong("Retrait (Withdrawal):"), " Cash withdrawal from the account"),
          tags$li(tags$strong("Foreign Tax Withholding:"), " Tax withheld on foreign dividends or interest"),
          tags$li(tags$strong("Commission:"), " Brokerage fees for executing trades"),
          tags$li(tags$strong("FX Translations P&L:"), " Profit & Loss from currency exchange rate fluctuations"),
          tags$li(tags$strong("Ajustement (Adjustment):"), " Adjustment due to revaluation of non-USD holdings")
        ),
        h4("Amount Colors:"),
        tags$ul(
          tags$li(tags$span(style = "color: green;", "Green"), " indicates positive amounts (money coming in)"),
          tags$li(tags$span(style = "color: red;", "Red"), " indicates negative amounts (money going out)")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Transaction table
  output$transaction_table <- renderDT({
    req(filtered_data())
    
    # Create a more readable version of the data
    data <- filtered_data() %>%
      mutate(
        # Format date
        Date = format(Date, "%Y-%m-%d"),
        # Format amounts with currency symbols and thousands separators
        `Montant.brut` = sprintf("%.2f", `Montant.brut`),
        `Montant.net` = sprintf("%.2f", `Montant.net`),
        Prix = sprintf("%.2f", Prix),
        # Format quantities with proper decimal places
        Quantité = sprintf("%.2f", Quantité),
        # Format exchange rate
        `Taux.de.change` = sprintf("%.4f", `Taux.de.change`),
        # Add transaction type descriptions
        `Type.de.transaction` = case_when(
          `Type.de.transaction` == "Achat" ~ "Achat (Purchase)",
          `Type.de.transaction` == "Vente" ~ "Vente (Sale)",
          `Type.de.transaction` == "Dividende" ~ "Dividende (Dividend)",
          `Type.de.transaction` == "Dépôt" ~ "Dépôt (Deposit)",
          `Type.de.transaction` == "Retrait" ~ "Retrait (Withdrawal)",
          `Type.de.transaction` == "Foreign Tax Withholding" ~ "Foreign Tax Withholding",
          `Type.de.transaction` == "Commission" ~ "Commission (Fee)",
          `Type.de.transaction` == "FX Translations P&L" ~ "FX Translations P&L",
          `Type.de.transaction` == "Ajustement" ~ "Ajustement (Adjustment)",
          `Type.de.transaction` == "Intérêts" ~ "Intérêts créditeurs",
          `Type.de.transaction` == "Vente Obligations" ~ "Opération sur titres",
          TRUE ~ `Type.de.transaction`
        )
      )
    
    datatable(data,
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                autoWidth = TRUE,
                columnDefs = list(
                  list(width = '80px', targets = c(0)),  # Date
                  list(width = '70px', targets = c(1)),  # Account
                  list(width = '100px', targets = c(2)),  # Description
                  list(width = '120px', targets = c(3)),  # Transaction Type
                  list(width = '60px', targets = c(4)),  # Symbol
                  list(width = '60px', targets = c(5)),  # Quantity
                  list(width = '70px', targets = c(6)),  # Price
                  list(width = '80px', targets = c(7)),  # Gross Amount
                  list(width = '70px', targets = c(8)),  # Commission
                  list(width = '80px', targets = c(9)),  # Net Amount
                  list(width = '70px', targets = c(10)), # Exchange Rate
                  list(width = '70px', targets = c(11)), # Transaction Fee
                  list(width = '60px', targets = c(12))  # Multiplier
                ),
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                language = list(
                  search = "Search transactions:",
                  lengthMenu = "Show _MENU_ transactions per page",
                  info = "Showing _START_ to _END_ of _TOTAL_ transactions"
                )
              ),
              rownames = FALSE,
              filter = "top") %>%
      formatStyle(
        'Montant.net',
        color = styleInterval(0, c('red', 'green')),
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Type.de.transaction',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Montant.net',
        target = 'row',
        backgroundColor = styleEqual(
          c('Achat (Purchase)'),
          c('#FFA500')
        )
      )
  })
  
  # Calculate current holdings
  current_holdings <- reactive({
    req(filtered_data())
    
    # Calculate net quantities by summing purchases and sales
    quantity_changes <- filtered_data() %>%
      filter(Type.de.transaction %in% c("Achat", "Vente")) %>%
      mutate(Quantity = ifelse(Type.de.transaction == "Achat", Quantité)) %>%
      group_by(Symbole, Description) %>%
      summarise(
        Net_Quantity = sum(Quantity, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculate average purchase price and total investment
    purchases <- filtered_data() %>%
      filter(Type.de.transaction %in% c("Achat", "Vente Obligations")) %>%
      mutate(Adjusted_Amount = ifelse(Type.de.transaction == "Vente Obligations", 
                                      -abs(Montant.net), 
                                      abs(Montant.net)),
             Adjusted_Quantity = ifelse(Type.de.transaction == "Vente Obligations", 
                                        -Quantité, 
                                        Quantité)) %>%
      group_by(Symbole, Description) %>%
      summarise(
        Total_Purchase_Amount = sum(Adjusted_Amount, na.rm = TRUE),
        Total_Purchase_Quantity = sum(Adjusted_Quantity, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join with quantity changes and calculate values
    holdings <- quantity_changes %>%
      left_join(purchases, by = c("Symbole", "Description")) %>%
      mutate(
        Average_Price = Total_Purchase_Amount / Total_Purchase_Quantity,
        Total_Investment = Average_Price * Net_Quantity,
        Currency = ifelse(grepl("USD", Description), "USD", 
                          ifelse(grepl("EUR", Description), "EUR", "CHF")))
        
        # Here you would need to get CURRENT prices from an API or other source
        # For now, we'll use average price as placeholder (not ideal)
        holdings <- holdings %>%
          mutate(
            Current_Value = Net_Quantity * Average_Price,  # REPLACE with real current prices
            Unrealized_PnL = Current_Value - Total_Investment,
            PnL_Percentage = (Unrealized_PnL / Total_Investment) * 100
          )
        
        return(holdings)
  })
  
  # Asset allocation plot
  output$asset_allocation <- renderPlotly({
    req(current_holdings())
    
    # Categorize assets
    print(current_holdings)
    asset_data <- current_holdings() %>%
      mutate(
        Asset_Class = case_when(
          grepl("^9", Symbole) ~ "Bonds",
          grepl("AUUSIUSD|ZSIL", Symbole) ~ "Commodities",
          grepl("REZ", Symbole) ~ "Real Estate",
          TRUE ~ "Stocks"
        )
      ) %>%
      group_by(Asset_Class) %>%
      summarise(Total_Value = sum(Current_Value, na.rm = TRUE)) %>%
      mutate(
        Percentage = Total_Value / sum(Total_Value) * 100,
        Label = sprintf("%s\n%.1f%%\n%s", 
                       Asset_Class, 
                       Percentage,
                       format(round(Total_Value), big.mark = "'"))
      )
    
    print(asset_data)
    
    # Calculate total value
    total_value <- sum(asset_data$Total_Value, na.rm = TRUE)
    
    # Define custom colors for each asset class
    colors <- c(
      "Bonds" = "#3498db",      # Blue
      "Stocks" = "#2ecc71",     # Green
      "Commodities" = "#e74c3c", # Red
      "Real Estate" = "#f1c40f"  # Yellow
    )
    
    # Create pie chart with enhanced styling
    plot_ly(asset_data, 
            labels = ~Label, 
            values = ~Total_Value,
            type = 'pie',
            hole = 0.4,  # Make it a donut chart
            textinfo = 'none',  # Hide default text
            insidetextorientation = 'radial',
            marker = list(
              colors = colors[asset_data$Asset_Class],
              line = list(color = '#FFFFFF', width = 2)
            )) %>%
      layout(
        title = list(
          text = paste("Asset Class Distribution\nTotal Portfolio Value:", format(round(total_value), big.mark = "'")),
          font = list(size = 16, color = "#2c3e50"),
          y = 0.95
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          y = -0.1,
          font = list(size = 12, color = "#2c3e50"),
          bgcolor = "rgba(255, 255, 255, 0.8)"
        ),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(t = 50, b = 50, l = 50, r = 50)
      ) %>%
      config(displayModeBar = FALSE)  # Hide the plotly modebar
  })
  
  # Asset details table
  output$asset_details <- renderDT({
    req(current_holdings())
    
    # Categorize assets and calculate totals
    asset_details <- current_holdings() %>%
      mutate(
        Asset_Class = case_when(
          grepl("^9", Symbole) ~ "Bonds",
          grepl("AUUSIUSD|ZSIL", Symbole) ~ "Commodities",
          grepl("REZ", Symbole) ~ "Real Estate",
          TRUE ~ "Stocks"
        )
      ) %>%
      group_by(Asset_Class) %>%
      summarise(
        Total_Value = sum(Current_Value, na.rm = TRUE),
        Percentage = Total_Value / sum(current_holdings()$Current_Value, na.rm = TRUE) * 100,
        Number_of_Holdings = n()
      ) %>%
      arrange(desc(Total_Value))
    
    datatable(asset_details,
              options = list(
                pageLength = 10,
                dom = 't',
                ordering = FALSE
              ),
              colnames = c(
                "Asset Class", "Total Value", "Percentage", "Number of Holdings"
              )) %>%
      formatCurrency("Total_Value", currency = "") %>%
      formatRound("Percentage", digits = 2) %>%
      formatStyle("Percentage",
                  color = styleInterval(c(0), c("red", "green")))
  })
  
  # Symbol details output
  output$symbol_details <- renderUI({
    req(input$symbol_search)
    
    symbol_info <- list(
      "AEEM" = list(
        name = "Amundi MSCI Emerging Markets ETF",
        description = "An ETF tracking the MSCI Emerging Markets Index, providing exposure to large and mid-sized companies across emerging markets worldwide.",
        type = "ETF",
        category = "Equity",
        region = "Emerging Markets",
        issuer = "Amundi"
      ),
      "AUUSIUSD" = list(
        name = "UBS ETF Gold",
        description = "An ETF that tracks the price of gold, providing exposure to the precious metal's performance.",
        type = "ETF",
        category = "Commodity",
        region = "Global",
        issuer = "UBS"
      ),
      "EXSA" = list(
        name = "iShares STOXX Europe 600 ETF",
        description = "An ETF tracking the STOXX Europe 600 Index, providing exposure to 600 large, mid, and small-cap companies across 17 European countries.",
        type = "ETF",
        category = "Equity",
        region = "Europe",
        issuer = "iShares"
      ),
      "REZ" = list(
        name = "iShares Residential & Multifamily Real Estate ETF",
        description = "An ETF tracking the FTSE NAREIT All Residential Capped Index, providing exposure to residential and multifamily real estate companies.",
        type = "ETF",
        category = "Real Estate",
        region = "United States",
        issuer = "iShares"
      ),
      "SPICHA" = list(
        name = "UBS ETF SPI (Swiss Market)",
        description = "An ETF tracking the Swiss Performance Index (SPI), providing exposure to the Swiss equity market.",
        type = "ETF",
        category = "Equity",
        region = "Switzerland",
        issuer = "UBS"
      ),
      "US-T Govt Bills" = list(
        name = "U.S. Government Short-Term Bonds",
        description = "Short-term debt securities issued by the U.S. government, typically with maturities of less than one year.",
        type = "Bond",
        category = "Fixed Income",
        region = "United States",
        issuer = "U.S. Treasury"
      ),
      "VOO" = list(
        name = "Vanguard S&P 500 ETF",
        description = "An exchange-traded fund that tracks the S&P 500 Index, providing exposure to 500 of the largest U.S. companies.",
        type = "ETF",
        category = "Equity",
        region = "United States",
        issuer = "Vanguard"
      ),
      "ZSIL" = list(
        name = "Swisscanto CH Silver ETF",
        description = "An ETF that tracks the price of silver, providing exposure to the precious metal's performance.",
        type = "ETF",
        category = "Commodity",
        region = "Global",
        issuer = "Swisscanto"
      )
    )
    
    if (input$symbol_search %in% names(symbol_info)) {
      info <- symbol_info[[input$symbol_search]]
      div(
        h3(paste(input$symbol_search, "-", info$name), style = "color: #2c3e50; margin-bottom: 15px;"),
        p(info$description, style = "margin-bottom: 20px;"),
        div(
          style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 15px;",
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
            h4("Type", style = "color: #2c3e50; margin-bottom: 5px; font-size: 14px;"),
            p(info$type)
          ),
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
            h4("Category", style = "color: #2c3e50; margin-bottom: 5px; font-size: 14px;"),
            p(info$category)
          ),
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
            h4("Region", style = "color: #2c3e50; margin-bottom: 5px; font-size: 14px;"),
            p(info$region)
          ),
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
            h4("Issuer", style = "color: #2c3e50; margin-bottom: 5px; font-size: 14px;"),
            p(info$issuer)
          )
        )
      )
    } else {
      div(
        h3("Select a Symbol", style = "color: #2c3e50;"),
        p("Choose a symbol from the dropdown to view its details.")
      )
    }
  })
  
  # Update the cumulative deposits plot
  output$cumulative_deposits <- renderGirafe({
    req(filtered_data_by_period())
    
    deposits_data <- filtered_data_by_period() %>%
      filter(Type.de.transaction == "Dépôt") %>%
      arrange(Date) %>%
      mutate(
        Cumulative_Deposits = cumsum(Montant.net),
        tooltip = paste0(
          "Date: ", format(Date, "%Y-%m-%d"), "\n",
          "Amount: ", format(round(Montant.net), big.mark = "'"), "\n",
          "Description: ", Description, "\n",
          "Cumulative: ", format(round(Cumulative_Deposits), big.mark = "'")
        )
      )
    
    p <- ggplot(deposits_data, aes(x = Date, y = Cumulative_Deposits)) +
      # Add area with gradient fill
      geom_area(fill = "#2E86C1", alpha = 0.3) +
      # Add line on top
      geom_line(size = 1, color = "#2E86C1") +
      # Add interactive points with tooltips
      geom_point_interactive(
        aes(tooltip = tooltip),
        color = "#2E86C1",
        size = 3
      ) +
      theme_minimal() +
      labs(x = "Date", y = "Cumulative Deposits") +
      scale_y_continuous(labels = scales::comma) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10)
      )
    
    girafe(ggobj = p, width_svg = 10, height_svg = 6) %>%
      girafe_options(
        opts_hover(css = "fill: #1a5276; stroke: #1a5276;"),
        opts_tooltip(css = "background-color: white; color: black; padding: 5px; border-radius: 3px; border: 1px solid #ddd;"),
        opts_sizing(rescale = TRUE)
      )
  })
  
  # Cumulative dividends plot
  output$cumulative_dividends <- renderGirafe({
    req(filtered_data_by_period())
    
    dividends_data <- filtered_data_by_period() %>%
      filter(Type.de.transaction == "Dividende") %>%
      arrange(Date) %>%
      mutate(
        Cumulative_Amount = cumsum(Montant.net),
        tooltip = paste0(
          "Date: ", format(Date, "%Y-%m-%d"), "\n",
          "Amount: ", format(round(Montant.net, 2), big.mark = "'"), "\n",
          "Description: ", Description, "\n",
          "Cumulative: ", format(round(Cumulative_Amount), big.mark = "'")
        )
      )
    
    p <- ggplot(dividends_data, aes(x = Date, y = Cumulative_Amount)) +
      geom_area(fill = "#27AE60", alpha = 0.3) +
      geom_line(size = 1, color = "#27AE60") +
      geom_point_interactive(
        aes(tooltip = tooltip),
        color = "#27AE60",
        size = 3
      ) +
      theme_minimal() +
      labs(x = "Date", y = "Cumulative Dividends") +
      scale_y_continuous(labels = scales::comma) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10)
      )
    
    girafe(ggobj = p, width_svg = 10, height_svg = 6) %>%
      girafe_options(
        opts_hover(css = "fill: #196F3D; stroke: #196F3D;"),
        opts_tooltip(css = "background-color: white; color: black; padding: 5px; border-radius: 3px; border: 1px solid #ddd;"),
        opts_sizing(rescale = TRUE)
      )
  })
  
  # Cumulative purchases plot
  output$cumulative_purchases <- renderGirafe({
    req(filtered_data_by_period())
    
    purchases_data <- filtered_data_by_period() %>%
      filter(Type.de.transaction == "Achat") %>%
      arrange(Date) %>%
      mutate(
        Cumulative_Amount = cumsum(Montant.net),
        tooltip = paste0(
          "Date: ", format(Date, "%Y-%m-%d"), "\n",
          "Amount: ", format(round(Montant.net), big.mark = "'"), "\n",
          "Description: ", Description, "\n",
          "Cumulative: ", format(round(Cumulative_Amount), big.mark = "'")
        )
      )
    
    p <- ggplot(purchases_data, aes(x = Date, y = Cumulative_Amount)) +
      geom_area(fill = "#E74C3C", alpha = 0.3) +
      geom_line(size = 1, color = "#E74C3C") +
      geom_point_interactive(
        aes(tooltip = tooltip),
        color = "#E74C3C",
        size = 3
      ) +
      theme_minimal() +
      labs(x = "Date", y = "Cumulative Purchases") +
      scale_y_continuous(labels = scales::comma) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10)
      )
    
    girafe(ggobj = p, width_svg = 10, height_svg = 6) %>%
      girafe_options(
        opts_hover(css = "fill: #922B21; stroke: #922B21;"),
        opts_tooltip(css = "background-color: white; color: black; padding: 5px; border-radius: 3px; border: 1px solid #ddd;"),
        opts_sizing(rescale = TRUE)
      )
  })
  
  # Cumulative sales plot
  output$cumulative_sales <- renderGirafe({
    req(filtered_data_by_period())
    
    sales_data <- filtered_data_by_period() %>%
      filter(Type.de.transaction == "Vente") %>%
      arrange(Date) %>%
      mutate(
        Cumulative_Amount = cumsum(Montant.net),
        tooltip = paste0(
          "Date: ", format(Date, "%Y-%m-%d"), "\n",
          "Amount: ", format(round(Montant.net), big.mark = "'"), "\n",
          "Description: ", Description, "\n",
          "Cumulative: ", format(round(Cumulative_Amount), big.mark = "'")
        )
      )
    
    p <- ggplot(sales_data, aes(x = Date, y = Cumulative_Amount)) +
      geom_area(fill = "#3498DB", alpha = 0.3) +
      geom_line(size = 1, color = "#3498DB") +
      geom_point_interactive(
        aes(tooltip = tooltip),
        color = "#3498DB",
        size = 3
      ) +
      theme_minimal() +
      labs(x = "Date", y = "Cumulative Sales") +
      scale_y_continuous(labels = scales::comma) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10)
      )
    
    girafe(ggobj = p, width_svg = 10, height_svg = 6) %>%
      girafe_options(
        opts_hover(css = "fill: #1A5276; stroke: #1A5276;"),
        opts_tooltip(css = "background-color: white; color: black; padding: 5px; border-radius: 3px; border: 1px solid #ddd;"),
        opts_sizing(rescale = TRUE)
      )
  })
  
  # Cumulative investment tax withholding plot
  output$cumulative_investment_tax <- renderGirafe({
    req(filtered_data_by_period())
    
    investment_tax_data <- filtered_data_by_period() %>%
      filter(Type.de.transaction == "Foreign Tax Withholding" & 
             grepl("Dividend|ETF|Stock", Description, ignore.case = TRUE)) %>%
      arrange(Date) %>%
      mutate(
        Cumulative_Amount = cumsum(Montant.net),
        tooltip = paste0(
          "Date: ", format(Date, "%Y-%m-%d"), "\n",
          "Amount: ", format(round(Montant.net), big.mark = "'"), "\n",
          "Description: ", Description, "\n",
          "Cumulative: ", format(round(Cumulative_Amount), big.mark = "'")
        )
      )
    
    p <- ggplot(investment_tax_data, aes(x = Date, y = Cumulative_Amount)) +
      geom_area(fill = "#8E44AD", alpha = 0.3) +
      geom_line(size = 1, color = "#8E44AD") +
      geom_point_interactive(
        aes(tooltip = tooltip),
        color = "#8E44AD",
        size = 3
      ) +
      theme_minimal() +
      labs(x = "Date", y = "Cumulative Investment Tax Withholding") +
      scale_y_continuous(labels = scales::comma) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10)
      )
    
    girafe(ggobj = p, width_svg = 10, height_svg = 6) %>%
      girafe_options(
        opts_hover(css = "fill: #6C3483; stroke: #6C3483;"),
        opts_tooltip(css = "background-color: white; color: black; padding: 5px; border-radius: 3px; border: 1px solid #ddd;"),
        opts_sizing(rescale = TRUE)
      )
  })
  
  # Cumulative interest tax withholding plot
  output$cumulative_interest_tax <- renderGirafe({
    req(filtered_data_by_period())
    
    interest_tax_data <- filtered_data_by_period() %>%
      filter(Type.de.transaction == "Foreign Tax Withholding" & 
             grepl("Withholding.*Intérêt créditeur", Description)) %>%
      arrange(Date) %>%
      mutate(
        Cumulative_Amount = cumsum(Montant.net),
        tooltip = paste0(
          "Date: ", format(Date, "%Y-%m-%d"), "\n",
          "Amount: ", format(round(Montant.net), big.mark = "'"), "\n",
          "Description: ", Description, "\n",
          "Cumulative: ", format(round(Cumulative_Amount), big.mark = "'")
        )
      )
    
    p <- ggplot(interest_tax_data, aes(x = Date, y = Cumulative_Amount)) +
      geom_area(fill = "#D35400", alpha = 0.3) +
      geom_line(size = 1, color = "#D35400") +
      geom_point_interactive(
        aes(tooltip = tooltip),
        color = "#D35400",
        size = 3
      ) +
      theme_minimal() +
      labs(x = "Date", y = "Cumulative Interest Tax Withholding") +
      scale_y_continuous(labels = scales::comma) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10)
      )
    
    girafe(ggobj = p, width_svg = 10, height_svg = 6) %>%
      girafe_options(
        opts_hover(css = "fill: #A04000; stroke: #A04000;"),
        opts_tooltip(css = "background-color: white; color: black; padding: 5px; border-radius: 3px; border: 1px solid #ddd;"),
        opts_sizing(rescale = TRUE)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
