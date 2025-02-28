library(shiny)
library(bslib)
library(fPortfolio)
library(fBasics)
library(timeSeries)
library(quantmod)
library(Rsymphony)

# Define stock universe
stock_symbols <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "NVDA", "TSLA", "JPM", "V", "WMT")

ui <- page_sidebar(
  title = "Portfolio Optimization with fPortfolio and Rsymphony",
  sidebar = sidebar(
    selectInput("stocks", "Select Stocks (2-6 recommended)",
                choices = stock_symbols,
                multiple = TRUE,
                selected = stock_symbols[1:4]),
    sliderInput("risk_aversion", "Risk Aversion Parameter",
                min = 0.1, max = 5, value = 1, step = 0.1),
    numericInput("min_weight", "Minimum Weight per Asset",
                 value = 0.05, min = 0, max = 1, step = 0.05),
    numericInput("max_weight", "Maximum Weight per Asset",
                 value = 0.4, min = 0, max = 1, step = 0.05),
    actionButton("optimize", "Optimize Portfolio", class = "btn-primary")
  ),
  
  layout_columns(
    card(
      card_header("Efficient Frontier"),
      plotOutput("frontier_plot")
    ),
    card(
      card_header("Optimal Portfolio Weights"),
      plotOutput("weights_plot")
    ),
    card(
      card_header("Portfolio Statistics"),
      tableOutput("stats_table")
    )
  )
)

server <- function(input, output, session) {
  
  # Get stock data
  getData <- reactive({
    req(input$stocks)
    validate(need(length(input$stocks) >= 2, "Please select at least 2 stocks"))
    
    # Get daily returns for selected stocks
    returns_list <- lapply(input$stocks, function(symbol) {
      tryCatch({
        prices <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE,
                            from = Sys.Date() - 365, to = Sys.Date())
        returns <- dailyReturn(prices)
        returns
      }, error = function(e) NULL)
    })
    
    # Combine returns into a timeSeries object
    returns_matrix <- do.call(cbind, returns_list)
    colnames(returns_matrix) <- input$stocks
    as.timeSeries(returns_matrix)
  })
  
  # Create portfolio specification
  getSpec <- reactive({
    # Create basic specification
    spec <- portfolioSpec()
    
    # Modify specification settings directly
    spec@model$nFrontierPoints <- 25
    spec@model$riskFreeRate <- 0.02
    spec@model$riskAversion <- input$risk_aversion
    spec@model$optimizer <- "solveRsymphony"
    spec@model$optControl <- list(
      solver = "symphony",
      maxiter = 1000,
      trace = TRUE
    )
    
    spec
  })
  
  # Optimize portfolio
  portfolio <- eventReactive(input$optimize, {
    data <- getData()
    spec <- getSpec()
    
    # Set up constraints using proper format
    constraints <- paste(
      "box", 
      paste(rep(input$min_weight, ncol(data)), collapse = " "),
      paste(rep(input$max_weight, ncol(data)), collapse = " "),
      "eqsumw",
      sep = "\n"
    )
    
    # Perform optimization
    tryCatch({
      portfolioFrontier(data, spec, constraints)
    }, error = function(e) {
      # Fall back to minimum variance if optimization fails
      minvariancePortfolio(data, spec, constraints)
    })
  })
  
  # Plot efficient frontier
  output$frontier_plot <- renderPlot({
    req(portfolio())
    frontierPlot(portfolio(), risk = "Sigma", return = "mu")
  })
  
  # Plot portfolio weights
  output$weights_plot <- renderPlot({
    req(portfolio())
    
    # Extract weights from the tangency portfolio
    weights <- getWeights(portfolio())
    if (is.matrix(weights)) {
      weights <- weights[nrow(weights),]  # Get the last row for tangency portfolio
    }
    
    # Create barplot
    barplot(weights, 
            main = "Portfolio Weights",
            col = "steelblue",
            las = 2,
            ylim = c(0, max(weights) * 1.2))
  })
  
  # Display portfolio statistics
  output$stats_table <- renderTable({
    req(portfolio())
    
    # Get the optimal portfolio data
    pfolio <- portfolio()
    
    # Extract return and risk from the optimal portfolio
    if ("tangencyPortfolio" %in% slotNames(pfolio)) {
      # For frontier portfolios, use tangency portfolio
      ret <- getTargetReturn(pfolio@tangencyPortfolio)
      risk <- getTargetRisk(pfolio@tangencyPortfolio)
    } else {
      # For single portfolios (like minvariance)
      ret <- getTargetReturn(pfolio)
      risk <- getTargetRisk(pfolio)
    }
    
    # Calculate Sharpe ratio
    rf_rate <- getSpec()@model$riskFreeRate
    sharpe <- (ret - rf_rate) / risk
    
    # Annualize statistics
    data.frame(
      Metric = c("Expected Return (Annual)", 
                 "Risk (Annual StdDev)", 
                 "Sharpe Ratio"),
      Value = round(c(
        ret * 252,  # Annualize return
        risk * sqrt(252),  # Annualize risk
        sharpe * sqrt(252)  # Annualize Sharpe ratio
      ), 4)
    )
  })
}

shinyApp(ui, server)
