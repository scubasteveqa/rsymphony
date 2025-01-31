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
    spec@nFrontierPoints <- 25
    spec@risk <- list(
      risk = "Cov",
      riskFreeRate = 0.02,
      riskAversion = input$risk_aversion
    )
    spec@optimizer <- list(
      type = "solveRsymphony",
      control = list(
        solver = "symphony",
        maxiter = 1000,
        trace = TRUE
      )
    )
    
    spec
  })
  
  # Optimize portfolio
  portfolio <- eventReactive(input$optimize, {
    data <- getData()
    spec <- getSpec()
    
    # Create constraints string
    constraints <- paste0("minW[1:length(input$stocks)]=", input$min_weight,
                         "; maxW[1:length(input$stocks)]=", input$max_weight,
                         "; sum(weights)=1")
    
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
    
    # Extract portfolio statistics
    stats <- portfolioStatistics(portfolio())
    
    data.frame(
      Metric = c("Expected Return (Annual)", 
                 "Risk (Annual StdDev)", 
                 "Sharpe Ratio"),
      Value = round(c(
        mean(getTargetReturn(portfolio())) * 252,
        mean(getTargetRisk(portfolio())) * sqrt(252),
        mean(getTargetReturn(portfolio()) / getTargetRisk(portfolio())) * sqrt(252)
      ), 4)
    )
  })
}

shinyApp(ui, server)
