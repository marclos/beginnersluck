library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)


# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well { background-color: #f8f9fa; border: 1px solid #dee2e6; }
      .metric-box { 
        padding: 15px; 
        border-radius: 8px; 
        margin: 5px;
        text-align: center;
      }
      .metric-value { font-size: 24px; font-weight: bold; }
      .metric-label { font-size: 12px; color: #6c757d; }
    "))
  ),
  
  titlePanel("Cap and Trade System Explorer"),
  
  p("Adjust parameters to see how emissions caps and price constraints affect market dynamics"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Market Parameters"),
      
      sliderInput("emissionsCap",
                  "Emissions Cap (tons):",
                  min = 50, max = 200, value = 100, step = 5),
      
      sliderInput("demand",
                  "Total Demand (tons):",
                  min = 50, max = 200, value = 120, step = 5),
      
      hr(),
      
      h4("Price Constraints"),
      
      sliderInput("minPrice",
                  "Price Floor ($):",
                  min = 5, max = 50, value = 10, step = 1),
      
      sliderInput("maxPrice",
                  "Price Ceiling ($):",
                  min = 60, max = 200, value = 100, step = 5),
      
      hr(),
      
      sliderInput("initialAllocation",
                  "Initial Allocation (tons):",
                  min = 50, max = 200, value = 100, step = 5)
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        column(3, 
               div(class = "metric-box", style = "background-color: #dbeafe;",
                   div(class = "metric-label", "Equilibrium Price"),
                   div(class = "metric-value", textOutput("eqPrice", inline = TRUE))
               )
        ),
        column(3,
               div(class = "metric-box", style = "background-color: #dcfce7;",
                   div(class = "metric-label", "Permits Traded"),
                   div(class = "metric-value", textOutput("eqQuantity", inline = TRUE))
               )
        ),
        column(3,
               div(class = "metric-box", style = "background-color: #fed7aa;",
                   div(class = "metric-label", textOutput("shortageLabel", inline = TRUE)),
                   div(class = "metric-value", textOutput("shortageValue", inline = TRUE))
               )
        ),
        column(3,
               div(class = "metric-box", style = "background-color: #e9d5ff;",
                   div(class = "metric-label", "Market Value"),
                   div(class = "metric-value", textOutput("marketValue", inline = TRUE))
               )
        )
      ),
      
      br(),
      
      plotOutput("marketPlot", height = "400px"),
      
      br(),
      
      h4("Firm Trading Positions"),
      plotOutput("firmPlot", height = "300px"),
      
      br(),
      
      tableOutput("firmTable")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  marketData <- reactive({
    cap <- input$emissionsCap
    dem <- input$demand
    minP <- input$minPrice
    maxP <- input$maxPrice
    
    eqQty <- min(cap, dem)
    
    if (dem > cap) {
      eqPrice <- minP + ((cap / dem) * (maxP - minP))
    } else {
      eqPrice <- minP + ((dem / cap) * (maxP - minP) * 0.5)
    }
    
    eqPrice <- max(minP, min(maxP, eqPrice))
    
    list(
      equilibriumQuantity = eqQty,
      equilibriumPrice = eqPrice,
      shortage = max(0, dem - cap),
      surplus = max(0, cap - dem)
    )
  })
  
  marketCurves <- reactive({
    cap <- input$emissionsCap
    dem <- input$demand
    minP <- input$minPrice
    maxP <- input$maxPrice
    
    maxQ <- max(cap, dem)
    quantities <- seq(0, maxQ, by = 2)
    
    # Convex supply curve (increasing marginal cost)
    supply <- ifelse(quantities <= cap,
                     minP + (maxP - minP) * (quantities / cap)^1.5,
                     NA)
    
    # Concave demand curve (decreasing marginal benefit)
    demand <- ifelse(quantities <= dem,
                     maxP - (maxP - minP) * (quantities / dem)^0.7,
                     NA)
    
    data.frame(
      quantity = quantities,
      supply = supply,
      demand = demand
    )
  })
  
  firmData <- reactive({
    numFirms <- 5
    allocation <- input$initialAllocation
    dem <- input$demand
    eqPrice <- marketData()$equilibriumPrice
    
    set.seed(42)
    
    firms <- data.frame(
      firm = paste("Firm", 1:numFirms),
      allocated = allocation / numFirms,
      desired = (dem / numFirms) * runif(numFirms, 0.8, 1.2)
    )
    
    firms <- firms %>%
      mutate(
        netPosition = allocated - desired,
        action = ifelse(netPosition > 0, "Seller", "Buyer"),
        tradingValue = abs(netPosition) * eqPrice
      )
    
    firms
  })
  
  output$eqPrice <- renderText({
    paste0("$", sprintf("%.2f", marketData()$equilibriumPrice))
  })
  
  output$eqQuantity <- renderText({
    sprintf("%.0f", marketData()$equilibriumQuantity)
  })
  
  output$shortageLabel <- renderText({
    if (marketData()$shortage > 0) "Shortage" else "Surplus"
  })
  
  output$shortageValue <- renderText({
    value <- ifelse(marketData()$shortage > 0, 
                    marketData()$shortage, 
                    marketData()$surplus)
    sprintf("%.0f", value)
  })
  
  output$marketValue <- renderText({
    value <- marketData()$equilibriumQuantity * marketData()$equilibriumPrice
    paste0("$", sprintf("%.0f", value))
  })
  
  output$marketPlot <- renderPlot({
    curves <- marketCurves()
    cap <- input$emissionsCap
    
    ggplot(curves, aes(x = quantity)) +
      geom_line(aes(y = supply, color = "Supply"), size = 1.2, na.rm = TRUE) +
      geom_line(aes(y = demand, color = "Demand"), size = 1.2, na.rm = TRUE) +
      geom_vline(xintercept = cap, linetype = "dashed", color = "red", size = 1) +
      annotate("text", x = cap, y = max(curves$demand, na.rm = TRUE) * 0.95, 
               label = "Cap", color = "red", hjust = -0.1) +
      scale_color_manual(values = c("Supply" = "#3b82f6", "Demand" = "#10b981")) +
      labs(
        title = "Supply and Demand Curves",
        x = "Emission Permits (tons)",
        y = "Price ($)",
        color = "Curve"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "top"
      )
  })
  
  output$firmPlot <- renderPlot({
    firms <- firmData()
    
    firms_long <- firms %>%
      select(firm, allocated, desired) %>%
      tidyr::pivot_longer(cols = c(allocated, desired), 
                          names_to = "type", 
                          values_to = "value")
    
    ggplot(firms_long, aes(x = firm, y = value, fill = type)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      scale_fill_manual(
        values = c("allocated" = "#60a5fa", "desired" = "#f97316"),
        labels = c("Allocated", "Desired")
      ) +
      labs(
        title = "Firm Permit Allocations vs Desired Emissions",
        x = "Firm",
        y = "Permits (tons)",
        fill = "Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top"
      )
  })
  
  output$firmTable <- renderTable({
    firms <- firmData()
    
    firms %>%
      mutate(
        Firm = firm,
        Allocated = sprintf("%.1f", allocated),
        Desired = sprintf("%.1f", desired),
        NetPosition = sprintf("%+.1f", netPosition),
        Role = action,
        TradingValue = sprintf("$%.2f", tradingValue)
      ) %>%
      select(Firm, Allocated, Desired, NetPosition, Role, TradingValue)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui = ui, server = server)
