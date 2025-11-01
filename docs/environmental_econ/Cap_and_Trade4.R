library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

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
      .firm-input-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
        gap: 10px;
        margin-top: 10px;
      }
      .firm-input-item {
        padding: 8px;
        background: #f8f9fa;
        border-radius: 4px;
      }
      .firm-input-item label {
        font-size: 11px;
        font-weight: bold;
        display: block;
        margin-bottom: 3px;
      }
      .firm-input-item input {
        width: 100%;
        padding: 4px;
        font-size: 12px;
      }
    "))
  ),
  
  titlePanel("Cap and Trade System Explorer - 24 Firms"),
  
  p("Adjust parameters and firm cost levels to see how emissions caps and willingness to pay affect market dynamics"),
  
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
      
      h4("Firm Cost Levels (Willingness to Pay Multiplier)"),
      p("Adjust each firm's cost level to determine their maximum willingness to pay. Range: 0.5 (low cost) to 1.5 (high cost)", 
        style = "font-size: 13px; color: #6c757d;"),
      
      uiOutput("firmCostInputs"),
      
      br(),
      
      h4("Firm Trading Details"),
      DTOutput("firmTable")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Initialize firm cost levels
  firmCostLevels <- reactiveVal({
    set.seed(42)
    runif(24, 0.5, 1.5)
  })
  
  # Generate cost level inputs dynamically
  output$firmCostInputs <- renderUI({
    costs <- firmCostLevels()
    
    inputs <- lapply(1:24, function(i) {
      div(class = "firm-input-item",
          tags$label(paste("Firm", i)),
          numericInput(
            inputId = paste0("cost_", i),
            label = NULL,
            value = round(costs[i], 2),
            min = 0.5,
            max = 1.5,
            step = 0.1
          )
      )
    })
    
    div(class = "firm-input-grid", inputs)
  })
  
  # Update cost levels when inputs change
  observe({
    costs <- sapply(1:24, function(i) {
      val <- input[[paste0("cost_", i)]]
      if (is.null(val)) return(firmCostLevels()[i])
      return(val)
    })
    firmCostLevels(costs)
  })
  
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
    numFirms <- 24
    allocation <- input$initialAllocation
    dem <- input$demand
    eqPrice <- marketData()$equilibriumPrice
    minP <- input$minPrice
    maxP <- input$maxPrice
    costs <- firmCostLevels()
    
    set.seed(42)
    
    firms <- data.frame(
      firm = paste("Firm", 1:numFirms),
      firmNum = 1:numFirms,
      allocated = allocation / numFirms,
      desired = (dem / numFirms) * runif(numFirms, 0.7, 1.3)
    )
    
    firms <- firms %>%
      mutate(
        costLevel = costs,
        willingnessToPayMax = minP + (maxP - minP) * costLevel,
        netPosition = allocated - desired,
        action = ifelse(netPosition > 0, "Seller", "Buyer"),
        effectivePrice = ifelse(netPosition < 0, 
                                pmin(willingnessToPayMax, eqPrice), 
                                eqPrice),
        canTrade = ifelse(netPosition < 0, willingnessToPayMax >= eqPrice, TRUE),
        tradingValue = ifelse(canTrade, abs(netPosition) * effectivePrice, 0)
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
    
    firms_summary <- firms %>%
      arrange(netPosition) %>%
      mutate(firmSorted = row_number())
    
    ggplot(firms_summary, aes(x = firmSorted, y = netPosition, fill = action)) +
      geom_bar(stat = "identity", width = 0.8) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
      scale_fill_manual(
        values = c("Buyer" = "#f97316", "Seller" = "#60a5fa"),
        labels = c("Buyer (Deficit)", "Seller (Surplus)")
      ) +
      labs(
        title = "Firm Trading Positions (Sorted by Net Position)",
        x = "Firm (sorted)",
        y = "Net Position (tons)",
        fill = "Role"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  })
  
  output$firmTable <- renderDT({
    firms <- firmData()
    
    firms_display <- firms %>%
      mutate(
        Firm = firm,
        Allocated = sprintf("%.1f", allocated),
        Desired = sprintf("%.1f", desired),
        `Net Position` = sprintf("%+.1f", netPosition),
        Role = action,
        `Cost Level` = sprintf("%.2f", costLevel),
        `Max WTP` = sprintf("$%.2f", willingnessToPayMax),
        `Can Trade` = ifelse(canTrade, "Yes", "No"),
        `Trading Value` = sprintf("$%.2f", tradingValue)
      ) %>%
      select(Firm, Allocated, Desired, `Net Position`, Role, `Cost Level`, `Max WTP`, `Can Trade`, `Trading Value`)
    
    datatable(
      firms_display,
      options = list(
        pageLength = 24,
        dom = 't',
        ordering = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Role',
        backgroundColor = styleEqual(c('Buyer', 'Seller'), c('#fed7aa', '#bfdbfe'))
      ) %>%
      formatStyle(
        'Can Trade',
        backgroundColor = styleEqual(c('Yes', 'No'), c('#d1fae5', '#fee2e2'))
      )
  })
}

shinyApp(ui = ui, server = server)