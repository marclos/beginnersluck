library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# =============================================================================
# Cap and Trade System Explorer
# =============================================================================
# This Shiny app simulates a cap-and-trade emissions market where firms can
# buy and sell emission permits. The app demonstrates how:
# - Emission caps constrain total pollution
# - Market prices emerge from supply and demand
# - Firms with different costs make trading decisions
# - Price floors and ceilings affect market outcomes
# =============================================================================

# UI Definition
# =============================================================================
ui <- fluidPage(
  # Custom CSS styling for a polished appearance
  tags$head(
    tags$style(HTML("
      .well { background-color: #f8f9fa; border: 1px solid #dee2e6; }
      
      /* Metric boxes displaying key market indicators */
      .metric-box { 
        padding: 15px; 
        border-radius: 8px; 
        margin: 5px;
        text-align: center;
      }
      .metric-value { font-size: 24px; font-weight: bold; }
      .metric-label { font-size: 12px; color: #6c757d; }
      
      /* Grid layout for firm cost inputs */
      .firm-input-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(140px, 1fr));
        gap: 10px;
        margin-top: 10px;
      }
      .firm-input-item {
        padding: 10px;
        background: #f8f9fa;
        border-radius: 4px;
      }
      .firm-input-item label {
        font-size: 12px;
        font-weight: bold;
        display: block;
        margin-bottom: 5px;
      }
      .firm-input-item input {
        width: 100%;
        padding: 5px;
        font-size: 13px;
      }
    "))
  ),
  
  titlePanel("Cap and Trade System Explorer - 6 Firms"),
  
  p("Adjust parameters and firm cost levels to see how emissions caps and willingness to pay affect market dynamics"),
  
  sidebarLayout(
    # Left sidebar: Market parameters and price constraints
    sidebarPanel(
      width = 3,
      
      h4("Market Parameters"),
      
      # Total emissions cap - this is the regulatory constraint
      sliderInput("emissionsCap",
                  "Emissions Cap (tons):",
                  min = 50, max = 200, value = 100, step = 5),
      
      # Total demand for permits across all firms
      sliderInput("demand",
                  "Total Demand (tons):",
                  min = 50, max = 200, value = 120, step = 5),
      
      hr(),
      
      h4("Price Constraints"),
      
      # Minimum price (price floor) - prevents prices from falling too low
      sliderInput("minPrice",
                  "Price Floor ($):",
                  min = 5, max = 50, value = 10, step = 1),
      
      # Maximum price (price ceiling) - caps how high prices can go
      sliderInput("maxPrice",
                  "Price Ceiling ($):",
                  min = 60, max = 200, value = 100, step = 5),
      
      hr(),
      
      # Initial allocation of permits to all firms (before trading)
      sliderInput("initialAllocation",
                  "Initial Allocation (tons):",
                  min = 50, max = 200, value = 100, step = 5)
    ),
    
    # Main panel: Visualizations and data tables
    mainPanel(
      width = 9,
      
      # Top row: Key market metrics
      fluidRow(
        # Equilibrium price - where supply meets demand
        column(3, 
               div(class = "metric-box", style = "background-color: #dbeafe;",
                   div(class = "metric-label", "Equilibrium Price"),
                   div(class = "metric-value", textOutput("eqPrice", inline = TRUE))
               )
        ),
        # Number of permits actually traded
        column(3,
               div(class = "metric-box", style = "background-color: #dcfce7;",
                   div(class = "metric-label", "Permits Traded"),
                   div(class = "metric-value", textOutput("eqQuantity", inline = TRUE))
               )
        ),
        # Shortage (demand > cap) or surplus (cap > demand)
        column(3,
               div(class = "metric-box", style = "background-color: #fed7aa;",
                   div(class = "metric-label", textOutput("shortageLabel", inline = TRUE)),
                   div(class = "metric-value", textOutput("shortageValue", inline = TRUE))
               )
        ),
        # Total value of permits traded
        column(3,
               div(class = "metric-box", style = "background-color: #e9d5ff;",
                   div(class = "metric-label", "Market Value"),
                   div(class = "metric-value", textOutput("marketValue", inline = TRUE))
               )
        )
      ),
      
      br(),
      
      # Supply and demand curves showing market equilibrium
      plotOutput("marketPlot", height = "400px"),
      
      br(),
      
      # Bar chart showing which firms are buyers vs sellers
      h4("Firm Trading Positions"),
      plotOutput("firmPlot", height = "300px"),
      
      br(),
      
      # Interactive inputs for adjusting each firm's cost level
      h4("Firm Cost Levels (Willingness to Pay Multiplier)"),
      p("Adjust each firm's cost level to determine their maximum willingness to pay. Range: 0.5 (low cost) to 1.5 (high cost)", 
        style = "font-size: 13px; color: #6c757d;"),
      
      uiOutput("firmCostInputs"),
      
      br(),
      
      # Detailed table of firm trading data
      h4("Firm Trading Details"),
      DTOutput("firmTable")
    )
  )
)

# Server Logic
# =============================================================================
server <- function(input, output, session) {
  
  # Number of firms in the simulation
  NUM_FIRMS <- 6
  
  # Reactive value storing cost levels for each firm
  # Cost levels are multipliers (0.5 to 1.5) that determine willingness to pay
  # Higher cost = higher willingness to pay for permits
  firmCostLevels <- reactiveVal({
    set.seed(42)  # For reproducible random values
    runif(NUM_FIRMS, 0.5, 1.5)
  })
  
  # Generate dynamic UI inputs for firm cost levels
  # Each firm gets its own numeric input
  output$firmCostInputs <- renderUI({
    costs <- firmCostLevels()
    
    inputs <- lapply(1:NUM_FIRMS, function(i) {
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
  
  # Update cost levels when any firm input changes
  # Uses individual observers to avoid re-rendering all inputs
  lapply(1:NUM_FIRMS, function(i) {
    observeEvent(input[[paste0("cost_", i)]], {
      current_costs <- firmCostLevels()
      current_costs[i] <- input[[paste0("cost_", i)]]
      firmCostLevels(current_costs)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  })
  
  # Calculate market equilibrium
  # Returns: equilibrium price, quantity traded, and shortage/surplus
  marketData <- reactive({
    cap <- input$emissionsCap
    dem <- input$demand
    minP <- input$minPrice
    maxP <- input$maxPrice
    
    # Quantity traded is constrained by the lower of cap or demand
    eqQty <- min(cap, dem)
    
    # Price calculation based on scarcity
    if (dem > cap) {
      # Shortage scenario: price rises toward ceiling
      eqPrice <- minP + ((cap / dem) * (maxP - minP))
    } else {
      # Surplus scenario: price falls toward floor
      eqPrice <- minP + ((dem / cap) * (maxP - minP) * 0.5)
    }
    
    # Ensure price stays within floor and ceiling
    eqPrice <- max(minP, min(maxP, eqPrice))
    
    list(
      equilibriumQuantity = eqQty,
      equilibriumPrice = eqPrice,
      shortage = max(0, dem - cap),  # Positive if demand exceeds cap
      surplus = max(0, cap - dem)    # Positive if cap exceeds demand
    )
  })
  
  # Generate supply and demand curve data for plotting
  marketCurves <- reactive({
    cap <- input$emissionsCap
    dem <- input$demand
    minP <- input$minPrice
    maxP <- input$maxPrice
    
    maxQ <- max(cap, dem)
    quantities <- seq(0, maxQ, by = 2)
    
    # Supply curve: convex (increasing marginal cost)
    # As more permits are issued, each additional permit costs more
    supply <- ifelse(quantities <= cap,
                     minP + (maxP - minP) * (quantities / cap)^1.5,
                     NA)
    
    # Demand curve: concave (decreasing marginal benefit)
    # Firms value first permits highly, then less for additional permits
    demand <- ifelse(quantities <= dem,
                     maxP - (maxP - minP) * (quantities / dem)^0.7,
                     NA)
    
    data.frame(
      quantity = quantities,
      supply = supply,
      demand = demand
    )
  })
  
  # Calculate individual firm positions and trading behavior
  firmData <- reactive({
    allocation <- input$initialAllocation
    dem <- input$demand
    eqPrice <- marketData()$equilibriumPrice
    minP <- input$minPrice
    maxP <- input$maxPrice
    costs <- firmCostLevels()
    
    set.seed(42)  # For reproducible firm characteristics
    
    # Create firm data with allocations and desired emissions
    firms <- data.frame(
      firm = paste("Firm", 1:NUM_FIRMS),
      firmNum = 1:NUM_FIRMS,
      allocated = allocation / NUM_FIRMS,  # Equal initial allocation
      # Desired emissions vary by firm (70%-130% of average)
      desired = (dem / NUM_FIRMS) * runif(NUM_FIRMS, 0.7, 1.3)
    )
    
    # Calculate trading positions and decisions
    firms <- firms %>%
      mutate(
        # Cost level determines max willingness to pay
        costLevel = costs,
        willingnessToPayMax = minP + (maxP - minP) * costLevel,
        
        # Net position: positive = seller, negative = buyer
        netPosition = allocated - desired,
        action = ifelse(netPosition > 0, "Seller", "Buyer"),
        
        # Effective price: buyers pay up to their max, sellers get market price
        effectivePrice = ifelse(netPosition < 0, 
                                pmin(willingnessToPayMax, eqPrice), 
                                eqPrice),
        
        # Can only trade if willing to pay at least the market price
        canTrade = ifelse(netPosition < 0, willingnessToPayMax >= eqPrice, TRUE),
        
        # Value of permits traded (zero if can't afford to trade)
        tradingValue = ifelse(canTrade, abs(netPosition) * effectivePrice, 0)
      )
    
    firms
  })
  
  # Output: Equilibrium price
  output$eqPrice <- renderText({
    paste0("$", sprintf("%.2f", marketData()$equilibriumPrice))
  })
  
  # Output: Quantity of permits traded
  output$eqQuantity <- renderText({
    sprintf("%.0f", marketData()$equilibriumQuantity)
  })
  
  # Output: Label for shortage/surplus metric
  output$shortageLabel <- renderText({
    if (marketData()$shortage > 0) "Shortage" else "Surplus"
  })
  
  # Output: Value of shortage or surplus
  output$shortageValue <- renderText({
    value <- ifelse(marketData()$shortage > 0, 
                    marketData()$shortage, 
                    marketData()$surplus)
    sprintf("%.0f", value)
  })
  
  # Output: Total market value of traded permits
  output$marketValue <- renderText({
    value <- marketData()$equilibriumQuantity * marketData()$equilibriumPrice
    paste0("$", sprintf("%.0f", value))
  })
  
  # Plot: Supply and demand curves
  output$marketPlot <- renderPlot({
    curves <- marketCurves()
    cap <- input$emissionsCap
    
    ggplot(curves, aes(x = quantity)) +
      geom_line(aes(y = supply, color = "Supply"), size = 1.2, na.rm = TRUE) +
      geom_line(aes(y = demand, color = "Demand"), size = 1.2, na.rm = TRUE) +
      # Vertical line showing the emissions cap
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
  
  # Plot: Firm trading positions (buyer vs seller)
  output$firmPlot <- renderPlot({
    firms <- firmData()
    
    # Sort firms by net position for better visualization
    firms_summary <- firms %>%
      arrange(netPosition) %>%
      mutate(firmSorted = row_number())
    
    ggplot(firms_summary, aes(x = firmSorted, y = netPosition, fill = action)) +
      geom_bar(stat = "identity", width = 0.7) +
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
  
  # Table: Detailed firm trading information
  output$firmTable <- renderDT({
    firms <- firmData()
    
    # Format data for display
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
    
    # Create interactive datatable with color coding
    datatable(
      firms_display,
      options = list(
        pageLength = NUM_FIRMS,
        dom = 't',  # Show only table (no search/pagination for 6 firms)
        ordering = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      # Color code buyer/seller roles
      formatStyle(
        'Role',
        backgroundColor = styleEqual(c('Buyer', 'Seller'), c('#fed7aa', '#bfdbfe'))
      ) %>%
      # Color code trading capability
      formatStyle(
        'Can Trade',
        backgroundColor = styleEqual(c('Yes', 'No'), c('#d1fae5', '#fee2e2'))
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)