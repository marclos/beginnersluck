library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# =============================================================================
# Southern California Cap and Trade System
# =============================================================================
# This Shiny app simulates California's cap-and-trade program using real
# Southern California industrial facilities. It models how these facilities
# trade emission allowances under California's AB 32 Climate Solutions Act.
#
# Based on California Air Resources Board (CARB) regulations and actual
# facility types in the South Coast Air Quality Management District.
# =============================================================================

# Define real Southern California facilities
# Based on major emission sources in LA, Orange, Riverside, San Bernardino counties
FACILITIES <- data.frame(
  name = c(
    "Chevron El Segundo Refinery",
    "Phillips 66 Los Angeles Refinery",
    "Torrance Refinery (PBF Energy)",
    "AES Alamitos Power Plant",
    "Inland Empire Energy Center",
    "CEMEX Riverside Cement Plant"
  ),
  type = c("Refinery", "Refinery", "Refinery", "Power", "Power", "Cement"),
  location = c("El Segundo", "Carson/Wilmington", "Torrance", "Long Beach", "Riverside", "Riverside"),
  # Baseline emissions in metric tons CO2e per year (realistic estimates)
  baseline_emissions = c(4500, 4200, 3800, 2800, 3200, 1500),
  # Cost to reduce emissions ($/ton) - varies by technology and sector
  abatement_cost = c(65, 70, 68, 45, 48, 85),
  stringsAsFactors = FALSE
)

# UI Definition
# =============================================================================
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
      
      .info-box {
        background-color: #e0f2fe;
        border-left: 4px solid #0284c7;
        padding: 12px;
        margin: 10px 0;
        border-radius: 4px;
      }
      
      .facility-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
        gap: 10px;
        margin-top: 10px;
      }
      .facility-item {
        padding: 10px;
        background: #f8f9fa;
        border-radius: 4px;
        border-left: 3px solid #3b82f6;
      }
      .facility-item label {
        font-size: 11px;
        font-weight: bold;
        display: block;
        margin-bottom: 5px;
      }
      .facility-item .facility-type {
        font-size: 10px;
        color: #6c757d;
        font-style: italic;
      }
      .facility-item input {
        width: 100%;
        padding: 5px;
        font-size: 12px;
      }
    "))
  ),
  
  titlePanel("Southern California Cap and Trade System v3"),
  
  div(class = "info-box",
      p(strong("California's Cap-and-Trade Program"), 
        "- This simulation models how major Southern California facilities trade emission allowances under CARB regulations. 
        Allowance prices reflect actual California Carbon Allowance (CCA) market dynamics (typically $20-40/ton).",
        style = "margin: 0; font-size: 13px;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Regulatory Parameters"),
      
      # Annual emissions cap set by CARB
      sliderInput("emissionsCap",
                  "Annual Emissions Cap (1000s tons CO2e):",
                  min = 10, max = 30, value = 20, step = 1),
      
      helpText("California's declining cap reduces emissions ~3% annually"),
      
      hr(),
      
      h4("Market Conditions"),
      
      # Reserve price (price floor)
      sliderInput("reservePrice",
                  "Allowance Reserve Price ($):",
                  min = 15, max = 35, value = 25, step = 1),
      
      helpText("CARB's minimum auction price (adjusted for inflation)"),
      
      # Price containment (ceiling)
      sliderInput("ceilingPrice",
                  "Price Containment ($):",
                  min = 50, max = 100, value = 70, step = 5),
      
      helpText("Price Containment Reserve triggers additional allowances"),
      
      hr(),
      
      sliderInput("allowancePrice",
                  "Current Market Price ($):",
                  min = 20, max = 45, value = 32, step = 1),
      
      helpText("Actual CCA prices range $28-38 (2024-2025)")
    ),
    
    mainPanel(
      width = 9,
      
      # Key market metrics
      fluidRow(
        column(3, 
               div(class = "metric-box", style = "background-color: #dbeafe;",
                   div(class = "metric-label", "Clearing Price"),
                   div(class = "metric-value", textOutput("clearingPrice", inline = TRUE))
               )
        ),
        column(3,
               div(class = "metric-box", style = "background-color: #dcfce7;",
                   div(class = "metric-label", "Allowances Traded"),
                   div(class = "metric-value", textOutput("allowancesTraded", inline = TRUE))
               )
        ),
        column(3,
               div(class = "metric-box", style = "background-color: #fed7aa;",
                   div(class = "metric-label", "Total Compliance Cost"),
                   div(class = "metric-value", textOutput("complianceCost", inline = TRUE))
               )
        ),
        column(3,
               div(class = "metric-box", style = "background-color: #e9d5ff;",
                   div(class = "metric-label", "Emissions Reduced"),
                   div(class = "metric-value", textOutput("emissionsReduced", inline = TRUE))
               )
        )
      ),
      
      br(),
      
      plotOutput("marketPlot", height = "400px"),
      
      br(),
      
      h4("Facility Compliance Positions"),
      plotOutput("facilityPlot", height = "300px"),
      
      br(),
      
      h4("Facility Production Adjustments (% of baseline)"),
      p("Adjust production levels to simulate operational changes. Lower production = fewer emissions but also reduced output.", 
        style = "font-size: 13px; color: #6c757d;"),
      
      uiOutput("facilityInputs"),
      
      br(),
      
      h4("Facility Compliance Details"),
      DTOutput("facilityTable")
    )
  )
)

# Server Logic
# =============================================================================
server <- function(input, output, session) {
  
  NUM_FACILITIES <- nrow(FACILITIES)
  
  # Reactive value storing production levels for each facility (as % of baseline)
  productionLevels <- reactiveVal({
    rep(1.0, NUM_FACILITIES)  # Start at 100% baseline production
  })
  
  # Generate dynamic UI inputs for facility production levels
  output$facilityInputs <- renderUI({
    production <- productionLevels()
    
    inputs <- lapply(1:NUM_FACILITIES, function(i) {
      div(class = "facility-item",
          tags$label(FACILITIES$name[i]),
          div(class = "facility-type", paste0(FACILITIES$type[i], " - ", FACILITIES$location[i])),
          sliderInput(
            inputId = paste0("prod_", i),
            label = NULL,
            min = 0.5,
            max = 1.2,
            value = production[i],
            step = 0.05,
            post = "%"
          )
      )
    })
    
    div(class = "facility-grid", inputs)
  })
  
  # Update production levels when any facility input changes
  lapply(1:NUM_FACILITIES, function(i) {
    observeEvent(input[[paste0("prod_", i)]], {
      current_prod <- productionLevels()
      current_prod[i] <- input[[paste0("prod_", i)]]
      productionLevels(current_prod)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  })
  
  # Calculate facility-level compliance data
  facilityData <- reactive({
    cap <- input$emissionsCap * 1000  # Convert to tons
    marketPrice <- input$allowancePrice
    reservePrice <- input$reservePrice
    production <- productionLevels()
    
    # Calculate actual emissions based on production levels
    facilities <- FACILITIES %>%
      mutate(
        production_level = production,
        actual_emissions = baseline_emissions * production_level,
        # Free allocation based on baseline (typically 90% for industrial, less for power)
        free_allocation = ifelse(type %in% c("Refinery", "Cement"), 
                                 baseline_emissions * 0.90,
                                 baseline_emissions * 0.75),
        # Net position: negative = need to buy, positive = can sell
        net_position = free_allocation - actual_emissions,
        compliance_action = ifelse(net_position < 0, "Buy Allowances", "Sell Allowances"),
        # Marginal abatement cost determines willingness to pay
        willingness_to_pay = abatement_cost,
        # Can trade if price is below abatement cost (for buyers)
        can_trade = ifelse(net_position < 0, 
                           marketPrice <= willingness_to_pay,
                           TRUE),
        # Cost calculation
        allowance_cost = ifelse(can_trade & net_position < 0,
                                abs(net_position) * marketPrice,
                                0),
        # If can't buy allowances, must reduce emissions (more expensive)
        abatement_needed = ifelse(!can_trade & net_position < 0,
                                  abs(net_position),
                                  0),
        abatement_expense = abatement_needed * abatement_cost,
        # Total compliance cost
        total_cost = allowance_cost + abatement_expense,
        # Revenue from selling excess allowances
        revenue = ifelse(net_position > 0, net_position * marketPrice, 0)
      )
    
    facilities
  })
  
  # Calculate market-wide metrics
  marketData <- reactive({
    facilities <- facilityData()
    cap <- input$emissionsCap * 1000
    
    total_demand <- sum(abs(facilities$net_position[facilities$net_position < 0]))
    total_supply <- sum(facilities$net_position[facilities$net_position > 0])
    
    # Actual emissions after compliance actions
    total_actual_emissions <- sum(facilities$actual_emissions)
    baseline_total <- sum(FACILITIES$baseline_emissions * productionLevels())
    
    # Market clearing price (simplified - based on marginal abatement cost)
    buyers <- facilities %>% filter(net_position < 0)
    if (nrow(buyers) > 0) {
      # Price tends toward marginal abatement cost of buyers
      clearingPrice <- weighted.mean(buyers$abatement_cost, abs(buyers$net_position))
      clearingPrice <- max(input$reservePrice, min(input$ceilingPrice, clearingPrice))
    } else {
      clearingPrice <- input$reservePrice
    }
    
    list(
      clearingPrice = clearingPrice,
      allowancesTraded = min(total_demand, total_supply),
      totalComplianceCost = sum(facilities$total_cost),
      emissionsReduced = baseline_total - total_actual_emissions,
      shortage = max(0, total_demand - total_supply)
    )
  })
  
  # Generate supply and demand curves
  marketCurves <- reactive({
    facilities <- facilityData()
    reservePrice <- input$reservePrice
    ceilingPrice <- input$ceilingPrice
    
    # DEMAND CURVE: Buyers sorted by willingness to pay (highest first)
    # Shows how much quantity is demanded at each price level
    buyers <- facilities %>% 
      filter(net_position < 0) %>%
      arrange(desc(abatement_cost))  # Highest willingness to pay first
    
    # Build demand curve (downward sloping step function)
    if (nrow(buyers) > 0) {
      cumulative_qty <- cumsum(abs(buyers$net_position))
      
      # Create step function: price on y-axis, cumulative quantity on x-axis
      demand_df <- data.frame(
        quantity = c(0, cumulative_qty),
        price = c(max(buyers$abatement_cost) * 1.1, buyers$abatement_cost)
      )
    } else {
      demand_df <- data.frame(quantity = c(0, 100), price = c(ceilingPrice, reservePrice))
    }
    
    # SUPPLY CURVE: Sellers sorted by asking price (lowest first)
    # Shows how much quantity is supplied at each price level
    sellers <- facilities %>%
      filter(net_position > 0) %>%
      arrange(abatement_cost)  # Lowest cost sellers first (willing to sell at lower prices)
    
    # Build supply curve (upward sloping step function starting at reserve price)
    if (nrow(sellers) > 0) {
      cumulative_qty <- cumsum(sellers$net_position)
      
      # Sellers' asking prices increase with quantity
      # Base price on reserve price plus markup based on their opportunity cost
      asking_prices <- reservePrice + (0:(nrow(sellers)-1)) * 3 + 
        (sellers$abatement_cost - min(sellers$abatement_cost)) * 0.2
      
      supply_df <- data.frame(
        quantity = c(0, cumulative_qty),
        price = c(reservePrice, asking_prices)
      )
    } else {
      # No supply available - show high price at zero quantity
      supply_df <- data.frame(quantity = c(0, 0, 0.1), 
                              price = c(reservePrice, ceilingPrice, ceilingPrice))
    }
    
    list(demand = demand_df, supply = supply_df)
  })
  
  # Outputs
  output$clearingPrice <- renderText({
    paste0("$", sprintf("%.2f", marketData()$clearingPrice))
  })
  
  output$allowancesTraded <- renderText({
    sprintf("%.0f", marketData()$allowancesTraded)
  })
  
  output$complianceCost <- renderText({
    paste0("$", sprintf("%.1fM", marketData()$totalComplianceCost / 1e6))
  })
  
  output$emissionsReduced <- renderText({
    sprintf("%.0f tons", marketData()$emissionsReduced)
  })
  
  # Market plot
  output$marketPlot <- renderPlot({
    curves <- marketCurves()
    mktData <- marketData()
    
    # Combine curves for plotting
    demand_df <- curves$demand %>% mutate(curve = "Demand")
    supply_df <- curves$supply %>% mutate(curve = "Supply")
    
    # Calculate plot limits
    max_qty <- max(c(demand_df$quantity, supply_df$quantity), na.rm = TRUE)
    
    ggplot() +
      # Demand curve (step function - horizontal then vertical = downward sloping)
      geom_step(data = demand_df, aes(x = quantity, y = price, color = curve), 
                linewidth = 1.2, direction = "vh") +
      # Supply curve (step function - vertical then horizontal = upward sloping)  
      geom_step(data = supply_df, aes(x = quantity, y = price, color = curve), 
                linewidth = 1.2, direction = "vh") +
      # Regulatory price bounds
      geom_hline(yintercept = input$reservePrice, linetype = "dashed", 
                 color = "darkgreen", linewidth = 0.8, alpha = 0.6) +
      geom_hline(yintercept = input$ceilingPrice, linetype = "dashed", 
                 color = "darkred", linewidth = 0.8, alpha = 0.6) +
      annotate("text", x = max_qty * 0.02, 
               y = input$reservePrice, label = "Reserve Price", 
               color = "darkgreen", vjust = -0.5, hjust = 0, size = 3.5) +
      annotate("text", x = max_qty * 0.02, 
               y = input$ceilingPrice, label = "Price Ceiling", 
               color = "darkred", vjust = -0.5, hjust = 0, size = 3.5) +
      # Mark equilibrium point
      geom_point(aes(x = mktData$allowancesTraded, y = mktData$clearingPrice),
                 color = "purple", size = 4, shape = 19) +
      annotate("text", x = mktData$allowancesTraded, y = mktData$clearingPrice,
               label = "Equilibrium", color = "purple", vjust = -1, hjust = 0.5,
               fontface = "bold", size = 3.5) +
      scale_color_manual(values = c("Demand" = "#dc2626", "Supply" = "#059669")) +
      labs(
        title = "California Allowance Market - Supply and Demand",
        x = "Emission Allowances (tons CO2e)",
        y = "Price ($/ton)",
        color = "Curve"
      ) +
      xlim(0, max_qty * 1.1) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "top"
      )
  })
  
  # Facility positions plot
  output$facilityPlot <- renderPlot({
    facilities <- facilityData()
    
    facilities_plot <- facilities %>%
      arrange(net_position) %>%
      mutate(facility_order = row_number(),
             short_name = gsub(" (Refinery|Power Plant|Cement Plant)", "", name))
    
    ggplot(facilities_plot, aes(x = reorder(short_name, net_position), 
                                y = net_position, 
                                fill = compliance_action)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
      scale_fill_manual(
        values = c("Buy Allowances" = "#f97316", "Sell Allowances" = "#60a5fa")
      ) +
      labs(
        title = "Facility Compliance Positions",
        x = "Facility",
        y = "Net Position (tons CO2e)",
        fill = "Action"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
      )
  })
  
  # Facility table
  output$facilityTable <- renderDT({
    facilities <- facilityData()
    
    facilities_display <- facilities %>%
      mutate(
        Facility = name,
        Type = type,
        Location = location,
        `Production` = sprintf("%.0f%%", production_level * 100),
        `Actual Emissions` = sprintf("%.0f", actual_emissions),
        `Free Allocation` = sprintf("%.0f", free_allocation),
        `Net Position` = sprintf("%+.0f", net_position),
        Action = compliance_action,
        `Abatement Cost` = sprintf("$%.0f", abatement_cost),
        `Can Trade` = ifelse(can_trade, "Yes", "No"),
        `Compliance Cost` = sprintf("$%.2fM", total_cost / 1e6)
      ) %>%
      select(Facility, Type, Location, Production, `Actual Emissions`, 
             `Free Allocation`, `Net Position`, Action, `Abatement Cost`, 
             `Can Trade`, `Compliance Cost`)
    
    datatable(
      facilities_display,
      options = list(
        pageLength = NUM_FACILITIES,
        dom = 't',
        ordering = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Action',
        backgroundColor = styleEqual(
          c('Buy Allowances', 'Sell Allowances'), 
          c('#fed7aa', '#bfdbfe')
        )
      ) %>%
      formatStyle(
        'Can Trade',
        backgroundColor = styleEqual(c('Yes', 'No'), c('#d1fae5', '#fee2e2'))
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)