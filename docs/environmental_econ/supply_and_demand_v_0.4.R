library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Marc's Supply & Demand Interactive Model (v. 0.4)"),
  
  tags$head(
    tags$style(HTML("
      .well { background-color: #f8f9fa; }
      .scenario-btn { margin: 5px; }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Scenario Selection
      h4("Quick Scenarios"),
      actionButton("scenario_normal", "Normal Market", class = "btn-primary scenario-btn", width = "100%"),
      actionButton("scenario_inelastic", "Inelastic Demand", class = "btn-info scenario-btn", width = "100%"),
      actionButton("scenario_elastic", "Elastic Demand", class = "btn-info scenario-btn", width = "100%"),
      actionButton("scenario_shortage", "Shortage (Price Ceiling)", class = "btn-warning scenario-btn", width = "100%"),
      actionButton("scenario_surplus", "Surplus (Price Floor)", class = "btn-warning scenario-btn", width = "100%"),
      
      hr(),
      
      # Price Control
      h4(style = "color: #6c757d;", "Price Controls"),
      checkboxInput("enable_price_control", "Enable Price Control", value = FALSE),
      
      conditionalPanel(
        condition = "input.enable_price_control",
        selectInput("price_control_type", "Type:",
                    choices = c("Price Ceiling (Max Price)" = "ceiling",
                                "Price Floor (Min Price)" = "floor"),
                    selected = "ceiling"),
        sliderInput("control_price", "Control Price:",
                    min = 0, max = 100, value = 30, step = 1)
      ),
      
      hr(),
      
      # Demand Controls
      h4(style = "color: #dc3545;", "Demand Curve"),
      selectInput("demand_type", "Curve Shape:",
                  choices = c("Linear" = "linear",
                              "Exponential" = "exponential",
                              "Logarithmic" = "logarithmic",
                              "Power" = "power"),
                  selected = "linear"),
      
      conditionalPanel(
        condition = "input.demand_type == 'power'",
        sliderInput("demand_curvature", "Curvature:",
                    min = 0, max = 0.1, value = 0.02, step = 0.01)
      ),
      
      sliderInput("demand_slope", "Slope:",
                  min = -5, max = -0.1, value = -2, step = 0.1),
      
      sliderInput("demand_intercept", "Intercept:",
                  min = 50, max = 150, value = 100, step = 5),
      
      hr(),
      
      # Supply Controls
      h4(style = "color: #007bff;", "Supply Curve"),
      selectInput("supply_type", "Curve Shape:",
                  choices = c("Linear" = "linear",
                              "Exponential" = "exponential",
                              "Logarithmic" = "logarithmic",
                              "Power" = "power"),
                  selected = "linear"),
      
      conditionalPanel(
        condition = "input.supply_type == 'power' || input.supply_type == 'exponential'",
        sliderInput("supply_curvature", "Curvature:",
                    min = 0.01, max = 0.1, value = 0.02, step = 0.005)
      ),
      
      sliderInput("supply_slope", "Slope:",
                  min = 0.1, max = 3, value = 1.5, step = 0.1),
      
      sliderInput("supply_intercept", "Intercept:",
                  min = 0, max = 50, value = 10, step = 5),
      
      hr(),
      
      checkboxInput("show_equilibrium", "Show Equilibrium", value = TRUE),
      actionButton("reset", "Reset to Default", class = "btn-secondary", width = "100%")
    ),
    
    mainPanel(
      width = 9,
      
      plotOutput("supply_demand_plot", height = "500px"),
      
      br(),
      
      conditionalPanel(
        condition = "input.show_equilibrium",
        wellPanel(
          style = "background-color: #d4edda; border: 2px solid #28a745;",
          h4(style = "color: #155724;", "Market Equilibrium"),
          fluidRow(
            column(6,
                   div(style = "background-color: white; padding: 15px; border-radius: 5px;",
                       h5("Equilibrium Quantity"),
                       h3(textOutput("eq_quantity"))
                   )
            ),
            column(6,
                   div(style = "background-color: white; padding: 15px; border-radius: 5px;",
                       h5("Equilibrium Price"),
                       h3(textOutput("eq_price"))
                   )
            )
          ),
          br(),
          p("At equilibrium, the quantity demanded equals the quantity supplied, and the market clears with no shortage or surplus.")
        )
      ),
      
      conditionalPanel(
        condition = "input.enable_price_control",
        wellPanel(
          style = "background-color: #fff3cd; border: 2px solid #ffc107;",
          h4(style = "color: #856404;", uiOutput("price_control_title")),
          fluidRow(
            column(4,
                   div(style = "background-color: white; padding: 15px; border-radius: 5px;",
                       h5("Quantity Demanded"),
                       h3(textOutput("qd_at_control"))
                   )
            ),
            column(4,
                   div(style = "background-color: white; padding: 15px; border-radius: 5px;",
                       h5("Quantity Supplied"),
                       h3(textOutput("qs_at_control"))
                   )
            ),
            column(4,
                   div(style = "background-color: white; padding: 15px; border-radius: 5px;",
                       h5(uiOutput("imbalance_label")),
                       h3(style = "font-weight: bold;", textOutput("imbalance"))
                   )
            )
          ),
          br(),
          uiOutput("price_control_explanation")
        )
      ),
      
      wellPanel(
        style = "background-color: #cfe2ff; border: 2px solid #0d6efd;",
        h4(style = "color: #084298;", "Key Concepts"),
        tags$ul(
          tags$li(strong(style = "color: #dc3545;", "Demand Curve:"), 
                  "Shows the quantity consumers want to buy at each price. Generally slopes downward because people buy more when prices are lower."),
          tags$li(strong(style = "color: #007bff;", "Supply Curve:"), 
                  "Shows the quantity producers want to sell at each price. Generally slopes upward because producers supply more when prices are higher."),
          tags$li(strong(style = "color: #800080;", "Equilibrium:"), 
                  "The point where supply equals demand. This determines the market price and quantity traded."),
          tags$li(strong(style = "color: #dc3545;", "Shortage:"), 
                  "Occurs when quantity demanded exceeds quantity supplied, typically caused by a price ceiling (maximum price) below equilibrium."),
          tags$li(strong(style = "color: #007bff;", "Surplus:"), 
                  "Occurs when quantity supplied exceeds quantity demanded, typically caused by a price floor (minimum price) above equilibrium."),
          tags$li(strong("Curve Shapes:"), 
                  "Different curve shapes (linear, exponential, logarithmic, power) represent different market conditions and elasticities.")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Function to calculate demand price
  calculate_demand <- function(q, slope, intercept, type, curvature = 0.02) {
    if (type == "linear") {
      price <- intercept + slope * q
    } else if (type == "exponential") {
      price <- intercept * exp(slope * q / 20)
    } else if (type == "logarithmic") {
      price <- intercept + slope * 10 * log(q + 1)
    } else if (type == "power") {
      price <- intercept - (q ^ (1 + curvature))
    }
    return(pmax(0, price))
  }
  
  # Function to calculate supply price
  calculate_supply <- function(q, slope, intercept, type, curvature = 0.02) {
    if (type == "linear") {
      price <- intercept + slope * q
    } else if (type == "exponential") {
      price <- intercept + slope * exp(curvature * q)
    } else if (type == "logarithmic") {
      price <- intercept + slope * 10 * log(q + 1)
    } else if (type == "power") {
      price <- intercept + (q ^ (1 + curvature))
    }
    return(pmax(0, price))
  }
  
  # Generate data
  generate_data <- reactive({
    q <- seq(0, 50, by = 0.5)
    
    demand_price <- calculate_demand(
      q, 
      input$demand_slope, 
      input$demand_intercept, 
      input$demand_type,
      input$demand_curvature
    )
    
    supply_price <- calculate_supply(
      q, 
      input$supply_slope, 
      input$supply_intercept, 
      input$supply_type,
      input$supply_curvature
    )
    
    data.frame(
      quantity = q,
      demand = demand_price,
      supply = supply_price
    )
  })
  
  # Calculate equilibrium
  equilibrium <- reactive({
    data <- generate_data()
    data$diff <- abs(data$demand - data$supply)
    eq_row <- data[which.min(data$diff), ]
    
    list(
      quantity = eq_row$quantity,
      price = (eq_row$demand + eq_row$supply) / 2
    )
  })
  
  # Calculate quantities at control price
  control_quantities <- reactive({
    if (!input$enable_price_control) return(NULL)
    
    data <- generate_data()
    control_p <- input$control_price
    
    # Find quantity demanded at control price
    qd_row <- data[which.min(abs(data$demand - control_p)), ]
    qd <- qd_row$quantity
    
    # Find quantity supplied at control price
    qs_row <- data[which.min(abs(data$supply - control_p)), ]
    qs <- qs_row$quantity
    
    list(
      qd = qd,
      qs = qs,
      imbalance = qd - qs
    )
  })
  
  # Plot
  output$supply_demand_plot <- renderPlot({
    data <- generate_data()
    eq <- equilibrium()
    
    p <- ggplot(data, aes(x = quantity)) +
      geom_line(aes(y = demand, color = "Demand"), linewidth = 1.5) +
      geom_line(aes(y = supply, color = "Supply"), linewidth = 1.5) +
      scale_color_manual(values = c("Demand" = "#dc3545", "Supply" = "#007bff")) +
      labs(
        title = "Supply and Demand Curves",
        x = "Quantity",
        y = "Price ($)",
        color = ""
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95")
      ) +
      coord_cartesian(ylim = c(0, max(c(data$demand, data$supply)) * 1.1))
    
    if (input$show_equilibrium && eq$quantity > 0 && eq$price > 0) {
      p <- p +
        geom_point(aes(x = eq$quantity, y = eq$price), 
                   color = "#800080", size = 5, shape = 19) +
        geom_vline(xintercept = eq$quantity, linetype = "dashed", 
                   color = "#28a745", alpha = 0.5) +
        geom_hline(yintercept = eq$price, linetype = "dashed", 
                   color = "#28a745", alpha = 0.5) +
        annotate("text", x = eq$quantity, y = eq$price, 
                 label = "Equilibrium", vjust = -1, hjust = -0.1,
                 color = "#800080", fontface = "bold", size = 5)
    }
    
    # Add price control visualization
    if (input$enable_price_control) {
      control_p <- input$control_price
      cq <- control_quantities()
      
      p <- p +
        geom_hline(yintercept = control_p, linetype = "solid", 
                   color = "#ffc107", linewidth = 2, alpha = 0.8) +
        annotate("text", x = 45, y = control_p, 
                 label = ifelse(input$price_control_type == "ceiling", 
                                "Price Ceiling", "Price Floor"), 
                 vjust = -0.5, hjust = 1,
                 color = "#856404", fontface = "bold", size = 5)
      
      # Show shortage or surplus
      if (cq$imbalance != 0) {
        min_q <- min(cq$qd, cq$qs)
        max_q <- max(cq$qd, cq$qs)
        
        # Add shaded region for shortage/surplus
        shade_color <- ifelse(cq$imbalance > 0, "#dc354555", "#007bff55")
        
        p <- p +
          annotate("rect", xmin = min_q, xmax = max_q, 
                   ymin = 0, ymax = control_p,
                   fill = shade_color, alpha = 0.3) +
          geom_segment(aes(x = cq$qs, y = control_p, 
                           xend = cq$qd, yend = control_p),
                       arrow = arrow(length = unit(0.3, "cm"), ends = "both"),
                       color = "#856404", linewidth = 1.2)
      }
    }
    
    p
  })
  
  # Equilibrium outputs
  output$eq_quantity <- renderText({
    eq <- equilibrium()
    paste0(round(eq$quantity, 2), " units")
  })
  
  output$eq_price <- renderText({
    eq <- equilibrium()
    paste0("$", round(eq$price, 2))
  })
  
  # Price control outputs
  output$price_control_title <- renderUI({
    if (input$price_control_type == "ceiling") {
      "Price Ceiling Analysis"
    } else {
      "Price Floor Analysis"
    }
  })
  
  output$qd_at_control <- renderText({
    cq <- control_quantities()
    if (is.null(cq)) return("N/A")
    paste0(round(cq$qd, 2), " units")
  })
  
  output$qs_at_control <- renderText({
    cq <- control_quantities()
    if (is.null(cq)) return("N/A")
    paste0(round(cq$qs, 2), " units")
  })
  
  output$imbalance_label <- renderUI({
    cq <- control_quantities()
    if (is.null(cq)) return("N/A")
    
    if (cq$imbalance > 0) {
      HTML("<span style='color: #dc3545;'>Shortage</span>")
    } else if (cq$imbalance < 0) {
      HTML("<span style='color: #007bff;'>Surplus</span>")
    } else {
      "Balance"
    }
  })
  
  output$imbalance <- renderText({
    cq <- control_quantities()
    if (is.null(cq)) return("N/A")
    paste0(round(abs(cq$imbalance), 2), " units")
  })
  
  output$price_control_explanation <- renderUI({
    cq <- control_quantities()
    if (is.null(cq)) return(NULL)
    
    if (input$price_control_type == "ceiling") {
      p("A price ceiling sets a maximum price below equilibrium. At this lower price, consumers want to buy more than producers want to sell, creating a shortage. The shaded area shows the gap between quantity demanded and quantity supplied.")
    } else {
      p("A price floor sets a minimum price above equilibrium. At this higher price, producers want to sell more than consumers want to buy, creating a surplus. The shaded area shows the gap between quantity supplied and quantity demanded.")
    }
  })
  
  # Scenario buttons
  observeEvent(input$scenario_normal, {
    updateSliderInput(session, "demand_slope", value = -2)
    updateSliderInput(session, "demand_intercept", value = 100)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 10)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
    updateCheckboxInput(session, "enable_price_control", value = FALSE)
  })
  
  observeEvent(input$scenario_inelastic, {
    updateSliderInput(session, "demand_slope", value = -4)
    updateSliderInput(session, "demand_intercept", value = 120)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 10)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
    updateCheckboxInput(session, "enable_price_control", value = FALSE)
  })
  
  observeEvent(input$scenario_elastic, {
    updateSliderInput(session, "demand_slope", value = -0.5)
    updateSliderInput(session, "demand_intercept", value = 80)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 10)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
    updateCheckboxInput(session, "enable_price_control", value = FALSE)
  })
  
  observeEvent(input$scenario_shortage, {
    # Set up normal curves first
    updateSliderInput(session, "demand_slope", value = -2)
    updateSliderInput(session, "demand_intercept", value = 100)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 10)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
    
    # Enable price ceiling below equilibrium
    updateCheckboxInput(session, "enable_price_control", value = TRUE)
    updateSelectInput(session, "price_control_type", selected = "ceiling")
    updateSliderInput(session, "control_price", value = 30)
  })
  
  observeEvent(input$scenario_surplus, {
    # Set up normal curves first
    updateSliderInput(session, "demand_slope", value = -2)
    updateSliderInput(session, "demand_intercept", value = 100)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 10)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
    
    # Enable price floor above equilibrium
    updateCheckboxInput(session, "enable_price_control", value = TRUE)
    updateSelectInput(session, "price_control_type", selected = "floor")
    updateSliderInput(session, "control_price", value = 60)
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session, "demand_slope", value = -2)
    updateSliderInput(session, "demand_intercept", value = 100)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 10)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
    updateCheckboxInput(session, "enable_price_control", value = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)