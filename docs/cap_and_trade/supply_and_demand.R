library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Supply & Demand Interactive Learning"),
  
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
      actionButton("scenario_shortage", "Shortage", class = "btn-warning scenario-btn", width = "100%"),
      actionButton("scenario_surplus", "Surplus", class = "btn-warning scenario-btn", width = "100%"),
      
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
      
      wellPanel(
        style = "background-color: #cfe2ff; border: 2px solid #0d6efd;",
        h4(style = "color: #084298;", "Key Concepts"),
        tags$ul(
          tags$li(strong(style = "color: #dc3545;", "Demand Curve:"), 
                  "Shows the quantity consumers want to buy at each price. Generally slopes downward because people buy more when prices are lower."),
          tags$li(strong(style = "color: #007bff;", "Supply Curve:"), 
                  "Shows the quantity producers want to sell at each price. Generally slopes upward because producers supply more when prices are higher."),
          tags$li(strong(style = "color: #28a745;", "Equilibrium:"), 
                  "The point where supply equals demand. This determines the market price and quantity traded."),
          tags$li(strong("Curve Shapes:"), 
                  "Different curve shapes (linear, exponential, logarithmic, power) represent different market conditions and elasticities.")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store current state
  rv <- reactiveValues()
  
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
  
  # Plot
  output$supply_demand_plot <- renderPlot({
    data <- generate_data()
    eq <- equilibrium()
    
    p <- ggplot(data, aes(x = quantity)) +
      geom_line(aes(y = demand, color = "Demand"), size = 1.5) +
      geom_line(aes(y = supply, color = "Supply"), size = 1.5) +
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
                   color = "#28a745", size = 5, shape = 19) +
        geom_vline(xintercept = eq$quantity, linetype = "dashed", 
                   color = "#28a745", alpha = 0.5) +
        geom_hline(yintercept = eq$price, linetype = "dashed", 
                   color = "#28a745", alpha = 0.5) +
        annotate("text", x = eq$quantity, y = eq$price, 
                 label = "Equilibrium", vjust = -1, hjust = -0.1,
                 color = "#28a745", fontface = "bold", size = 5)
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
  
  # Scenario buttons
  observeEvent(input$scenario_normal, {
    updateSliderInput(session, "demand_slope", value = -2)
    updateSliderInput(session, "demand_intercept", value = 100)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 10)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
  })
  
  observeEvent(input$scenario_inelastic, {
    updateSliderInput(session, "demand_slope", value = -0.5)
    updateSliderInput(session, "demand_intercept", value = 80)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 10)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
  })
  
  observeEvent(input$scenario_elastic, {
    updateSliderInput(session, "demand_slope", value = -4)
    updateSliderInput(session, "demand_intercept", value = 120)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 10)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
  })
  
  observeEvent(input$scenario_shortage, {
    updateSliderInput(session, "demand_slope", value = -2)
    updateSliderInput(session, "demand_intercept", value = 120)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 30)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
  })
  
  observeEvent(input$scenario_surplus, {
    updateSliderInput(session, "demand_slope", value = -2)
    updateSliderInput(session, "demand_intercept", value = 80)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 5)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session, "demand_slope", value = -2)
    updateSliderInput(session, "demand_intercept", value = 100)
    updateSliderInput(session, "supply_slope", value = 1.5)
    updateSliderInput(session, "supply_intercept", value = 10)
    updateSelectInput(session, "demand_type", selected = "linear")
    updateSelectInput(session, "supply_type", selected = "linear")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

