library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  
  titlePanel("Emissions Reduction Trajectories"),
  
  sidebarLayout(
    sidebarPanel(
      
      numericInput("start_year", "Start year:", value = 2025, min = 1900, max = 2100),
      numericInput("start_emissions", "Starting emissions (Gt CO₂-eq):", value = 54.7),
      
      checkboxGroupInput(
        "rates",
        "Annual reduction rates:",
        choices = c("-0.5%" = -0.005, "-1.0%" = -0.01, "-1.5%" = -0.015,
                    "-2.0%" = -0.02, "-2.5%" = -0.025, "-3.0%" = -0.03,
                    "-3.5%" = -0.035),
        selected = c(-0.02)   # default = –2%
      ),
      
      checkboxGroupInput(
        "targets",
        "Target years:",
        choices = seq(2030, 2100, by = 10),
        selected = c(2030, 2040, 2050)
      )
    ),
    
    mainPanel(
      plotOutput("traj_plot", height = "550px")
    )
  )
)

server <- function(input, output, session) {
  
  traj_data <- reactive({
    start <- input$start_year
    end <- max(as.integer(input$targets), 2100)
    years <- seq(start, end, by = 1)
    
    expand.grid(year = years, rate = as.numeric(input$rates)) %>%
      mutate(
        t = year - start,
        emissions = input$start_emissions * exp(rate * t)
      )
  })
  
  output$traj_plot <- renderPlot({
    
    traj_df <- traj_data()
    
    ggplot(traj_df, aes(x = year, y = emissions, color = as.factor(rate))) +
      geom_line(size = 1.4) +
      # ✅ Removed geom_point that caused oscillation
      scale_color_discrete(name = "Annual Reduction Rate") +
      labs(
        x = "Year",
        y = "Emissions (Gt CO₂-eq)",
        title = "Exponential Emissions Reduction Trajectories"
      ) +
      theme_minimal(base_size = 15)
  })
}

shinyApp(ui = ui, server = server)
