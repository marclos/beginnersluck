#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Green House Gas Emissions Calculator"),
    # Slider Input for Reference Year

#rate.fun(refyear = 2020, goalyear= 2050, ref=5000, current= 5400, pergoal=.40)
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("refyear", "Reference Year", value=2020, min=1980, max=2030),
            sliderInput("goalyear", "Goal Year", value=2050, min=2030, max=2070),
            numericInput("ref", "Reference Emissions (Gt)", value=0, min=0, max=15, step=.1),
            numericInput("current", "Current Emissions (Gt)", value=0, min=0, max=15, step=.1),
            numericInput("pergoal", "Reduction goal from Reference", value=0, min=0, max=200, step=1),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            #plotOutput("distPlot")
            tableOutput("pergoal")
        )
    )
))
