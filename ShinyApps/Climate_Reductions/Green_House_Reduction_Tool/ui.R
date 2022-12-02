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

    HTML("<p>Emission reductions are usually developed relative to an 
         emissions goal based on a reference year. However, we need to 
         convert these to a annualized rate for our activity. Thus, 
         I have created a very simple tool to convert promised reductions 
         into annualized values.</p>"),

    HTML("<p>The inputs include: 1) Reference Year -- based on a inventory 
         year, e.g. based on 1990 emissions; 2) Goal Year -- target year to 
         reach a reduction; 3) Reference Emissions -- amount of emmissions 
         estimated during the reference year (Gt or billons of tons of C02 
         emitted); 4) The current emissions (Gt), which 
         might be above or below the reference year. Thus, taking into 
         consideration the emissions completed so far or excesses that need 
         to be redued; 5) What is the reduction goals as a percent, often 
         based on Paris accord emissions reduction commitments. </p>"),
    
    #rate.fun(refyear = 2020, goalyear= 2050, ref=5000, current= 5400, pergoal=.40)
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("refyear", "Reference Year", value=2020, min=1980, max=2030),
            sliderInput("goalyear", "Goal Year", value=2050, min=2020, max=2070),
            numericInput("ref", "Reference Emissions (Gt)", value=1, min=0, max=15, step=.1),
            numericInput("current", "Current Emissions (Gt)", value=2, min=0, max=15, step=.1),
            numericInput("pergoal", "Percent Reduction goal from Reference", value=2, min=0, max=200, step=1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("distPlot")
            # tableOutput("pergoal")
        )
    )
))
