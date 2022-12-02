#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$distPlot <- renderTable({
        
        # generate annualized rate based on inputs from ui.R
        
        ref_reduction = input$ref*input$pergoal/100; 
        goal=input$ref-ref_reduction; 
        n = input$goalyear-input$refyear
            
        reduction=input$current-goal
            
        rate = round(((input$current - reduction)/input$current)^(1/n)-1,4)
        # test fv = 1000*(1+.1)^5; fv
        future = input$current*(1+ rate)^n
        output = data.frame(Ref_Year=input$refyear, 
                Goal_Year=input$goalyear, 
                Ref_Emmissions=input$ref, 
                Current_Emmissions=input$current, 
                Goal=input$pergoal, 
                Reduction_Rate=rate*100, 
                Future_Emmissions=future)
        print(output)
        
    })
    
})