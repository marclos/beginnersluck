library(shiny)
library(shinysurveys)

df <- data.frame(question = "What session date are you evaluating?",
                 option = c("Session 1", "Session 2"),
                 input_type = "mc",
                 input_id = "favorite_food",
                 dependence = NA,
                 dependence_value = NA,
                 required = F)

ui <- fluidPage(
  surveyOutput(df = df,
               survey_title = "Hello, World!",
               survey_description = "Welcome! This is a demo survey showing off the {shinysurveys} package.")
)


server <- function(input, output, session) {
  renderSurvey()
  # Upon submission, print a data frame with participant responses
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Congrats, you completed your first shinysurvey!",
      "You can customize what actions happen when a user finishes a survey using input$submit."
    ))    
    print(getSurveyData())
  })
}

shinyApp(ui, server)

