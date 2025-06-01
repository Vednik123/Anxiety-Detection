############################################
# Enhanced Anxiety Detection App UI/Server #
############################################

# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
# library(randomForest)
library(xgboost)
library(caret)

# Load the trained XGBoost model
model <- readRDS("final_model.rds")

# UI
ui <- fluidPage(
  theme = shinytheme("superhero"),  # You can try others like "cerulean", "cosmo", "yeti"
  
  titlePanel("🧠 Anxiety Detection App"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h3("Input Parameters"),
      tags$p("Note: 0 = No, 1 = Yes"),
      
      numericInput("Medication", label = "Medication", value = NULL, min = 0, max = 1),
      numericInput("Dizziness", label = "Dizziness", value = NULL, min = 0, max = 1),
      numericInput("Smoking", label = "Smoking", value = NULL, min = 0, max = 1),
      numericInput("Gender", label = "Gender (0 = Male, 1 = Female, 2 = Other)", value = NULL, min = 0, max = 2),
      numericInput("Family.History.of.Anxiety", label = "Family History of Anxiety", value = NULL, min = 0, max = 1),
      numericInput("Recent.Major.Life.Event", label = "Recent Major Life Event", value = NULL, min = 0, max = 1),
      numericInput("Sweating.Level..1.5.", label = "Sweating Level (1–5)", value = NULL, min = 1, max = 5),
      numericInput("Occupation", label = "Occupation Stress Level (1–5)", value = NULL, min = 1, max = 5),
      numericInput("Sleep.Hours", label = "Sleep Hours (1–10)", value = NULL, min = 1, max = 10),
      numericInput("Physical.Activity..hrs.week.", label = "Physical Activity (hrs/week)", value = NULL, min = 0, max = 10),
      
      actionButton("submitbutton", "🧾 Submit", class = "btn btn-success mt-3"),
      
      width = 4
    ),
    
    mainPanel(
      tags$h3("📊 Prediction Output"),
      verbatimTextOutput("contents"),
      tableOutput("tabledata"),
      
      tags$hr(),
      tags$p("Developed by Batch-2 Group 5")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  datasetInput <- reactive({
    
    # Collect user inputs into a data frame
    user_input <- data.frame(
      Medication = as.numeric(input$Medication),
      Dizziness = as.numeric(input$Dizziness),
      Smoking = as.numeric(input$Smoking),
      Gender = as.numeric(input$Gender),
      Family.History.of.Anxiety = as.numeric(input$Family.History.of.Anxiety),
      Recent.Major.Life.Event = as.numeric(input$Recent.Major.Life.Event),
      Sweating.Level..1.5. = as.numeric(input$Sweating.Level..1.5.),
      Occupation = as.numeric(input$Occupation),
      Sleep.Hours = as.numeric(input$Sleep.Hours),
      Physical.Activity..hrs.week. = as.numeric(input$Physical.Activity..hrs.week.)
    )
    
    # Prepare matrix as required by xgboost
    input_matrix <- model.matrix(~ . - 1, data = user_input)
    
    # Predict using XGBoost model
    pred_prob <- predict(model, input_matrix)
    pred_class <- ifelse(pred_prob > 0.5, "⚠️ Anxiety Detected", "✅ No Anxiety Detected")
    
    # Return results
    data.frame(
      Result = pred_class,
      Probability = round(pred_prob, 3)
    )
  })
  
  output$contents <- renderPrint({
    if (input$submitbutton > 0) {
      isolate("✅ Prediction complete.")
    } else {
      return("📡 Waiting for input...")
    }
  })
  
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) {
      isolate(datasetInput())
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
