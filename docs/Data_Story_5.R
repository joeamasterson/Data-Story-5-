library(shiny)
library(dplyr)
library(DT)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

log_data <- function(newdata, file = "survey_data.csv") {
  if (!file.exists(file)) {
    write.csv(newdata, file, row.names = FALSE)
  } else {
    write.table(
      newdata,
      file,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE
    )
  }
}

if (file.exists("survey_data.csv")) {
  starting_data <- read.csv("survey_data.csv", stringsAsFactors = FALSE)
} else {
  starting_data <- data.frame(
    name = character(),
    age = numeric(),
    siblings = numeric(),
    pets = numeric(),
    location = character(),
    climate = character(),
    sport = character(),
    school_year = character(),
    favorite_food = character(),
    social_media = character(),
    sleep_hours = numeric(),
    music_genre = character(),
    dream_city = character(),
    streaming_hours = numeric(),
    stringsAsFactors = FALSE
  )
}

responses <- reactiveVal(starting_data)

ui <- fluidPage(
  
  titlePanel("Fun Facts"),
  
  br(),
  
  fluidRow(
    column(3, textInput("name", "Name")),
    column(3, numericInput("age", "Age", value = 18, min = 0, max = 100)),
    column(3, numericInput("siblings", "Number of Siblings", value = 0, min = 0, max = 10)),
    column(3, numericInput("pets", "Number of Pets", value = 0, min = 0, max = 20))
  ),
  
  fluidRow(
    column(3, textInput("location", "Where Do You Live?")),
    column(3, selectInput("climate", "Climate",
                          choices = c("Hot", "Cold", "Moderate"))),
    column(3, selectInput("sport", "Favorite Sport to Watch",
                          choices = c("Football", "Basketball", "Baseball",
                                      "Soccer", "Golf", "Tennis", "Other"))),
    column(3, selectInput("school_year", "Year in School",
                          choices = c("Freshman", "Sophomore", "Junior", "Senior", "Other")))
  ),
  
  fluidRow(
    column(3, textInput("favorite_food", "Favorite Food")),
    column(3, selectInput("social_media", "Favorite Social Media App",
                          choices = c("Instagram", "TikTok", "Snapchat", "X",
                                      "Facebook", "YouTube", "Other"))),
    column(3, numericInput("sleep_hours", "Hours of Sleep Per Night", value = 8, min = 0, max = 24)),
    column(3, selectInput("music_genre", "Favorite Music Genre",
                          choices = c("Pop", "Rap", "Country", "Rock",
                                      "Hip-Hop", "Classical", "Other")))
  ),
  
  fluidRow(
    column(6, textInput("dream_city", "Dream City to Visit")),
    column(6, numericInput("streaming_hours", "Hours of TV/Streaming Per Day", value = 2, min = 0, max = 24))
  ),
  
  br(),
  
  actionButton("submit", "Submit", class = "btn-primary"),
  
  br(), br(),
  
  h3("Survey Responses"),
  DTOutput("table")
)

server <- function(input, output, session) {
  
  observeEvent(input$submit, {
    
    new_row <- data.frame(
      name = input$name,
      age = input$age,
      siblings = input$siblings,
      pets = input$pets,
      location = input$location,
      climate = input$climate,
      sport = input$sport,
      school_year = input$school_year,
      favorite_food = input$favorite_food,
      social_media = input$social_media,
      sleep_hours = input$sleep_hours,
      music_genre = input$music_genre,
      dream_city = input$dream_city,
      streaming_hours = input$streaming_hours,
      stringsAsFactors = FALSE
    )
    
    responses(bind_rows(responses(), new_row))
    log_data(new_row)
    updateTextInput(session, "name", value = "")
    updateNumericInput(session, "age", value = 18)
    updateNumericInput(session, "siblings", value = 0)
    updateNumericInput(session, "pets", value = 0)
    updateTextInput(session, "location", value = "")
    updateSelectInput(session, "climate", selected = "Hot")
    updateSelectInput(session, "sport", selected = "Football")
    updateSelectInput(session, "school_year", selected = "Freshman")
    updateTextInput(session, "favorite_food", value = "")
    updateSelectInput(session, "social_media", selected = "Instagram")
    updateNumericInput(session, "sleep_hours", value = 8)
    updateSelectInput(session, "music_genre", selected = "Pop")
    updateTextInput(session, "dream_city", value = "")
    updateNumericInput(session, "streaming_hours", value = 2)
  })
  
  output$table <- renderDT({
    datatable(
      responses(),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)