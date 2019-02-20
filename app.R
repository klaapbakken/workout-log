library(shiny)
library("dplyr")
library("RMySQL")
library("stringr")
library("purrr")
library("lubridate")
library("dbplyr")

sqlString <- function(value){
  return(paste0("\"", as.character(value), "\""))
}

sqlAdapt <- function(value){
  if (is.character(value) && value != "NULL"){
    return(sqlString(value))
  }
  else{
    return(value)
  }
}

dbInsert <- function(con, table, data_frame){
  query <- str_replace("insert into .db", ".db", table) %>%
    paste("values ")
  for (row in 1:nrow(data_frame)){
    query <- paste0(query, "(")
    for (column in 1:(ncol(data_frame) - 1)){
      query <- query %>% paste0(data_frame[row, column] %>%
                                  pull() %>%
                                  sqlAdapt(), ",")
    }
    query <- query %>% paste0(data_frame[row, column + 1] %>%
                                pull() %>%
                                sqlAdapt(), "),")
  }
  query <- query %>% str_sub(1, -2) %>% paste0(";")
  dbSendQuery(con, query)
}

con <- DBI::dbConnect(MySQL(), host="localhost",
                      port=3306, password=rstudioapi::askForPassword(),
                      dbname="workoutlog", user="oyvinkla")

exercise_names <- tbl(con, "exercise") %>%
  select(exerciseName) %>%
  pull() %>%
  as.list()

workout_df <- tibble(id = character(),
                     datetime = character(),
                     length = integer(),
                     shape = integer(),
                     performance = integer(),
                     note = character())

exercise_df <- tibble(id = character(),
                        name = character())

equipment_df <- tibble(id = character(),
                       name = character(),
                       description = character())

# Define UI for application that draws a histogram
ui <- fluidPage(
        titlePanel("Workout Log"),
        tabsetPanel(         
        tabPanel("Workout Registration",
         dateInput("workoutDate",
                     "Date:"),
         numericInput("workoutHour",
                      "Hour:",
                      12, min=0, max=23),
         numericInput("workoutMinute",
                      "Minute:",
                      0, min=0, max=59),
         numericInput("workoutSecond",
                      "Second: ",
                      0, min=0, max=59),
         numericInput("workoutLength",
                      "Length of workout:",
                      30),
         numericInput("shape",
                      "Shape:",
                      5, min=1, max=10),
         numericInput("performance",
                      "Performance:",
                      5, min=1, max=10),
         textInput("workoutNote",
                   "Note:"),
         uiOutput("exerciseSelector"),
         actionButton("workoutSubmit",
                      "Submit workout")
      ),
      tabPanel("Exercise registration",
               textInput("exerciseName",
                         "Name of exercise:"),
               actionButton("exerciseSubmit",
                            "Submit exercise")
      ),
      tabPanel("Equipment registration",
               textInput("equipmentName",
                         "Name of equipment"),
               textInput("equipmentDescription",
                         "Description of equipment: "),
               actionButton("equipmentSubmit",
                             "Submit equipment"))
      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$exerciseSelector <- renderUI({
    checkboxGroupInput("exerciseSelector",
                       "Choose exercises (" %>%
                         paste0(length(input$exerciseSelector),
                                "selected):"), 
                       exercise_names)
  })
  
  observeEvent(input$exerciseSubmit, {
    exercise_names <- tbl(con, "exercise") %>%
      select(exerciseName) %>%
      pull() %>%
      as.list()
  })
  
  observeEvent(input$workoutSubmit, {
    datetime <- ymd_hms(paste0(input$workoutDate,
                       input$workoutHour %>%
                         as.character() %>%
                         str_pad(2, side="left", pad="0"),
                       input$workoutMinute %>%
                         as.character() %>%
                         str_pad(2, side="left", pad="0"),
                       input$workoutSecond %>%
                         as.character() %>%
                         str_pad(2, side="left", pad="0"))) %>%
      as.character()
    
    workout_df <- workout_df %>%
      add_row(id = "NULL",
              datetime = datetime,
              length = input$workoutLength,
              shape = input$shape,
              performance = input$performance,
              note = input$workoutNote)
    dbInsert(con, "workout", workout_df)
    workout_df <- workout_df[-1, ]
  })
  
  observeEvent(input$exerciseSubmit, {
    exercise_df <- exercise_df %>%
      add_row(id = "NULL",
              name = input$exerciseName)
    dbInsert(con, "exercise", exercise_df)
    exercise_df <- exercise_df[-1, ]
  })
  
  observeEvent(input$equipmentSubmit, {
    equipment_df <- equipment_df %>%
      add_row(id = "NULL",
              name = input$equipmentName,
              description = input$equipmentDescription)
    dbInsert(con, "equipment", equipment_df)
    equipment_df <- equipment_df[-1, ]
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

