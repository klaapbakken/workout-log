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

con <- DBI::dbConnect(MySQL(), host="mysql.stud.ntnu.no",
                      password="dbpw",
                      dbname="oyvinkla_workoutLog", user="oyvinkla")

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

exercise_in_workout_df <- tibble(workoutId = character(),
                                 exerciseId = character())

exercise_with_equipment_df <- tibble(id = character(),
                                     exerciseId = character(),
                                     equipmentId = character(),
                                     kg = numeric(),
                                     sets = integer())

exercise_without_equipment_df <- tibble(id = character(),
                                        exerciseId = character(),
                                        description = character())

exercise_group_df <- tibble(id = character(),
                            description = character())

exercise_in_group_df <- tibble(groupId = character(),
                               exerciseId = character())

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
      tabPanel("Equipment exercise registration",
               textInput("equipmentExerciseName",
                         "Name of exercise:"),
               numericInput("weight",
                            "Weight: ",
                            value=10, min=0, max=300),
               numericInput("sets",
                            "Sets: ",
                            value=5, min=0, max=100),
               uiOutput("equipmentSelector"),
               actionButton("equipmentExerciseSubmit",
                            "Submit exercise")
      ),
      tabPanel("Body exercise registration",
               textInput("bodyExerciseName",
                         "Name of exercise:"),
               textInput("bodyExerciseDescription",
                         "Description: "),
               actionButton("bodyExerciseSubmit",
                            "Submit exercise")
      ),
      tabPanel("Equipment registration",
               textInput("equipmentName",
                         "Name of equipment"),
               textInput("equipmentDescription",
                         "Description of equipment: "),
               actionButton("equipmentSubmit",
                             "Submit equipment")),
      tabPanel("Create exercise groups",
               textInput("groupDescription",
                         "Description of group:"),
               uiOutput("exerciseToGroup"),
               actionButton("submitGroup", 
                            "Submit group")),
      tabPanel("Recent workouts",
               numericInput("n",
               "Number of recent workouts: ",
               value = 10),
               DT::dataTableOutput("recentWorkouts")),
      tabPanel("Workout history",
               dateInput("start",
                         "Starting date:"),
               dateInput("end",
                         "End date:"),
               DT::dataTableOutput("workoutWindow")),
      tabPanel("Show exercise groups",
               uiOutput("similarGroup"),
               DT::dataTableOutput("exerciseGroupDT"))
      )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  exercise_names <- reactiveVal(tbl(con, "exercise") %>%
    select(exerciseName) %>%
    collect() %>%
    pull() %>%
    as.list())
  
  equipment_names <- reactiveVal(tbl(con, "equipment") %>%
                                   select(equipmentName) %>%
                                   collect() %>%
                                   pull() %>%
                                   as.list())
  
  group_ids = reactiveVal()
  
  observeEvent(input$bodyExerciseSubmit, {
    exercise_df <- exercise_df %>%
      add_row(id = "NULL",
              name = input$bodyExerciseName)
    dbInsert(con, "exercise", exercise_df)
    exercise_df <- exercise_df[-1, ]
    
    new_exercise_names <- tbl(con, "exercise") %>%
      select(exerciseName) %>%
      pull() %>%
      as.list()
    exercise_names(new_exercise_names)
    
    last_exercise_id <- dbGetQuery(con, "select last_insert_id()")
    
    exercise_without_equipment_df <- exercise_without_equipment_df %>% 
      add_row(id = "NULL",
              exerciseId = last_exercise_id,
              description = input$bodyExerciseDescription)
    dbInsert(con, "exerciseWithoutEquipment", exercise_without_equipment_df)
    exercise_without_equipment_df <- exercise_without_equipment_df[-1, ]
    
  })
  
  observeEvent(input$equipmentExerciseSubmit, {
    exercise_df <- exercise_df %>%
      add_row(id = "NULL",
              name = input$equipmentExerciseName)
    dbInsert(con, "exercise", exercise_df)
    exercise_df <- exercise_df[-1, ]
    
    new_exercise_names <- tbl(con, "exercise") %>%
      select(exerciseName) %>%
      pull() %>%
      as.list()
    exercise_names(new_exercise_names)
    
    last_exercise_id <- dbGetQuery(con, "select last_insert_id()")
    
    selected_equipment <- input$equipmentSelection
    equipment_id <- tbl(con, "equipment") %>%
      filter(equipmentName == selected_equipment) %>%
      select(equipmentId) %>%
      collect() %>%
      slice(1) %>%
      as.character()
    
    exercise_with_equipment_df <- exercise_with_equipment_df %>% 
      add_row(id = "NULL",
              exerciseId = last_exercise_id,
              equipmentId = equipment_id,
              kg = input$weight,
              sets = input$sets)
    dbInsert(con, "exerciseWithEquipment", exercise_with_equipment_df)
    exercise_without_equipment_df <- exercise_without_equipment_df[-1, ]
    
  })
  
  output$equipmentSelector <- renderUI({
    checkboxGroupInput("equipmentSelection",
                "Choose equipment:",
                choices = equipment_names())
  })
  
  
  output$exerciseSelector <- renderUI({
    checkboxGroupInput("exerciseSelection",
                     "Choose exercises: ", 
                     choices = exercise_names())
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
    
    last_workout_id <- dbGetQuery(con, "select last_insert_id()")
    
    for (exercise in input$exerciseSelection){
      id <- tbl(con, "exercise") %>%
        filter(exerciseName == exercise) %>%
        select(exerciseId) %>%
        collect() %>%
        slice(1)
        
      exercise_in_workout_df <- exercise_in_workout_df %>% add_row(workoutId = as.character(last_workout_id),
                                         exerciseId = as.character(id))
    }
    dbInsert(con, "exerciseInWorkout", exercise_in_workout_df)
    exercise_in_workout_df <- tibble(workoutId = character(),
                                     exerciseId = character())
  })
  
  observeEvent(input$equipmentSubmit, {
    equipment_df <- equipment_df %>%
      add_row(id = "NULL",
              name = input$equipmentName,
              description = input$equipmentDescription)
    dbInsert(con, "equipment", equipment_df)
    equipment_df <- equipment_df[-1, ]
    
    new_equipment_names <- tbl(con, "equipment") %>%
      select(equipmentName) %>%
      collect() %>%
      pull() %>%
      as.list()
    equipment_names(new_equipment_names)
  })
  
  observeEvent(input$submitGroup, {
    exercise_group_df <- exercise_group_df %>%
      add_row(id = "NULL",
              description = input$groupDescription)
    dbInsert(con, "exerciseGroup", exercise_group_df)
    
    last_group_id <- dbGetQuery(con, "select last_insert_id()")
    
    for (exercise in input$groupedExercises){
      id <- tbl(con, "exercise") %>%
        filter(exerciseName == exercise) %>%
        select(exerciseId) %>%
        collect() %>%
        slice(1)
      
      exercise_in_group_df <- exercise_in_group_df %>% add_row(groupId = as.character(last_group_id),
                                                                   exerciseId = as.character(id))
    }
    dbInsert(con, "exerciseInGroup", exercise_in_group_df)
    exercise_in_group_df <- tibble(groupId = character(),
                                     exerciseId = character())

  })
  
  observeEvent(input$similarTo, {
    group_ids(tbl(con, "exerciseInGroup") %>%
      left_join(tbl(con, "exercise"), by = c("exerciseID" = "exerciseId")) %>%
      filter(exerciseName == as.character(input$similarTo)) %>%
      select(groupID) %>%
      collect() %>%
      pull()) %>%
      as.list()
  })
  
  output$recentWorkouts <- DT::renderDataTable({
    tbl(con, "workout") %>%
      arrange(desc(workoutDatetime)) %>% 
      collect() %>%
      slice(1:input$n)
  })
  
  output$workoutWindow <- DT::renderDataTable({
    tbl(con, "workout") %>%
      filter(between(workoutDatetime, input$start, input$end)) %>%
      collect()
  })
  
  output$exerciseToGroup <- renderUI({
    checkboxGroupInput("groupedExercises",
                       "Exercises to add to group",
                       choices = exercise_names())
  })
  
  output$similarGroup <- renderUI({
    selectInput("similarTo", 
                "Similar To: ", 
                choices = exercise_names())
  })
  
  output$exerciseGroupDT <- DT::renderDataTable({
    ids <- group_ids()
    tbl(con, "exerciseGroup") %>%
      filter(groupID %in% ids) %>%
      collect()
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

