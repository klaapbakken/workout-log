library("dplyr")
library("RMySQL")
library("stringr")
library("purrr")
library("lubridate")

sqlString <- function(value){
  return(paste0("\"", as.character(value), "\""))
}

sqlDate <- function(value){
  
}

sqlAdapt <- function(value){
  if (is.character(value) && value != "NULL"){
    return(sqlString(value))
  }
  else{
    return(value)
  }
}

dbInsert <- function(con, table, data_frame, auto_increment=FALSE){
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
  print(query)
  dbSendQuery(con, query)
}

con <- DBI::dbConnect(MySQL(), host="localhost",
                      port=3306, password=rstudioapi::askForPassword(),
                      dbname="workoutlog", user="oyvinkla")


equipment_df <- tibble(ID = rep("NULL", 4),
                       Equipment = c("Treadmill", "Spinning Bike",
                                     "Dumbbells", "Kettlebells"),
                       Description = c("For running", "For biking",
                                       "For lifting", "For lifting"))
workout_df <- tibble(ID = c("NULL"),
                     date = c(paste0("2014", "04", "03", "19", "22", "00") %>%
                       ymd_hms() %>%
                       as.character()),
                     length = c(60),
                     shape = c(5),
                     performance = c(10),
                     note = c("Crushed it")
                     )

exercise_df <- tibble(ID = rep("NULL", 4),
                      name = c("Sit-ups", "Push-ups", "Squat", "Running"))

exercise_in_workout_df <- tibble(exerciseID = c(1,1,1),
                                 workoutID = c(1,2,4))

exercise_with_equipment_df <- tibble(ID = rep("NULL", 1),
                                     exerciseID = c(4),
                                     equipmentID = c(1),
                                     kg = c(0),
                                     sets = c(1))

exercise_without_equipment_df <- tibble(ID = rep("NULL", 3),
                                        exerciseID = c(1,2,3),
                                        description = rep("Self - explanatory"))
                      
exercise_group_df <- tibble(ID = c("NULL"),
                            groupDesc = c("Can be done at home"))

exercise_in_group_df <- tibble(groupID = c(1,1,1),
                               exerciseID = c(1,2,3))

dbInsert(con, "equipment", equipment_df)
dbInsert(con, "workout", workout_df)
dbInsert(con, "exercise", exercise_df)
dbInsert(con, "exerciseInWorkout", exercise_in_workout_df)
dbInsert(con, "exerciseWithEquipment", exercise_with_equipment_df)
dbInsert(con, "exerciseWithoutEquipment", exercise_without_equipment_df)
dbInsert(con, "exerciseGroup", exercise_group_df)
dbInsert(con, "exerciseInGroup", exercise_in_group_df)
                   
                   