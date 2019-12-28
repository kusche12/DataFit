library(shiny)
library(ggplot2)
library(ggthemes)
library(DT)
dataFit <- read.csv('./DataFit_Real_Clean.csv', stringsAsFactors = FALSE)

shinyServer(
  function(input, output, session) { 

     # Display dot plot of the graph
     output$workoutType <- renderPlot(
       if (input$categories %in% c('Bench', 'Squat', 'Deadlift', 'Overhead Press', 'Running')) {
         ggplot() +
           geom_point(data = dataFit[dataFit$type == input$categories, ], aes(x = Date, y = strength),
                      colour = 'gray23', size = 3) +
           labs(
             title = paste('', input$categories, 'strength over time'),
             x = 'Time',
             y = paste('', input$categories, 'strength')) +
           geom_rangeframe() +
           theme_few()
       } else {
         ggplot() +
           geom_point(data = dataFit, aes(x = Date, y = Weight), colour = 'gray23', size = 3) +
           labs(
             title = 'Body weight (in lbs) over time',
             x = 'Time',
             y = 'Body weight'
           ) +
           geom_rangeframe() +
           theme_few()
       }
     )
     
     # Display information about that day
     output$workoutDay <- renderText({ 
       day <- dataFit[dataFit$Date == toString(input$date), ]
       print(day)
       str1 <- paste('<b><font size = \"3\">', input$date, '</font></b>')
       str2 <- paste('You weighed<b>', day$Weight, 'lbs</b>')
       str3 <- paste('You ate<b>', day$Caloric.Intake, 'calories</b>')
       
       if (day$type == '') {
         str4 <- paste('<b>This was a rest day</b>')
       } else {
         str4 <- paste('You trained<b>', day$type, '</b>')
       }
       
       str5 <- paste('You completed', substr(day$Main.Lift, 2, nchar(day$Main.Lift)), 'at a strength of', round(day$strength, digits = 2))
       str6 <- paste('You took this note:', day$Notes)
       str7 <- paste('<font size = \"5\">Here\'s what you looked like!</font>')
       HTML(paste(str1, str2, str3, str4, str5, str6, br(), str7, sep = '<br/>'))
     })
     
     # Display an image
     output$workoutPic <- renderUI ({
       picture <- paste0(input$date, '-1', '.jpg')
       tags$img(src=picture, width = '400px', height = '550px')
     })
     
     # Tells user submission accepted
     observeEvent(input$add, {
       session$sendCustomMessage(type = 'testmessage',
                                 message= 'Your addition has been recorded')
     })
     
     # Add date to dataset
     Data = reactive({
        if(input$add > 0) {
          activity <- mainLift <- substr(input$newMain, 0, 1)

          if (mainLift == 'B' | mainLift == 'O' | mainLift == 'S' | mainLift == 'D') {
            mainLift <- paste0(mainLift, input$weight, 'x', input$reps)
          } else if (mainLift == '*') { 
            mainLift <- '*Rest'
          } else if (mainLift == 'R') {
            mainLift <- paste0(mainLift, input$distance, 'x', input$time)
          } else {
            mainLift <- 'NA'
          }
          
          # Find strength
          strength <- 0
          total_min <- 0

          if (activity == 'B' || activity == 'D' || activity == 'O' || activity == 'S') {
            strength <- (input$weight / (1.0278 - (.0278 * input$reps)))
            
          } else if (activity == 'R') {
            if (nchar(input$time) >= 7) {  # If it is more than an hour
              total_min <- as.numeric(times(input$time)) * 60 * 24
              
            } else { # Less than an hour
              total_min <- as.numeric(substring(input$time, 1, 2)) + (as.double(substring(input$time, 4, 5)) / 60)
            }
            strength <- (total_min / input$distance) # adds the pace to the strength column
          } else { # Either rest day or No main lift
            strength <- 0.00
          }
       
          dataFitNew <- data.frame(X = nrow(dataFit) + 1, Date = as.character(input$newDate), Weight = input$newWeight,
                                   Caloric.Intake = input$newCals, Main.Lift = mainLift,
                                   Notes = input$newNotes, strength = strength, type = input$newMain)
          dataFit <- rbind(dataFit, dataFitNew)
          write.csv(dataFit, file = 'DataFit_Real_Clean.csv', row.names = FALSE)
          
          return(list(dataFit = dataFit))
        }
     })

     output$table <- renderTable(
       dataFit, # reactive data
       class = "display nowrap compact", # style
       filter = "top", # location of column filters
       
       options = list(  # options
         scrollX = TRUE # allow user to scroll wide tables horizontally
       )
     )
     
   }
)
