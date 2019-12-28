library(shiny)
library(DT)
dataFit <- read.csv('./DataFit_Real_Clean.csv', stringsAsFactors = FALSE)

shinyUI(
  navbarPage(
    'DataFit',
    tabPanel(
      'Home',
      titlePanel('Welcome to DataFit!'),
      sidebarPanel(
        
      ## Add a date
        dateInput('newDate',
                  h4('What day is it?'),
                  value = Sys.Date(),
                  format = 'mm/dd/yyyy'),
                  
      ## Add your weight
        numericInput('newWeight', value = 0.0,
                     h4('How much do you weigh?')),
        
      ## Add your calories
        numericInput('newCals', value = 0.0,
                     h4('How many calories are you going to eat?')),
        
      ## Add your main lift
        selectInput('newMain', choices = list('Bench', 'Deadlift', 'Squat', 'Overhead Press', 
                                              'Running', 'Rest Day', 'Other'),
                    h4('What are you going to train?')),
        
        conditionalPanel(condition = "input.newMain == 'Bench' | input.newMain == 'Squat' | 
                         input.newMain == 'Deadlift' | input.newMain == 'Overhead Press'",
                         numericInput('weight', value = 0.0, h5('How much weight?')),
                         numericInput('reps', value = 0.0, h5('How many reps?'))
        ),
        
        conditionalPanel(condition = "input.newMain == 'Running'",
                         numericInput('distance', value = 0.0, h5('What was your distance?')),
                         textInput('time', value = 0.0, h5('What was your time?'))
        
        ),
        
        conditionalPanel(condition = "input.newMain == 'Rest Day' | input.newMain == 'Other'", 
                         helpText('')),
      
        ## Add a note
        textInput('newNotes', 'Add a note:', value=''),
        
        ## Confirm Date
        tags$head(tags$script(src = 'message-handler.js')),
        actionButton('add', 'Add Entry')
      ),
      
      mainPanel(
        uiOutput('table')
      )
    ),
    
    tabPanel (
      'Progress by the numbers',
      
      # Prompts user to choose a lift to show on line graph
      sidebarPanel(
        radioButtons('categories', label = 'Select the progression you would like to see', 
                     choices = list('Bench', 'Deadlift', 'Squat',
                                    'Overhead Press', 'Running', 'Body Weight'))
      ),
      mainPanel(
        plotOutput('workoutType') # Line plot key
      )
    ),
    
    tabPanel (
      'Progress by the day',
      sidebarPanel (
        dateInput('date', label = 'Select day you would like to see', 
                  min = '2019-06-05', max = '2019-12-31', format = 'mm/dd/yyyy')
      ),
      mainPanel(
        htmlOutput('workoutDay'),
        uiOutput('workoutPic')
      )
    )
  )
)
