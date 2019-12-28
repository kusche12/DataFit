library(ggplot2)
library(stringr)
library(chron)

dataFit <- read.csv('./DataFit_Real.csv', stringsAsFactors = FALSE)
dataFit$strength = 0
dataFit$type = ''
within(dataFit, rm('X.1'))

## Make this while loop take in the Main.Lift argument and convert it into a 1-rep-max potential or
## race pace and put it into the strength column
a <- 1
while ((!nchar(dataFit$Main.Lift[a]) == 0)) {
  workout <- dataFit$Main.Lift[a]
  activity <- substr(workout, 0, 1)
  
  intensity <- substring(workout, 2, (str_locate(pattern = 'x', workout))[1] - 1) # extracts the weight/distance
  intensity <- as.numeric(intensity)
  reps <- substring(workout, (str_locate(pattern = 'x', workout)[1] + 1), nchar(workout)) # extracts reps/time
  
  if (activity == 'B' || activity == 'D' || activity == 'O' || activity == 'S') {
    reps <- strtoi(reps)
    dataFit$strength[a] <- (intensity / (1.0278 - (.0278 * reps)))
    
  } else if (activity == 'R') {
    if (nchar(reps) >= 7) {  # If it is more than an hour
      total_min <- as.numeric(times(reps)) * 60 * 24

    } else { # Less than an hour
      total_min <- as.numeric(substring(reps, 1, 2)) + (as.double(substring(reps, 4, 5)) / 60)
    }
    dataFit$strength[a] <- (total_min / intensity) # adds the pace to the strength column
  } else { # Either rest day or No main lift
    dataFit$strength[a] <- 0.00
  }
  a = a + 1
}

## Take in the Main.Lift and return the name of the lift. Helpful for server side
a <- 1
while (!str_detect(dataFit$Main.Lift[a], 'NA')) {
  activity <- substr((dataFit$Main.Lift[a]), 0, 1)
  if (activity == 'B') {
    dataFit$type[a] <- 'Bench'
  } else if (activity == 'D') {
    dataFit$type[a] <- 'Deadlift'
  } else if (activity == 'S') {
    dataFit$type[a] <- 'Squat'
  } else if (activity == 'O') {
    dataFit$type[a] <- 'Overhead Press'
  } else if (activity == 'R') {
    dataFit$type[a] <- 'Running'
  } else {
    dataFit$type[a] <- ''
  }
  a = a + 1
}
  
write.csv(dataFit, file = 'DataFit_Real_Clean.csv')

