library(ggplot2)
library(stringr)
library(chron)

dataFit <- read.csv('./DataLifts_Kyle.csv', stringsAsFactors = FALSE)
dataFit$strength = 0
dataFit$X <- NULL
dataFit$X.1 <- NULL

#ggplot(data = dataFit) +
 # geom_point(mapping = aes(x = Date, y = Weight))

## Make this while loop take in the Main.Lift argument and convert it into a 1-rep-max potential or
## race pace and put it into the strength column

a <- 1
while (!str_detect(dataFit$Main.Lift[a], 'NA')) {
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
  } else {
    dataFit$Strength[a] <- 0.00
  }
  a = a + 1
}
  
write.csv(dataFit, file = 'DataFit_Kyle_Clean.csv')


dataFit[nrow(dataFit) + 1][Date = 2019-09-04]

newDate <- c(92, '2019-09-04', 158, NA, 3200, 'S300x5', 'This is a note', 'NA', 200, 'Squat')
dataFit[92, ] <- newDate

write.csv(dataFit, file = 'DataFit_Real_Clean')

dataFit <- read.csv('./DataFit_Real_Clean.csv', row.names=NULL)
dataFit[nrow(dataFit), ] <- c(0, 0,0,0,0,0,0,0)
write.csv(dataFit, './DataFit_Real_Clean.csv', row.names=FALSE)

library('magrittr')
library('dplyr')

A <- cbind(c(1,0), c(0,1))
f <- function(matrix1) {
  dot(dot(t(matrix1), A), matrix1)
}

nGrid <- 30 
x1 <- seq(-4, 4, length.out=nGrid) 
x2 <- seq(-2, 2, length.out=nGrid) 
X <- expand.grid(x1=x1, x2=x2) 
y <- apply(X, 1, function(x) f(x)) %>% 
  # f() is your function you compute at grid points 
    matrix(nrow=length(x1)) 
contour(x1, x2, y, asp=1)

typeof(dataFit[1, 2])

print('hi')

start <- 3961 + 3681
present <- 4985 + 4668
((present - start) / start )*100

present - start
