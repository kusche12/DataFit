## File name converter
#STEP 1: SET WORKING DIRECTORY TO WWW
#STEP 2: SET THE MONTH STRING AND DIGIT

dirIn = getwd()
dn = list.files(dirIn, full.names = FALSE)

i <- 1

while (i < length(dn)) {
  filename <- dn[i]

  if (substring(filename, 0, 5) == 'Photo') { ## Rename photo
    
    if (substring(filename, 7, 9) == 'Dec') { ## December
      date <- paste0('2019-12-', substring(filename, 11, 12))
      newfile1 <- paste0(date, '-1.jpg')
      file.rename(filename, newfile1)

      if (substring(filename, 11, 12) == substring(dn[i + 1], 11, 12)) { ## Two in a day
        newfile2 <- paste0(date, '-2.jpg')
        file.rename(dn[i+1], newfile2)
        i <- i + 1
      }
    }
  }
  i <- i + 1
}
