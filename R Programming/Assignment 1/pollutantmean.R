pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  files <- list.files(path=directory, full.names = TRUE)
  filenames <- files[id]
  data <- lapply(filenames, function(x) {read.csv(file=x, header = T)})
  Reduce(function(x,y) {merge(x,y)}, data)
  
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  total <- 0
  num <- 0
  
  for (i in 1:length(id)){
    out <- data[[i]]
    out <- out[[pollutant]]
    out <- out[!is.na(out)]
    total <- total + sum(out)
    num <- num + length(out)
  }

  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  totalMean <- total / num
  return(totalMean)
  print(totalMean, digits = 4)
}