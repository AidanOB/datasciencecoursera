corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  filenames <- list.files(path=directory, full.names = TRUE)
  
  data <- lapply(filenames, function(x) {read.csv(file=x, header = T)})
  Reduce(function(x,y) {merge(x,y)}, data)
  
  output <- NULL
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
               
  for(i in 1:length(filenames)){
    if(sum(complete.cases(data[[i]])) > threshold) {
      #Extract the data for the two variables we're interested in
      nitr <- data[[i]]$nitrate
      sulf <- data[[i]]$sulfate
      #make them into a 2xn matrix
      mat <- rbind(nitr, sulf)
      #transpose into a nx2 matrix
      mat <- t(mat)
      #convert into a data frame for use with the corr function
      mat <- data.frame(mat)
      corr <- cor(mat, use = "complete.obs")
      #cor outputs a 2x2 matrix, with the second and third indicies the 
      #correlation between the two metrics
      if(is.null(output[[1]])) { #check if output is still null
        output <- corr[2]
      } else {
        output <- rbind(output, corr[2])
      }
    }
  }
  
  ## Return a numeric vector of correlations
  return (output)
}