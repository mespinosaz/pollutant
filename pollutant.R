readPollutantData <- function(directory, id) {
  filePath <- paste(directory, str_pad(id, 3, pad = "0"), ".csv",sep="")
  read.csv(filePath)
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  p <- c()
  for (i in id) {
    x <- readPollutantData(directory, i)
    p <- append(p, x[[pollutant]])
  }
  p <- p[!is.na(p)]
  mean (p)
}

complete <- function(directory, id = 1:332) {
  mat <- matrix(0, ncol = 2, nrow=0)
  colnames(mat) <- c("id", "nobs")
  for (i in id) {
    x <- readPollutantData(directory, i)
    x <- x["nitrate"]
    x <- x[!is.na(x)]
    size <- length(x)
    mat <- rbind(mat, c(i, length(x)))
  }
  mat
}

corr <- function(directory, threshold = 0) {
  mat <- complete(directory)
  mat <- subset(mat, mat[, "nobs"] > threshold)
  id <- mat[,1]
  r = c()
  for (i in id) {
    x <- readPollutantData(directory, i)
    x <- x[!is.na(x[,1]) & !is.na(x[,2]),2:3]
    c <- cor(x[1], x[2])
    r <- append(r, c)
  }
  r
}
