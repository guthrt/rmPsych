#' standard Error of Wide Format Data
#'
#' This function allows you to iteratively obtain standard error for each individual column in a wide format dataframe in which each row represents a particular subject and each column represents a specific test or task. This format generally displays summary data for particular tests or tasks where each individual cell is a mean or median of several trials of that particular test or task. 
#' @param data The dataframe. Note that the dataframe should be in wide format where the first column represents individual subjects with only one row per subject and each additional column represents a specific test or task.
#' @param na.rm Default set to FALSE. If TRUE, missing data will be removed from the data frame before calculations proceed.
#' @keywords standard error, wide format, summary data
#' @export
#' @return dataframe
#' @examples subject_num <- c(1, 2, 3, 4) task1 <- c(99, 87, 82, 76) task2 <- c(67, 74, 68, 88) total <- as.data.frame(cbind(subject_num, task1, task2)) se_wide_format(total)
#' se_wide_format()



se_wide_format <- function(data, na.rm = FALSE) {
  se <- vector()
  test <- colnames(data)
  for (i in 2:ncol(data)) {
    if (na.rm)
      data <- data[!is.na(data)]
    x <- (sd(unlist(data[i])))/sqrt(nrow(data))
    se[i] <- x
  }
  output <- as.data.frame(cbind(test, se))
  output <- output[-1,]
}


#' Effect Size for Repeated Measures Using Cohen's D
#'
#' This function allows you to calculate Cohen's D effect size for a repeated measures design. 
#' @param data A dataframe for a repeated measures design
#' @param col1 The name of the column that represents a paticular condition 
#' @param col2 The name of the repeated measure that is being compared to the first column that was identify
#' @param na.rm Default set to FALSE. If TRUE, missing data will be removed from the data frame before calculations proceed.
#' @keywords effect size, cohen's d, repeated measures
#' @export
#' @examples neutral <- c(.92, .94, .84, .82, .93, .76, .78, .84, .97, .98) emotion <- c(.86, .78, .45, .75, .87, .65, .63, .76, .87, .91) total <- as.data.frame(cbind(neutral, emotion)) cohen_d(total, col1 = "neutral", col2 = "emotion")
#' se_wide_format()


cohen_d <- function(data, col1, col2, na.rm = FALSE) {
  if (na.rm) {
    data <- data[!is.na(data)]
  }
  pooled <- sqrt(((sd(data[[col1]])^2) + (sd(data[[col2]])^2))/2)
  (mean(data[[col1]]) - mean(data[[col2]]))/pooled
}


#' Importing list of Matlab files
#'
#' This function allows you to import a list of specific .mat files in your working directory by specifying a particular pattern that is in the matlab file name. 
#' @param pattern A pattern that is unique to a set of matlab files in your working directory in the form of a string
#' @keywords .mat, matlab, pattern, list, import
#' @export
#' @return list of files
#' @examples mat_file_import("_test1_")


mat_file_import <- function(pattern) {
  files <- list.files(pattern = "\\.mat")
  files <- files[grep(pattern, files)]
  map(files, rmatio::read.mat)
}

setwd("./rmPsych") 
document()


install("rmPsych")
library(rmPsych)