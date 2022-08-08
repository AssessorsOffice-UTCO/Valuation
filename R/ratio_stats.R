#' Build a ratio analysis comparable to SPSS "Ratio Statistics".
#'
#' This function returns a named list of Ratio Statistics comparing predicted and actual
#' sale prices
#'
#' @param assessed_value Numeric vector of assessed values (must be same length as sold_prices)
#' @param sold_prices Numeric vector of actual sold prices (must be same length as preds)
#' @param CI The confidence interval between 0 and 1. Defaults to .95
#' @return A named list of ratio statistics:
#'     Mean (incl. lower and upper bounds), Median (incl. lower and upper bounds),
#'     Actual coverage, Price related differential, Coefficient of dispersion, and
#'     median centered coefficient of variation. The "Ratio" refers to Assessed Value / Sold Price
#' @examples
#' preds <- runif(100,min = 100000,500000)
#' sales <- runif(100,min = 100000,500000)
#' RatioAnalysis(preds = preds,sold_prices = sales, CI = .95)
#' When Price_Related_Differential less than .98, high value properties are over appraised
#' When Price_Related_Differential greater than 1.03, high value properties are under appraised
#' @export

ratio_stats <- function(assessed_value,sold_prices,CI=0.95){

  # calculate ratio mean and median
  ratio <- assessed_value / sold_prices
  CI <- CI
  meantest <- t.test(ratio,mu = 1,na.rm=TRUE,conf.level = CI)
  mean_estimate <- unname(meantest$estimate)
  mean_lowerbound <- unname(meantest$conf.int[1])
  mean_upperbound <- unname(meantest$conf.int[2])
  median_test <- MedianCI(ratio,na.rm=TRUE,conf.level = CI)
  median_estimate <- unname(median_test[1])
  median_lowerbound <- unname(median_test[2])
  median_upperbound <- unname(median_test[3])

  # weighted mean
  weighted_mean <- sum(assessed_value,na.rm = TRUE) / sum(sold_prices,na.rm = TRUE)

  # coefficient of dispersion
  COD <- (sum(abs(ratio - median_estimate),na.rm = TRUE) / length(ratio)) / median_estimate

  # coefficient of variation
  COV <- sd(ratio,na.rm = TRUE) / mean(ratio,na.rm = TRUE)

  # price related differential
  PRD <- mean(ratio,na.rm = TRUE) / weighted_mean

  # Build a table of the calculations
  RA_Stats <- list(Mean=mean_estimate, #numeric
                   Mean_Lower=mean_lowerbound, #numeric
                   Mean_Upper=mean_upperbound, #numeric
                   Median=median_estimate, #numeric
                   Median_Lower=median_lowerbound, #numeric
                   Median_Upper=median_upperbound, #numeric
                   Actual_Coverage=paste0(CI*100,"%"), #character
                   Price_Related_Differential=PRD, #numeric
                   Coef_of_Dispersion=COD, #numeric
                   Coef_of_Variation=paste0(round(COV*100,2),"%")) #character

  return(RA_Stats)

}





