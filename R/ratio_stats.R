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
  ratio <- ratio[!is.infinite(ratio)] # remove any sales for $0 that would mess up mean calculation
  CI <- CI
  meantest <- t.test(ratio,na.rm=TRUE,conf.level = CI)
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

  # price related bias (from Daniel's function)
  val = .5 * (assessed_value / median(ratio)) + .5*sold_prices
  ln_val = log(val)/0.693
  diff_val = (ratio - median(ratio)) / median(ratio)
  fit <- lm(diff_val ~ ln_val)
  ret_val <- coef(fit)[2]
  PRB <- c(ret_val, confint(fit, level = CI)[2,])

  # current best guess of how NCSS decides which stat to recommend ...
  # Shapiro Test of normailty of ratios
  # If it IS normal, use the mean, if NOT normal, use median
  if(length(ratio) <= 5000) {
  shap.test <- shapiro.test(ratio)
  shap.pval <- shap.test$p.value
    }
  # NCSS documentation mentions shapiro test and pvalue cutoff of 0.10
  stat_to_use <- ifelse(shap.pval < 0.10 | is.na(shap.pval), "use median", "use mean")

  # Build a table of the calculations
  RA_Stats <- list(Count=length(ratio), #numeric
                   Mean=mean_estimate, #numeric
                   Mean_Lower=mean_lowerbound, #numeric
                   Mean_Upper=mean_upperbound, #numeric
                   Median=median_estimate, #numeric
                   Median_Lower=median_lowerbound, #numeric
                   Median_Upper=median_upperbound, #numeric
                   Actual_Coverage=paste0(CI*100,"%"), #character
                   Price_Related_Differential=PRD, #numeric
                   Coef_of_Dispersion=COD, #numeric
                   Coef_of_Variation=COV,
                   Price_Related_Bias=unname(PRB[1]),
                   NormTest_Results=stat_to_use) #numeric

  return(RA_Stats)

}




