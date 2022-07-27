#' Build a ratio analysis for a trained real estate valuation model.
#'
#' This function will auto-determine the name of the outcome variable based on your model formula.
#'     It takes, as inputs, the raw property data, the trained model, and returns a custom ratio analysis.
#'     This serves as a replacement for SPSS 'Ratio Analysis'
#'
#' @param dat A data frame of property sales data that was used to train a model.
#'     This is the 'raw' data.
#' @param model A trained model object
#' @param pred The name of the column of predicted values as a character string. Defaults to "pred"
#' @param CI The confidence interval between 0 and 1. Defaults to .95
#' @return A data frame of ratio statistics for the model:
#'     Mean (incl. lower and upper bounds), Median (incl. lower and upper bounds),
#'     Actual coverage, Price related differential, Coefficient of dispersion, and
#'     median centered coefficient of variation. The "Ratio" refers to Assessed Value / Sold Price
#' @examples
#' df <- data.frame(BR=c(1,2,3,4,1,2,3,4,1,2,3,4),SQFT=c(1000,1500,1750,2500,1100,1250,1550,2350,1350,1600,2000,2300),ASSESSED=c(300000,350000,475000,560000,349000,387000,421000,622000,368000,402000,521000,600000),SOLD=c(295000,356000,470000,555000,342000,380000,411000,630000,363000,407000,501000,610000))
#' mod <- glm(SOLD ~ BR + SQFT,data=df)
#' RatioAnalysis(dat=df,model = mod)
#' @export






RatioAnalysis <- function(dat=dat,model=mod,pred="pred",CI=.95){

  # dat is your original data frame of observations
  # model is the model you wish to test that has already been trained on your observations
  # pred is the name of the predicted values (in quotes...defaults to "pred")
  # CI is the confidence interval you wish to use for mean and median estimation of the ratios (default is .95)


  # package dependencies
  packages = c("tidyverse", "modelr","DescTools")
  ## Now load or install&load all
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )

  if(!"tidyverse" %in% (.packages())){
    stop("tidyverse needs to be loaded.")
  }
  if(!"modelr" %in% (.packages())){
    stop("modelr needs to be loaded.")
  }
  if(!"DescTools" %in% (.packages())){
    stop("DescTools needs to be loaded.")
  }


  # auto-determine response variable from designated model
  outcome <- model$formula[2] %>% as.character()


  #  add predictions in new data frame inside this function
  df_pred <- add_predictions(data=dat,model=model,var = pred)

  # ratio
  ratio <- pluck(df_pred,pred) / pluck(df_pred,outcome)
  CI <- CI
  meantest <- t.test(ratio,mu = 1,na.rm=TRUE,conf.level = CI)
  mean_estimate <- meantest$estimate %>% unname()
  mean_lowerbound <- meantest$conf.int[1] %>% unname()
  mean_upperbound <- meantest$conf.int[2] %>% unname()
  median_test <- MedianCI(ratio,na.rm=TRUE,conf.level = CI) # from DescTools package
  median_estimate <- median_test[1] %>% unname()
  median_lowerbound <- median_test[2] %>% unname()
  median_upperbound <- median_test[3] %>% unname()
  # weighted mean
  weighted_mean <- sum(df_pred[,pred],na.rm = TRUE) / sum(df_pred[,outcome],na.rm = TRUE)
  # coefficient of dispersion
  COD <- (sum(abs(ratio - median_estimate),na.rm = TRUE) / length(ratio)) / median_estimate
  # coefficient of variation
  COV <- sd(ratio,na.rm = TRUE) / mean(ratio,na.rm = TRUE)
  # price related differential
  PRD <- mean(ratio,na.rm = TRUE) / weighted_mean
  # Build a table of the calculations
  RA_Table <- data.frame(Mean=mean_estimate,
                         Mean_Lower=mean_lowerbound,
                         Mean_Upper=mean_upperbound,
                         Median=median_estimate,
                         Median_Lower=median_lowerbound,
                         Median_Upper=median_upperbound,
                         Actual_Coverage=paste0(CI*100,"%"),
                         Price_Related_Differential=PRD,
                         Coef_of_Dispersion=COD,
                         Coef_of_Variation=paste0(round(COV*100,2),"%"))

  return(RA_Table)
}
