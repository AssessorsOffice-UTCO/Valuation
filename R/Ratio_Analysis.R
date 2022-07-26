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
  COD <- abs(sum(ratio - median_estimate,na.rm = TRUE) / length(ratio)) / median_estimate
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
