#' Find cap rate
#'
#' This function returns a named list of estimated cap rate based on income and sale values from similar property types.
#' It can handle missing, NaN, and Inf values.
#'
#' @param net_income Numeric vector of net income
#' @param sale_price Numeric vector of actual sale prices
#' @return A named list of estimated cap rate, including upper and lower bounds:
#'     mean_estimate, mean_lowerbound, and mean_upperbound
#' @examples
#' net_income <- c(51329.93,76826.74,42861.05,34066.37,142085.22,81460.92)
#' sale_price <- c(NA,1600000,0,1050000,1050000,479000)
#' cap_rate <- find_cap_rate(net_income,sale_price)
#' cap_rate$mean_estimate; cap_rate$mean_lowerbound; cap_rate$mean_upperbound
#' @export

# find cap rate based on property values
find_cap_rate <- function(net_income,sale_price){
  cap_rate <- net_income / sale_price
  #remove Inf and NaN
  cap_rate <- cap_rate[!is.infinite(cap_rate)]
  cap_rate <- cap_rate[!is.nan(cap_rate)]
  meantest <- t.test(cap_rate,na.rm=TRUE)
  mean_estimate <- unname(meantest$estimate)
  mean_lowerbound <- unname(meantest$conf.int[1])
  mean_upperbound <- unname(meantest$conf.int[2])
  cap_rate <- list(mean_estimate=mean_estimate,
                   mean_lowerbound=mean_lowerbound,
                   mean_upperbound=mean_upperbound)
  return(cap_rate)
}
