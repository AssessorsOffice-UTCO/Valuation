#' Estimate value from income and cap rate
#'
#' This function returns a vector of estimated property values based on net income and cap rate
#' It can handle missing, NaN, and Inf values.
#'
#' @param net_income Numeric vector of net income
#' @param cap_rate Numeric vector of length 1 indicating the cap rate for similar properties
#' @return something
#' @examples
#' net_income <- c(51329.93,76826.74,42861.05,34066.37,142085.22,0,NA,Inf)
#' cap_rate <- 0.0964
#' value_from_cap_rate(net_income,cap_rate)
#' @export



value_from_cap_rate <- function(net_income,cap_rate){
  value <- net_income / cap_rate
  value[is.infinite(value)] <- NA
  return(value)
}
