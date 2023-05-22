#' Generate median with estimated confidence intervals
#'
#' Taken from DescTools::StripAttr
#'
#' @param x a (non-empty) numeric vector of data values
#' @param conf.level confidence level of the interval
#' @param sides a character string specifying the side of the confidence interval, must be one of "two.sided" (default), "left" or "right". You can specify just the initial letter. "left" would be analogue to a hypothesis of "greater" in a t.test.
#' @param na.rm logical. Defaults to FALSE
#' @param method defining the type of interval that should be calculated (one out of "exact", "boot"). Default is "exact".
#' @param R The number of bootstrap replicates. Usually this will be a single positive integer.
#' @return A numeric vector with 3 elements: median, lwr.ci, upr.ci
#' @examples
#' MedianCI(rnorm(100),na.rm=TRUE)
#' @export


MedianCI <- function (x, conf.level = 0.95, sides = c("two.sided", "left",
                                          "right"), na.rm = FALSE, method = c("exact", "boot"), R = 999)
{
  if (na.rm)
    x <- na.omit(x)
  MedianCI_Binom <- function(x, conf.level = 0.95, sides = c("two.sided",
                                                             "left", "right"), na.rm = FALSE) {
    if (na.rm)
      x <- na.omit(x)
    n <- length(x)
    switch(match.arg(sides), two.sided = {
      k <- qbinom(p = (1 - conf.level)/2, size = n, prob = 0.5,
                  lower.tail = TRUE)
      ci <- sort(x)[c(k, n - k + 1)]
      # to match NCSS, re-run without lower.tail and use that version of upper median CI
      k2 <- qbinom(p = (1 - conf.level)/2, size = n, prob = 0.5,
                   lower.tail = FALSE)
      ci2 <- sort(x)[c(k2, n - k2 + 1)]
      ci[2] <- ci2[1] # replace original upper median CI with non-lowertail test version

      attr(ci, "conf.level") <- 1 - 2 * pbinom(k - 1, size = n,
                                               prob = 0.5)
    }, left = {
      k <- qbinom(p = (1 - conf.level), size = n, prob = 0.5,
                  lower.tail = TRUE)
      ci <- c(sort(x)[k], Inf)
      attr(ci, "conf.level") <- 1 - pbinom(k - 1, size = n,
                                           prob = 0.5)
    }, right = {
      k <- qbinom(p = conf.level, size = n, prob = 0.5,
                  lower.tail = TRUE)
      ci <- c(-Inf, sort(x)[k])
      attr(ci, "conf.level") <- pbinom(k, size = n, prob = 0.5)
    })
    if (identical(StripAttr(ci), NA_real_)) {
      ci <- c(-Inf, Inf)
      attr(ci, "conf.level") <- 1
    }
    return(ci)
  }
  sides <- match.arg(sides, choices = c("two.sided", "left",
                                        "right"), several.ok = FALSE)
  method <- match.arg(arg = method, choices = c("exact", "boot"))
  switch(method, exact = {
    r <- MedianCI_Binom(x, conf.level = conf.level, sides = sides)
  }, boot = {
    if (sides != "two.sided") conf.level <- 1 - 2 * (1 -
                                                       conf.level)
    boot.med <- boot(x, function(x, d) median(x[d], na.rm = na.rm),
                     R = R)
    r <- boot.ci(boot.med, conf = conf.level, type = "basic")[[4]][4:5]
  })
  med <- median(x, na.rm = na.rm)
  if (is.na(med)) {
    res <- rep(NA, 3)
  }
  else {
    res <- c(median = med, r)
    if (method == "exact")
      attr(res, "conf.level") <- attr(r, "conf.level")
  }
  names(res) <- c("median", "lwr.ci", "upr.ci")
  if (sides == "left")
    res[3] <- Inf
  else if (sides == "right")
    res[2] <- -Inf
  return(res)
}
