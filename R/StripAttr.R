#' Remove attributes from an object
#'
#' For convenience we sometimes want to strip some or all attributes in a oneliner.
#' Taken from DescTools::StripAttr
#'
#' @param x the object whose attributes should be removed
#' @param attr_names a vector with attribute names, which will be removed. Leaving the default to NULL will cause all the attributes to be deleted.
#' @return the object x without the attributes contained in attr_names
#' @examples
#' x <- runif(10)
#' StripAttr(x)
#' @export


StripAttr <- function (x, attr_names = NULL)
{
  if (is.null(attr_names))
    attributes(x) <- NULL
  else for (a in attr_names) attr(x, which = a) <- NULL
  return(x)
}
