#' Pivot multiple years
#'
#' This function helps when dealing with pivoting multiple years of assessment values. Data is commonly found with columns like:
#' 2022_re_res, 2022_re_agr, 2022_re_com, 2021_re_res, etc.  This "wide format" can be cleaned with pivot_longer, but this function
#' provides a way of dealing with multiple years. It splits the data frame into pieces by year, and then joins them together in long format.
#'
#' @param dat Data frame containing multiple years of valuation data spread out over columns. This assumes that colnames are clean (i.e., x2022_VAR1,x2022_VAR2) and start with the year number.
#' @param years integer or numeric vector for the years to pull and pivot. These values are converted to character class and used to search in regex
#' @return A transformed data set, reduced to the listed years, and converted to long format.
#' @examples
#' long <- pivot_multiple_years(df,2018:2020)
#'
#' @export

pivot_multiple_years <- function(dat,years){
library(tidyverse)
  df_list <- list()
  z=1
  for(i in years){

    my_grep_pattern <- paste0("^x",as.character(i),"_[r,i]")
    n_prefix <- paste0("x",i,"_")
    proptype_col <- paste0("x",i,"_prop_type")
    x <- grep(my_grep_pattern,names(dat))

    dat_i <- dat %>%
      pivot_longer(x,
                   names_to = "category",
                   names_prefix = n_prefix) %>%
      select(-starts_with("x")) %>%
      mutate(year = i)

    dat_i$annual_prop_type <- dat %>%
      pluck(proptype_col) %>%
      rep(each=6)

    df_list[[z]] <- dat_i
    z=z+1
  }
  long <- purrr::reduce(df_list,full_join)
  return(long)
}

