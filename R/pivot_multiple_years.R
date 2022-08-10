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


  # package dependencies
  packages = c("tidyverse")
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
  library(tidyverse)

  if(!"tidyverse" %in% (.packages())){
    stop("tidyverse needs to be loaded.")
  }

  # create empty list and counter var
  df_list <- list()
  z=1

  # for-loop
  for(i in years){

    # build character strings out of i, used later for subsetting and grep
    my_grep_pattern <- paste0("^x",as.character(i),"_[r,i]") # grep pattern for, eg, "x2022_im_res"
    n_prefix <- paste0("x",i,"_")
    proptype_col <- paste0("x",i,"_prop_type") # this will be the first thing to break
    x <- grep(my_grep_pattern,names(dat)) # find column positions

    # pivot on just year i columns
    dat_i <- dat %>%
      pivot_longer(x,
                   names_to = "category",
                   names_prefix = n_prefix) %>%
      select(-starts_with("x")) %>%
      mutate(year = i)

    # add in prop_type from year i
    dat_i$annual_prop_type <- dat %>%
      pluck(proptype_col) %>%
      rep(each=6)

    # put pivoted data frame into list
    df_list[[z]] <- dat_i
    z=z+1 # increment
  }

  # combine all data frames in that list using full_join()
  long <- purrr::reduce(df_list,full_join)
  return(long)
}

