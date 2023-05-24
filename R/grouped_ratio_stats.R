#' Build a grouped ratio analysis comparable to NCSS output.
#'
#' This function returns either a named list of Ratio Statistics comparing predicted and actual
#' sale prices, OR it can return a table comparable to NCSS
#'
#' This function requires a grouping variable, such as a Neighborhood
#' It will not perform tests for any groups that have fewer than 3 observations, but will still display them if output="table"
#'
#' The NormTest_Results field is just a guess at how NCSS decides whether to recommend Mean or Median, but is based on a Shapiro test of normality with a 0.10 cutoff.
#'
#' @param df Data frame containing columns with assessed values, sold prices, and a grouping variable at the minimum
#' @param assessed_column The name (not quoted) of the column with assessed values
#' @param sold_price_column The name (not quoted) of the column with the sale prices you wish to compare
#' @param grouping_column The name (not quoted) of the column containing grouping data (e.g., neighborhood)
#' @param output A character vector that must be either "list" (default) or "table" that determines how the output is presented. "list" returns a list of lists. "table" returns something similar to the NCSS output.
#' @param min_count Integer (default=10) determining the minimum number of observations. Cannot be less than 3.
#' @return Either a list or table of ratio statistics, broken down by a grouping variable.
#' @examples
#' grouped_ratio_stats(df,
#'   assessed_column = factor_value,
#'   sold_price_column = time_adjusted_sold_price,
#'   grouping_column = nbhd,
#'   output="table")
#' @export


grouped_ratio_stats <- function(df,assessed_column,sold_price_column,grouping_column,output="list",min_count=10){

  split_df <- df %>%
    group_by({{grouping_column}}) %>%
    group_split()

  find_names <- function(x){x %>% select({{grouping_column}}) %>% unlist() %>% unname() %>% unique()}
  group_names <- split_df %>%
    map(find_names) %>%
    as.character()

  names(split_df) <- group_names

  # find groups with too-few observations (N<10)
  counts <- map_dbl(split_df,nrow)
  low_count_groups <- group_names[which(counts < min_count)]
  good_groups <- group_names[which(counts >= 3)]
  deleted_groups <- group_names[which(counts < 3)]

  # remove groups with fewer than 3 observations
  filtered_split_df <- Filter(function(x){nrow(x) >= 3},split_df)


  ind_ratio <- function(x){
    assessed_value <- x %>%
      select({{assessed_column}}) %>%
      as.vector() %>%
      unlist() %>%
      unname()
    sold_prices <- x %>%
      select({{sold_price_column}}) %>%
      as.vector() %>%
      unlist() %>%
      unname()

    ratio_stats(assessed_value = assessed_value,sold_prices = sold_prices)
  }

  # output data, as list of lists
  stats_lists <- map(filtered_split_df,ind_ratio)

  # if output = 'list' then just return list of lists
  if(output=="list"){
    return(stats_lists)
  }

  # if output = 'table" then output the NCSS format table
  if(output=="table"){
    stats_table <- map(stats_lists,as.data.frame) %>%
      reduce(full_join) %>%
      mutate(Group=good_groups) %>%
      dplyr::select(Group,everything()) %>%
      mutate(across(where(is.numeric),function(x){round(x,4)}))
      # roud all numeric to 4 decimals
    stats_table <- stats_table %>%
      bind_rows(
        data.frame(Group=deleted_groups,
                   Count=counts[counts < 3],
                   Mean=rep(NA,length(deleted_groups)),
                   Mean_Lower=rep(NA,length(deleted_groups)),
                   Mean_Upper=rep(NA,length(deleted_groups)),
                   Median=rep(NA,length(deleted_groups)),
                   Median_Lower=rep(NA,length(deleted_groups)),
                   Median_Upper=rep(NA,length(deleted_groups)),
                   Actual_Coverage=rep(NA,length(deleted_groups)),
                   Price_Related_Differential=rep(NA,length(deleted_groups)),
                   Coef_of_Dispersion=rep(NA,length(deleted_groups)),
                   Coef_of_Variation=rep(NA,length(deleted_groups)),
                   Price_Related_Bias=rep(NA,length(deleted_groups)),
                   NormTest_Results=rep(NA,length(deleted_groups)))) %>%
      arrange(Group)

    row.names(stats_table) <- NULL

    # format column order to look like NCSS
    stats_table <-
      stats_table %>%
      dplyr::select(Group,Count,Median,LCL_Median=Median_Lower,UCL_Median=Median_Upper,
                    Mean,LCL_Mean=Mean_Lower,UCL_Mean=Mean_Upper,
                    PRD=Price_Related_Differential,COD=Coef_of_Dispersion,COV=Coef_of_Variation,
                    PRB=Price_Related_Bias,Actual_Coverage,NormTest_Results)

    return(stats_table)
    # stats_table$Group <- good_groups
  }

}





