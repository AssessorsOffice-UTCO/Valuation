#' Merge districts
#'
#' This function finds districts with low sales volume and merges them into similar larger districts.
#' Similarity is determined as the "high-volume" district with the closest median res_rcnld (or other column).
#' The function returns an updated data frame with a new column "new_district"
#'
#' @param df Data frame containing, at a minimum: district, rcnld, and sold_price. These can be named anything and specified in the function.
#' @param district_id_col Character string specifying the name of the column that specifies district. Default = "district"
#' @param sold_price_col Character string specifying the name of the column containing sale price values. Default = "sold_price"
#' @param res_rcnld_col Character string specifying the name of the column containing rcnld or other measure for comparing districts. Default = "res_rcnld"
#' @param rl_value_col Character string specifying the name of the column containing land_value or other measure for comparing districts. Default = "rl_value"
#' @param acreage_col Character string specifying the name of the column containing acreage or other measure for comparing districts. Default = "acreage"
#' @return A named list of estimated cap rate, including upper and lower bounds:
#'     mean_estimate, mean_lowerbound, and mean_upperbound
#' @examples
#' df <- data("property_info")
#' merged <- merge_districts(df)
#' merged %>%
#' pivot_longer(c(district,new_district),
#'              names_to = "type",
#'              values_to = "district") %>%
#'              mutate(district=factor(district),
#'              type=case_when(type == "district" ~ "old district",
#'                             TRUE ~ "new district")) %>%
#' ggplot(aes(x=district,fill=type)) + geom_bar()
#' @export

merge_districts <- function(df,
                             N=50,
                             district_id_col="district",
                             sold_price_col="sold_price",
                             res_rcnld_col="res_rcnld",
                             rl_value_col="rl_value",
                             acreage_col="acreage",
                             to_group = 'val_ratio'){
  # find cutoff for splitting "small" and "large" sales groups by land_district
  sales_summary <- df %>%
    group_by(!! sym(district_id_col)) %>% # !! sym() syntax allows string as quosure
    summarize(N=n(), N_sales = sum(!is.na(!! sym(sold_price_col))))

  sales_cutoff <- N #setting arbitrary cutoff



  sales_summary <- sales_summary %>%
    mutate(district_sales_count = case_when(N_sales >= sales_cutoff ~ "large",
                                            TRUE ~ "small"))
  df <- left_join(df,sales_summary,by=district_id_col)


  # find closest res_rcnld from a large group and fold the smaller groups (districts) into those
  small_districts_summaries <-
    df %>%
    dplyr::filter(district_sales_count == "small") %>%
    group_by(!! sym(district_id_col)) %>%
    summarize(mean_res_rcnld = mean(!! sym(res_rcnld_col),na.rm=TRUE),
              median_res_rcnld = median(!! sym(res_rcnld_col),na.rm = TRUE),
              sd_res_rcnld = sd(!! sym(res_rcnld_col),na.rm = TRUE),
              val_ratio = median(!! sym(rl_value_col) / !! sym(acreage_col),na.rm=TRUE)) %>%
    mutate(sales_volume = "small")

  large_districts_summaries <-
    df %>%
    dplyr::filter(district_sales_count == "large") %>%
    group_by(!! sym(district_id_col)) %>%
    summarize(mean_res_rcnld = mean(!! sym(res_rcnld_col),na.rm=TRUE),
              median_res_rcnld = median(!! sym(res_rcnld_col),na.rm = TRUE),
              sd_res_rcnld = sd(!! sym(res_rcnld_col),na.rm = TRUE),
              val_ratio = median(!! sym(rl_value_col) / !! sym(acreage_col),na.rm=TRUE)) %>%
    mutate(sales_volume = "large")

  # for each "small" district, find the "large" district that's most similar
  # minimum difference between median costs per acre
  small_districts_summaries$folded_district <- NA
  for(i in seq_along(small_districts_summaries$median_res_rcnld)){
    closest_large_district <-
      large_districts_summaries[[district_id_col]][which.min(abs(small_districts_summaries[[to_group]][i]-large_districts_summaries[[to_group]]))]

    if(length(closest_large_district) == 0){
      small_districts_summaries$folded_district[i] <- NA
    } else {
      small_districts_summaries$folded_district[i] <- closest_large_district
    }
  }

  large_districts_summaries <- large_districts_summaries %>%
    mutate(new_district= !! sym(district_id_col))
  small_districts_summaries <- small_districts_summaries %>%
    mutate(new_district = folded_district)
  new_districts <- full_join(large_districts_summaries,small_districts_summaries) %>%
    select(!! sym(district_id_col),new_district)

  df <- left_join(df,new_districts,by=district_id_col)

  return(df)
}

