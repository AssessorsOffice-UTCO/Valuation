#' Estimate value from income and cap rate
#'
#' This function returns whether a parcel has enough comparables to do a sales grid to get an estimated value for a parcel
#' Best used with purrr::map to apply over all parcels
#'
#' @param dat data frame containg lat, long, and sales information of all parcels
#' @param ID character or numeric of serial number of parcel you want to test
#' @param miles_radius numeric for number of miles you want your range to be to search for sales
#' @param n_sale numeric for number of sales requried to have enough comparables
#' @return something
#' @examples
#' ID <- "094837839"
#' dat <- lat_long_data
#' value_from_cap_rate(dat,ID, mile_radius = 1, n_sale =10)
#' @export


enough_comps <- function (dat,ID,mile_radius=3, n_sale = 5) {
  # deal with leading zero, if present
  if(nchar(ID) == 8){
    paste0("0",ID)
  }
  # convert to character if there are actually 9 'digits'
  ID <- as.character(ID)

  if(sum(dat$serno == ID) == 0){
    stop("Invalid or missing parcel ID.")
  }
  x <- as.matrix(dat[dat$serno == ID,c("longx","latx")])


  if(is.na(x[,"latx"])){
    stop("Subject parcel you entered does not have valid GPS points!")
  }

  if(is.na(x[,"longx"])){
    stop("Subject parcel you entered does not have valid GPS points!")
  }

  # build simplified lat long matrix
  latlong_mat <- as.matrix(dat[,c("longx","latx")])

  # convert NA values to 0 ... for now ... back to NA after distance calculation
  latlong_mat[is.na(latlong_mat)] <- 0
  rownames(latlong_mat) <- dat$serno

  # function to find parcels in range, gather info on them, and calculate scores from:
  #     distance | sale date | similarity
  dists <- spDistsN1(latlong_mat,x,longlat=TRUE)
  names(dists) <- rownames(latlong_mat)
  in_range_dists <- dists[dists <= mile_radius * 1.609]
  comp_ids <- names(in_range_dists)
  in_range_dists <- in_range_dists %>% unname()
  in_range_df <- dat %>% filter(serno %in% comp_ids)
  return(!sum(in_range_df$sale_valid) > n_sale )
}
