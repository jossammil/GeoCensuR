#' Geocoding function for Census API
#'
#' This function allows you geocode an address and return extra layers using the Census Bureau's geocoding API
#' @param oneline One or single line address combining street, city, state, and zip for geocoding.
#' @param street Street text for address provided. Defaults to NULL for "oneline" preference.
#' @param city City text for address provided. Defaults to NULL for "oneline" preference.
#' @param state State text for address provided. Defaults to NULL for "oneline" preference.
#' @param zip Zip text for address provided. Defaults to NULL for "oneline" preference.
#' @param separate If TRUE, geocoder automatically uses separate address field arguments in request. Defaults to FALSE.
#' @param vintage Defaults to "4".
#' @param layers Returns BASENAME info from layers listed in the Census WMS. Default is only "8" for addition Census Tract info.
#' @param benchmark Defaults to "Public_AR_Current".
#' @param format Defaults to "json".
#' @keywords census geocoder
#' @export
#' @examples
#' geocen()

geocen <- function(oneline = NULL,
                  street = NULL, city = NULL, state = NULL, zip = NULL,
                  separate = FALSE,
                  vintage="4", layers = "8,10,12",
                  benchmark = "Public_AR_Current",
                  format="json") {

  begin <- "https://geocoding.geo.census.gov/geocoder/geographies/"
  # TESTING PURPOSE
  #oneline <- ""
  #oneline1 <- gsub(",", "", gsub("\\s", "+", oneline))

  given <- names(as.list(match.call())[-1])
  needed <- ls()

  #print(given)
  #print(needed)
  #given <- "oneline"
  sepadd <- c("street", "city", "state", "zip")

  if("oneline" %in% given & separate == F) {
    oneline1 <- gsub(",", "", gsub("\\s", "+", oneline))
    geourl <- paste0(begin,
                     "onelineaddress?address=", oneline1,
                     "&benchmark=", benchmark,
                     "&vintage=", vintage,
                     "&format=", format,
                     "&layers=", layers
    )

  } else {
    if(any(!sepadd %in% given)){
      stop(paste("Need variables:", paste(setdiff(sepadd, given), collapse = ", ")))
    } else{ geourl <- paste0(begin,
                             "address?street=", street,
                             "&city=", city,
                             "&state=", state,
                             "&zip=", zip,
                             "&benchmark=", benchmark,
                             "&vintage=", vintage,
                             "&format=", format,
                             "&layers=", layers
    )}
  }

  retc <- httr::GET(geourl)
  ra_retc <- httr::content(retc, as = "text", encoding = "UTF-8")
  ra_retc <- jsonlite::fromJSON(ra_retc, flatten = T)

  #retc <- RCurl::getURL(geourl)
  #ra_retc <- jsonlite::fromJSON(retc, flatten = T)

  #### check error here in future updates
  n2 <- as.data.frame(ra_retc)

  out <- data.frame(long.x = n2$result.addressMatches.coordinates.x,
                    lat.y = n2$result.addressMatches.coordinates.y,
                    oneline = ifelse(is.null(oneline), NA, as.character(oneline)),
                    street = ifelse(is.null(street), NA, as.character(street)),
                    city = ifelse(is.null(city), NA, as.character(city)),
                    state = ifelse(is.null(state), NA, as.character(state)),
                    zip = ifelse(is.null(zip), NA, as.character(zip))
                    #, city = city, state = state, zip = zip
  )

  layers1 <- unlist(strsplit( (gsub(pattern = ",", replacement = " ", layers)) , split =" ") )
  laylength <- length(layers1)

  for(i in seq_along(1:laylength)) {
    out[,paste0("lay", layers1[i])] <- n2[[(length(names(n2))-i)+1]][[1]]$BASENAME
  }

  return(out)

}
