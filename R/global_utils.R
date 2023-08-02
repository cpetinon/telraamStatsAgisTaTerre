pkg.globals <- new.env(parent = emptyenv())

#' Function to set up the global variables for public holidays an vacations, with the default
#' being the french dates from a governmental API.
#'
#' @param vacations data frame containing the vacation dates
#' @param public_holidays data frame containing the public holidays dates
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd_hms
#' @importFrom rlang .data
#'
#'@keywords internal
#'
set_global_vars <- function(vacations = NULL, public_holidays = NULL){

  if(is.null(vacations)){
    response <- GET(
      url = "https://data.education.gouv.fr/api/v2/catalog/datasets/fr-en-calendrier-scolaire/exports/json",
      query = list(refine = "location:Rennes",
                   exclude = "population:Enseignants")
    )

    pkg.globals$vacations <- content(response, as = "text") %>%
      fromJSON() %>%
      select("description", "start_date", "end_date") %>%
      mutate(
        start_date = ymd_hms(.data$start_date),
        end_date = ymd_hms(.data$end_date)
      )
  }
  else{
    pkg.globals$vacations <- vacations
  }
  if(is.null(public_holidays)){
    response2 <- GET("https://calendrier.api.gouv.fr/jours-feries/metropole.json")

    pkg.globals$public_holidays <- content(response2, as = "text") %>%
      fromJSON() %>%
      names() %>%
      ymd()
  }
  else{
    pkg.globals$public_holidays <- public_holidays
  }
}


#' Select data within a specified date range
#'
#' @param data Data frame containing the date column
#' @param date_range A vector of two dates specifying the range
#'
#' @return Subset of the input data frame within the specified date range
#' @export
#' @importFrom lubridate ymd
#'
#' @keywords internal
#'
filter_date <- function(data, date_range){
  # if dates are exchanged
  date1 <- ymd(date_range[1])
  date2 <- ymd(date_range[2])
  if (date1<date2){
    start <- date1
    end <- date2
  } else {
    start <- date2
    end <- date1
  }
  # Filter according to dates
  matching_dates <- (data$date>=start & data$date<=end)
  return(data[matching_dates,])
}

#' Select data based on vacation criteria
#'
#' @param data Data frame containing the date column
#' @param vacation A character indicating the vacation selection criteria
#'
#'
#' @return Subset of the input data frame based on the vacation criteria
#' @export
#'
#'@keywords internal
#'
filter_vacation <- function(data, vacation){
  if (vacation=='Oui'){
    return(data)
  }

  # Building of the filter
  matching_dates <- rep(FALSE,nrow(data))
  for (i in 1:nrow(pkg.globals$vacations)){
    temp <- (data$date>=pkg.globals$vacations$start_date[i] & data$date<=pkg.globals$vacations$end_date[i])
    matching_dates <- matching_dates + temp
  }
  matching_dates <- (matching_dates>0)

  if(vacation=="Non"){
    return(data[!matching_dates,])
  }
  if(vacation=="Seulement les vacances"){
    return(data[matching_dates,])
  }
}


#' Select data based on public holiday criteria
#'
#' @param data Data frame containing the date column
#' @param JF A character indicating the public holiday selection criteria
#'
#'
#' @return Subset of the input data frame based on the public holiday criteria
#' @export
#'
#' @keywords internal
#'
filter_public_holidays <- function(data, JF){
  if (JF=='Oui'){
    return(data)
  }
  d <- substr(as.character(data$date),1,10)
  matching_dates <- rep(0,nrow(data))
  chara <- as.character(pkg.globals$public_holidays)
  for (ferie in chara){
    matching_dates <- matching_dates + (ferie==d)
  }
  matching_dates <- (matching_dates>0)

  if(JF=="Non"){
    return(data[!matching_dates,])
  } else if(JF=="Seulement les jours feries"){
    return(data[matching_dates,])
  }
}


#' Filter by selected criteria.
#' Not all criteria need to be filled in. Unfilled criteria are set by default so that no filtering is performed.
#'
#' @param data Raw data. See the "importation" function in the '2-import.R' file
#' @param sensor character. Name of desired sensor
#' @param direction character. Direction of the street: " " or "_lft" or "_rgt"
#' @param mobility character. Type of mobility: c("car","heavy","pedestrian","bike")
#' @param date_range character. Date or character. c("aaaa-mm-jj","aaaa-mm-jj")
#' @param vac character. With, without, or only with vacation: "Oui" or "Non" or "Seulement les vacances"
#' @param p_h character. With, without, or only with public holiday: "Oui" or "Non" or "Seulement les jours feries"
#' @param wkd character. Selected days of the week: c("1","2","3","4","5","6","7") here all days are selected
#' @param vacations vacation periods, set by default on the french ones
#' @param public_holidays public holidays period, set by default on the french ones
#'
#' @return the filtered data
#' @export
#' @importFrom dplyr filter %>%
#' @importFrom lubridate wday
#' @importFrom purrr is_empty
#' @importFrom tibble tibble
#'
filtering <- function(data = NULL, sensor    = NULL, direction = ' ', mobility  = c("car","heavy","pedestrian","bike"),
                      date_range = NULL, vac  = NULL, p_h = NULL, wkd  = NULL, vacations = NULL, public_holidays = NULL
){

  set_global_vars(vacations, public_holidays)

  if (is_empty(data)|is.null(data)|is.null(sensor)|is.null(mobility)|is.null(direction)){
    return(tibble())
  }
  filtre <- data[ data$segment_id == sensor, ]
  S <- trimws(paste0(mobility,direction))
  filtre$total <- apply(filtre[,S], MARGIN = 1 ,FUN = sum)
  if (!is.null(wkd)){
    filtre <- filtre %>% filter(wday(date) %in% wkd)
  }
  if (!is.null(date_range)){
    filtre <- filtre %>% filter_date(date_range)
  }
  if (!is.null(vac)){
    filtre <- filtre %>% filter_vacation(vac)
  }
  if (!is.null(p_h)){
    filtre <- filtre %>% filter_public_holidays(p_h)
  }
  return(filtre)
}

