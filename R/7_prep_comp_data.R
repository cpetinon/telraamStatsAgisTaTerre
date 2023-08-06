#' Prepares and completes road traffic data by replacing missing hour values with NA
#'
#' This function prepares the input road traffic data by converting the date to a character format,
#' filtering out rows with missing dates, and performing additional data cleaning and imputation.
#' It also completes the data by adding missing hours for each sensor and converting the necessary
#' variables to numeric format.
#'
#' @param data Data frame containing road traffic data.
#'
#' @return A complete data frame with all missing hour values replaced by NA.
#'
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_sub str_replace_all
#' @importFrom dplyr arrange filter mutate right_join %>%
#' @importFrom tidyr separate pivot_wider pivot_longer
#'
#' @export
#'
prep_comp_data <- function(data){

  # transform date in character

  data <- data %>%
    mutate(date = as.character(date)) %>%
    filter(!is.na(date))

  # put 0 equal to NA when uptime = 0

  uptime <- data$uptime

  data <- mutate_uptime(data, uptime, "uptime")

  data[data$uptime %in% 0, c("car_lft", "car_rgt",
                             "bike_lft", "bike_rgt",
                             "pedestrian_lft", "pedestrian_rgt",
                             "heavy_lft", "heavy_rgt",
                             "car", "bike","pedestrian",
                             "heavy", "v85")] <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  data[data$uptime %in% 0, "car_speed_hist_0to70plus"] <- "NA,NA,NA,NA,NA,NA,NA,NA"
  data[data$uptime %in% 0, "car_speed_hist_0to120plus"] <- "NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA"
  data[data$uptime %in% 0, "uptime"] <- NA

  # separate the variables car_speed_hist_0to70plus and car_speed_hist_0to120plus

  speed_var_70 <- paste0("v",1:8,"_0to70")
  speed_var_120 <- paste0("v",1:25,"_0to120")

  speed_var <- c(speed_var_70, speed_var_120)

  car_speed_hist_0to70plus <- data$car_speed_hist_0to70plus
  car_speed_hist_0to120plus <- data$car_speed_hist_0to120plus

  data <- data %>%
    separate(car_speed_hist_0to70plus, speed_var_70, sep = ",") %>%
    separate(car_speed_hist_0to120plus, speed_var_120, sep = ",")

  # retreive all sensors id
  segment_id <- data$segment_id
  list_sensor <- distinct(data, segment_id)$segment_id

  # clean the column date removing the T between the date and the hour and removing the Z at the end
  data <- data %>%
    mutate(date = str_sub(str_replace_all(date, "T", " "), 1, 19))

  # prepare a vector of dates in order to complete our data, it start from the first date available
  # at 05am to the last date available at 18pm
  dates <- seq(from = ymd_hms(paste(str_sub(min(data$date), 1, 10), "06:00:00")),
               to = ymd_hms(paste(str_sub(max(data$date), 1, 10), "18:00:00")),
               by = "1 hour") %>%
    as.character() %>%
    as.data.frame()
  names(dates) <- "dates"

  # remove all the hours on which we don't have enough data
  hours <- c(paste0("0",0:5,":00:00"),paste0(19:23,":00:00"))
  dates <- dates %>%
    filter(!str_sub(dates, 12, 19) %in% hours)

  # on some computers, midnight dose not end by 00:00:00 so we delete the dates shorter than 19
  # characters by only keeping those who are longer than 12
  dates <- dates %>%
    filter(nchar(dates) > 12)

  # for each sensor, we make a right join : data RIGHT JOIN dates by "date" = "dates" and we complete
  # the segment_id columns by repeat it as long as our vector of dates
  data_complete <- data.frame()
  for (sensor in list_sensor){

    data_filter <- data %>%
      filter(segment_id == sensor)

    data_comp <- data_filter %>%
      right_join(dates,  by = c("date" = "dates")) %>%
      arrange(date)
    data_comp <- data_comp %>%
      mutate(segment_id = rep(sensor, nrow(data_comp)))

    data_complete <- rbind(data_complete, data_comp) %>%
      arrange(date)
  }

  # in some of our variable we have dec = "," and for the others, we have dec = ".", so we corrected that and
  # convert all to numeric

  for(variable in speed_var){
    data_complete <- data_complete %>%
      mutate(!!variable := suppressWarnings(as.numeric(get(variable))))
  }

  return(data_complete)
}


#' Mutate Data
#'
#' This function summarizes the data by calculating the mean values of different variables
#' for each hour of the day.
#'
#' @param data A data frame containing the input data.
#' @param uptime column uptime of data
#' @param upname new column name
#'
#'
#' @importFrom dplyr mutate %>%
#' @importFrom rlang :=
#'
#' @noRd
#'
mutate_uptime <- function(data, uptime, upname) {

  data <- data %>%
    mutate({{upname}} := ifelse({{uptime}} < 0.5, 0, {{uptime}}))
}
