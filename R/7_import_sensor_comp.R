#' Import Sensor Data and Combine
#'
#' This function imports sensor data from CSV files for given sensor IDs and combines them.
#' The function also processes the data by converting certain columns to lists.
#'
#' @param sensor_ids A character vector containing the sensor IDs to import.
#' @param sensor_comp_names A character vector containing the names of CSV files to import.
#'
#' @importFrom dplyr data_frame filter mutate arrange %>%
#' @importFrom purrr map_dfr
#' @importFrom lubridate ymd_hms
#'
#' @return A data frame containing the imported and combined sensor data.
#'
#' @export
#'
import_sensor_comp <- function(sensor_ids,sensor_comp_names){
  data <- data.frame()
  data <- map_dfr(sensor_comp_names, ~ {
    file <- paste0('data/', .x, '.csv')
    if (file.exists(file)) {
      read.csv(file, header = TRUE, sep = ";", dec = ",")
    } else {
      NULL
    }
  })

  car <- data$car
  car_speed_hist_0to120plus <- data$car_speed_hist_0to120plus
  car_speed_hist_0to70plus <- data$car_speed_hist_0to70plus

  data_na <- data %>% filter(is.na(car)) %>% mutate(car_speed_hist_0to120plus = NA,
                                                  car_speed_hist_0to70plus = NA)
  data_sana <- data %>% filter(!is.na(car)) %>%
    mutate(car_speed_hist_0to70plus = convert_string_to_list(car_speed_hist_0to70plus),
           car_speed_hist_0to120plus = convert_string_to_list(car_speed_hist_0to120plus))
  data <- rbind(data_na, data_sana) %>% arrange(date)

  # data$date <- ymd_hms(data$date)
  return(data)
}
