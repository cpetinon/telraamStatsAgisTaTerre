#' Import data associated with a list of sensors
#'
#' This function imports data associated with a given list of sensor names.
#'
#' @param list_sensor A character vector specifying the names of sensors to import data for.
#' @param sensor_names A character vector containing the name of each sensor that is displayed to the user
#' @param sensor_ids A character vector containing the identifier name for each vector
#'
#' @return A data.frame containing the imported data.
#'
#' @importFrom purrr map_dfr
#' @importFrom lubridate ymd_hms
#'
#'
#' @export
#'
import_sensor <- function(list_sensor,
                        sensor_names = c("Burel-01","Leclerc-02","ParisMarche-03","rueVignes-04","ParisArcEnCiel-05","RteVitre-06",
                                         "RueGdDomaine-07","StDidierNord-08","rueVallee-09","StDidierSud-10","RuePrieure-11",
                                         "RueCottage-12","RueVeronniere-13","RueDesEcoles-14","RueManoirs-15","RueToursCarree-16",
                                         "PlaceHotelDeVille-17","BoulevardLiberte-18"),
                        sensor_ids = c(9000002156, 9000001906, 9000001618,9000003090,9000002453,9000001844,
                                       9000001877,9000002666,9000002181,9000002707,9000003703,
                                       9000003746,9000003775,9000003736,9000004971,9000004130,
                                       9000004042,9000004697)
                        ){

  data <- data.frame()
  name_sensor <- sensor_names[sensor_ids%in%list_sensor]
  data <- map_dfr(name_sensor, ~ {
    file <- paste0('data/', .x, '.RData')
    if (file.exists(file)) {
      # we select the data that we don't consider null (arbitrary choice)
      import <- load(file) %>% filter(.data$uptime > 0.5,
                                           .data$heavy_lft + .data$car_lft + .data$pedestrian_lft + .data$bike_lft +
                                             .data$heavy_rgt + .data$car_rgt + .data$pedestrian_rgt + .data$bike_rgt >0)
      import$car_speed_hist_0to70plus <-  convert_string_to_list(import$car_speed_hist_0to70plus)
      import$car_speed_hist_0to120plus <- convert_string_to_list(import$car_speed_hist_0to120plus)
      import$date <- ymd_hms(import$date)


      import
    } else {
      NULL
    }
  })
  data
}

#' Convert a character string into a numeric vector
#'
#' @param vector Something in the shape "10,20,30"
#'
#' @return Numeric vector. Something in the shape c(10,20,30)
#'
#' @export
#'
#' @examples
#' convert_string_to_list("10,20,30")
#'
#' @keywords internal
#'
convert_string_to_list <- function(vector){
  convert <- as.list(vector)
  lapply(vector, function(x) {
    elements <- unlist(strsplit(x, ",\\s*"))
    numeric_elements <- as.numeric(elements)
    return(numeric_elements)
  })
}
