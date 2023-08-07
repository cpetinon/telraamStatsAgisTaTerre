#' Write or update the sensor data in the data folder
#'
#' This function writes or updates the sensor data in the data folder. It retrieves the data for the specified sensor between \code{date1} and \code{date2} (inclusive) using the \code{retrieve_sensor} function, and then converts certain columns to character strings before writing the data to a CSV file in the data folder.
#'
#' @param id_sensor Numeric. ID of the sensor
#' @param date1 Date. Start date "aaaa-mm-jj"
#' @param date2 Date. End date "aaaa-mm-jj"
#' @param sensor_names list with the name of all the studied sensors
#' @param sensor_ids list with the ids of all the studied sensors
#'
#'
#' @importFrom dplyr slice select mutate arrange filter
#' @importFrom readr read_csv2 write_csv2
#' @importFrom stringr str_sub
#'
#' @export
#'
write_update_data_comp <- function(id_sensor, date1, date2,
                                   sensor_names = c("Burel-01","Leclerc-02","ParisMarche-03","rueVignes-04","ParisArcEnCiel-05","RteVitre-06",
                                                    "RueGdDomaine-07","StDidierNord-08","rueVallee-09","StDidierSud-10","RuePrieure-11",
                                                    "RueCottage-12","RueVeronniere-13","RueDesEcoles-14","RueManoirs-15","RueToursCarree-16",
                                                    "PlaceHotelDeVille-17","BoulevardLiberte-18"),
                                   sensor_ids = c(9000002156, 9000001906, 9000001618,9000003090,9000002453,9000001844,
                                                  9000001877,9000002666,9000002181,9000002707,9000003703,
                                                  9000003746,9000003775,9000003736,9000004971,9000004130,
                                                  9000004042,9000004697)
                                    ){

  # retrieve file name
  sensor_name <- sensor_names[which(sensor_ids==id_sensor)]
  comp_name <- paste(str_sub(sensor_name,1,-4), "_comp", str_sub(sensor_name,-3,-1), sep = "")
  file_name <- paste0("data/",comp_name,".csv")

  # Preparation of the dataset
  data <- read_csv2(file_name)

  data_update <- data |> slice((nrow(data)-805):nrow(data)) |>
    select(-.data$imputation, -.data$period)
  new_data <- retrieve_sensor(id_sensor,date1, date2) |>
    filter(!is.na(date)) |>
    mutate(date = as.character(date))

  if(nrow(new_data) == 0){
    print("Mise a jour impossible")
  }else{

    # conversion from a numeric vector to a character string of car_speed_hist_0to70plus and car_speed_hist_0to120plus
    new_data$car_speed_hist_0to70plus <- sapply(new_data$car_speed_hist_0to70plus, function(x) paste(x, collapse = ", "))
    new_data$car_speed_hist_0to120plus <- sapply(new_data$car_speed_hist_0to120plus, function(x) paste(x, collapse = ", "))

    # data on which we are going to impute
    data_update <- rbind(data_update, new_data)

    # retrieve public holidays
    date_pub_hol <- filter_public_holidays(data_update, "Seulement les jours feries") |> select(date)
    date_pub_hol <- date_pub_hol[[1]]

    # retrieve vacations
    date_hol <- filter_vacation(data_update, "Seulement les vacances") |> select(date)
    date_hol <- date_hol[[1]]
    date_hol <- date_hol[!date_hol %in% date_pub_hol]

    # retrieve open days
    date_od <- data_update |> filter(!date %in% date_pub_hol & !date %in% date_hol) |> select(date)
    date_od <- date_od[[1]]

    # impute for public_holidays
    data_pub_hol <- imp_na(data_update, "public holidays")
    data_pub_hol <- data_pub_hol |> filter(date %in% date_pub_hol) |> mutate(period = "public holidays")

    # impute for vacations
    data_hol <- imp_na(data_update, "holidays")
    data_hol <- data_hol |> filter(date %in% date_hol) |> mutate(period = "holidays")

    # impute for open days
    data_od <- imp_na(data_update, "open days")
    data_od <- data_od |> filter(date %in% date_od) |> mutate(period = "open days")

    # retrieve complete data for the update
    data_comp <- rbind(data_pub_hol, data_hol, data_od) |> arrange(date)

    # combine data (old and new)
    data <- data |> slice(1:(nrow(data)-806))
    data_comp <- rbind(data, data_comp)

    if (!is.null(new_data)){
      if (file.exists(file_name)){
        data_comp <- data_comp[!duplicated(data_comp$date),] # if some lines are repeated they are eliminated
      }
      write_csv2(data_comp, file = file_name)
    }
  }
}
