# in this function, there are many spots can could gain by being functionnalised and using tidyeval syntaxe.

#' gg_working_sensor
#' the function gives a graphical representation of the sensors operation during a selected period,
#' the representation can be grouped or individual
#'
#' @param list_sensor a vector containing sensor IDs, ex : c("9000001906", "9000003090)
#' @param data a data frame containing road traffic data
#' @param start the start of the period in the format "yyyy-mm-dd"
#' @param end the start of the period in the format "yyyy-mm-dd"
#' @param hours a vector containing hours on which you want to see the representation, ex : c("05h", "18h")
#' @param aggregate is aggregate = TRUE, you'll see information for all sensors together. if aggregate = FALSe,
#' the information are show sensor by sensor
#' @param list_weekday a vector containing weekdays in french, ex : c("lundi", "jeudi")
#' @param pub_holyday "YES" : if you want to see results with public holydays inclue;
#' "NO" if you want to exclude them from the representation;
#' "ONLY" if you only want to see them in the result
#' @param holydays "YES" : if you want to see results including holydays;
#' "NO" if you want to exclude them from the representation;
#' "ONLY" if you only want to see them in the result
#'
#'
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom gridExtra grid.arrange
#'
#' @return a ggplot graph
#' @export
#'
gg_working_sensor <- function(list_sensor = "ALL",
                              data,
                              start,
                              end,
                              hours = "ALL",
                              aggregate = TRUE,
                              list_weekday = "ALL",
                              pub_holyday = "YES",
                              holydays = "YES"){

  # avoid warning messages
  options(dplyr.summarise.inform = FALSE)

  # retrieve a complete dataframe
  df <- prep_view_data(data, start, end, list_weekday, pub_holyday, holydays)
  sensor_id <- df$sensor_id

  # create a warning message
  if(nrow(df) == 0){
    return("Il n'y a pas de donnees pour la periode selectionnee")
  }

  # param the sensor's list if you want all of them
  if(length(list_sensor) == 1){
    if(list_sensor == "ALL")
      list_sensor <- df %>% select(sensor_id) %>% distinct()
    list_sensor <- list_sensor[[1]]
  }

  # if you study all hours
  if (setequal(hours, rep("ALL", length(hours)))){

    # you want a graph per sensor
    if (aggregate == FALSE){

      # initialisation
      graph_fin <- list()

      # create a graph for each sensors
      for(id in list_sensor[c(2:length(list_sensor))]){

        # filter by sensor
        df_filter <- df %>% filter(sensor_id == id)
        df_filter <- df_filter %>%
          pivot_longer(!c(sensor_id, days), names_to = "hour", values_to = "working")

        working <- df_filter$working

        df_filter <- df_filter %>%
          mutate(working = as.factor(working))

        # graph storage
        graph_bis <- ggplot(df_filter, aes(days, hour, fill = .data$working)) +
          geom_tile(col = "grey") +
          theme(axis.text.x = element_blank()) +
          scale_fill_manual(breaks = c("1", "0"), values = c("black", "white"), labels = c("Yes","No")) +
          xlab("") +
          ylab(sprintf("%s",id)) +
          theme(legend.position = 'none')

        # add graphs in a list
        graph_fin[[(length(graph_fin) + 1)]] <- graph_bis
      }

      # create the graph for one sensor in order to have the legend only one time
      # filter by sensor
      df_filter <- df %>% filter(sensor_id == list_sensor[1])
      df_filter <- df_filter %>%
        pivot_longer(!c(sensor_id,days), names_to = "hour", values_to = "working")

      working <- df_filter$working

      df_filter <- df_filter %>%
        mutate(working = as.factor(working))

      # graph storage
      graph <- ggplot(df_filter, aes(days, hour, fill= .data$working)) +
        geom_tile(col = "grey") +
        theme(axis.text.x = element_text(angle = 90)) +
        theme(legend.position ='none') +
        scale_fill_manual(breaks = c("1", "0"), values = c("black", "white"), labels = c("Yes","No")) +
        ylab(sprintf("%s", list_sensor[1]))

      # add the last graph in a list
      graph_fin[[(length(graph_fin) + 1)]] <- graph

      # graph
      grid.arrange(grobs = graph_fin, ncol = 1)

      # you want a graph for all sensors
    }else if(aggregate == TRUE){

      # summarize data we have for each days and hours the number of working sensor
      df_filter <- df %>%
        filter(sensor_id %in% list_sensor) %>%
        pivot_longer(!c(sensor_id,days), names_to = "hour", values_to = "working") %>%
        group_by(days, hour)

      working <- df_filter$working

      df_filter <- df_filter %>%
        summarise(Working_sensor = sum(working))

      Working_sensor <-df_filter$Working_sensor

      df_filter <- df_filter %>%
        mutate(Working_sensor = as.factor(Working_sensor))

      # graph
      ggplot(df_filter, aes(days, hour, fill = .data$Working_sensor)) +
        geom_tile(col = "grey") +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_fill_grey()
    }
  }else{
    # the same but filter by hours
    if (aggregate == FALSE){

      graph_fin <- list()
      for(id in list_sensor[c(2:length(list_sensor))]){

        df_filter <- df %>% filter(sensor_id == id)
        df_filter <- df_filter %>%
          pivot_longer(!c(sensor_id, days), names_to = "hour", values_to = "working")

          working <- df_filter$working

        df_filter <- df_filter %>%
          mutate(working = as.factor(working))%>%
          filter(hour %in% hours)

        graph_bis <- ggplot(df_filter, aes(days, hour, fill = .data$working)) +
          geom_tile(col = "grey") +
          theme(axis.text.x = element_blank()) +
          scale_fill_manual(breaks = c("1", "0"), values = c("black", "white"), labels = c("Yes", "No")) +
          xlab("") +
          ylab(sprintf("%s",id)) +
          theme(legend.position = 'none')
        graph_fin[[(length(graph_fin) + 1)]] <- graph_bis

      }

      df_filter <- df %>%
        filter(sensor_id == list_sensor[1]) %>%
        pivot_longer(!c(sensor_id,days), names_to = "hour", values_to = "working")

      working <- df_filter$working

      df_filter <- df_filter %>%
        mutate(working = as.factor(working)) %>%
        filter(hour %in% hours)

      graph <- ggplot(df_filter, aes(days, hour, fill = .data$working)) +
        geom_tile(col = "grey") +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_fill_manual(breaks = c("1", "0"), values = c("black", "white"), labels = c("Yes","No")) +
        ylab(sprintf("%s", list_sensor[1]))
      graph_fin[[(length(graph_fin) + 1)]] <- graph

      grid.arrange(grobs = graph_fin, ncol = 1)

    }else if(aggregate == TRUE){
      df_filter <- df %>%
        filter(sensor_id %in% list_sensor) %>%
        pivot_longer(!c(sensor_id, days), names_to = "hour", values_to = "working") %>%
        group_by(days, hour)

        working <- df_filter$working

        df_filter <- df_filter %>%
          summarise(Working_sensor = sum(working))

        Working_sensor <-df_filter$Working_sensor

        df_filter <- df_filter %>%
          mutate(Working_sensor = as.factor(Working_sensor)) %>%
          filter(hour %in% hours)

      ggplot(df_filter, aes(days, hour, fill = .data$Working_sensor)) +
        geom_tile(col = "grey") +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_fill_grey()
    }
  }
}

#' Prepare and filter data for viewing
#'
#' This function prepares and filters the input data based on specified parameters
#'
#' @param data The input data frame containing the sensor data.
#' @param start The start date in "yyyy-mm-dd" format (default is the minimum date in the data).
#' @param end The end date in "yyyy-mm-dd" format (default is the maximum date in the data).
#' @param list_weekday A character vector of weekdays to include in the analysis (default is "ALL").
#' @param pub_holidays A character indicating how to filter public holidays. Possible values are "YES", "NO", or "ONLY" (default is "YES").
#' @param holidays A character indicating how to filter holidays. Possible values are "YES", "NO", or "ONLY" (default is "YES").
#'
#' @importFrom dplyr filter distinct mutate %>%
#' @importFrom lubridate ymd ymd_hms
#' @importFrom tidyr pivot_longer
#'
#' @return A data frame containing summarized information on operating sensors.
#' The data frame will have columns for sensor_id, days, and hourly uptime data (06h to 18h).
#'
prep_view_data <- function(data,
                           start = str_sub(min(data$date), 1, 10),
                           end = str_sub(max(data$date), 1, 10),
                           list_weekday = "ALL",
                           pub_holidays = "YES",
                           holidays = "YES"){

  # we complete our data
  data <- prep_comp_data(data)

  # retreive all sensors id
  segment_id <- data$segment_id
  liste_sensor <- distinct(data, segment_id)$segment_id

  # create a vector of days on which we want to work
  days <- seq(from = ymd(start), to = ymd(end), by = "1 day")

  # filter by weekdays
  if (!setequal(list_weekday, rep("ALL", length(list_weekday)))){

    data <- data %>%
      filter(weekdays(ymd_hms(date)) %in% list_weekday) %>%
      mutate(date = as.character(date))

    days <- days[weekdays(days) %in% list_weekday] %>%
      as.character()

  }else{
    data <- data %>% mutate(date = as.character(date))
    days <- as.character(days)}

  # filter pub holidays
  if (pub_holidays == "YES"){

    data <- data
    days <- as.character(days)

  }else if(pub_holidays == "NO"){

    data <- filter_public_holidays(data, "Non")

    days <- days[days %in% str_sub(data$date, 1, 10)] %>%
      as.character()

  }else if(pub_holidays == "ONLY"){

    data <- filter_public_holidays(data, "Seulement les jours feries")

    days <- days[days %in% str_sub(data$date, 1, 10)] %>%
      as.character()
  }

  # filter holidays
  if (holidays == "YES"){
    data <- data
    days <- as.character(days)

  }else if(holidays == "NO"){

    data <- filter_vacation(data, "Non")
    days <- days[days %in% str_sub(data$date, 1, 10)] %>%
      as.character()

  }else if(holidays == "ONLY"){

    data <- filter_vacation(data, "Seulement les vacances")

    days <- days[days %in% str_sub(data$date, 1, 10)] %>%
      as.character()
  }

  # summarize information on operating sensors
  uptime <- data$uptime
  date <- data$date
  data_result <- mutate_udh(data, uptime, date, "uptime", "days", "hours")

  data_result <- data_result %>%
    select(.data$segment_id, .data$uptime, .data$days, .data$hours) %>%
    pivot_wider(names_from = "hours", values_from = "uptime")

  names(data_result) <- c("sensor_id", "days", "06h", "07h", "08h", "09h", "10h",
                          "11h", "12h", "13h", "14h", "15h", "16h", "17h", "18h")

  return(data_result)
}

#' Mutate Data
#'
#' This function summarizes the data by calculating the mean values of different variables
#' for each hour of the day.
#'
#' @param data A data frame containing the input data.
#' @param uptime column uptime of data
#' @param date column date of data
#' @param upname name of the new column
#' @param days name of new column
#' @param hours name of new column
#'
#'
#'
#' @importFrom dplyr mutate %>%
#' @importFrom rlang :=
#'
#' @noRd
#'
mutate_udh <- function(data, uptime, date, upname, days, hours) {

  data <- data %>% mutate({{upname}} := ifelse(is.na({{uptime}}), 0, ifelse({{uptime}} > 0.5, 1, 0)),
                                 {{days}} := str_sub({{date}}, 1, 10),
                                 {{hours}} := str_sub({{date}}, 12, 19))
}
