#' gg_na_dist
#' The function gives a graphical representation of NA's distribution (monthly) during a selected period for one sensor.
#'
#' @param data Data.Frame. A data frame containing road traffic data.
#' @param sensor Numeric. The sensor ID on which you want to see the representation.
#' @param start Character. The start of the period in the format "yyyy-mm-dd".
#' @param end Character. The end of the period in the format "yyyy-mm-dd".
#' @param hours Vector. A vector containing hours on which you want to see the representation, e.g., c("05h", "18h").
#' @param list_weekday Vector. A vector containing weekdays in French, e.g., c("Monday", "Sunday").
#' @param pub_holidays Character. "YES" if you want to see results with public holidays included;
#' "NO" if you want to exclude them from the representation;
#' "ONLY" if you only want to see them in the result.
#' @param holidays Character. "YES" if you want to see results including holidays;
#' "NO" if you want to exclude them from the representation;
#' "ONLY" if you only want to see them in the result.
#'
#' @import ggplot2
#' @import lubridate
#' @importFrom dplyr arrange filter group_by mutate n right_join select summarise
#' @importFrom stringr str_sub
#' @importFrom tidyr pivot_longer
#'
#' @return A ggplot graph.
#'
#' @export
#'
gg_na_dist <- function(data,
                       sensor,
                       start = NULL,
                       end = NULL,
                       hours = "ALL",
                       list_weekday = "ALL",
                       pub_holidays = "YES",
                       holidays = "YES"){

  # create a month-year column
  data <- data %>% mutate(month = as.factor(str_sub(date, 1, 7)))

  if (is.null(start)){
    start <- str_sub(min(data$date), 1, 10)
  }
  if (is.null(end)){
    end <- str_sub(max(data$date), 1, 10)
  }

  # filter data by date
  data <- data %>%
    filter(date >= start & date <= paste(end, "18:00:00")) %>%
    filter(.data$segment_id %in% sensor) %>%
    arrange(date)

  # create ticks label
  labels_legend <- data %>% select(month) %>% distinct()

  # filter by weekdays
  if (!setequal(list_weekday, rep("ALL", length(list_weekday)))){

    data <- data %>%
      filter(weekdays(ymd_hms(date)) %in% list_weekday) %>%
      mutate(date = as.character(date))

  }else{
    data <- data %>% mutate(date = as.character(date))}

  # filter pub holidays
  if (pub_holidays == "YES"){

    data <- data

  }else if(pub_holidays == "NO"){

    data <- data %>% filter(period != "public holidays")

  }else if(pub_holidays == "ONLY"){

    data <- data %>% filter(period == "public holidays")

  }

  # filter holidays

  if (holidays == "YES"){

    data <- data

  }else if(holidays == "NO"){

    data <- data %>% filter(period != "holidays")

  }else if(holidays == "ONLY"){

    data <- data %>% filter(period == "holidays")

  }

  # filter by hours

  if (length(hours) == 1){
    if(hours != "ALL"){
      data <- data %>% filter(str_sub(date,12,19) == hours)
    }
  }else{
    data <- data %>% filter(str_sub(date,12,19) %in% hours)
  }

  res <- data %>% group_by(month)

  imputation <- data$imputation
  res <- summarise_na_stats(res, imputation, "Before", "After", "nb_obs")

  Before <- res$Before
  After <- res$After
  nb_obs <- res$nb_obs
  res <- calculate_and_round_percentage(res, Before, After, nb_obs, "Before", "After")

  res <- res %>% right_join(labels_legend, by = c("month" = "month"))

  labels_legend <- labels_legend %>% mutate(month = ifelse(str_sub(month,6,8) != "06",str_sub(month,6,8), paste("06", "\n", str_sub(month,1,4))))
  labels_legend <- labels_legend$month

  res <- res %>% pivot_longer(cols = !month, names_to = "label" ) %>%
    mutate("value" = ifelse(is.na(.data$value), 0, .data$value))

  ggplot(res,aes(x = month, y = .data$value, fill = factor(.data$label)))+
    geom_bar(stat = "identity", position = position_dodge(-0.9)) +
    scale_y_continuous(name = "Proportion de \n valeur manquantes",
                       limits = c(0,101),
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c("0 %", "25 %", "50 %", "75 %", "100 %")) +
    scale_x_discrete(name = "Mois",
                     labels = labels_legend) +
    scale_fill_manual(values=c("Before"="firebrick2", "After"="chartreuse2")) +
    labs(fill = "") +
    guides(fill = guide_legend(reverse = TRUE))
}

#' Summarize NA Statistics
#'
#' Internal function to summarize NA statistics for a given data frame.
#'
#' This function calculates the number of non-missing values (`Before`),
#' the number of missing values (`After`), and the total number of observations (`nb_obs`).
#'
#' @param data Data frame containing the required columns.
#' @param imputation Name of the column representing imputation status (TRUE for imputed, FALSE for not imputed).
#' @param Before new column name
#' @param After new column name
#' @param nb_obs new column name
#'
#' @importFrom dplyr summarise %>%
#' @importFrom rlang :=
#'
#' @return Data frame with the summarised NA statistics.
#'
#' @noRd
#'
summarise_na_stats <- function(data, imputation, Before, After, nb_obs) {
  data %>%
    summarise({{Before}} := sum({{imputation}}, na.rm = TRUE) + sum(is.na({{imputation}})),
              {{After}} := sum(is.na({{imputation}})),
              {{nb_obs}} := n())
}

#' Calculate and Round Percentage
#'
#' Internal function to calculate and round percentage values based on the 'Before' and 'After' columns.
#'
#' @param data Data frame containing the required columns.
#' @param Before Name of the column representing 'Before' statistic.
#' @param After Name of the column representing 'After' statistic.
#' @param nb_obs Name of the column representing the total number of observations.
#' @param before_name New name for the calculated 'Before' percentage column.
#' @param after_name New name for the calculated 'After' percentage column.
#'
#' @importFrom dplyr mutate %>%
#' @importFrom rlang :=
#'
#' @return Data frame with rounded percentage values.
#'
#' @noRd
#'
calculate_and_round_percentage <- function(data, Before, After, nb_obs, before_name, after_name) {
  data %>%
    mutate(
      {{before_name}} := round({{Before}} * 100 / {{nb_obs}}),
      {{after_name}} := round({{After}} * 100 / {{nb_obs}})
    ) %>%
    select(month, {{before_name}}, {{after_name}})
}

