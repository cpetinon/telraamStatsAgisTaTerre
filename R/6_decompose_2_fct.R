#' Decompose data from two sensors
#'
#' @param data Data frame containing the data
#' @param sensor1 The segment ID for sensor 1
#' @param sensor2 The segment ID for sensor 2
#' @param hour_x The hour to filter on
#' @param direction1 The direction for sensor 1
#' @param direction2 The direction for sensor 2
#' @param mobility A character vector indicating the mobility types to include in the filter
#' @param norm A character indicating whether to normalize the results
#' @param vacations vacations period for filtering function
#' @param public_holidays public holidays period for filtering function
#' @param sensor_names A character vector containing the name of each sensor that is displayed to the user
#' @param sensor_ids A character vector containing the identifier name for each vector
#'
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate hour
#' @importFrom zoo na.trim
#' @importFrom stats decompose ts cor
#' @importFrom synchrony peaks
#'
#' @return A list containing the decomposed data, seasonal data, peaks, and correlation
#'
#' @export
#'
decompose_2_fct <- function(data, sensor1,sensor2, hour_x, direction1, direction2, mobility, norm, vacations=NULL, public_holidays=NULL,
                            sensor_names = c("Burel-01","Leclerc-02","ParisMarche-03","rueVignes-04","ParisArcEnCiel-05","RteVitre-06",
                                             "RueGdDomaine-07","StDidierNord-08","rueVallee-09","StDidierSud-10","RuePrieure-11",
                                             "RueCottage-12","RueVeronniere-13","RueDesEcoles-14"),
                            sensor_ids = c(9000002156, 9000001906, 9000001618,9000003090,9000002453,9000001844,
                                           9000001877,9000002666,9000002181,9000002707,9000003703,
                                           9000003746,9000003775,9000003736)
                            ){

  data1 <- data %>% filtering(sensor = sensor1, mobility = mobility, direction = direction1, vacations=NULL, public_holidays=NULL) %>%
    filter(hour(date)==hour_x)
  data2 <- data %>% filtering(sensor = sensor2, mobility = mobility, direction = direction2, vacations=NULL, public_holidays=NULL) %>%
    filter(hour(date)==hour_x)

  data1 <- data1 %>% filter(date %in% data2$date) # intersection of dates between the two tables
  data2 <- data2 %>% filter(date %in% data1$date)

  if(length(data1$date)<28){ # if the period is too short to make a valid decomposition
    return(NULL)
  }

  d1 <- ts(data1$total, frequency = 7) %>% # conversion in time serie
    decompose(type="additive",filter = c(0.5, rep(1, 14 - 1), 0.5)/14) # decomposition
  d2 <- ts(data2$total, frequency = 7) %>%
    decompose(type="additive",filter = c(0.5, rep(1, 14 - 1), 0.5)/14)

  result <- data.frame(d1$trend,d2$trend,d1$seasonal,d2$seasonal,d1$random,d2$random,data1$total,data2$total)

  if (norm=="Oui"){ # normalisation
    result <- data.frame(apply(result,2,scale))
  }

  lab_sensor1 <- paste(sensor_names[sensor_ids==sensor1],direction1)
  lab_sensor2 <- paste(sensor_names[sensor_ids==sensor2],direction2)

  trend_random <- data.frame(
    base = c(result$data1.total,result$data2.total),
    trend = c(result$d1.trend,result$d2.trend),
    random = c(result$d1.random,result$d2.random),
    date = data1$date,
    sensor = c(rep(lab_sensor1, nrow(result)), rep(lab_sensor2, nrow(result))))

  seas <- data.frame(seas = c(result$d1.seasonal[1:7],result$d2.seasonal[1:7]),
                     sensor = c(rep(lab_sensor1,7), rep(lab_sensor2, 7)),
                     date = c(1:7,1:7))
  peaks <-  peaks(result$d1.random %>% na.trim(),result$d2.random %>% na.trim(),nrands = 100)
  correl <- round(cor(result$d1.random,result$d2.random,use = "na.or.complete"),3)

  return(list(trend_random=trend_random, seas=seas, peaks = peaks, correl = correl, export_d = result))
}
