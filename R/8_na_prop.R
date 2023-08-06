#' Visualisation NA stats
#'
#' This function calculates and returns the proportion of missing values (NA) and imputed values for a given sensor within a specified date range.
#'
#' @param data Data frame. Containing road traffic data.
#' @param start Character. The start date in the format "yyyy-mm-dd".
#' @param end Character. The end date in the format "yyyy-mm-dd".
#' @param sensor Character. The sensor ID on which you want to work.
#'
#' @return A character vector containing two phrases:
#' 1. The proportion of missing values before any data treatment.
#' 2. The proportion of missing values replaced by imputed data and the remaining proportion of missing values.
#'
#' @export
#'
na_prop <- function(data, start, end, sensor){

  # filter by date and sensor

  data_filter <- data %>% filter(date >= start,
                                 date <= end,
                                 .data$segment_id %in% sensor)

  # calcul proportion of NA, of NA imputed and of NA remaining
  prop_na <-  round(nrow(data_filter[data_filter$imputation %in% c(TRUE,NA),]) / nrow(data_filter), digits = 2)*100
  prop_na_imp <- round(sum(is.na(data_filter$car)) / nrow(data_filter), digits = 2)*100
  prop_imp <- prop_na - prop_na_imp

  # prepare the text
  phrase_1 <- sprintf("Avant de traiter les donnees manquantes, nous avions %s pourcents de valeurs manquantes.", prop_na)
  phrase_2 <- sprintf("Post-traitement, nous avons %s pourcents de donnees remplacees et %s pourcents de valeurs manquantes restantes.",prop_imp, prop_na_imp)

  texte <- c(phrase_1, phrase_2)
  # return the text with all the information
  return(texte)
}
