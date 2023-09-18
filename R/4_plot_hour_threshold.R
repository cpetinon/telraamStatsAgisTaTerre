#' Plots the number of vehicles in each direction for a selected sensor, aswell as the average speed and traffic volume
#' by hour of the day.
#'
#' @param ... data and parameters to filter on, see \link{filtering}
#'
#'
#' @return A list containing the plotly figure and the aggregated traffic data by hour.
#'
#' @import ggplot2
#' @importFrom dplyr group_by summarise full_join %>%
#' @importFrom lubridate hour
#' @importFrom stats sd
#' @importFrom rlang :=
#' @importFrom tidyr pivot_longer
#'
#' @export
#'
#'
plot_hour_threshold <- function(...) {
  data <- do.call(filtering,list(...))

  if (is_empty(data)) {
    return(NULL)
  }

  v85 <- data$v85
  car_rgt <- data$car_rgt
  heavy_rgt <- data$heavy_rgt
  car_lft <- data$car_lft
  heavy_lft <- data$heavy_lft


  data <- summarize_data(data, v85, car_rgt, heavy_rgt, car_lft, heavy_lft,
                         "v85", "B_to_A", "A_to_B")

  v85 <- data$v85
  B_to_A <- data$B_to_A
  A_to_B <- data$A_to_B


  # Calculate means and standard deviations if both directions are present

  mean_voiture <- mean(c(data$B_to_A, data$A_to_B))
  sd_voiture <- sd(c(data$B_to_A, data$A_to_B))
  mean_speed <- mean(data$v85)
  sd_speed <- sd(data$v85)

  data_vis <- data

  data <- data %>%
    mutate(v85 = ((v85 - mean_speed) / sd_speed) * sd_voiture + mean_voiture) %>%
    pivot_longer(cols = c(v85, B_to_A, A_to_B), names_to = "Legend", values_to = "valeur")


  graph <- ggplot(data, aes(x = hour, y = .data$valeur, group=.data$Legend, shape=.data$Legend, colour=.data$Legend)) +
    geom_line(aes(linetype=.data$Legend),size = 1) +
    geom_point(size = 3)+
    scale_color_manual(values = c("#006bb6", "#006bb6", "#ff5900"))+ # legend
    scale_linetype_manual(values = c("dotted", "solid","solid")) + # legend
    ylab('Nombre de vehicules moyen')+
    scale_y_continuous(sec.axis = sec_axis(~((.-mean_voiture)/sd_voiture)*sd_speed+mean_speed, # tracing the second axis
                                           name = "Vitesse v85 moyenne (km/h)")) +
    scale_x_continuous(breaks = min(data$hour):max(data$hour)) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "horizontal", # the whole code in this theme() function is about color
          axis.ticks.y.left = element_line(color = "#006bb6"), # ticks (left axis)
          axis.text.y.left = element_text(color = "#006bb6"), # number associated to a tick (left axis)
          axis.title.y.left = element_text(color = "#006bb6"), # title of an axis (left axis)
          axis.title.y.right = element_text(color = "#ff5900"), # ticks (right axis)
          axis.text.y.right = element_text(color = "#ff5900") , # number associated to a tick (right axis)
          axis.ticks.y.right = element_line(color = "#ff5900"), # title of an axis (right axis)
          panel.background = element_rect(fill = "#F5F5F5"), # background
          panel.grid = element_line(color = "#E3E3E3"), # grid
          panel.border = element_rect(color = "#E3E3E3", size = 2)) #border of the chart

  return(list(graph=graph,data=data_vis))

}

#' Summarize Data
#'
#' This function summarizes the data by calculating the mean values of different variables
#' for each hour of the day.
#'
#' @param data A data frame containing the input data.
#' @param v85 The variable representing v85.
#' @param car_rgt The variable representing car_rgt.
#' @param heavy_rgt The variable representing heavy_rgt.
#' @param car_lft The variable representing car_lft.
#' @param heavy_lft The variable representing heavy_lft.
#' @param speed The name of the variable to store the mean v85 values.
#' @param B_to_A The name of the variable to store the mean sum of car_rgt and heavy_rgt values.
#' @param A_to_B The name of the variable to store the mean sum of car_lft and heavy_lft values.
#'
#' @return A summarized data frame with mean values for each hour.
#'
#' @importFrom dplyr group_by summarise %>%
#' @importFrom rlang :=
#' @importFrom lubridate hour
#'
#'@noRd
#'
summarize_data <- function(data, v85, car_rgt, heavy_rgt, car_lft, heavy_lft,
                           speed, B_to_A, A_to_B) {

    data %>%
      group_by(hour = hour(date)) %>%
      summarise({{ speed }} := mean({{ v85 }}, na.rm = TRUE),
                {{ B_to_A }} := mean({{ car_rgt }} + {{ heavy_rgt }}, na.rm = TRUE),
                {{ A_to_B }} := mean({{ car_lft }} + {{ heavy_lft }}, na.rm = TRUE))
}


