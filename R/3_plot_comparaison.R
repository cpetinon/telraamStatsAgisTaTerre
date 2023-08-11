#' Comparaison of the number of users for each hour of the day there is data for over two different time periods.
#'
#' @param param_ref List of your data and parameters you want to filter on and compare : list(data=,sensor=,direction=,mobility=,date_range=,vac=,wkd=,p_h=)
#' Where: - data: dataframe containing your data
#' - sensor: character, id of the sensor you want to study between quotes
#' - direction: character, " " for all, "_rgt" for right (B to A), "_lft" for left (A to B)
#' - mobility: character vector, c("car", "heavy", "pedestrian", "bike") are all the types you can choose from
#' - date_range: date vector, c("starting_date", "ending_date")
#' - vac: character, "YES" to keep, "NO" to filter out, or "ONLY" to only take vacation in the database
#' - wkd: character vector, c("1", "2", "3", "4", "5", "6", "7"), where "1" stands for Monday and "7" for Sunday. Use options(lubridate.week.start = 1) for this function to perform correctly.
#' - p_h: character, "YES" to keep, "NO" to filter out, or "ONLY" to only take public holidays in the database
#' @param param_1 List, same as param_ref
#' @param color_graph Color palette for the plot
#'
#'
#' @import ggplot2
#' @importFrom dplyr filter case_when %>%
#' @importFrom purrr map_dbl
#' @importFrom lubridate hour
#' @importFrom stats qt
#'
#' @return A ggplot object representing the comparison plot
#'
#' @export
#'
plot_comparaison <- function(param_ref, param_1, color_graph) {

  ## data preparation

  data_filtered_ref <- do.call(filtering, param_ref)
  data_filtered_compar <- do.call(filtering, param_1)
  if (is_empty(data_filtered_ref)|is_empty(data_filtered_compar)){

    return(NULL)

  }

  data_mean_ref <-  data_filtered_ref %>% prep() %>% mutate(Periode = "Periode de reference")
  data_mean_compar <-  data_filtered_compar %>% prep() %>% mutate(Periode = "Periode comparee")

  hour_inter <- intersect(data_mean_ref$hour_x,data_mean_compar$hour_x) %>% sort() # Selection des heures communes
  k <- length(hour_inter) # Nombre d'heure

  data_comparison <- rbind(data_mean_ref,data_mean_compar)

  # Selection des colonnes sur lesquelles on a une variance (2 valeurs au moins)

  count <- data_comparison$count
  total_mean <- data_comparison$total_mean
  variance <- data_comparison$variance
  hour_x <- data_comparison$hour_x
  Periode <- data_comparison$Periode

  data_comparison <- data_comparison %>% filter(count>1)

  data_comparison <- mutate_q(data_comparison, "q", count)
  data_comparison <- mutate_born(data_comparison, total_mean, q, variance, count, "Born1", "Born2")

  Born1 <- data_comparison$Born1
  Born2 <- data_comparison$Born2

  graph_compar <- ggplot(data_comparison, aes(x = hour_x, y=total_mean, group = Periode, color = Periode)) +

    geom_line(aes(linetype=Periode),size=1.5) + # drawing of the mean curv

    labs(x="Heure", y = "Nombre moyen d'usagers") + # axes labs

    geom_ribbon(aes(ymin=Born1, ymax=Born2, fill = Periode), linetype = "blank",alpha = 1/4) + # drawing of Student interval

    scale_x_continuous(breaks=hour_inter, limits = c(hour_inter[1]-0.5,hour_inter[length(hour_inter)]+0.5))+ # x axe scale

    expand_limits(y = 0) +

    scale_color_manual(values = color_graph) + # legend

    scale_fill_manual(values = color_graph) + # legend

    theme_bw() + # set the design of the chart

    theme(panel.background = element_rect(fill = "#F5F5F5"), # background color

          panel.grid = element_line(color = "#E3E3E3"), # grid color

          panel.border = element_rect(color = "#E3E3E3", size=2)) # border color and size


  ## wilcoxon plot
  Stat_wilcox <- map_dbl(hour_inter, ~ { # loop for each hour
    i <- .
    usager_1 <- data_filtered_ref %>% filter(hour(date) == i) %>% .$total
    usager_2 <- data_filtered_compar %>% filter(hour(date) == i) %>% .$total
    wilcox.test(usager_1,usager_2,exact=FALSE)$p.value # wilcoxon test for one hour
  })

  # Choix de la couleur en fonction de la p-valeur du test
  color <- case_when(
    Stat_wilcox < 0.05 ~ "Significatif",
    Stat_wilcox >= 0.05 & Stat_wilcox < 0.1 ~ "Entre deux",
    Stat_wilcox >= 0.1 ~ "Non-significatif",
    TRUE ~ "Autre"
  )

  # Creation du tableau pour la barre indiquant la significativite des tests
  data_wilcox <- data.frame(hour_inter, color)
  hour_inter <- data_wilcox$hour_inter
  color <- data_wilcox$color


  # Graphique: histogramme d'une unite de hauteur, indiquant la valeur de la significativite
  graph_wilcox <- ggplot(data_wilcox, aes(x = hour_inter , color = color, fill = color)) +

    geom_histogram(bins = length(hour_inter)+1) +

    labs(title="Significativite d'une difference de comportement (*)",x="Heure",y="") +

    scale_x_continuous(breaks=hour_inter,limits = c(hour_inter[1]-0.5,hour_inter[length(hour_inter)]+0.5))+ # x axe scale

    scale_color_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4")) + # legend

    scale_fill_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4")) + # legend

    theme_bw() +

    theme(panel.background = element_rect(fill = "#F5F5F5"), # background color

          panel.grid = element_line(color = "#E3E3E3"), # grid color

          panel.border = element_rect(color = "#E3E3E3", size=2), # border color and size

          title = element_text(hjust = 0.5),

          axis.text.y = element_blank(), # y axe is removed

          axis.ticks.y = element_blank()) # y axe is removed



  #--- ouput variables ---

  graph <- cowplot::plot_grid(graph_compar, graph_wilcox, align = "v", ncol = 1, rel_heights = c(0.75, 0.25)) # graph_compar is set above graph_wilcox

  count_ref <- data_mean_ref$count %>% mean() %>% round(3) # number of values used to obtain data_mean_ref

  count_p <- data_mean_compar$count %>% mean() %>% round(3) # number of values used to obtain data_mean_compar

  return(list(graph = graph,

              count_ref = count_ref,

              count_p = count_p,

              data_mean_ref = data_mean_ref,

              data_mean_compar = data_mean_compar))

}

#' Mutate Q-Value Calculation
#'
#' Internal function to calculate the Q-value.
#'
#' @param data Data frame containing the required columns.
#' @param q Name of the column to store the calculated Q-value.
#' @param count Name of the column representing the count.
#'
#' @importFrom dplyr mutate %>%
#' @importFrom stats qt
#' @importFrom rlang :=
#'
#' @return Data frame with the Q-value column added.
#'
#' @noRd
#'
mutate_q <- function(data, q, count) {
  data %>%
    mutate({{q}} := qt(.975, df = {{count}} - 1))
}


#' Mutate Born Calculation
#'
#' Internal function to calculate the Born values.
#'
#' @param data Data frame containing the required columns.
#' @param total_mean Name of the column representing total_mean.
#' @param q Name of the column representing the q-value.
#' @param variance Name of the column representing variance.
#' @param count Name of the column representing count.
#' @param Born1 Name of the column to store the calculated Born1 values.
#' @param Born2 Name of the column to store the calculated Born2 values.
#'
#' @importFrom dplyr mutate %>%
#' @importFrom rlang :=
#'
#' @return Data frame with the Born1 and Born2 columns added.
#'
#' @noRd
#'
mutate_born <- function(data, total_mean, q, variance, count, Born1, Born2) {
  data %>%
    mutate({{Born1}} := {{total_mean}} - {{q}} * sqrt({{variance}} / {{count}}),
           {{Born2}} := {{total_mean}} + {{q}} * sqrt({{variance}} / {{count}}))
}


#' Prepare data for plotting
#'
#' @param data Data table to be processed
#' @param count Column name representing the count
#' @param mean Column name representing the mean
#' @param var Column name representing the variance
#' @param total Column total of table
#'
#' @importFrom dplyr group_by summarise n arrange select %>%
#' @importFrom lubridate hour
#' @importFrom stats var
#'
#' @return A processed data table for plotting
#'
#' @keywords internal
#'
prep <- function(data, count = "count", mean = "mean", var = "var", total = .data$total) {
  data %>%
    group_by(hour = hour(date)) %>%
    summarise(
      {{ count }} := n(),
      {{ mean }} := mean({{ total }}),
      {{ var }} := var({{ total }})
    ) %>%
    arrange(hour) %>%
    select(hour_x = hour, total_mean = {{ mean }}, variance = {{ var }}, count = {{ count }})
}
