#' @description
#' A short description...
#' Calculate necessary data to trace the axes of the fundamental diagram
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#'
#' @return enriched_data
#' @export
#'
#'
#' @examples
#' calculate_axes(traffic)

calculate_axes<-function(enriched_data,direction_choice=NULL)
{
  if(!("speed_hist_car_lft" %in% colnames(enriched_data)) || is.null(direction_choice))
  {
    enriched_data$veh_h<-enriched_data$car
    enriched_data$km_h<-enriched_data$v85
    enriched_data$veh_km<-0
    for(i in 1:length(enriched_data$veh_km))
    {enriched_data$veh_km[i]<-enriched_data$veh_h[i]/enriched_data$km_h[i]}
  }



  else if(direction_choice=='lft')
  {
    enriched_data$veh_h_lft<-enriched_data$car_lft*4
    enriched_data$km_h_lft<-enriched_data$v85_lft
    enriched_data$veh_km_lft<-0

    for(i in 1:length(enriched_data$car))
    {
      enriched_data$veh_km_lft[i]<-enriched_data$veh_h_lft[i]/enriched_data$km_h_lft[i]
    }
  }

  else
  {
    enriched_data$veh_h_rgt<-enriched_data$car_rgt*4
    enriched_data$km_h_rgt<-enriched_data$v85_rgt
    enriched_data$veh_km_rgt<-0

    for(i in 1:length(enriched_data$car))
    {
      enriched_data$veh_km_rgt[i]<-enriched_data$veh_h_rgt[i]/enriched_data$km_h_rgt[i]
    }
  }
  return(enriched_data)
}

#' @description
#' A short description...
#' Calculate characteristics values of the fundamental diagram.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param direction_choice Character. Direction choosen. Default to NULL.
#'
#' @return list(flow,speed,density)
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' calculate_characteristic(traffic)
#' calculate_characteristic(traffic,
#'   direction_choice='lft')

calculate_characteristic<- function (enriched_data,
                                     direction_choice=NULL)
{
  if(is.null(direction_choice) || !("speed_hist_car_lft" %in% colnames(enriched_data)))
  {
    density <- sum(enriched_data$veh_km,na.rm = TRUE)/length(enriched_data$veh_km)
    speed<- sum(enriched_data$km_h,na.rm = TRUE)/length(enriched_data$km_h)
  }


  else if (direction_choice=='lft')
  {
    density<-sum(enriched_data$veh_km_lft,na.rm = TRUE)/length(enriched_data$veh_km_lft)
    speed<-sum(enriched_data$km_h_lft,na.rm = TRUE)/length(enriched_data$km_h_lft)
  }

  else
  {
    density<-sum(enriched_data$veh_km_rgt,na.rm = TRUE)/length(enriched_data$veh_km_rgt)
    speed<-sum(enriched_data$km_h_rgt,na.rm = TRUE)/length(enriched_data$km_h_rgt)
  }


  return(list(speed=speed,density=density))

}

#' @description
#' A short description...
#' Create news columns to have dimensionless data.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param direction_choice Character Direction choosen. Default to NULL.
#' @param list_charac List of Reals. Characteristics values.
#'
#' @return enriched_data
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' calculate_data_dimensionless(traffic,
#'   direction_choice='lft',
#'   list_charac=calculate_characteristic(traffic))


calculate_data_dimensionless<- function (enriched_data,
                                         direction_choice=NULL,
                                         list_charac)
{
  if(!("speed_hist_car_lft" %in% colnames(enriched_data)) || is.null(direction_choice))
  {
    enriched_data <- enriched_data %>%
      mutate (
        veh_km_adim=veh_km/list_charac$density,
        km_h_adim=km_h/list_charac$speed)
  }

  else if (direction_choice=='lft')
  {
    enriched_data <- enriched_data %>%
      mutate (
        veh_km_lft_adim=veh_km_lft/list_charac$density,
        km_h_lft_adim=km_h_lft/list_charac$speed)
  }

  else
  {
    enriched_data <- enriched_data %>%
      mutate (
        veh_km_rgt_adim=veh_km_rgt/list_charac$density,
        km_h_rgt_adim=km_h_rgt/list_charac$speed)
  }

  return(enriched_data)

}


#' @description
#' A short description...
#' Calculate the slope of the interpolation line for each slope tested.
#'
#'
#' @param ordinate_line Real vector. Ordinate test.Length vector = NumberOfOrdinate.
#' @param mat_inter Real matrix. Percent of point under the tested line. Matrix result of the function 'count_point'
#' @param NumberOfSlope Integer. Number of slope tested. Default to 50.
#' @param NumberOfOrdinate Integer. Number of ordinate tested. Default to 45.
#'
#' @return alpha
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' calculate_interpolation_line(ordinate_line,
#'   mat_inter,
#'   NumberOfSlope=60,
#'   NumberOfOrdinate=65)

calculate_interpolation_line <- function(ordinate_line,
                                         mat_inter,
                                         NumberOfSlope = 50,
                                         NumberOfOrdinate = 45) {

  alpha <- numeric(NumberOfSlope)

  alpha <- sapply(1:NumberOfSlope, function(i) {
    if (mat_inter[i, NumberOfOrdinate + 1] > 1) {
      xout <- seq(ordinate_line[mat_inter[i, NumberOfOrdinate + 1] - 1],
                  ordinate_line[mat_inter[i, NumberOfOrdinate + 1]],
                  by = ordinate_line[mat_inter[i, NumberOfOrdinate + 1]] - ordinate_line[mat_inter[i, NumberOfOrdinate + 1] - 1])
      interpolated <- approx(ordinate_line, mat_inter[i, 1:NumberOfOrdinate], xout = xout, method = 'linear')
      x_interp <- interpolated$x
      y_interp <- interpolated$y
      return((y_interp[2] - y_interp[1]) / (x_interp[2] - x_interp[1]))
    } else {
      return(0)
    }
  })

  return(alpha)
}


#' @description
#' A short description...
#' Calculate a and b from the equation y=ax+b
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param NumberOfSlope Integer. Number of slope tested. Default to 50.
#' @param NumberOfOrdinate Integer. Number of ordinate tested. Default to 45.
#' @param slope Real vector. vector of 2 reals.
#' @param ordinate Real vector. vector of 2 reals.
#' @param direction_choice Character. Direction choosen.default to NULL.
#'
#' @return list(a,b)
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' calculate_necessary_data (traffic,
#'   NumberOfSlope=60,
#'   NumberOfOrdinate=65,
#'   slope=c(-2,-0.1),
#'   ordinate=c(1,1.5),
#'   direction_choice=NULL)

calculate_necessary_data <- function (enriched_data,
                                      NumberOfSlope=50,
                                      NumberOfOrdinate=45,
                                      slope,
                                      ordinate,
                                      direction_choice=NULL,
                                      percent1=95,
                                      percent2=99)
{
  percent<-percent1

  for(i in 1:2)
  {
    # Create vectors to have the values we want to test
    slope_line<-seq(from=slope[1],to=slope[2],length.out=NumberOfSlope)
    ordinate_line<-seq(from=ordinate[1],to=ordinate[2],length.out=NumberOfOrdinate)

    # Percent of the point under the line
    mat_inter<-count_point(enriched_data,slope_line,ordinate_line,direction_choice,
                           NumberOfSlope,NumberOfOrdinate,percent)

    # Calculate the alpha slope
    alpha<-calculate_interpolation_line(ordinate_line,mat_inter,
                                        NumberOfSlope,NumberOfOrdinate)

    # Calculate the final line
    final_slope<-slope_line[which(alpha==max(alpha))]
    final_ordinate<-ordinate_line[mat_inter[which(alpha==max(alpha)),NumberOfOrdinate+1]]

    # Update data for the following loop
    percent<- percent2
    slope<-c(final_slope,final_slope)
    ordinate<-c(final_ordinate,4*final_ordinate)

  }
  final_slope<-final_slope[1]
  final_ordinate<-final_ordinate[1]

  return(list(a=final_slope,b=final_ordinate))
}


#' @description
#' A short description...
#' Calculate the percent of point under the line tested.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param slope_line Real vector. Slope tested.Length vector = NumberOfSlope.
#' @param ordinate_line Real vector. Ordinate tested.Length vector = NumberOfOrdinate.
#' @param direction_choice Character. Direction choosen.
#' @param NumberOfSlope Integer. Number of slope tested. Default to 50.
#' @param NumberOfOrdinate Integer. Number of ordinate tested. Default to 45.
#' @param percent Real. Percent of point under which we want the number of points.
#'
#' @return mat_inter
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' count_point(traffic,
#'   slope_line=seq(from=-2,to=-0.1,length.out=50),
#'   ordinate_line=seq(from=1,to=1.5,length.out=45),
#'   direction_choice='lft',
#'   NumberOfSlope=50,
#'   NumberofOrdinate=45,
#'   percent=95)

count_point <- function(enriched_data,
                        slope_line,
                        ordinate_line,
                        direction_choice = NULL,
                        NumberOfSlope = 50,
                        NumberOfOrdinate = 45,
                        percent) {

  if (is.null(direction_choice) || !("speed_hist_car_lft" %in% colnames(enriched_data))) {
    abscissa_point <- enriched_data$veh_km_adim
    ordinate_point <- enriched_data$km_h_adim
  } else if (direction_choice == 'lft') {
    abscissa_point <- enriched_data$veh_km_lft_adim
    ordinate_point <- enriched_data$km_h_lft_adim
  } else {
    abscissa_point <- enriched_data$veh_km_rgt_adim
    ordinate_point <- enriched_data$km_h_rgt_adim
  }

  mat_inter <- matrix(0, nrow = NumberOfSlope, ncol = NumberOfOrdinate + 1)

  for (a in 1:NumberOfSlope) {
    test_slope <- slope_line[a]

    for (b in 1:NumberOfOrdinate) {
      test_ordinate <- ordinate_line[b]

      # Calcul vectorisé de NumberOfPoint
      test_equation <- test_slope * abscissa_point + test_ordinate
      NumberOfPoint <- sum(ordinate_point < test_equation, na.rm = TRUE)

      mat_inter[a, b] <- 100 * NumberOfPoint / sum(!is.na(abscissa_point))

      if (mat_inter[a, b] >= percent && mat_inter[a, NumberOfOrdinate + 1] == 0) {
        mat_inter[a, NumberOfOrdinate + 1] = b
      }
    }

    if (mat_inter[a, NumberOfOrdinate + 1] == 0) {
      mat_inter[a, NumberOfOrdinate + 1] = 1
    }
  }

  return(mat_inter)
}

#' @description
#' A short description...
#' Create the necessary columns to have the final envelope of the fundamental diagram.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#'
#' @return enriched_data
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' create_necessary_column(traffic)

create_necessary_column <- function (enriched_data,direction_choice=NULL)
{
  if("speed_hist_car_lft" %in% colnames(enriched_data))
  {enriched_data<-restore_v85(enriched_data)}

  enriched_data<-retrieve_missing_data(enriched_data,show=FALSE)

  enriched_data<-calculate_axes(enriched_data,direction_choice)

  return(enriched_data)
}

#' @description
#' A short description...
#' Filter the dataframe from user's demand
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments  character, id of the sensor you want to study between quotes. Example: 9000002156, all if NULL (default).
#' @param weekday_choice integer vector. Weekday choosen. Example: 1 for monday etc. Default to the all week.
#' @param hour_choice Integer vector. Hours choosen, default to the all day.
#' @param vacation_choice character, "YES" to keep, "NO" to filter out, or "ONLY" to only take vacation in the database
#' @param holiday_choise character, "YES" to keep, "NO" to filter out, or "ONLY" to only take public holidays in the database
#'
#' @return enriched_data
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
#' @examples
#' filter_demand_user(traffic)
#' filter_demand_user(traffic,
#'  date_range = c('2022-07-01','2022-09-01'),
#'  weekday_choice= c('monday','friday','sunday),
#'  hour_choice= c(1,5,10,14,21),
#'  vacation_choice=NULL,
#'  holiday_choice=TRUE,
#'  segments = 'RteVitre-06')

filter_demand_user <- function(enriched_data,
                               segments = NULL,
                               date_range = NULL,
                               weekday_choice = NULL,
                               hour_choice = NULL,
                               vacation_choice = NULL,
                               holiday_choice = NULL) {


  if (!is.null(segments)) {
    enriched_data <- enriched_data %>% filter(segment_id %in% segments)

  }

  if (!is.null(date_range)) {
    enriched_data <- enriched_data %>% filter(day >= date_range[1] & day <= date_range[2])

  }

  if (!is.null(vacation_choice)) {

    if (vacation_choice == "NO") {
      enriched_data <- enriched_data %>% filter(vacation %in% "No vacation")
    } else if (vacation_choice == "ONLY") {
      enriched_data <- enriched_data %>% filter(!vacation %in% "No vacation")
    }

  }

  if (!is.null(holiday_choice)) {

    if (holiday_choice == "NO") {
      enriched_data <- enriched_data %>% filter(!holiday)
    } else if (holiday_choice == "ONLY") {
      enriched_data <- enriched_data %>% filter(holiday)
    }

  }

  if(!is.null(weekday_choice))
  {day_to_number <- c("sunday" = 1, "monday" = 2, "tuesday" = 3, "wednesday" = 4,
                      "thursday" = 5, "friday" = 6, "saturday" = 7)

  enriched_data <- enriched_data %>%
    mutate(weekday_num = day_to_number[weekday]) %>% filter(weekday_num %in% weekday_choice)
  }

  if(!is.null(hour_choice))
  {enriched_data<-enriched_data %>% filter(hour %in% hour_choice)}

  return(enriched_data)
}


#' @description
#' A short description...
#' Plot the fundamental diagram and its envelope
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param date_range Date vector. example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segments  character, id of the sensor you want to study between quotes. Example: 9000002156, all if NULL (default).
#' @param weekday_choice integer vector. Weekday choosen. Example: 1 for monday etc. Default to the all week.
#' @param hour_choice Integer vector. Hours choosen, default to the all day.
#' @param vacation_choice character, "YES" to keep, "NO" to filter out, or "ONLY" to only take vacation in the database
#' @param holiday_choise character, "YES" to keep, "NO" to filter out, or "ONLY" to only take public holidays in the database
#' @param direction_choice character, " " for all, "rgt" for right (B to A), "lft" for left (A to B)
#' @param NumberOfSlope Integer. Number of slope tested. Default to 50.
#' @param NumberOfOrdinate Integer. Number of ordinate tested. Default to 45.
#'
#'
#' @return
#' @export
#'
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#'
#' @examples
#' plot_diagram_envelope(traffic)
#' plot_diagram_envelope(traffic,
#'   date_range = c('2022-07-01','2022-09-01'),
#'   weekday_choice= c(1,2,3),
#'   hour_choice= c(1,5,10,14,21),
#'   vacation_choice=NULL,
#'   holiday_choice=TRUE,
#'   segments = 9000002156,
#'   NumberOfSlope=60,
#'   NumberOfOrdinate=65,
#'   direction_choice='lft)



plot_diagram_envelope <- function(enriched_data,
                                  segment,
                                  date_range = NULL,
                                  weekday_choice = NULL,
                                  hour_choice = NULL,
                                  vacation_choice = NULL,
                                  holiday_choice = NULL,
                                  direction_choice = NULL,
                                  NumberOfSlope = 50,
                                  NumberOfOrdinate = 45,
                                  percent1 = c(95, 99),
                                  percent2 = c(95, 99)) {

  # Check if segment is provided
  if (missing(segment) || is.null(segment)) {
    stop("A segment must be specified.")
  }

  # Filter data for the specified segment
  enriched_data <- filter_demand_user(enriched_data,
                                      segments = segment,
                                      date_range = date_range,
                                      weekday_choice = weekday_choice,
                                      hour_choice = hour_choice,
                                      vacation_choice = vacation_choice,
                                      holiday_choice = holiday_choice)

  # Check if the specified segment exists in the filtered data
  if (nrow(enriched_data) == 0) {
    stop("No data available for the specified segment and filters.")
  }

  df <- create_necessary_column(enriched_data, direction_choice)
  list_charac <- calculate_characteristic(df, direction_choice)
  df <- calculate_data_dimensionless(df, direction_choice, list_charac)

  list_final_1 <- calculate_necessary_data(df,
                                           NumberOfSlope,
                                           NumberOfOrdinate,
                                           slope = c(-2, -0.1),
                                           ordinate = c(1, 7),
                                           direction_choice,
                                           percent1 = percent1[1],
                                           percent2 = percent1[2])

  list_final_2 <- calculate_necessary_data(df,
                                           NumberOfSlope,
                                           NumberOfOrdinate,
                                           slope = c(-6, list_final_1$a),
                                           ordinate = c(list_final_1$b, 8 * list_final_1$b),
                                           direction_choice,
                                           percent1 = percent2[1],
                                           percent2 = percent2[2])

  # Convert back to dimensional data
  list_final_1$a <- list_charac$speed * list_final_1$a / list_charac$density
  list_final_1$b <- list_charac$speed * list_final_1$b

  list_final_2$a <- list_charac$speed * list_final_2$a / list_charac$density
  list_final_2$b <- list_charac$speed * list_final_2$b

  # Plot the diagram and lines
  graphique <- plot_lines(df, list_final_1, list_final_2, direction_choice)

  return(list(linear = graphique$linear, parabolic = graphique$parabolic))
}



#' @description
#' A short description...
#' Plot the fundamental diagram and its envelope.
#'
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#' @param list_final_1 List of reals. Values of the line's equation.
#' @param list_final_2 List of reals. Values of the line's equation.
#' @param direction_choice Character Direction choosen. Default to NULL.
#'
#' @return
#' @export
#'
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#'
#' @examples
#' plot_lines(traffic,
#'   list_final_1=c(-0.1,70),
#'   list_final_2=c(-5,80),
#'   direction_choice=NULL)

plot_lines <- function (enriched_data,
                        list_final_1,
                        list_final_2,
                        direction_choice=NULL)
{
  #Define the axes of the diagram
  if(!("speed_hist_car_lft" %in% colnames(enriched_data)) || is.null(direction_choice))
  {
    abscissa<-enriched_data$veh_km
    ordinate1<-enriched_data$km_h
    ordinate2<-enriched_data$veh_h
  }

  else if(direction_choice=='lft')
  {
    abscissa<-enriched_data$veh_km_lft
    ordinate1<-enriched_data$km_h_lft
    ordinate2<-enriched_data$veh_h_lft
  }

  else
  {
    abscissa<-enriched_data$veh_km_rgt
    ordinate1<-enriched_data$km_h_rgt
    ordinate2<-enriched_data$veh_h_rgt
  }

  # Calculate the intersection between the lines
  x_inter<-(list_final_2$b - list_final_1$b)/(list_final_1$a - list_final_2$a)
  y_inter_1 <- list_final_1$a * x_inter + list_final_1$b
  y_inter_2 <- list_final_1$a * x_inter*x_inter + list_final_1$b*x_inter

  x_lim_1<--list_final_2$b/list_final_2$a
  x_lim_2<-x_lim_1/2
  y_lim_2<- list_final_2$a * x_lim_2*x_lim_2+ list_final_2$b*x_lim_2

  #Plot the fundamental diagram and its envelope
  graphique<-list(linear=NULL,parabolic=NULL)

  graphique$linear <- ggplot(data = enriched_data, mapping = aes(x = abscissa, y = ordinate1)) +
    geom_point(pch = 20, na.rm = TRUE) +
    labs(x = 'Density (veh/km)', y = 'Speed (km/h)', title = paste('Segment :', enriched_data$segment_fullname[1])) +
    geom_line(mapping = aes(x = abscissa, y = list_final_1$a * abscissa + list_final_1$b), color = 'red', na.rm = TRUE) +
    geom_line(mapping = aes(x = abscissa, y = list_final_2$a * abscissa + list_final_2$b), color = 'blue', na.rm = TRUE) +
    annotate("point", x = x_inter, y = y_inter_1, shape = 15, color = "orange", size = 3) +
    annotate("text", x = x_inter, y = y_inter_1, hjust = -0.5, label = paste("x =", sprintf("%.2f", x_inter))) +
    annotate("point", x = x_lim_1, y = 0, shape = 15, color = "orange", size = 3) +
    annotate("text", x = x_lim_1, y = 0, hjust = 1.4, label = paste("x =", sprintf("%.2f", x_lim_1)))+
    coord_cartesian(xlim = c(0, x_lim_1), ylim = c(0, max(ordinate1,na.rm = TRUE)))


  #Plot the other one
  graphique$parabolic <- ggplot(data = enriched_data, mapping = aes(x = abscissa, y = ordinate2)) +
    geom_point(pch = 20, na.rm = TRUE) +
    labs(x = 'Density (veh/km)', y = 'Flow (veh/h)', title = paste('Segment :', enriched_data$segment_fullname[1])) +
    geom_line(mapping = aes(x = abscissa, y = list_final_1$a * abscissa * abscissa + list_final_1$b * abscissa), color = 'red', na.rm = TRUE) +
    geom_line(mapping = aes(x = abscissa, y = list_final_2$a * abscissa * abscissa + list_final_2$b * abscissa), color = 'blue', na.rm = TRUE) +
    annotate("point", x = x_inter, y = y_inter_2, shape = 15, color = "orange", size = 3) +
    annotate("text", x = x_inter, y = y_inter_2, hjust = 1.5, label = paste("y =", sprintf("%.2f", y_inter_2))) +
    annotate("point", x = x_lim_2, y = y_lim_2, shape = 15, color = "orange", size = 3) +
    annotate("text", x = x_lim_2, y = y_lim_2, hjust = -0.5, label = paste("y =", sprintf("%.2f", y_lim_2)))+
    coord_cartesian(xlim = c(0, max(abscissa,na.rm=TRUE)), ylim = c(0, y_lim_2))

  return(graphique)
}


#' Restore the v85 by direction from speed_hist_car
#'
#' Add two colums in the dataframe : v85_lft and v85_rgt
#'
#' @param enriched_data enriched data.frame containing all the data for all your sensors
#'
#' @return enriched_data
#' @export
#'
#' @examples
#' restore_v85(enriched_data)

restore_v85 <- function(enriched_data, direction_choice) {
  speed <- seq(5, 125, by = 5)

  process_direction <- function(speed_hist_col) {
    sapply(speed_hist_col, function(vec) {
      elements <- as.numeric(gsub("\\[|\\]", "", strsplit(vec, ",")[[1]]))
      cum_sum <- cumsum(elements)
      per_85 <- 0.85 * sum(elements)
      speed[which.max(cum_sum >= per_85)]
    })
  }

  if (direction_choice == 'lft') {
    enriched_data$v85_lft <- process_direction(enriched_data$speed_hist_car_lft)
  } else {
    enriched_data$v85_rgt <- process_direction(enriched_data$speed_hist_car_rgt)
  }

  return(enriched_data)
}

#' Clean data from inactivity periods and missing hours
#'
#' This function cleans the data by removing nighttime hours and periods of inactivity.
#' It combines the functionalities of `retrieve_missing_hours` and `replace_inactivity_period`.
#'
#' @param data data.frame containing all the data for all your sensors
#' @param date_range Date vector. Example: c('2021-01-01','2022-01-01'). Full period if NULL (default).
#' @param segment_id Character vector. Selected road segment, all if NULL (default).
#' @param threshold_uptime Numeric. Uptime threshold chosen. Default is 0.5.
#' @param successive_day Integer. Number of days chosen to define an inactivity period. Default is 2.
#' @param remove_data Logical. If TRUE, completely removes inactivity periods. If FALSE, replaces data with NA.
#' @param show_graph Logical. If TRUE, displays a graph of inactivity periods. Default is TRUE.
#' @return Cleaned data
#'
#' @export
#'
#' @import dplyr
#' @import lubridate
#'

retrieve_missing_data <- function(data,
                                  date_range = NULL,
                                  segment_id = NULL,
                                  threshold_uptime = 0.5,
                                  successive_day = 2,
                                  remove_data = TRUE,
                                  show_graph = TRUE) {

  # Convert date column to datetime format
  data$date <- ymd_hms(data$date)

  # Filter data by segment_id if provided
  if (!is.null(segment_id)) {
    data <- data[data$segment_id %in% segment_id,]
  }

  # Filter data by date range if provided
  if (!is.null(date_range)) {
    data <- data[data$day >= date_range[1] & data$day <= date_range[2],]
  }

  # Check if there's data after filtering
  if (nrow(data) == 0) {
    stop("No data in the selected period")
  } else {
    # Remove inactivity periods
    data_without_inactivity_period <- replace_inactivity_period(data, successive_day, threshold_uptime, remove_data, show_graph)

    # Remove hours with no data
    data_clean <- retrieve_missing_hours(data_without_inactivity_period, threshold_uptime)
  }

  return(data_clean)
}

#' Remove nighttime hours from traffic data
#'
#' This function removes nighttime hours when sensors cannot capture information.
#' It filters the data to retain only the hours with reliable uptime.
#'
#' @param data data.frame containing all the data for all your sensors
#' @param threshold_uptime Numeric. Uptime threshold chosen. Default is 0.5.
#'
#' @return data without nighttime hours
#' @export
#'
#' @import dplyr
#' @import lubridate
#'
retrieve_missing_hours <- function(data, threshold_uptime = 0.5) {

  # Convert date column to datetime format
  data$date <- ymd_hms(data$date)

  # Determine season based on date
  data <- data %>%
    mutate(season = case_when(
      (month(.data$date) == 3 & day(.data$date) >= 20) | (month(.data$date) == 6 & day(.data$date) < 21) | (month(.data$date) %in% 4:5) ~ "Spring",
      (month(.data$date) == 6 & day(.data$date) >= 21) | (month(.data$date) == 9 & day(.data$date) < 23) | (month(.data$date) %in% 7:8) ~ "Summer",
      (month(.data$date) == 9 & day(.data$date) >= 23) | (month(.data$date) == 12 & day(.data$date) < 21) | (month(.data$date) %in% 10:11) ~ "Autumn",
      TRUE ~ "Winter"
    ))

  # Calculate condition for each segment, season, and hour
  df_season <- data %>%
    group_by(.data$segment_id, .data$season, .data$hour) %>%
    summarise(
      condition = mean(.data$car == 0 & .data$uptime <= threshold_uptime) <= 0.05,
      .groups = 'drop'
    )

  # Filter data based on the calculated condition
  data <- data %>%
    semi_join(df_season %>% filter(.data$condition), by = c("segment_id", "season", "hour"))

  return(data)
}

#' Replace or remove inactivity periods in traffic data
#'
#' This function identifies periods of sensor inactivity and either replaces them with NA or removes them.
#' An inactivity period is defined as a consecutive period where the uptime is below the chosen threshold
#' or if no vehicles are counted during this period.
#'
#' @param data data.frame containing all the data for all your sensors
#' @param successive_day Integer. Number of consecutive days to define an inactivity period. Default is 2.
#' @param threshold_uptime Numeric. Uptime threshold chosen. Default is 0.5.
#' @param remove_data Logical. If TRUE, removes inactivity periods. If FALSE, replaces data with NA.
#' @param show_graph Logical. If TRUE, displays a graph of inactivity periods. Default is TRUE.
#'
#' @return data with inactivity periods replaced or removed
#' @export
#'
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#'
replace_inactivity_period <- function(data, successive_day = 2, threshold_uptime = 0.5, remove_data = TRUE, show_graph = TRUE) {

  # Convert date column to datetime format
  data$date <- ymd_hms(data$date)

  # Function to identify inactive periods
  identify_inactive_periods <- function(df) {
    df <- df %>%
      mutate(
        is_inactive = (.data$uptime < threshold_uptime) | (.data$car == 0 & .data$uptime >= threshold_uptime),
        inactive_group = cumsum(c(0, diff(as.numeric(.data$is_inactive)) != 0))
      ) %>%
      group_by(.data$inactive_group) %>%
      mutate(
        period_start = first(.data$date),
        period_end = last(.data$date),
        period_length = as.numeric(difftime(.data$period_end, .data$period_start, units = "days"))
      ) %>%
      ungroup()

    return(df)
  }

  # Process the data
  processed_data <- data %>%
    group_by(.data$segment_id) %>%
    group_modify(~identify_inactive_periods(.x)) %>%
    ungroup() %>%
    mutate(should_remove = .data$is_inactive & .data$period_length >= successive_day)

  # Columns to process
  columns_to_process <- c("heavy", "car", "bike", "pedestrian",
                          "heavy_lft", "heavy_rgt", "car_lft", "car_rgt",
                          "bike_lft", "bike_rgt", "pedestrian_lft", "pedestrian_rgt")

  # Apply removal or replacement with NA
  if (remove_data) {
    result <- processed_data %>%
      filter(!.data$should_remove) %>%
      select(-"is_inactive", -"inactive_group", -"period_start", -"period_end", -"period_length", -"should_remove")
  } else {
    result <- processed_data %>%
      mutate(across(all_of(columns_to_process),
                    ~if_else(.data$should_remove, NA_real_, .))) %>%
      select(-"is_inactive", -"inactive_group", -"period_start", -"period_end", -"period_length", -"should_remove")

  }

  # Display graph if show_graph is TRUE
  if (show_graph) {
    graph_data <- processed_data %>%
      filter(.data$should_remove) %>%
      select(.data$segment_fullname, .data$period_start, .data$period_end, .data$period_length) %>%
      mutate(segment_fullname = as.factor(.data$segment_fullname))

    p <- ggplot(graph_data, aes(x = .data$period_start, xend = .data$period_end, y = .data$segment_fullname, yend = .data$segment_fullname)) +
      geom_segment(color = "red", size = 2) +
      labs(title = "Inactivity periods to be removed",
           x = "Date",
           y = "Segment")

    # Display the graph
    print(p)
  }

  return(result)
}

