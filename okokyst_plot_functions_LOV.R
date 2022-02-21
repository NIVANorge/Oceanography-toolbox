
library(lubridate)
library(purrr)

#
# Make ggplot-ready data (including interpolation)
#
# data:         Data on "long" format. Variables must be "Depth" and "Time"
# varname:      Name of variable to plot (quoted string)
# gam:          Uses mgcv:gam if gam = TRUE, otherwise uses akima. Default is gam = FALSE
# gam_k:        The "k" (degrees of freedom + 1) for mgcv:gam. Ignored if gam = FALSE
# linear:       TRUE/FALSE depending on whether you will use a linear akima interpolation or not. Ignored if gam = TRUE
# nx:           Number of points in "time" direction you want in the filal smoothed data
# ny:           Number of points in "depth" direction you want in the filal smoothed data
# max_timediff: Maximum depth of colours on plot = the maximum depth for data within 'max_timediff' days from that point  
#               Default value = 21, i.e. 21 days
#               If there number of days between two observation dates is > 2*max_timediff, there will be a white "break"  

okokyst_make_plotdata <- function(data, varname, 
                                  gam = FALSE, gam_k = 20, linear = TRUE,
                                  nx = 100, ny = 100, max_timediff = 21){
  
  data <- as.data.frame(data)
  if (!("Time" %in% names(data)) & !("Date" %in% names(data)))
    stop("Data must contain either a variable 'Date' (date format) or Time (time format)")
  if (!("Time" %in% names(data)) & "Date" %in% names(data))
    data <- data %>%
      mutate(Time = floor_date(as.POSIXct(Date, tz = "GMT"), "day"))
  sel <- complete.cases(data[,c("Time", "Depth", varname)])
  data <- dplyr::rename(data, "z" = varname)
  # Interpolation
  data$Time2 <- as.numeric(data$Time)/86400    # convert to day-scale, for better interpolation
  if (!gam){
    interpol <- with(data[sel,], akima::interp(Time2, Depth, z, nx = nx, ny = ny, linear = linear))   # spline
    # Reshaping data
    dimnames(interpol$z)[[2]] <- interpol$y
    result <- as.data.frame(interpol$z) %>%
      data.frame(x = interpol$x, .) %>%
      tidyr::gather(key = "y", value = "z", -x) %>%
      rename(Time2 = x) %>%
      mutate(Depth = as.numeric(sub("X", "", y)),
             Time = as.POSIXct(Time2*86400, origin = "1970-01-01")) %>%
      select(-y)
    # Make maximum depth for every time in smooth
    times_smooth <- sort(unique(result$Time2))
    smooth_maxdepth <- data.frame(
      Time2 = times_smooth,
      
      Max_depth = seq_along(times_smooth) %>% 
        map_dbl(get_maxdepth, obsdata = data[sel,], smoothdata = result, max_timediff = max_timediff)
    )
    # Add maximum depth to smoothed data, and filter data so we keep only 
    #   data < maximum depth
    result <- result %>%
      left_join(smooth_maxdepth, by = "Time2") %>%
      filter(Depth <= Max_depth)
  } else {
    model <- gam(z ~ te(Time2, Depth, k = gam_k), data = data[sel,])
    result <- with(
      data[sel,],
      expand.grid(
        Time2 = seq(min(Time2), max(Time2), length = nx),
        Depth = seq(min(Depth), max(Depth), length = ny)
      ))
    result$z <- predict.gam(model, result)
    result$Time <- as.POSIXct(result$Time2*86400, origin = "1970-01-01", tz = "GMT")
    # Make maximum depth for every time in smooth
    times_smooth <- sort(unique(result$Time2))
    smooth_maxdepth <- data.frame(
      Time2 = times_smooth,
      Max_depth = seq_along(times_smooth) %>% map_dbl(get_maxdepth, obsdata = data[sel,], smoothdata = result)
    )
    # Add maximum depth to smoothed data, and filter data so we keep only 
    #   data < maximum depth
    result <- result %>%
      left_join(smooth_maxdepth, by = "Time2") %>%
      filter(Depth <= Max_depth)
  }
  result
}

# df_plot <- okokyst_make_plotdata(df_ctd, "salt")
# df_plot <- okokyst_make_plotdata(df_ctd, "salt", gam = TRUE)

okokyst_plot <- function(data, varname, ctd_variable = FALSE, title = "", 
                         binwidth = 1, limits = c(NA,NA), color_ctdtime = "black", 
                         gam = FALSE, gam_k = 20, linear = TRUE,
                         nx = 200, ny = 200, 
                         max_timediff = 21,                
                         colored_points = TRUE, colored_points_size = 0.1,
                         colored_points_rim = FALSE,
                         reverse_colors = FALSE,
                         palette = "D",
                         maxdepth = NULL,
                         xlabel = TRUE,
                         ylabel = TRUE,
                         plotdata = NULL,
                         return_rasterdata = FALSE){
  direction <- ifelse(reverse_colors, -1, 1)
  if (!("Time" %in% names(data)) & !("Date" %in% names(data)))
    stop("Data must contain either a variable 'Date' (date format) or Time (time format)")
  if (!("Time" %in% names(data)) & "Date" %in% names(data))
    data <- data %>%
      mutate(Time = floor_date(as.POSIXct(Date, tz = "GMT"), "day"))
  if (is.null(plotdata)){
    df_plot <- okokyst_make_plotdata(data, varname, 
                                     gam = gam, gam_k = gam_k, linear = linear,
                                     nx = ny, ny = ny, max_timediff = max_timediff)
  } else {
    df_plot <- plotdata
  }
  # Remove data where there are no interpolated results
  df_plot <- df_plot %>%
    filter(!is.na(z))
  if (is.null(maxdepth)){
    ylim <- data$Depth[!is.na(data[[varname]])] %>% range() %>% rev()
  } else {
    ylim <- c(maxdepth, 0)
  }
  gg <- ggplot(df_plot, aes(Time, Depth)) +
    geom_raster(aes(fill = z), interpolate = F, hjust = 0.5, vjust = 0.5) +
    #geom_contour(aes(z = z), binwidth = binwidth) + 
    #scale_x_datetime(breaks = seq(dmy(23022017), dmy(30112021), by ="1 month"), date_labels =  "%b %y")+
    scale_y_reverse(limits = ylim) +
    scale_x_datetime(date_breaks = "2 month", date_minor_breaks = "2 month", date_labels = "%b %y") +
    # scale_fill_gradientn(varname, colours = fields::tim.colors(16), limits = limits) +
    scale_fill_viridis_c(varname, option = palette, limits = limits, direction = direction) +
    theme_bw()
  if (ctd_variable){
    gg <- gg + geom_vline(xintercept = unique(data$Time), color = color_ctdtime)
  } else if (!colored_points) {
    gg <- gg + geom_point(data = data, aes(Time, Depth), color = "grey", size = 0.1)
  } else if (colored_points) {
    if (colored_points_rim){
      gg <- gg + 
        geom_point(data = data, aes(Time, Depth), 
                   pch = 21, size = colored_points_size + 0.1, color = "white")
    }
    gg <- gg + 
      geom_point(data = data, aes(Time, Depth, fill = .data[[varname]]), 
                 pch = 21, size = colored_points_size) +
      scale_color_gradientn(varname, colours = fields::tim.colors(16), limits = limits)
  }
  if (title != "")
    gg <- gg + ggtitle(title)
  if (!xlabel)
    gg <- gg + theme(axis.title.x = element_blank())
  gg
  if (!ylabel)
    gg <- gg + theme(axis.title.y = element_blank())
  if (return_rasterdata){
    result <- list(
      plot = gg,
      rasterdata = df_plot)
  } else {
    result = gg
  }
  result
}

# For times_smooth number i, return
#  maximum depth for all data within 15 days  
# Assume that variables z, Time2 and Depth exists in both data sets
get_maxdepth <- function(i, max_timediff = 21, obsdata, smoothdata){
  data_maxdepth <- obsdata %>% 
    group_by(Time2) %>%
    summarise(Max_depth = max(Depth), .groups = 'drop')
  times_smooth <- sort(unique(smoothdata$Time2))
  obsdata %>% 
    count(Time2) %>%
    mutate(Timediff = abs(times_smooth[i] - Time2))%>%
    select(-n) %>%
    # just add column from data_maxdepth, doon't need join as times should be identical 
    mutate(Max_depth = data_maxdepth$Max_depth) %>% 
    # Here we remove all data further away than 'max_timediff'  
    filter(Timediff <= max_timediff) %>%
    summarise(Max_depth = max(Max_depth), .groups = 'drop') %>%
    pull(Max_depth)
}

