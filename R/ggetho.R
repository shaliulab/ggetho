#' Prepare a ggplot object to represent behavioural data
#'
#' This function summarises a variable of interest (y or z axis)
#' in order to subsequently represent it over time (x axis)
#' (using layers provided either by `ggplot2` or `ggetho`).
#'
#' @param data [fslbehavr::behavr] table containing the data and metadata
#' @param mapping default list of aesthetic mappings to use for plot
#' @param summary_FUN method (function) used to summarise `variable` over time (typically, the mean)
#' @param summary_time_window width (in seconds) of the time window to compute a summary on
#' @param time_wrap time (in seconds) used to wrap the data (see details)
#' @param time_offset time offset (i.e. phase, in seconds) when using `time_wrap`
#' @param multiplot integer, greater than two, or NULL, the default (see details)
#' @param multiplot_period the duration of the period when mutiplotting (see details)
#' @param cache path of the temporary dir to which data shall be output.
#' @param ... additional arguments to be passed to [ggplot2::ggplot()]
#' @details `time_wrap` is typically used to express time relatively to the start of the the day.
#' In other words, it can help be used to pull all days together in one representative day.
#' In this case, `time_wrap = hours(24)`.
#' Instead of representing data from the start of the day, it can be done from any offset, using `time_offset`.
#' For instance,  `time_offset = hours(12)` puts the circadian reference (ZT0) in the middle of the plot.
#'
#' Multiplotting is a generalisation of double-plotting, triple-plotting...
#' This type or representation is useful to understand periodic behaviours.
#' When `multiplot` is *not* NULL, data is repeated as
#' many time as its value, along the x axis.
#' The y axis is then the period (typically the day) onset.
#' It is possible to set duration of the period, which is typically 24 h to arbitrary values using the
#' `multiplot_period` argument.
#'
#' @return An initial plot object that can be further edited.
#'
#' @examples
#' # We start by making a dataset with 20 animals
#' metadata <- data.table(id = sprintf("toy_experiment|%02d", 1:20),
#'                    condition = c("A", "B"))
#' dt <- toy_activity_data(metadata, 3)
#' # We build a plot object with **nothing inside** (just the axis)
#' # we want to show proportion of time sleeping  on the y axis:
#' pl <- ggetho(dt, aes(y = asleep))
#' pl
#' # Sometimes, the variable of interest in not on the y axis, but on z axis (colour scale).
#' # When we do not provide a y axis,
#' # ggetho will make an ID fo each animal and display them on separate rows
#' pl <- ggetho(dt, aes(z = asleep))
#' pl
#' # this one is the same type, but it groups the animals by condition
#' pl <- ggetho(dt, aes(z = asleep, y = condition))
#' pl
#' # sorting with paste
#' pl <- ggetho(dt, aes(z = asleep,y = paste(condition, id)))
#' pl
#'
#' # we want to summarise (wrap) data along a circadian day:
#' pl <- ggetho(dt, aes(y = asleep), time_wrap = hours(24))
#' pl
#'
#' # double-plotted actogram:
#' pl <- ggetho(dt,
#'               aes(z = moving),
#'               multiplot = 2,
#'               multiplot_period = hours(24))
#' pl
#' # then use `+ stat_tile_etho()` , or `+ stat_bar_tile_etho()`
#' @seealso
#' * [stat_pop_etho] to show population trend by aggregating individuals over time
#' * [stat_tile_etho] to show variable of interest as colour intensity
#' * [stat_ld_annotations] to show light and dark phases on the plot
#'@references
#' * The relevant [rethomic tutorial section](https://rethomics.github.io/ggetho.html#the-ggetho-function)
#' @export
ggetho <- function(data,
                    mapping,
                    summary_FUN = mean,
                    summary_time_window = mins(30),
                    time_wrap = NULL,
                    time_offset = 0,
                    multiplot = NULL, # 1
                    multiplot_period = hours(24),
                    ...){

  preprocess_result <- ggetho_preprocess(data, mapping, summary_FUN,
                                         summary_time_window,
                                         time_wrap,
                                         time_offset,
                                         multiplot,
                                         multiplot_period)

  processed_dt <- preprocess_result$dt
  mapping <- preprocess_result$mapping
  scale_x_FUN <- preprocess_result$scale_x_fun
  discrete_y <- preprocess_result$discrete_y
  time_offset <- preprocess_result$time_offset

  plot <- ggetho_plot(processed_dt, mapping, scale_x_FUN, discrete_y, time_wrap, time_offset, multiplot, multiplot_period, ...)

  return(plot)
}


auto_x_time_scale <- function(t){
  rng <- range(as.numeric(t))
  diff <- rng[2] - rng[1]
  if(diff > fslbehavr::days(3)){
    return(scale_x_days)
  }
  else if(diff > fslbehavr::hours(3)){
    return(scale_x_hours)
  }
  else{
    return(scale_x_seconds)
  }
}

make_multiplot <- function(data, n, per, summary_time_window){

  # trick to avoid NOTES from R CMD check:
  period = NULL

  data[, period := floor(t/per)]
  t_map <- data.table(t = seq(0, per* n, by=summary_time_window))
  min_per <- data[, min(period)]
  max_per <- data[, max(period)-(n-1)]
  l <- lapply(min_per:max_per, function(x){
                  o <- data[period %between% c(x,x+(n-1))]
                  o[, t := t - per * x ]
                  o <- na.omit(o[t_map, on="t", roll=summary_time_window, rollend=TRUE])
                  o[, period:= x]
                  o
  })

  out <- rbindlist(l)

  out[, period := factor(period, levels = max_per:min_per)]
  setkeyv(out, key(data))
}
