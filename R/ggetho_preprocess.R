#' Aggregate behavioral time series of binary variables into numerical summaries
#'
#' Compute a summary statistic described by `summary_FUN` over
#' windows of duration `summary_time_window` seconds. If `summary_FUN` is the mean,
#' the computation is equivalent is equivalent
#' to calculating the fraction of time the animal spent on the binary's variable positive
#' during the window
#'
#' @param data An scored behavioral dataset as returned by an annotating function
#' @param summary_FUN A function describing a summary statistic of a binary variable
#' @param summary_time_window Duration of the windows upon which the statistic should be applied, in seconds
#' @param time_wrap For datasets encompassing more than 1 full circadian cycle (24 hours),
#' whether to average data belonging to the same ZT point or not
#' @param time_offset TODO
#' @param multiplot TODO
#' @param multiplot_period TODO
#'
#' @importFrom behavr bin_apply_all
#' @export
ggetho_preprocess <- function(data, mapping,
                              summary_FUN = mean,
                              summary_time_window = mins(30),
                              time_wrap = NULL,
                              time_offset = 0,
                              multiplot = NULL, # 1
                              multiplot_period = hours(24)) {


  # trick to avoid NOTES from R CMD check:
  x_off = x_name = y =  . = NULL

  ## Handle the time_offset and time_wrap args
  ## In case time_offset is greater than the time_wrap
  ## it has to be set then to its modulus
  if(time_offset != 0 & is.null(time_wrap))
    warning("Time offset only relevant when using time_wrap.
             Ignoring argument")
  else
    time_offset <- time_offset %% time_wrap

  #todo check argument types!!
  if(!is.null(multiplot)){
    if(!is.null(time_wrap))
      stop("Cannot use time wrapping on a multiplot!") #todo
    if(!multiplot %in% 2:10000)
      stop("multiplot must be an integer >1, typically 2, for double plotting")
  }

  ## Handle the possible combination of aesthetics variables
  ## Get a character vector containing the mappings so as to
  ## implement the logic that fine tunes the plot
  ## For example, the x axis is set as a function of what var
  ## is mapped to it. ggetho sets the scale accordingly to hours, days, etc
  #mapping_list <- as.list(as.character(mapping))
  mapping_list <- make_labels(mapping)
  aes_names <- names(mapping_list)
  has_colour = "colour" %in% aes_names
  has_fill = "fill" %in% aes_names

  # if has only colour Xor fill defined
  if( xor(has_fill, has_colour)){
    col = c(mapping_list$fill, mapping_list$colour)[[1]]
    mapping_list$fill <- col
    mapping_list$colour <- col
  }


  # by default, t is the dimension mapped to X
  if(!"x" %in% aes_names)
    mapping_list$x = "t"

  # by default y is not discrete and is the variable of interest
  # (unless z is given), in which case y is discrete
  # and z becomes the variable of interest
  # You cannot provide both
  x_name <- mapping_list$x
  discrete_y <- FALSE
  if("z" %in% aes_names){
    var_of_interest = mapping_list$z
    discrete_y <- TRUE
  }
  else if("y" %in% aes_names)
    var_of_interest = mapping_list$y
  else
    stop("Either `y` or `z` should be provided as variable of interest")

  ## Apply the summarizing function to the data using a window size
  ## of summary_time_window (default 30 mins)
  ## if time_wrap, then datapoints lying in the same fraction of the 24h
  ## are averaged, which results in a plot showing the average cycle
  ## The summarizing function is by default mean because it computes
  ## the fraction of TRUES (asleep) in the window
  data_binned <- behavr::bin_apply_all(data,
                                  var_of_interest,
                                  x = x_name,
                                  x_bin_length = summary_time_window,
                                  wrap_x_by = time_wrap,
                                  FUN = summary_FUN)

  # Join data and metadata together so ggplot2 has access to everything
  data_rejoined <- rejoin(data_binned)
  # when no `y` is provided, the default is to have a
  # discrete/factor axis with individuals as rows


  if(is.null(multiplot)){
    if(!"y" %in% aes_names){
      # todo check those columns exist
      mapping_list$y = "id"
    }
  }
  else{
    if("y" %in% aes_names){
      stop("Cannot use y AND multiplot.
            When multiploting, the y axis is used for consecutive periods,
            the variable of interest should be on the z axis")
    }
    data_rejoined <- make_multiplot(data_rejoined, multiplot, multiplot_period, summary_time_window)
    mapping_list$y = "period"
    discrete_y <- TRUE
  }

  if(!is.null(time_wrap)){
    data_rejoined[, x_off := eval(parse(text = x_name)) ]
    data_rejoined[, x_off := ((x_off + time_offset) %% time_wrap ) - time_offset]
    data_rejoined[, x_name] <- data_rejoined[, x_off]
    data_rejoined[, x_off := NULL]
  }

  # Smart-select the right time scale for the X axis
  scale_x_FUN <- auto_x_time_scale(data_rejoined[[mapping_list$x]])
  mapping_list <- lapply(mapping_list,
                         function(x){
                           if(x %in% colnames(data_rejoined))
                             paste0("`", x, "`")
                           else
                             x
                         })

  mapping = do.call(aes_string, mapping_list)

  return(list(
    data = data_rejoined, mapping = mapping,
    scale_x_FUN = scale_x_FUN, discrete_y = discrete_y, time_offset = time_offset
  ))
}
