#' Make a ggetho plot
#'
#' @title `ggetho_plot` isolates the plotting functionality in ggetho
#' @details This function receives as input the outut produced by ggetho_preprocess and returns
#' a ggplot2 plot
#' @param mapping A summarised behavioral time series
#' @param mapping A mapping object as defined in ggplot2
#' @param scale_x_FUN A scale function that can set the units of the x axis,
#' usually showing time i.e. seconds, hours, days, etc
#' @param discrete_y Whether the variable mapped to the y axis is discrete or not
#@param time_wrap
#@param time_offset
#'
#' @import ggplot2
#' @export
ggetho_plot <- function(data, mapping,
                        scale_x_FUN = NULL,
                        discrete_y = FALSE,
                        time_wrap = NULL, time_offset = 0, ...) {

  mapping_list <- make_labels(mapping)

  if(!"x" %in% names(mapping_list) & "t" %in% colnames(data))
    mapping_list$x <- "t"
    mapping <- do.call(aes_string, mapping_list)


  out <- ggplot(data, mapping,...)
  # add some vertical margin  to the plot when needed, this is to show
  # LD annotations

  if(discrete_y){
    p <- ggplot_build(out)
    yr <- p$layout$panel_ranges[[1]]$y.range
    ymin <- yr[1]
    ymax <- yr[2]
    mar <- c(ymin - (ymax - ymin) * 0.03, (ymax - ymin) + 0.5 +  (ymax - ymin) * 0.03)
    # TODO Explain this line
    out <- out + geom_blank() + geom_blank(aes(y=y), data.frame(y=mar), inherit.aes = FALSE)
  }

  if(!is.null(time_wrap) & !is.null(scale_x_FUN))
    return( out + scale_x_FUN(limits=c(-time_offset, time_wrap - time_offset)))
  return(out)
}
