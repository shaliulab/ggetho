
#' @import ggplot2
#' @export
ggetho_plot <- function(data, mapping, scale_x_FUN, discrete_y,
                        time_wrap = NULL, time_offset = 0, multiplot = NULL, # 1
                        multiplot_period = hours(24), ...) {

  out <- ggplot(data, mapping,...)
  # add some vertical margin  to the plot when needed, this is to show
  # LD annotations

  if(discrete_y){
    p <- ggplot_build(out)
    yr <- p$layout$panel_ranges[[1]]$y.range
    ymin <- yr[1]
    ymax <- yr[2]
    mar <- c(ymin - (ymax-ymin) *0.03, (ymax-ymin) + 0.5 +  (ymax-ymin) *0.03)
    out <- out + geom_blank() +   geom_blank(aes(y=y), data.frame(y=mar), inherit.aes = FALSE)
  }

  if(!is.null(time_wrap))
    return( out + scale_x_FUN(limits=c(- time_offset, time_wrap - time_offset)))

  plot <- out + scale_x_FUN()
  return(plot)
}
