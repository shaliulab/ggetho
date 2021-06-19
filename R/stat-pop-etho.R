#' Compute and display a population aggregate for a variable of interest
#'
#' This function displays the temporal (time on the x axis) trend of variable of interest,
#' on the y axis as a line with confidence interval as a shaded area.
#'
#' @family layers
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::stat_smooth
#' @param method function used to compute the aggregate and  confidence intervals.
#' It should return (`y`, `ymin` and `ymax`).
#' The default is [ggplot2::mean_se], which computes the mean + or - standard error.
#' [ggplot2::mean_cl_boot] can be used instead to generate bootstrap confidence interval instead.
#' @examples
#' library(behavr)
#' metadata <- data.frame(id = sprintf("toy_experiment | %02d", 1:20),
#'                    age=c(1, 5, 10, 20),
#'                    condition=c("A", "B"))
#' dt <- toy_activity_data(metadata, 3)
#' # We build a plot object
#' pl <-  ggetho(dt, aes(y = asleep))
#' # A standard plot of the whole population:
#' pl + stat_pop_etho()
#' # We can also split by condition, and display the two population on different facets:
#' pl + stat_pop_etho() + facet_grid(condition ~ .)
#'
#' # Instead, we can use different colour for separate conditions:
#' pl <-  ggetho(dt, aes(y = asleep, colour = condition))
#' pl + stat_pop_etho()
#'
#' # Sometimes, we also have numeric condition (e.g. age)
#' pl <-  ggetho(dt, aes(y = asleep, colour = age))
#' pl + stat_pop_etho()
#' # We could want to aggreate several days of data to one circadian day (i.e. time wrapping)
#' # here, we also plot the invert of moving (!moving)
#' pl <-  ggetho(dt, aes(y = !moving), time_wrap = hours(24))
#' pl + stat_pop_etho()
#' @seealso
#' * [ggetho] to generate a plot object
#' * [stat_tile_etho] to show variable of interest as colour intensity
#' * [stat_ld_annotations] to show light and dark phases on the plot
#' * [ggplot2::stat_smooth] to understand how to change the type of confidence interval, line colour and so forth
#' @references
#' * The relevant [rethomic tutorial section](https://rethomics.github.io/ggetho.html#population-plots)
#' @export
stat_pop_etho <- function(mapping = NULL, data = NULL,
                          geom = "smooth", position = "identity",
                          ...,
                          method = mean_se,
                          method.args = list(),
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = pop_etho,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      method.args = method.args,
      se=TRUE,
      ...
    )
  )
}


pop_etho <- ggplot2::ggproto("pop_etho", ggplot2::Stat,
                       compute_group = function(data, scales, method, method.args = list()) {
                         data <- data.table::as.data.table(data)
                         foo <- function(y){
                           all_args <- append(list(y), method.args)
                           do.call(method, all_args)
                         }
                         out <- data[,
                                     foo(y),
                                     keyby="x"]
                         # this is a patch to deal when situations where only one data point
                         # is available for each trace (because each trace is only one individual)
                         # without the patch, the plot is obtained but a warning for each trace is generated
                         # In max(ids, na.rm = TRUE) : no non-missing arguments to max; returning -Inf
                         # Fortunately, the effect of a single individual trace is easy to solve:
                         # ymin and ymax become NA, and just setting them to the same as y patches the problem
                         if(all(is.na(out$ymin))) out$ymin <- out$y
                         if(all(is.na(out$ymax))) out$ymax <- out$y

                         out
                       },
                       required_aes = c("x", "y")
)


#' @export
geom_pop_etho <- function(mapping = NULL, data = NULL,
                        stat = pop_etho, position = "identity",
                        ...,
                        method = ggplot2::mean_se,
                        formula = NULL,
                        se = TRUE,
                        na.rm = FALSE,
                        orientation = NA,
                        show.legend = NA,
                        inherit.aes = TRUE) {

  params <- list(
    na.rm = na.rm,
    orientation = orientation,
    se = se,
    ...
  )
  params$method <- method
  params$formula <- formula

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomSmooth,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}
