context("ld annotation")

test_that("annotations work", {
  t <- mins(0: 60 * 24 * 10)
  dt <- behavr(data.table(id=1, t=t, x=runif(length(t)), key="id"),
               data.table(id=1,key="id"))
  dt
  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations()

  plh <- pl + scale_x_hours()
  pld <- pl + scale_x_days()
  plc <- pl + scale_x_continuous()

  testthat::expect_equal(ggplot_build(pld)$data[[1]], ggplot_build(plh)$data[[1]])
  testthat::expect_equal(ggplot_build(plc)$data[[1]], ggplot_build(plh)$data[[1]])
  pld


  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations(colour=NA)
  pl
})


test_that("annotations work with unbound limits", {
  t <- mins(0:60 * 24 * 10)
  dt <- behavr(data.table(id=1, t=t, x=runif(length(t)), key="id"),
               data.table(id=1,key="id"))
  dt
  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations(x_limits = c(days(2.5), NA)) + geom_point()
  pl

  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations(x_limits = c(NA, days(3.4))) + geom_point()
  pl

  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations(x_limits = c(days(1), days(3.4))) + geom_point()
  pl


  pl <- ggetho(dt, aes(t,x)) + stat_ld_annotations(x_limits = c(days(10), days(3.4))) + geom_point()
  testthat::expect_error(print(pl), "limits are not in order")
})

test_that("annotations work with experiment starting in D phase", {
  t <- mins(0:60 * 24 * 10) + hours(13)
  dt <- behavr(data.table(id=1, t=t, x=runif(length(t)), key="id"),
               data.table(id=1,key="id"))


  dphase <- ggetho(dt, aes(t,x)) +
    stat_ld_annotations() +
    geom_point()


  ignore_first_night <- ggetho(dt, aes(t,x)) +
    stat_ld_annotations(x_limits = c(days(1), NA)) +
    geom_point()


  dphase_data <- ggplot_build(dphase)$data[[1]]
  igfn_data <- ggplot_build(ignore_first_night)$data[[1]]
  print(nrow(dphase_data))
  print(nrow(igfn_data))

  expect_false(isTRUE(all.equal(dphase_data, igfn_data)))

  igfn_data2 <- igfn_data
  igfn_data2[nrow(igfn_data2) + 1, ] <-  list(
    xmin = hours(13), xmax = hours(24),
     ld = "D", PANEL = 1, group = -1,
     colour = "black", size = 0.5,
     linetype = 1, alpha = NA, fill = "black"
    )
  igfn_data2 <- dplyr::arrange(igfn_data2, xmin)
  expect_true(all.equal(dphase_data, igfn_data2))

})




#
# libary(fslggetho)
# metadata <- data.table(id=sprintf("toy_experiment|%02d" , 1:40), region_id=1:40,
#                        condition=c("A","B"),
#                        sex=c("M","M", "F", "F"))
# head(metadata)
#
# dt <- toy_activity_data(metadata, seed=107)
#
# pl <- ggetho(dt, aes(x=t, y=moving)) +
#   stat_ld_annotations(phase = hours(-16)) +
#   stat_pop_etho()
# pl
#
# pl <- ggetho(dt, aes(x=t, y=moving)) +
#   stat_ld_annotations(phase = hours(0)) +
#   stat_pop_etho()
# pl
#
#
# pl <- ggetho(dt, aes(x=t, y=moving)) +
#   stat_ld_annotations(phase = hours(-1)) +
#   stat_pop_etho()
# pl
#
#
# pl <- ggetho(dt, aes(x=t, y=moving)) +
#   stat_ld_annotations(phase = hours(+1)) +
#   stat_pop_etho()
# pl
#
#
# pl <- ggetho(dt, aes(x=t, y=moving)) +
#   stat_ld_annotations(l_duration = hours(16)) +
#   stat_pop_etho()
# pl
#
