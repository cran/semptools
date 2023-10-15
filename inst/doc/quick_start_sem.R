## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width  =  8,
  fig.height =  6,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(semptools)
head(round(sem_example, 3), 3)

## -----------------------------------------------------------------------------
mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  f1 + f2
   f4 ~  f1 + f3
  '

## -----------------------------------------------------------------------------
library(lavaan)
fit <- lavaan::sem(mod, cfa_example)

## -----------------------------------------------------------------------------
library(semPlot)
p <- semPaths(fit, whatLabels="est",
        sizeMan = 5,
        node.width = 1,
        edge.label.cex = .75,
        style = "ram",
        mar = c(5, 5, 5, 5))

## -----------------------------------------------------------------------------
indicator_order  <- c("x04", "x05", "x06", "x07",
                      "x01", "x02", "x03",
                      "x11", "x12", "x13", "x14",
                      "x08", "x09", "x10")

## -----------------------------------------------------------------------------
indicator_factor <- c( "f2",  "f2",  "f2",  "f2",
                       "f1",  "f1",  "f1",
                       "f4",  "f4",  "f4",  "f4",
                       "f3",  "f3",  "f3")

## -----------------------------------------------------------------------------
factor_layout <- matrix(c("f1",   NA,   NA,
                           NA, "f3", "f4",
                         "f2",   NA,   NA), byrow = TRUE, 3, 3)

## -----------------------------------------------------------------------------
factor_layout <- layout_matrix(f1 = c(1, 1),
                               f2 = c(3, 1),
                               f3 = c(2, 2),
                               f4 = c(2, 3))
factor_layout

## -----------------------------------------------------------------------------
factor_point_to <- matrix(c("left",     NA,      NA,
                                NA, "down", "down",
                            "left",     NA,      NA), byrow = TRUE, 3, 3)

## -----------------------------------------------------------------------------
factor_point_to <- layout_matrix(left = c(1, 1),
                                 left = c(3, 1),
                                 down = c(2, 2),
                                 down = c(2, 3))
factor_point_to

## -----------------------------------------------------------------------------
p2 <- set_sem_layout(p,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to)
plot(p2)

## -----------------------------------------------------------------------------
indicator_push <- c(f3 = 2,
                    f4 = 1.5,
                    f1 = 1.5,
                    f2 = 1.5)

## -----------------------------------------------------------------------------
p2 <- set_sem_layout(p,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to,
                     indicator_push = indicator_push)
plot(p2)

## -----------------------------------------------------------------------------
indicator_spread <- c(f1 = 2,
                      f2 = 1.5,
                      f4 = 1.5)

## -----------------------------------------------------------------------------
p2 <- set_sem_layout(p,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to,
                     indicator_push = indicator_push,
                     indicator_spread = indicator_spread)
plot(p2)

## -----------------------------------------------------------------------------
loading_position <- c(f2 = .7,
                      f3 = .8,
                      f4 = .8)

## -----------------------------------------------------------------------------
p2 <- set_sem_layout(p,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to,
                     indicator_push = indicator_push,
                     indicator_spread = indicator_spread,
                     loading_position = loading_position)
plot(p2)

## ----eval = FALSE-------------------------------------------------------------
#  # If R version >= 4.1.0
#  p2 <- set_sem_layout(p,
#                      indicator_order = indicator_order,
#                      indicator_factor = indicator_factor,
#                      factor_layout = factor_layout,
#                      factor_point_to = factor_point_to,
#                      indicator_push = indicator_push,
#                      indicator_spread = indicator_spread,
#                      loading_position = loading_position) |>
#                      set_curve(c("f2 ~~ f1" = -1,
#                                  "f4 ~ f1" = 1.5)) |>
#                      mark_sig(fit)
#  plot(p2)

## ----echo = FALSE-------------------------------------------------------------
# if ((compareVersion(as.character(getRversion()), "4.1.0")) >= 0) {
#     p2 <- set_sem_layout(p,
#                         indicator_order = indicator_order,
#                         indicator_factor = indicator_factor,
#                         factor_layout = factor_layout,
#                         factor_point_to = factor_point_to,
#                         indicator_push = indicator_push,
#                         indicator_spread = indicator_spread,
#                         loading_position = loading_position) |>
#                         set_curve(c("f2 ~~ f1" = -1,
#                                     "f4 ~ f1" = 1.5)) |>
#                     mark_sig(fit)
#   } else {
    require(magrittr)
    p2 <- set_sem_layout(p,
                        indicator_order = indicator_order,
                        indicator_factor = indicator_factor,
                        factor_layout = factor_layout,
                        factor_point_to = factor_point_to,
                        indicator_push = indicator_push,
                        indicator_spread = indicator_spread,
                        loading_position = loading_position) %>%
                        set_curve(c("f2 ~~ f1" = -1,
                                    "f4 ~ f1" = 1.5)) %>%
                        mark_sig(fit)
  # }
plot(p2)

