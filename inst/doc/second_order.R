## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width  =  8,
  fig.height =  6,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(semptools)
head(round(sem_2nd_order_example, 3), 3)

## -----------------------------------------------------------------------------
mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f5 =~ x15 + x16 + x17 + x18
   f6 =~ x19 + x20 + x21
   f21 =~ 1*f1 + f3 + f4
   f22 =~ 1*f2 + f5 + f6
   f22 ~ f21
  '

## -----------------------------------------------------------------------------
library(lavaan)
fit <- lavaan::sem(mod, sem_2nd_order_example)

## -----------------------------------------------------------------------------
library(semPlot)
p <- semPaths(fit, whatLabels = "est",
              sizeMan = 5,
              nCharNodes = 0, nCharEdges = 0,
              edge.width = 0.8, node.width = 0.7,
              edge.label.cex = 0.6,
              style = "ram",
              mar = c(5, 5, 5, 5))

## -----------------------------------------------------------------------------
indicator_order  <- c("x01", "x03", "x02",
                      "x05", "x06", "x04", "x07",
                      "x08", "x09", "x10",
                      "x12", "x11", "x13", "x14",
                      "x16", "x15", "x18", "x17",
                      "x20", "x19", "x21",
                      "f1", "f3", "f4",
                      "f5", "f6", "f2")
indicator_factor <- c("f1", "f1", "f1",
                      "f2", "f2", "f2", "f2",
                      "f3", "f3", "f3",
                      "f4", "f4", "f4", "f4",
                      "f5", "f5", "f5", "f5",
                      "f6", "f6", "f6",
                      "f21", "f21", "f21",
                      "f22", "f22", "f22")
factor_layout <- matrix(c(  NA, "f21",   NA, NA,  NA, "f22",   NA,
                          "f1",  "f4", "f3", NA, "f2",  "f6", "f5"),
                          byrow = TRUE, 2, 7)
factor_layout <- matrix(c("f1",    NA,    NA, "f2",
                          "f4", "f21", "f22", "f6",
                          "f3",    NA,    NA, "f5"),
                          byrow = TRUE, 3, 4)
factor_point_to <- matrix(c("left",     NA,      NA, "right",
                            "left", "left", "right", "right",
                            "left",     NA,      NA, "right"),
                            byrow = TRUE, 3, 4)
indicator_spread <- c(f4 = 1.25,
                      f2 = 1.25,
                      f5 = 1.25)
p2 <- set_sem_layout(p,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = factor_layout,
                     factor_point_to = factor_point_to,
                     indicator_spread = indicator_spread)

## -----------------------------------------------------------------------------
plot(p2)

## -----------------------------------------------------------------------------
my_rotate_resid_list <- c(f4  =  45,
                          f21 =   0,
                          f22 =   0,
                          f6  = -45)
p3 <- rotate_resid(p2, my_rotate_resid_list)
plot(p3)

