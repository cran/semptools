## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width  =  6,
  fig.height =  6,
  fig.align = "center"
)

## ----setup, echo = FALSE------------------------------------------------------
library(semptools)

## ----conceptual_diagram, echo = FALSE-----------------------------------------
suppressMessages(library(lavaan))
suppressMessages(library(semPlot))
mod_pa <-
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "path",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram",
           nCharNodes = 0, nCharEdges = 0,
           layout = m,
           residuals = FALSE,
           DoNotPlot = TRUE)
p_pa2con <- set_curve(p_pa, list(list(from = "x1", to = "x2", new_curve = -2)))
plot(p_pa2con)

## ----wo_layout----------------------------------------------------------------
library(lavaan)
library(semPlot)
mod_pa <-
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram",
           nCharNodes = 0, nCharEdges = 0)

## ----grid, echo = FALSE-------------------------------------------------------
plot(p_pa2con)
xc <- seq(-1.25, 1.25, length.out = 5)
yc <- seq(1.25, -1.25, length.out = 4)
for (i in yc) {
    segments(xc[1], i, xc[5], i, col = "red", lwd = 4)
  }
for (i in xc) {
    segments(i, yc[1], i, yc[4], col = "red", lwd = 4)
  }
x_os <- (xc[2] - xc[1]) * .5
y_os <- (yc[2] - yc[1]) * .9
for (i in seq_len(length(xc) - 1)) {
    for (j in seq_len(length(yc) - 1)) {
        text(xc[i] + x_os, yc[j] + y_os,
             paste0("(", j, ", ", i, ")"),
             cex = 1.5, col = "red")
      }
  }

## -----------------------------------------------------------------------------
m <- matrix(NA, 3, 4)
m

## -----------------------------------------------------------------------------
m[1, 1] <- "x1"
m[3, 1] <- "x2"
m[2, 2] <- "x3"
m[2, 4] <- "x4"
m

## -----------------------------------------------------------------------------
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram",
           nCharNodes = 0, nCharEdges = 0,
           layout = m)

## -----------------------------------------------------------------------------
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
m

## -----------------------------------------------------------------------------
m2 <- layout_matrix(x1 = c(1, 1),
                    x2 = c(3, 1),
                    x3 = c(2, 2),
                    x4 = c(2, 4))
m2
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram",
           nCharNodes = 0, nCharEdges = 0,
           layout = m2)

## -----------------------------------------------------------------------------
m3 <- layout_matrix(x1 = c(1, 1),
                    x2 = c(3, 1),
                    x3 = c(2, 2),
                    x4 = c(2, 3))
m3
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram",
           nCharNodes = 0, nCharEdges = 0,
           layout = m3)

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
fit <- lavaan::sem(mod, cfa_example)
p <- semPaths(fit, whatLabels="est",
        sizeMan = 5,
        node.width = 1,
        edge.label.cex = .75,
        style = "ram",
        mar = c(5, 5, 5, 5))

## ----echo = FALSE-------------------------------------------------------------
m <- matrix(c("f1",   NA,   NA,
                NA, "f3", "f4",
              "f2",   NA,   NA), byrow = TRUE, 3, 3)
p_sem <- semPaths(fit, what = "mod",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram",
           nCharNodes = 0, nCharEdges = 0,
           layout = m,
           residuals = FALSE,
           structural = TRUE,
           DoNotPlot = TRUE)
p_semcon <- set_curve(p_sem, list(list(from = "f1", to = "f2", new_curve = -2)))
plot(p_semcon)

## ----echo = FALSE-------------------------------------------------------------
plot(p_semcon)
xc <- seq(-1.25, 1.25, length.out = 4)
yc <- seq(1.25, -1.25, length.out = 4)
for (i in yc) {
    segments(xc[1], i, xc[4], i, col = "red", lwd = 4)
  }
for (i in xc) {
    segments(i, yc[1], i, yc[4], col = "red", lwd = 4)
  }
x_os <- (xc[2] - xc[1]) * .5
y_os <- (yc[2] - yc[1]) * .9
for (i in seq_len(length(xc) - 1)) {
    for (j in seq_len(length(yc) - 1)) {
        text(xc[i] + x_os, yc[j] + y_os,
             paste0("(", j, ", ", i, ")"),
             cex = 1.5, col = "red")
      }
  }

## -----------------------------------------------------------------------------
m_sem <- layout_matrix(f1 = c(1, 1),
                    f2 = c(3, 1),
                    f3 = c(2, 2),
                    f4 = c(2, 3))
m_sem

## -----------------------------------------------------------------------------
point_to <- layout_matrix(left = c(1, 1),
                          left = c(3, 1),
                          down = c(2, 2),
                          up = c(2, 3))

## -----------------------------------------------------------------------------
indicator_order  <- c("x04", "x05", "x06", "x07",
                      "x01", "x02", "x03",
                      "x11", "x12", "x13", "x14",
                      "x08", "x09", "x10")
indicator_factor <- c( "f2",  "f2",  "f2",  "f2",
                       "f1",  "f1",  "f1",
                       "f4",  "f4",  "f4",  "f4",
                       "f3",  "f3",  "f3")
indicator_push <- c(f3 = 2.5,
                    f4 = 2.5,
                    f1 = 1.5,
                    f2 = 1.5)
indicator_spread <- c(f1 = 2,
                      f2 = 2,
                      f3 = 2,
                      f4 = 1.75)
loading_position <- c(f2 = .6,
                      f3 = .8,
                      f4 = .8)
p2 <- set_sem_layout(p,
                     indicator_order = indicator_order,
                     indicator_factor = indicator_factor,
                     factor_layout = m_sem,
                     factor_point_to = point_to,
                     indicator_push = indicator_push,
                     indicator_spread = indicator_spread,
                     loading_position = loading_position)
plot(p2)

