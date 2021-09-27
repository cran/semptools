## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width  =  6,
  fig.height =  6,
  fig.align = "center"
)

## ----setup, echo = FALSE------------------------------------------------------
library(semptools)

## ----mark_sig01---------------------------------------------------------------
library(lavaan)
mod_pa <- 
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)
parameterEstimates(fit_pa)

## -----------------------------------------------------------------------------
library(semPlot)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram", 
           nCharNodes = 0, nCharEdges = 0,
           layout = m)

## -----------------------------------------------------------------------------
library(semptools)
p_pa2 <- mark_sig(p_pa, fit_pa)
plot(p_pa2)

## -----------------------------------------------------------------------------
p_pa3 <- mark_sig(p_pa, fit_pa, alpha = c("(n.s.)" = 1.00, "*" = .01))
plot(p_pa3)

## -----------------------------------------------------------------------------
library(lavaan)
mod_pa <- 
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit_pa <- lavaan::sem(mod_pa, pa_example)
parameterEstimates(fit_pa)

## -----------------------------------------------------------------------------
library(semPlot)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram", 
           nCharNodes = 0, nCharEdges = 0,
           layout = m)

## -----------------------------------------------------------------------------
library(semptools)
p_pa2 <- mark_se(p_pa, fit_pa)
plot(p_pa2)

## -----------------------------------------------------------------------------
p_pa2 <- mark_se(p_pa, fit_pa, sep = "\n")
plot(p_pa2)

## -----------------------------------------------------------------------------
library(lavaan)
mod_pa <- 
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)

## -----------------------------------------------------------------------------
library(semPlot)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram", 
           nCharNodes = 0, nCharEdges = 0,
           layout = m)

## -----------------------------------------------------------------------------
library(semptools)
my_rotate_resid_list <- c(x3 =  45,
                          x4 = -45,
                          x2 = -90)
p_pa3 <- rotate_resid(p_pa, my_rotate_resid_list)
plot(p_pa3)

## -----------------------------------------------------------------------------
library(lavaan)
mod_pa <- 
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)

## -----------------------------------------------------------------------------
library(semPlot)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram", 
           nCharNodes = 0, nCharEdges = 0,
           layout = m)

## -----------------------------------------------------------------------------
my_curve_list <- c("x2 ~~ x1" = -3,
                   "x4  ~ x1" =  2)
p_pa3 <- set_curve(p_pa, my_curve_list)
plot(p_pa3)

## -----------------------------------------------------------------------------
library(lavaan)
mod_pa <- 
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)

## -----------------------------------------------------------------------------
library(semPlot)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram", 
           nCharNodes = 0, nCharEdges = 0,
           layout = m)

## -----------------------------------------------------------------------------
library(semptools)
my_position_list <- c("x3 ~ x1" = .25,
                      "x3 ~ x2" = .25,
                      "x4 ~ x1" = .75)
p_pa3 <- set_edge_label_position(p_pa, my_position_list)
plot(p_pa3)

## -----------------------------------------------------------------------------
library(lavaan)
library(semPlot)
library(semptools)
mod_pa <- 
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram", 
           nCharNodes = 0, nCharEdges = 0,
           layout = m)
p_pa2 <- mark_sig(p_pa, fit_pa, alpha = c("(n.s.)" = 1.00, "*" = .01))
plot(p_pa2)

## -----------------------------------------------------------------------------
p_pa3 <- change_node_label(p_pa2,
                           c(x1 = "Attitude",
                             x2 = "SbjNorm",
                             x3 = "Intention",
                             x4 = "Behavior"),
                           label.cex = 1.1)
plot(p_pa3)

## -----------------------------------------------------------------------------
library(lavaan)
mod_pa <- 
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod_pa, pa_example)

## -----------------------------------------------------------------------------
library(semPlot)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = 1.15,
           style = "ram", 
           nCharNodes = 0, nCharEdges = 0,
           layout = m)

## ----eval = FALSE-------------------------------------------------------------
#  my_position_list <- c("x4 ~ x1" = .75)
#  my_curve_list <- c("x2 ~ x1" = -2)
#  my_rotate_resid_list <- c(x1 = 0, x2 = 180, x3 = 140, x4 = 140)
#  my_position_list <- c("x4 ~ x1" = .65)
#  # If R version 4.1.0 or above
#  p_pa3 <- p_pa |> set_curve(my_curve_list) |>
#                    rotate_resid(my_rotate_resid_list) |>
#                    mark_sig(fit_pa) |>
#                    mark_se(fit_pa, sep = "\n") |>
#                    set_edge_label_position(my_position_list)
#  plot(p_pa3)

## ----echo = FALSE-------------------------------------------------------------
my_position_list <- c("x4 ~ x1" = .75)
my_curve_list <- c("x2 ~ x1" = -2)
my_rotate_resid_list <- c(x1 = 0, x2 = 180, x3 = 140, x4 = 140)
my_position_list <- c("x4 ~ x1" = .65)
if ((compareVersion(as.character(getRversion()), "4.1.0")) >= 0) {
    p_pa3 <- p_pa |> set_curve(my_curve_list) |>
                      rotate_resid(my_rotate_resid_list) |>
                      mark_sig(fit_pa) |>
                      mark_se(fit_pa, sep = "\n") |>
                      set_edge_label_position(my_position_list)
  } else {
    require(magrittr)
    p_pa3 <- p_pa %>% set_curve(my_curve_list) %>%
                      rotate_resid(my_rotate_resid_list) %>%
                      mark_sig(fit_pa) %>%
                      mark_se(fit_pa, sep = "\n") %>%
                      set_edge_label_position(my_position_list)
  }
plot(p_pa3)

