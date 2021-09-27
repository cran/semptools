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

## -----------------------------------------------------------------------------
library(lavaan)
mod <- 
  'x3 ~  x1 + x2 + cov1 + cov2 + cov3
   x4 ~  x1 + x3 + cov1 + cov2 + cov3
  '
fit <- lavaan::sem(mod, pa_example_3covs)

## -----------------------------------------------------------------------------
library(semPlot)
library(semptools)
m <- layout_matrix(x1 = c(1, 1),
                   x2 = c(3, 1),
                   x3 = c(2, 2),
                   x4 = c(2, 3),
                   cov1 = c(4, 1),
                   cov2 = c(5, 1),
                   cov3 = c(6, 1))
p_pa <- semPaths(fit, whatLabels = "est",
           sizeMan = 10,
           edge.label.cex = .5,
           style = "ram", 
           nCharNodes = 0, nCharEdges = 0,
           layout = m)

## -----------------------------------------------------------------------------
pm_no_covs <- semptools::drop_nodes(
                object = semPlotModel(fit),
                nodes = c("cov1", "cov2", "cov3"))

## -----------------------------------------------------------------------------
pm_no_covs <- semptools::drop_nodes(
                semPlotModel(fit),
                c("cov1", "cov2", "cov3"))

## -----------------------------------------------------------------------------
m_no_covs <- layout_matrix(x1 = c(1, 1),
                           x2 = c(3, 1),
                           x3 = c(2, 2),
                           x4 = c(2, 3))
pa_no_covs <- semPaths(pm_no_covs, whatLabels = "est",
                    sizeMan = 10,
                    edge.label.cex = .5,
                    style = "ram",
                    nCharNodes = 0, nCharEdges = 0,
                    layout = m_no_covs)

## -----------------------------------------------------------------------------
pm_only_xs <- semptools::keep_nodes(
                semPlotModel(fit),
                c("x1", "x2", "x3", "x4"))
pa_only_xs <- semPaths(pm_only_xs, whatLabels = "est",
                    sizeMan = 10,
                    edge.label.cex = .5,
                    style = "ram",
                    nCharNodes = 0, nCharEdges = 0,
                    layout = m_no_covs)

