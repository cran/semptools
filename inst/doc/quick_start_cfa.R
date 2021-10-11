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
head(round(cfa_example, 3), 3)

## -----------------------------------------------------------------------------
mod <- 
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
  '

## -----------------------------------------------------------------------------
library(lavaan)
fit <- lavaan::cfa(mod, cfa_example)

## -----------------------------------------------------------------------------
library(semPlot)
p <- semPaths(fit, whatLabels="est",
        sizeMan = 3.25,
        node.width = 1,
        edge.label.cex = .75,
        style = "ram",
        mar = c(10, 5, 10, 5))

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
p2 <- set_cfa_layout(p,
                     indicator_order,
                     indicator_factor)
plot(p2)

## -----------------------------------------------------------------------------
p2 <- set_cfa_layout(p, 
                     indicator_order,
                     indicator_factor,
                     fcov_curve = 1.75)
plot(p2)

## -----------------------------------------------------------------------------
p2 <- set_cfa_layout(p,
                     indicator_order,
                     indicator_factor,
                     fcov_curve = 1.75,
                     loading_position = .8)
plot(p2)

## -----------------------------------------------------------------------------
p2 <- set_cfa_layout(p,
                     indicator_order,
                     indicator_factor,
                     fcov_curve = 1.75,
                     loading_position = .8,
                     point_to = "up")
plot(p2)

## ----eval = FALSE-------------------------------------------------------------
#  # If R version >= 4.1.0
#  p2 <- set_cfa_layout(p,
#                       indicator_order,
#                       indicator_factor,
#                       fcov_curve = 1.75,
#                       loading_position = .9,
#                       point_to = "up") |>
#        mark_sig(fit)
#  plot(p2)

## ----echo = FALSE-------------------------------------------------------------
# if ((compareVersion(as.character(getRversion()), "4.1.0")) >= 0) {
#     p2 <- set_cfa_layout(p, indicator_order, indicator_factor, fcov_curve = 1.75,
#                         loading_position = .9, point_to = "up") |>
#                         mark_sig(fit)
#   } else {
    require(magrittr)
    p2 <- set_cfa_layout(p, indicator_order, indicator_factor, fcov_curve = 1.75,
                        loading_position = .9, point_to = "up") %>%
                        mark_sig(fit)
  # }
plot(p2)

