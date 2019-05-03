#' const
offset <- 0.1
h_length <- 0.04 # half lenght of whisker top/bottom bar

#' this function draws a stripplot for illustrating the t or Wilcoxon paired test.
#' @param (x, y) paired data
#' @param labelx label on the first dataset
#' @param labely label on the second dataset
#' @param greater categorical/numeric, shows which variable is expected to be greater -- if it is zero,
#' @param color1 the color of the less interesting case
#' @param color2 the color of the more interesting case
#' @export
# then all segments have the same color, if 1 then for x>y the segments are black,
# otherwise they are grey, if 2 then for y>x the segments are black, otherwise they are grey, if 2 then for
pasek_plot <- function(x, y, labelx="", labely="", ylabel = "", greater = 0,
                       color1 = ifelse(greater, "grey", "black"),
                       color2 = "black", side = "both"){

  if (! (greater %in% c(0, 1, 2))) stop("The values of the greater variable must be 0, 1 or 2")

  plot(0, type = "n", xlim = c(0.8,2.2), ylim = c(min(c(x, y)), max(c(x, y))), axes = F, xlab = "", ylab = ylabel)
  indeksy <- switch(greater + 1,
                    NULL,
                    which(x>y),
                    which(x<y))

  segments(rep(1, length(x)), x, rep(2, length(x)), y, col = color1)
  if (greater) {
    segments(rep(1, length(x[indeksy])), x[indeksy], rep(2, length(x[indeksy])), y[indeksy], col = color2)
  }
  axis(1, at = c(1,2), labels = c(labelx, labely))
  axis(1,labels = FALSE, tck = 0.0)
  axis(2)

  add_liness(x, y)
  add_sds(x, "left")
  add_sds(y, "right")
}

#' function adding bars to the strip-plot
#' @param (x, y) paired data
add_liness <- function(x, y, side = "both") {
  # mean lines
  mean_x <- mean(x)
  sd_x <- sd(x)
  mean_y <- mean(y)
  sd_y <- sd(y)
  points(1, mean_x, pch = 19, cex = 1)
  points(2, mean_y, pch = 19, cex = 1)
  segments(1, mean_x, 2, mean_y, lwd = 5)
}

#' function to add the whiskers left and right
#' @param x data_vector
#' @param side "left" or "right"
add_sds <- function(x, side) {
  mean_x <- mean(x)
  sd_x <- sd(x)
  x_position <- ifelse(side == "left", 1, 2)
  function_to_use <- ifelse(side == "right", `+`, `-`)
  # the stem of the whisker
  segments(function_to_use(x_position, offset), w_top(mean_x, sd_x),
           function_to_use(x_position, offset), w_bottom(mean_x, sd_x), lwd = 1)
  # top bar of the whisker
  segments(function_to_use(x_position, offset) - h_length, w_top(mean_x, sd_x),
           function_to_use(x_position, offset) + hlength, w_top(mean_x, sd_x))
  # bottom bar of the whisker
  segments(function_to_use(x_position, offset) - h_length, w_bottom(mean_x, sd_x),
           function_to_use(x_position, offset) + hlength, w_bottom(mean_x, sd_x))
}

#' function to calculate the y-coordinate of the top and bootom of the whiskers
#' @param mean_v mean of the v vector
#' @param sd_v sd of the v vector
#' @param topbottom whether we want top or bottom
whistker_top_bottom <- function(mean_v, sd_v, topbottom) {
  function_to_use <- ifelse(topbottom == "top", `+`, `-`)
  function_to_use(mean_v, sd_v)
}

#' helper function to get the bottom y-coordinate
#' @inheritParams whiskers_top_bottom
w_top <- function(mean_v, sd_v, topbottom = "top") {
  whistker_top_bottom(mean_v, sd_v, topbottom)
}

#' helper function to get the bottom y-coordinate
#' @inheritParams whiskers_top_bottom
w_bottom <- function(mean_v, sd_v, topbottom = "bottom") {
  whistker_top_bottom(mean_v, sd_v, topbottom)
}

set.seed(777)
x <- runif(10)
y <- rnorm(10)
pasek_plot(x, y, greater =2 , labelx = "x", labely = "y")
