#' Generates Nice Plot Breaks and Limits from Data Range
#' @name nice_plot_breaks
#' @importFrom dplyr %>% summarise group_by case_when relocate
#' @importFrom ggplot2 ggplot geom_point geom_line geom_tile ggtitle facet_grid
#'   scale_fill_gradient aes scale_x_continuous scale_y_continuous
#' @description This function uses caret grid training results to generate
#'   performance data.frames, heatmaps, and other plots for comparing the
#'   performance of models across their hyper-parameters and evaluating
#'   interactions between different model hyper-parameters.
#' @param x An object of class train.
#' @param sqrt Boolean value indicating whether assessment metrics should be
#'   adjusted via a square root transformation. Default is FALSE.
#' @return A list of caret training performance data.frames, heatmaps, and
#'   plots.
#' @examples
#' b <- nice_plot_breaks(x)
#' @export

nice_plot_breaks <- function(vec) {
  upp <- max(vec)
  low <- min(vec)
  d_range <- upp - low
  v <- 10^(-3:6)

  max(abs(vec))
  #compare abs(u) to abs(l)
  #max(abs(vec))
  # use just the interval for the larger one??

  indr <- findInterval(d_range, v)

  indu <- findInterval(abs(upp), v)
  new_upp <- upp / (10^(indu - 5))
  round_upp <- ceiling(new_upp / 10) * 10

  indl <- findInterval(abs(low), v)
  new_low <- low / (10^(indl - 5))
  round_low <- floor(new_low / 10) * 10

  # indl <- findInterval(abs(low), v)
  # new_low <- low / (10^(indu - 5))
  # round_low <- floor(new_low / 10) * 10

  new_range <- new_upp - new_low
  round_range <- round_upp - round_low

  if (any(na.omit(c(new_range / round_range,
                    round_low / new_low, new_upp / round_upp)) < 3 / 4)) {
    round_upp <- ceiling(new_upp / 4) * 4
    round_low <- floor(new_low / 4) * 4
  }

  new_upp <- round_upp * (10^(indu - 5))
  new_low <- round_low * (10^(indl - 5))
  new_range <- new_upp - new_low

  div <- case_when(
    round_range %% 5 == 0 ~ 5,
    round_range %% 4 == 0 ~ 4,
    round_range %% 3 == 0 ~ 3,
    .default = 4
  )

  l <- list()
  l$min <- new_low
  l$max <- new_upp
  l$breaks <- seq(new_low, new_upp, by = new_range / div)
  l
}
# n <- nice_plot_breaks(vec = c(-13, 117))
# n

################################################################################
# vec = c(-117, 6)
nice_plot_breaks <- function(vec) {
  vec = c(min(vec), max(vec))
  d_range <- vec[2] - vec[1]
  v <- 10^(-3:6)

  #compare abs(u) to abs(l)
  #max(abs(vec))
  # use just the interval for the larger one??

  indr <- findInterval(d_range, v)

  ind <- findInterval(max(abs(vec)), v)

  i = which.max(abs(vec))

  new_vec <- vec / (10^(ind - 5))

  round_vec <- c(floor(new_vec[which.min(new_vec)] / 10) * 10,
                 ceiling(new_vec[which.max(new_vec)] / 10) * 10)

  new_range <- max(new_vec) - min(new_vec)
  round_range <- max(round_vec) - min(round_vec)

  if (any(na.omit(c(new_range / round_range,
                    new_vec[i]/round_vec[i])) < 3 / 4)) {
    round_vec <- c(floor(new_vec[which.min(new_vec)] / 4) * 4,
                   ceiling(new_vec[which.max(new_vec)] / 4) * 4)
  }

  new_upp <- round_upp * (10^(indu - 5))
  new_low <- round_low * (10^(indl - 5))
  new_range <- new_upp - new_low

  div <- case_when(
    round_range %% 5 == 0 ~ 5,
    round_range %% 4 == 0 ~ 4,
    round_range %% 3 == 0 ~ 3,
    .default = 4
  )

  l <- list()
  l$min <- new_low
  l$max <- new_upp
  l$breaks <- seq(new_low, new_upp, by = new_range / div)
  l
}
# n <- nice_plot_breaks(vec = c(-13, 117))
# n
