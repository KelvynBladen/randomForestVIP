
#' Mtry Tune via VIPs
#' @name pdp_compare
#' @importFrom dplyr %>% arrange desc filter select
#'   summarise group_by left_join case_when
#' @importFrom ggplot2 ggplot geom_point geom_line xlab theme theme_bw
#'   scale_y_continuous
#' @importFrom gridExtra grid.arrange
#' @importFrom stats model.frame getCall
#' @importFrom pdp partial
#' @importFrom tidyr all_of
#' @description A list of partial dependence plots, and pdp importance values for
#'   assessing true affect of predictors on response.
#' @param x An object of class randomForest.
#' @param vars Optional vector argument for reducing the number of
#'   variables to consider and compare. Elements should be characters that
#'   match column names from the data used to generate the model x.
#' @return A list of partial dependence plots, and pdp importance values for
#'   assessing true affect of predictors on response.
#' @examples
#' mtcars.rf <- randomForest(formula = mpg ~ ., data = mtcars)
#' car_pd <- pdp_compare(x = mtcars.rf)
#' car_pd$full
#' car_pd$imp
#' grid.arrange(car_pd$wt, car_pd$disp, car_pd$hp, car_pd$cyl, nrow = 2)
#' @export

pdp_compare <- function(x = Lo.rf, vars,
                        trim = 0.1, which.class = 2L, prob = T, ...) {

  data <- eval(getCall(x)$data)
  model_frame <- model.frame(getCall(x)$formula, data = data)[-1]

  if(!missing(vars)){
    model_frame <- model_frame %>% select(all_of(vars))
  }

  vvec <- colnames(model_frame)

  pd_num <- NULL
  pd_fac <- NULL
  for (i in vvec) {
    tmp <- pdp::partial(x, pred.var = i, which.class = which.class,
                   prob = prob)#, ...)
    names(tmp) <- c("x", "y")

    if(inherits(tmp$x, "numeric")){
      pd_num <- rbind(pd_num,  cbind(tmp, var = i))
    } else {
      pd_fac <- rbind(pd_fac,  cbind(tmp, var = i))
    }
  }

  pd <- rbind(pd_num[-1], pd_fac[-1])

  imp <- pd %>% group_by(var) %>%  arrange(y) %>%
    summarise(range = max(y) - min(y),
              trim_range = y[length(y) - floor(length(y)*trim)] -
                y[floor(length(y)*trim)+1],
              sd = sd(y),
              mad = stats::mad(y, center = mean(y))) %>%
    arrange(desc(trim_range))

  # im <- as.data.frame(importance(x, scale = F))
  # im$var <- rownames(im)
  # im <- im[(length(im)-2):length(im)]
  # imp1 <- left_join(imp, im, by = c("var" = "var"))

  vvec <- unique(imp$var)

  # new
  u <- max(pd[1])
  l <- min(pd[1])
  rr <- u - l
  v <- 10^(-3:6)

  indu <- findInterval(abs(u), v)
  newu <- u / (10^(indu - 6))
  ru <- ceiling(newu/10)*10

  indl <- findInterval(abs(l), v)
  newl <- l / (10^(indl - 6))
  rl <- floor(newl/10)*10

  nu <- ru * (10^(indu - 6))
  nl <- rl * (10^(indl - 6))
  nrr <- nu - nl

  if(rr/nrr < 3/4 | nl/l < 3/4 | u/nu < 3/4){
    ru <- ceiling(newu/4)*4
    rl <- floor(newl/4)*4
    nu <- ru * (10^(indl - 6))
    nl <- rl * (10^(indl - 6))
    nrr <- nu - nl
  }

  div <- case_when(
    (nrr / 5) %% 5 == 0 ~ 5,
    (nrr / 5) %% 4 == 0 ~ 4,
    (nrr / 5) %% 3 == 0 ~ 3,
    .default = 4
  )

  gl <- list()

  if(length(pd_num) > 1){
    pd_num <- left_join(pd_num, imp, by = "var") %>%
      arrange(desc(range)) %>% select(-range, -trim_range, -sd, -mad)

    nvec <- unique(pd_num$var)

    pd_num <- arrange(transform(pd_num, var=factor(var,levels=nvec)),var)

    ng <- ggplot(pd_num, aes(x, y)) +
      geom_line() +
      facet_wrap(~ var, scales = "free_x") +
      theme_bw() +
      scale_y_continuous(limits = c(nl, nu),
                         breaks = seq(nl, nu, by = nrr / div))

    gl$full_num <- ng
  }

  if(length(pd_fac) > 1){
    pd_fac <- left_join(pd_fac, imp, by = "var") %>%
      arrange(desc(range)) %>% select(-range, -trim_range, -sd, -mad)

    fvec <- unique(pd_fac$var)

    pd_fac <- arrange(transform(pd_fac, var=factor(var,levels=fvec)),var)

    fg <- ggplot(pd_fac, aes(x, y)) +
      geom_point() +
      facet_wrap(~ var, scales = "free_x") +
      theme_bw() +
      scale_y_continuous(limits = c(nl, nu),
                         breaks = seq(nl, nu, by = nrr / div))

    gl$full_fac <- fg
  }

  gl$imp <- imp

  j = length(gl) + 1
  for(i in vvec){
    if(i %in% nvec){
      v <- pd_num %>% filter(var == i)
      g <- v %>%
        ggplot(aes(x = x, y = y)) +
        geom_line() +
        #geom_smooth(method = "loess", formula = 'y ~ x') +
        xlab(i) +
        scale_y_continuous(limits = c(nl, nu),
                           breaks = seq(nl, nu, by = nrr / div)) +
        guides(size = "none") +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 15),
              plot.title = element_text(size = 14, face = "bold"))
    }

    if(i %in% fvec){
      v <- pd_fac %>% filter(var == i)
      g <- v %>%
        ggplot(aes(x = x, y = y)) +
        geom_point() +
        #geom_smooth(method = "loess", formula = 'y ~ x') +
        xlab(i) +
        scale_y_continuous(limits = c(nl, nu),
                           breaks = seq(nl, nu, by = nrr / div)) +
        guides(size = "none") +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 15),
              plot.title = element_text(size = 14, face = "bold"))
    }

    gl$j <- g
    names(gl)[j] <- i
    j = j + 1
  }

  gl
}

#######################

mtcars.rf <- randomForest(formula = mpg ~ ., data = mtcars)
# p <- partial(mtcars.rf, pred.var = c("drat"))
# plotPartial(p)
car_pd <- pdp_compare(x = mtcars.rf)
car_pd$full
car_pd$drat
car_pd$cyl
car_pd$imp
grid.arrange(car_pd$wt, car_pd$disp, car_pd$hp, car_pd$cyl, nrow = 2)

house.rf <- randomForest(formula = medv ~ ., data = Boston,
                         importance = T)
house_pd <- pdp_compare(x = house.rf, trim = 0.1)
varImpPlot(house.rf)
house_pd$imp
house_pd$full_num

cor(house_pd$imp[-1])
plot(house_pd$imp[-1])

set.seed(123)
library(randomForest)
house.rf1 <- randomForest(formula = medv ~ ., data = MASS::Boston,
                          importance = T)
varImpPlot(house.rf1)
house_pd1 <- pdp_compare(x = house.rf1, trim = 0.1)
house_pd1$imp
house_pd1$full_num


cor(house_pd1$imp[-1])
plot(house_pd1$imp[-1])

varImpPlot(house.rf1)
library(pdp)
library(randomForest)
######################

library(EZtune)
li = EZtune::lichen
?EZtune::lichen
li = li[, -c(1, 3:8)]
li$StandAgeClass <- as.factor(li$StandAgeClass)
li$ReserveStatus <- as.factor(li$ReserveStatus)

#li = li %>% select(-StandAgeClass, -ReserveStatus)
library(tidyverse)
library(randomForest)
library(pdp)

set.seed(1234)
Lo.rf <- randomForest(formula = factor(LobaOreg) ~ ., data = li,
                      mtry = 5, importance = T)
Lo_pd <- pdp_compare(x = Lo.rf, which.class = 2L, prob = T, trim = 0.1)
f <- Lo_pd$imp
f
Lo_pd$full_num
Lo_pd$full_fac
Lo_pd$ACONIF
Lo_pd$StandAgeClass
vip = Lo_pd$imp
vip
pairs(vip[-1])
cor(vip[-1])
plot(vip$range, vip$trim_range)
plot(vip$sd, vip$trim_range)
order(desc(vip$trim_range))
varImpPlot(Lo.rf, scale = F)
vi = as.data.frame(importance(Lo.rf, scale = F))
vi = dplyr::arrange(vi, rev(MeanDecreaseGini))
vi
vi
plot(vi$MeanDecreaseAccuracy, vip$range)
plot(vi$MeanDecreaseGini, vip$range)
plot(vi$MeanDecreaseAccuracy, vi$MeanDecreaseGini)

grid.arrange(Lo_pd$ACONIF, Lo_pd$RelHumidDiff, Lo_pd$StandAgeClass,
             Lo_pd$MinTempAve, Lo_pd$ReserveStatus, Lo_pd$Elevation, nrow = 2)
g <- Lo_pd$RelHumidDiff
g + geom_smooth()

###############################

iris.rf <- randomForest(formula = factor(Species) ~ ., data = iris)
iris_pd <- pdp_compare(x = iris.rf)
iris_pd$full
iris_pd$imp
grid.arrange(iris_pd$Petal.Length, iris_pd$Petal.Width,
             iris_pd$Sepal.Length, iris_pd$Sepal.Width, nrow = 2)

p = partial(house.rf, pred.var = "rm", which.class = 1L, prob = T,
            plot.engine = "ggplot2")
plotPartial(p)

#############################

d <- matrix(rnorm(4000), ncol = 4, nrow = 1000)
y = d %*% 3:0 + rnorm(1000, sd = 0.1)
d = cbind(d, y)
d <- ifelse(d > 0, 1, 0)
d <- as.data.frame(d)
colnames(d)[5] <- "y"

d$V1 <- factor(d$V1)
d$V2 <- factor(d$V2)
d$V3 <- factor(d$V3)
d$V4 <- factor(d$V4)

d.rf <- randomForest(formula = factor(y) ~ ., data = d)
d.rf$confusion
d.rf$importance
d_pd <- pdp_compare(x = d.rf, prob = T)
d_pd$full_fac
d_pd$imp
