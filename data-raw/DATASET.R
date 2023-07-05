## code to prepare `lichen` dataset goes here

lichen <- EZtune::lichen
lichen <- lichen[, -c(1, 3:8)]

usethis::use_data(lichen, overwrite = TRUE)

## code to prepare `Boston` dataset goes here

Boston <- MASS::Boston
usethis::use_data(Boston, overwrite = TRUE)
