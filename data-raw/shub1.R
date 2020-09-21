# Prepare "shub1" dataset

shub1 <- readr::read_csv("data-raw/shub1.csv", col_types = "icccicc")
shub1 <- dplyr::mutate(
  shub1,
  above = stringr::str_split(above, ","),
  below = stringr::str_split(below, ",")
)

usethis::use_data(shub1, overwrite = TRUE)
