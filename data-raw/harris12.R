# Prepare "harris12" dataset

harris12 <- readr::read_csv("data-raw/harris12.csv", col_types = "cccc")
harris12 <- dplyr::mutate(harris12,
  above = stringr::str_split(above, ","),
  below = stringr::str_split(below, ",")
)

usethis::use_data(harris12, overwrite = TRUE)
