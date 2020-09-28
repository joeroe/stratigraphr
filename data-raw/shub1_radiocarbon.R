# Prepare "shub1_radiocarbon" dataset

# Raw data from Richter et al. (2017), ESM 2, "Radiocarbon Dates"
shub1_radiocarbon <- readxl::read_xlsx("data-raw/41598_2017_17096_MOESM2_ESM.xlsx")

# Discard unwanted columns and normalise the names of the rest
shub1_radiocarbon <- dplyr::select(shub1_radiocarbon,
  lab_id = `Laboratory Code`,
  phase = `Archaeological Phase`,
  sample = `Sample Context ID`,
  material = `Sample material`,
  age = `C-14 Age ±1σ year BP`
)

# Fill missing phases
shub1_radiocarbon <- tidyr::fill(shub1_radiocarbon, phase)

# Discard non-data rows
shub1_radiocarbon <- tidyr::drop_na(shub1_radiocarbon, lab_id)

# Normalise lab IDs
shub1_radiocarbon <- dplyr::mutate(shub1_radiocarbon,
                                   lab_id = stringr::str_remove(lab_id, "R_Date "))

# Add an outlier column (marked with a "*" next to the lab ID in the original data)
shub1_radiocarbon <- dplyr::mutate(shub1_radiocarbon,
                                   outlier = stringr::str_detect(lab_id, stringr::coll("*")),
                                   lab_id = stringr::str_remove(lab_id, stringr::coll("*")))

# Separate CRA and error in radiocarbon age
shub1_radiocarbon <- tidyr::separate(shub1_radiocarbon, age, c("cra", "error"), " ±")
shub1_radiocarbon$cra <- as.integer(shub1_radiocarbon$cra)
shub1_radiocarbon$error <- as.integer(shub1_radiocarbon$error)

# Add schematic context numbers to match "shub1" dataset
shub1_radiocarbon <- dplyr::mutate(shub1_radiocarbon,
                                   context = dplyr::recode(lab_id,
                                                           `RTD-7951` = 23,
                                                           `Beta-112146` = 24,
                                                           `RTD-7317` = 26,
                                                           `RTD-7318` = 27,
                                                           `RTD-7948` = 24,
                                                           `RTD-7947` = 22,
                                                           `RTD-7313` = 22,
                                                           `RTD-7311` = 22,
                                                           `RTD-7312` = 22,
                                                           `RTD-7314` = 22,
                                                           `RTD-7316` = 22,
                                                           `RTD-7315` = 22,
                                                           `RTK-6818` = 14,
                                                           `RTK-6820` = 14,
                                                           `RTK-6821` = 16,
                                                           `RTK-6822` = 16,
                                                           `RTK-6823` = 16,
                                                           `RTK-6813` = 12,
                                                           `RTK-6816` = 12,
                                                           `RTK-6819` = 5,
                                                           `RTK-6812` = 3,
                                                           `RTK-6817` = 2,
                                                           `RTD-8904` = 1,
                                                           `RTK-6814` = 1,
                                                           `RTD-8902` = 1,
                                                           `RTD-8903` = 1,
                                                           `RTK-6815` = 1))
shub1_radiocarbon <- dplyr::relocate(shub1_radiocarbon, context, .after = lab_id)
shub1_radiocarbon$context <- as.integer(shub1_radiocarbon$context)

# Export
usethis::use_data(shub1_radiocarbon, overwrite = TRUE)
