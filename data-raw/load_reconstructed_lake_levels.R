# Load reconstructed lake levels

# Reads in csv with reconstructed lake levels, as provided by Bob Smail.

csls_levels <- read.csv("data-raw/study_lakes_pred_levs_20191022.csv")
usethis::use_data(csls_levels, overwrite = TRUE, compress = "xz")
