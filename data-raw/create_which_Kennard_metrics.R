which_Kennard_metrics <- c(
  "meanMonth",
  "cvMonth",
  "hiLevs",
  "ARIs",
  "hiloAnn",
  "hiloDur",
  "hiloDurMM",
  "hiloCV",
  "meanRate",
  "cvRate"
)

library(devtools)
usethis::use_data(which_Kennard_metrics, overwrite = TRUE, compress = "xz")
