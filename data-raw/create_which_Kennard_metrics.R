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
  "cvRate",
  "mn_ann_Range"
)

library(devtools)
usethis::use_data(which_Kennard_metrics, overwrite = TRUE, compress = "xz")
