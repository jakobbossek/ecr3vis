library(usethis)

emoas_on_zdt = read.table("data-raw/emoas_on_zdt.csv", header = TRUE, stringsAsFactors = FALSE)
usethis::use_data(emoas_on_zdt, overwrite = TRUE)
