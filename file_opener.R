library(haven)
files <- list.files("ghost_coding_nhis/data", pattern="\\.sas7bdat$", full.names=TRUE)
for(f in files) {
  nm <- sub("\\.sas7bdat$","",basename(f))
  nm <- sub("nsc2_","",nm)
  nm <- sub("_1000$","",nm)
  assign(nm, read_sas(f))
}