library(haven);library(parallel);library(data.table)
setwd("~/ghost_coding_nhis")
files <- list.files("data", pattern="\\.sas7bdat$", full.names=TRUE)
remove_attributes <- function(dt) {
  dt[, lapply(.SD, function(x) {
    attributes(x) <- NULL  # 모든 attribute 제거
    x
  })]
}

for(f in files) {
  nm <- sub("\\.sas7bdat$", "", basename(f))
  nm <- sub("nsc2_", "", nm)
  nm <- sub("_1000$", "", nm)
  dt <- as.data.table(read_sas(f))
  dt_clean <- remove_attributes(dt) 
  assign(nm, dt_clean)
}



heads <- mclapply(files, function(f) {
  nm <- sub("\\.sas7bdat$", "", basename(f))
  nm <- sub("nsc2_", "", nm)
  nm <- sub("_1000$", "", nm)
  c(paste0("=== ", nm, " ==="), capture.output(print(head(read_sas(f)))))
}, mc.cores = 10)
writeLines(unlist(heads), "all_heads.txt")
bnd %>% class()



