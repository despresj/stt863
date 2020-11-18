
# reading in hw data ------------------------------------------------------


devtools::install_github("bryangoodrich/ALSM")
library(ALSM)


set_names <- function(data){
  colnames(data) <- setNames(nm = c(paste0("x", seq_along(data) - 1)))
  colnames(data)[1] <- "y"
  data
}
df <- set_names(CH06PR05)