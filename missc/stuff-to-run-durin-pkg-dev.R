
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

library(devtools)
library(tidyverse)
library(fs)
load_all()

devtools::document()
has_devel()

devtools::check()
