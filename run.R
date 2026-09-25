# Open this file in RStudio, set the working directory to this file's folder,
# then click Source. The first run installs missing dependencies from CRAN.
needed <- c("shiny", "bslib", "ggplot2", "dplyr", "readr", "readxl", "zip")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
if (!file.exists("inst/app/app.R")) stop("Set your working directory to the mmbslearn folder containing this run.R file.")
shiny::runApp("inst/app", launch.browser = TRUE)
