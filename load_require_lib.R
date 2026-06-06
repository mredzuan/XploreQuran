# Script to install and load all required libraries for XploreQuran
# Run this script manually in your R session: source("load_require_lib.R")

required_packages <- c(
  "dplyr",
  "RColorBrewer",
  "readr",
  "stringr",
  "tidyr",
  "tidytext",
  "wordcloud",
  "shiny",
  "bslib",
  "plotly",
  "quRan"
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  message("Installing missing packages: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

# Load all packages
message("Loading required packages...")
for (pkg in required_packages) {
  library(pkg, character.only = TRUE)
}
message("All required packages loaded successfully!")
