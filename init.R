# init.R
#
# Example R code to install packages if not already installed
#
my_packages = c("shiny", "shinydashboard","shinyWidgets","ggplot2","readr","dplyr","tidyr",
                "showtext","plotly","ggthemes","maps","shinyjs","shinycssloaders","timevis",
                "lubridate","DT","tidyverse")
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(my_packages, install_if_missing))