#--------------------------------------------------------------------------------------------------#
#----- Provision -----#
#---------------------#

# Provide the packages that hipercow is required to install for the simulations:
install.packages("devtools")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("stringr")
install.packages("tictoc")
install.packages("EnvStats")
install.packages("dqrng")

# Install a specific release of the individual package:
remotes::install_github('mrc-ide/individual@v0.1.19')

# Install the version of helios used for the paper:
remotes::install_github('petal-code/helios@helios-v1.0')
