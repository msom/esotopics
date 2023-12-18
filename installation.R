install.packages("cli")
install.packages("corrplot")
install.packages("datawizard")
install.packages("devtools")
install.packages("dplyr")
install.packages("ggrepel")
install.packages("ggplot2")
install.packages("ggpointdensity")
install.packages("hash")
install.packages("keyATM")
install.packages("quanteda")
install.packages("pbapply")
install.packages("philentropy")
install.packages("plyr")
install.packages("simplermarkdown")
install.packages("topicdoc")
install.packages("topicmodels")
install.packages("udpipe")
install.packages("matrixStats")

library(devtools)
install_github("msom/esocorpus", upgrade = "always", force = TRUE)
# might need to restart R after installation

# might need to increase memory limits in ~/.Renviron
# R_MAX_VSIZE=500Gb
