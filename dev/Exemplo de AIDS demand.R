#' Author: Bruno Tebaldi
#' 
#' Data: 2021-10-30
#' 
#' Script de analise de elasticidades. Modelo AIDS 


# Setup -------------------------------------------------------------------
rm(list =ls())

library(micEconAids)
library(dplyr)

data("Blanciforti86")

Blanciforti86 <- Blanciforti86[ 1:32, ]

priceNames <- c("pFood1", "pFood2", "pFood3", "pFood4" )
shareNames <- c( "wFood1", "wFood2", "wFood3", "wFood4" )

laaidsResult <- aidsEst(priceNames, shareNames, "xFood", data = Blanciforti86,
                        priceIndex = "S" )
print(laaidsResult)

Blanciforti86 %>% select(priceNames)
