#Install packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("data.table")
install.packages("ggbiplot")

#Load packages
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(ggbiplot)
library(ggplot2)

#Round dataframe numeric values to 'digits' decimal places
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

#Import data
Election_2022 <- read_excel("Desktop/DM 2/Exo 1/R outputs/Elections 2022.xlsx")

##################################### Descriptive Stats #####################################

Election_2022_Descp_Stats <- Election_2022 %>% select(starts_with("%"))
Election_2022_Descp_Stats <- sapply(Election_2022_Descp_Stats, function(x) c( "Stand dev" = sd(x), 
                         "Mean"= mean(x,na.rm=TRUE),
                         "n" = length(x),
                         "Median" = median(x),
                         "CoeffofVariation" = sd(x)/mean(x,na.rm=TRUE),
                         "Minimum" = min(x),
                         "Maximun" = max(x),
                         "Upper Quantile" = quantile(x,1),
                         "LowerQuartile" = quantile(x,0)
)
)

#Round values to 2 DP
Election_2022_Descp_Stats <- round_df(Election_2022_Descp_Stats, 2)

#Tranpose Election_2022_Descp_Stats
Election_2022_Descp_Stats <- t(Election_2022_Descp_Stats)

##################################### ACP #####################################

#Principal Component Analysis (PCA)
Election_2022$Inscrits <- as.numeric(Election_2022$Inscrits)
pca_results <- prcomp(Election_2022[,c(8:57)], scale = TRUE)

#Display the results
summary(pca_results)

#Dataframe to label the scores plot with `Code du b.vote`
pca_data <- cbind(Election_2022[,c(1:7)], as.data.frame(pca_results$x))

#Scores plot
ggbiplot(pca_results, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE, labels = pca_data$`Code du b.vote`, varname.size = 0) +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  ylim(-100, 100) 
