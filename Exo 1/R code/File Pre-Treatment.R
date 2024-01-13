#Install packages
install.packages("readxl")
install.packages("writexl")

#Load packages
library(readxl)
library(writexl)

#Import data
Election_2022 <- read_excel("Desktop/DM 2/Exo 1/Import Data/resultats-par-niveau-burvot-t1-france-entiere.xlsx")

#Get information to rename candidates columns
nom_candidats <- unname(unlist(Election_2022[1,seq(from=24, to=105, by=7)]))
desc_candidats <- c(names(Election_2022[,26:28]))

#Remove redundant columns
Election_2022 <- Election_2022[, -c(seq(from=22, to=105, by=7), seq(from=23, to=105, by=7), seq(from=24, to=105, by=7), seq(from=25, to=105, by=7))]

#Rename columns with respective candidates names
for(i in seq(from=22, to=55, by=3)){
  colnames(Election_2022)[c(i:(i+2))] <- paste(desc_candidats, nom_candidats[(i-22)/3+1], sep = " ")
}

#Export data
write_xlsx(Election_2022, "Desktop/DM 2/Exo 1/R outputs/Elections 2022.xlsx")