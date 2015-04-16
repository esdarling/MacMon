#note: packages loaded in .Rprofile

#code to calculate MacMon indicators
#======================================
#for Fiji year 0 baseline report


#bleaching susceptibility code
# - needs taxa susceptibilities
# - needs benthic data on coral community composition

#load taxa susceptibility
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily")
d <- read.csv("FIJI_DataEntry_2012_TaxaSuscept.csv", header = TRUE, stringsAsFactors = FALSE) 
head(d)
nrow(d)
str(d) 
names(d) 

#recode suscept values into low, high moderate
#recode suscept score into values to multiple into susceptibility index
d$suscept_cat <- recode(d$suscept,"'0' = 'High';'1' = 'Moderate';'2' = 'Low'")
d$suscept_score <- recode(d$suscept_cat,"'High' = '2';'Moderate'='1';'Low' = '0'")

#load benthic community data

#check genus merge in - names all good in taxa file? 
#how much percent cover is missing in a taxa merge? 

#calculate site/transet coral community susceptibilities