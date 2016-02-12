# Indonesia Year 2

# Make list of attributes from sample eco data
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Seascapes/Indonesia MacMon/Sample data/WCS eco data")

d <- read.csv("FISHRAW.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)

unique(d$Area.Status)
unique(d$Management.Type)
unique(d$Management.Rule)

b <- read.csv("BENTHICRAW.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(b)
nrow(b)

unique(b$Area.Status)
unique(b$Management.Type)
unique(b$Management.Rule)

#all Karimunjawa data
unique(b$Region)

#Coral attributes
unique(b$Genus)
unique(b$Lifeform)
unique(b$Hard.Soft.Substrate)

b2 <- b %>%
  select(Hard.Soft.Substrate, Genus, Lifeform) %>%
  unique() %>%
  arrange(Hard.Soft.Substrate, Genus, Lifeform)

write.csv(b2, "WCS Indo coral types.csv", row.names = FALSE)








# Parse Excel into .csv
installXLSXsupport()
excelFile <- ("/Users/emilydarling/Dropbox/4_WCS MacMon/Seascapes/Indonesia MacMon/Sample data/WCS sample of eco data_WCS IP.xlsx")

numSheets <- sheetCount(excelFile, verbose=TRUE)
numSheets

nameSheets <- sheetNames(excelFile)
nameSheets

setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Seascapes/Indonesia MacMon/Sample data/WCS eco data")
for ( i in 1:5) {
  mySheet <- read.xls(excelFile, sheet=i)
  mySheet$Source <- "WCS"
  mySheet$Country <- "Indonesia"
  write.csv(mySheet, file=paste(nameSheets[i], "csv", sep="."), row.names=FALSE)
}
getwd()


