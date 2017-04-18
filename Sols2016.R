#CODE TO IMPORT BLEACHING SURVEYS FROM TEMPLATE
library(dplyr)

###############
#Import Solomons 2016 Kolombangara survey
#code to read in 2016 bleaching template
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Bleaching/2016 data/Solomons 2016")
getwd()
xls_files <- dir(pattern='*.xlsx$', recursive = T)
xls_files
no_files <- length(xls_files)


#read siteinfo and dump into site file  
#import siteinfo and update from each sheet
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Bleaching/2016 data/Datasets")
siteinfo <- read.csv("siteinfo.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(siteinfo)
nrow(siteinfo)
names(siteinfo)
str(siteinfo)

##############
#SITE INFO file
#function to dump into siteinfo file
#sites that don't align need some deleted rows manually to get them
xls_files
for (i in 2:17) {
  setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Bleaching/2016 data/Solomons 2016")
  excelFile <- paste(getwd(), xls_files[i], sep = "/")
  mySheet <- read.xls(excelFile, sheet="GENUS BleachingTemplate")
  #dump info into siteinfo
  #use k to set row to dump into
  k=i-1
  siteinfo[k,"country"] <- as.character(mySheet[1,2])
  siteinfo[k,"location"] <- as.character(mySheet[2,2])
  siteinfo[k,"site"] <- as.character(mySheet[3,2])
  siteinfo[k,"observer"] <- as.character(mySheet[4,2])
  siteinfo[k,"year"] <- as.character(mySheet[5,2])
  siteinfo[k,"month"] <- as.character(mySheet[6,2])
  siteinfo[k,"day"] <- as.character(mySheet[7,2])
  siteinfo[k,"lat"] <- as.character(mySheet[8,2])
  siteinfo[k,"long"] <- as.character(mySheet[9,2])
  siteinfo[k,"depth_m"] <- as.character(mySheet[10,2])
  siteinfo[k,"temp_C"] <- as.character(mySheet[11,2])
  siteinfo[k,"management"] <- as.character(mySheet[12,2])
  siteinfo[k,"no_quadrats"] <- as.numeric(as.character(mySheet[99,2]))
  #not sure why just as.numeric doesn't work, but it doesn't
  siteinfo[k,"perc_hardcoral"] <- round(as.numeric(as.character(mySheet[96,2])), digits =2)
  siteinfo[k,"perc_softcoral"] <- round(as.numeric(as.character(mySheet[97,2])), digits =2)
  siteinfo[k,"perc_macroalgae"] <- round(as.numeric(as.character(mySheet[98,2])), digits =2)
  setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Bleaching/2016 data/Datasets")
  }
head(siteinfo)
write.csv(siteinfo, "siteinfo_update.csv", row.names = FALSE)

###CORAL COLONIES 
#read genera counts and save as .csv files - to bind later
#export with unique id, k
xls_files
for (i in 2:17) {
  setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Bleaching/2016 data/Solomons 2016")
  excelFile <- paste(getwd(), xls_files[i], sep = "/")
  mySheet <- read.xls(excelFile, sheet="GENUS BleachingTemplate")
  #clip out colony info
  clip <- mySheet[22:87,1:8]
  codes <- c("genus","normal","pale","bleach1","bleach2","bleach3","bleach4","recentdead")
  colnames(clip) <- codes
  #add unique.id column
  k = i-1
  clip$unique.id = k
  clip <- clip[,c(9,1:8)]
  #grab sitename for saving .csv file
  split <- strsplit(xls_files[i], split = "\\.")  
  sitename <- sapply(split, function(x) x[1])    
  setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Bleaching/2016 data/Solomons 2016/csv")
  write.csv(clip, file=paste(sitename, "csv", sep="."), row.names=FALSE) 
}  

#read in .csv files and bind together with unique id
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Bleaching/2016 data/Solomons 2016/csv")
csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files
sols_bind <- rbind_all(lapply(csv_files, read.csv))    

###calculate variables off sols_bind

#total number of colonies
head(sols_bind)

#1. remove Totals from Genus if imported
d1 <- sols_bind
d2 <- d1[-which(d1$genus == "Total"),]
unique(d2$genus)

#2. condense Galaxea spp
#smart R code to do this? 
#ifelse(sols_bind2$genus == "Galaxea astreata" 

#3. calculate things
d2$total <- rowSums(d2[,3:9])
head(d2)

#number of genera
str(d2)
names(d2)

check <- subset(d2, unique.id == 8)

#site-level metrics
d3 <- d2 %>%
  mutate(count = ifelse(d2$total > 0,1,0)) %>%
  mutate(bleached = total - (normal + pale)) %>%
  group_by(unique.id) %>%
  mutate(site.cols = sum(total)) %>%
  mutate(rel.abund = total / site.cols) %>%
  mutate(pi_squared = rel.abund^2) %>%
  summarise(no_genera = sum(count),
            no_colonies = sum(total), 
            no_bleached.cols = sum(bleached),
            perc_bleached = round((sum(bleached) / sum(total) * 100),digits =2),
            simp.diversity = round(1 - (sum(pi_squared)), digits = 2))
head(d3)  
head(siteinfo)

#bind to siteinfo
siteinfo2 <- left_join(siteinfo,d3)
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Bleaching/2016 data/Solomons 2016")
write.csv(siteinfo2, "Solomons2016 site summary.csv", row.names = FALSE)

#in total, counted 3776 colonies
sum(d3$no_colonies)

#of 52 genera
d4 <- d2 %>%
  group_by(genus) %>%
  summarise(total_cols = sum(total)) %>%
  arrange(desc(total_cols))

#472 bleached colonies
sum(d3$no_bleached.cols)
472/3776*100

#write genera-level
getwd()
write.csv(d5, "genera level bleaching summary.csv", row.names = FALSE)


#bleaching by genus
#37 genera with bleaching
d5 <- d2 %>%
  mutate(bleached = total - (normal + pale)) %>%
  group_by(genus) %>%
  summarise(no_bleached.cols = sum(bleached),
            total_cols = sum(total),
            perc_bleach = sum(bleached) / sum(total) * 100) %>%
  arrange(desc(total_cols))

#bleaching response by genus
head(d2)

d6 <- d2 %>%
  mutate(bleached.cols = total - (normal + pale)) %>%
  mutate(perc_bleach = ifelse(total == 0,0,bleached.cols / total)) %>%
  mutate(bleach_response = ifelse(total == 0, 0,
           ((pale / total) * 1) +
           ((bleach1 / total) * 2) +
           ((bleach2 / total) * 3) +
           ((bleach3 / total) * 4) +
           ((bleach4 / total) * 5) +
           ((recentdead / total) * 6))) %>%
  group_by(unique.id) %>%
  mutate(site.perc_bleach = sum(bleached.cols) / sum(total)*100)

par(mar  = c(2,2,2,2))
hist(d6$bleach_response)
plot(d6$bleach_response ~ d6$perc_bleach)

#filter for sites >> 10% bleaching, and calculate genus-level BR
names(d6)
d7 <- subset(d6, site.perc_bleach > 10)
head(d7)

## would you really calculate a bleaching response for 8 sites with >> 10% bleaching? 
## probably not, need more observations


















mySheet2 <- mySheet2[-1,]
head(mySheet2)
