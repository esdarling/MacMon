#code to calculate MacMon indicators
#======================================
#for MADAGASCAR year 0 baseline report

#site lat, long GPS
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar")
d <- read.csv("latlong_sites.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)

d2 <- d %>%
  group_by(Area,NAP,Site) %>%
  summarize(lat = mean(Latitude, na.rm =TRUE), long = mean(Longitude, na.rm = TRUE))
write.csv(d2, "latlong_sites for report.csv", row.names = FALSE)



#Roberto Komeno catch data - NW (Ankivonj, Ankarea) and ABS (Antongil Baie Seascape)
#setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar")
#nw <- read.csv("DONNEES_CPUE_NW_mai2015.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
#head(nw)
#nrow(nw)

#abs <- read.csv("DONNEES_CPUE_ABS_mai2015.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
#head(abs)
#nrow(abs)

#d <- bind_rows(nw, abs)
#write.csv(d, "DONNEES_CPUE_ABS + NW_esd clean.csv", row.names = FALSE)

setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar")
d <- read.csv("DONNEES_CPUE_ABS + NW_esd clean.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)

#remove entries with no fishers entered
d <- d[-which(is.na(d$No_pecheurs.dans.la.pirogue)),]
d[which(d$No_pecheurs.dans.la.pirogue == 0),]

nrow(d)

hist(d$Poids_kg)
subset(d, Poids_kg > 100)

#Gear types in Malagasy
d$Gear <- CA(d$Gear)
distinct(select(d, Gear))
geartypes <- data.frame(distinct(select(d, Gear)))
getwd()
write.csv(geartypes, "Geartypes_Malagasy for Roberto.csv", row.names = FALSE)

#Fish types in Malagasy
d$Fish <- CA(d$Fish)
distinct(select(d, Fish))
fishtypes <- data.frame(distinct(select(d, Fish)))
write.csv(fishtypes, "Fish_Malagasy for Roberto.csv", row.names = FALSE)

#1. EFFORT
#Total number of pirogues that went fishing per day per village 
head(d)
d2 <- d %>%
  group_by(Annee,Mois,Date,NAP,Village) %>%
  summarize(no_pirogues = mean(No_pirogues, na.rm = TRUE))
hist(d2$no_pirogues)

d2 <- d2 %>%
  group_by(NAP,Village) %>%
  summarize(no_pirogues = mean(no_pirogues, na.rm = TRUE))

d2  %>% 
  ungroup() %>%
  group_by(NAP) %>%
  summarize(pirogues = mean(no_pirogues, na.rm = TRUE), sd = sd(no_pirogues, na.rm = TRUE))
  

#2. CATCH per unit effort
#Sum to No - one No (datasheet) per pecheur
  d2 <- d %>%
  ungroup() %>%
  group_by(Annee,Mois,Date,NAP,Village,No,No_pecheurs.dans.la.pirogue,Gear) %>%
  summarize(catch = sum(Poids_kg)) %>%
  group_by(Annee,Mois,Date,NAP,Village,No,No_pecheurs.dans.la.pirogue) %>%
  summarize(totalcatch = sum(catch))  %>% 
  mutate(CPUE = totalcatch /No_pecheurs.dans.la.pirogue) %>%
  group_by(NAP,Village) %>%
  summarize(meanCPUE = mean(CPUE, na.rm = TRUE))

CPUE_NAP <- d2 %>%
  group_by(NAP) %>%
  summarize(CPUE = mean(meanCPUE, na.rm = TRUE), sd = sd(meanCPUE, na.rm = TRUE))


#no. of gears per day per village
gear_day_village <- d3 %>%
  group_by(Mois,Annee,NAP,Village,Gear) %>%
  summarize(catch_month_gear = mean(CPUE)) %>%
  group_by(Mois,Annee,NAP,Village) %>%
  summarize(no_gears_month = n()) %>%
  group_by(NAP) %>%
  summarize(avg_gear_month = mean(no_gears_month), sd =sd(no_gears_month))
  
#no. of fishers per day village  - effort
head(d2)
fisher_day_village <- d2 %>%
  ungroup() %>%
  group_by(No,Mois,Annee,NAP,Village) %>%
  summarize(no_pecheurs_day = sum(No_pecheurs)) %>%
  group_by(NAP,Village) %>%
  summarize(no_pecheurs_day = mean(no_pecheurs_day)) %>%
  group_by(NAP) %>%
  summarize(avg_pecheurs_day = mean(no_pecheurs_day), sd = sd(no_pecheurs_day))


hist(fisher_day_village$no_pecheurs_day)
  
  group_by(Mois,Annee,NAP,Village,Gear) %>%
  summarize(catch_month_gear = mean(CPUE)) %>%
  group_by(Mois,Annee,NAP,Village) %>%
  summarize(no_gears_month = n()) %>%
  group_by(NAP) %>%
  summarize(avg_gear_month = mean(no_gears_month), sd =sd(no_gears_month))


#gear CPUE
CPUE_gear <- d3  %>% 
  group_by(NAP,Village,Gear) %>%
  summarise(gearCPUE = median(CPUE, na.rm = TRUE))
write.csv(CPUE_gear, "CPUE_gear.csv", row.names = FALSE)

#total CPUE by sites
#remove big catches in Palangre gear
d3 <- d3[-which(d3$Gear == "Palangre"), ]
CPUE_total <- d3  %>% 
  group_by(NAP,Village) %>%
  summarise(totalCPUE = median(CPUE, na.rm = TRUE))
write.csv(CPUE_total, "CPUE_total.csv", row.names = FALSE)


#checks on high CPUE
check <- subset(d2, Village == "Amboditangena")
check <- subset(d2, Village == "Aniribe")
hist(check$CPUE)
check2 <- subset(check, CPUE > 50)


##########################
#BEMA fish data - need to compile spp richness and remove non-Tim DGS species
#use gdata to read xls and save individual csv files
## install support for xlsx files
installXLSXsupport()


#2013 - sheets in single XLS file - pull .csv files and summarize
#Masoala
excelFile <- ("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Suivi Dec 13/Masoala 13/Poisson MSL 13/Poissons MSL 13.xlsx")
## note that the perl scripts that gdata uses do not cope well will tilde expansion
## on *nix machines. So use the full path. 
numSheets <- sheetCount(excelFile, verbose=TRUE)
nameSheets <- sheetNames(excelFile)
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Suivi Dec 13/Masoala 13/Poisson MSL 13")
for ( i in 2:7) {
  mySheet <- read.xls(excelFile, sheet=i)
  mySheet$Year <- 2013
  mySheet$Area <- "Masoala"
  mySheet$Site <- nameSheets[i]
  write.csv(mySheet, file=paste(nameSheets[i], "csv", sep="."), row.names=FALSE)
}
getwd()

#Tampolo
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Suivi Dec 13/Tampolo 13/Poisson TPL 13")
excelFile <- ("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Suivi Dec 13/Tampolo 13/Poisson TPL 13/Poissons TPL 13.xlsx")
numSheets <- sheetCount(excelFile, verbose=TRUE)
nameSheets <- sheetNames(excelFile)
nameSheets
for ( i in 2:9) {
  mySheet <- read.xls(excelFile, sheet=i)
  mySheet$Year <- 2013
  mySheet$Area <- "Tampolo"
  mySheet$Site <- nameSheets[i]
  write.csv(mySheet, file=paste(nameSheets[i], "csv", sep="."), row.names=FALSE)
}

#Tanjona
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Suivi Dec 13/Tanjona 13/Poissons TNJ 13")
excelFile <- ("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Suivi Dec 13/Tanjona 13/Poissons TNJ 13/Poissons Tanjona 13.xlsx")
numSheets <- sheetCount(excelFile, verbose=TRUE)
nameSheets <- sheetNames(excelFile)
nameSheets
for ( i in 2:8) {
  mySheet <- read.xls(excelFile, sheet=i)
  mySheet$Year <- 2013
  mySheet$Area <- "Tanjona"
  mySheet$Site <- nameSheets[i]
  write.csv(mySheet, file=paste(nameSheets[i], "csv", sep="."), row.names=FALSE)
}

#ADD ANTEFANA
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Nosy Antafana 2013")
excelFile <- ("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Nosy Antafana 2013/Poissons N. Antafana 13.xlsx")
numSheets <- sheetCount(excelFile, verbose=TRUE)
nameSheets <- sheetNames(excelFile)
nameSheets
#don't use Ambatomilay.. missing values
for ( i in 2:7) {
  mySheet <- read.xls(excelFile, sheet=i)
  mySheet$Year <- 2013
  mySheet$Area <- "Nosy Antafana"
  mySheet$Site <- nameSheets[i]
  write.csv(mySheet, file=paste(nameSheets[i], "csv", sep="."), row.names=FALSE)
}
getwd()
#add column of location, site from CSV name files, folders

#http://stackoverflow.com/questions/26181122/how-to-merge-csv-files-from-nested-folders-in-r
#recursive into following folders.. TRUE / FALSE
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/")
csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files

#26 sites in 2013 from Masoala, Tampolo, Tanjona, Nosy Antefana
#use dplyr to bind all csv into same dataset
df <- rbind_all(lapply(csv_files, read.csv))
head(df)
getwd()
test <- df
write.csv()

levels(as.factor(test$GENRE))
clip <- test %>%
  select(Year,Area,Site,GENRE,ESPECE,CODE,Total) %>%
  filter(GENRE != "")

#load Tim DGS species to clip
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar/Poisson")
dgs <- read.csv("DGS to Bemahafaly.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(dgs)
nrow(dgs)

dgs <- dgs %>%
  filter(GENRE != "" & DGS == 1) %>%
  select(GENRE, DGS) %>%
  group_by(GENRE) %>%
  summarize(DGS = mean(DGS))
dgs_key <- dgs
write.csv(dgs_key, "dgs_key.csv", row.names = FALSE)

#DGS KEY is WCS Mada GENRE matched to Tim DGS
#If GENRE matches into a 1, then count all species as a DGS tally
#if DGS doesn't match in, don't count
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar/Poisson")
dgs <- read.csv("dgs_key.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(dgs)
nrow(dgs)

#merge in DGS, keep all 1s, delete rest for spp richness counts comparable to Tim
clip2 <- left_join(clip, dgs, by = "GENRE")

#count DGS spp & total spp
spp_rich <- clip2 %>%
  filter(Total > 0) %>%
  group_by(Year,Area,Site) %>%
  mutate(ALLspp = n()) %>%
  ungroup() %>%
  filter(DGS == 1) %>%
  group_by(Year,Area,Site) %>%
  summarize(ALLspp = mean(ALLspp), DGSspp = n())

plot(spp_rich$ALLspp ~ spp_rich$DGSspp)
summary(lm(spp_rich$ALLspp ~ spp_rich$DGSspp))

#summarize biomass across sites

#join biomass, richness
#summarize by area, management for MacMon report
head(test)
levels(as.factor(test$X.2))

#high biomass site: Antsirikisoihy - 150 30-40cm Naso hexacanthus, school of 80cm barracuda
biom <- test %>%
  select(Year,Area,Site,X.2, Biomasse) %>%
  filter(X.2 != "")
#subset(biom, Site == "Antsirikisoihy")

biom <- test %>%
  select(Year,Area,Site,X.2, Biomasse) %>%
  filter(X.2 != "") %>%
  group_by(Year,Area,Site) %>%
  summarize(Biomass = sum(Biomasse))

rich_biom <- inner_join(spp_rich, biom)
plot(rich_biom$Biomass ~ rich_biom$DGSspp)

#join management
getwd()
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar/Poisson")
mgmt <- read.csv("site mgmt from Bema.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(mgmt)
nrow(mgmt)

rich_biom2 <- inner_join(rich_biom, mgmt)
head(rich_biom2)
write.csv(rich_biom2, "FISH_ABS 2013.csv", row.names = FALSE)
#to Ambroise, leave summary of 2013 fish, coral, CPUE data behind report
#these are sites and numbers that will need to be resurveyed in 2017 to calculate change

boxplot(rich_biom2$Biomass ~ rich_biom2$Mgmt)

rich_biom2 %>%
  filter(Site != "Antsirikisoihy") %>%
  group_by(Mgmt) %>%
  summarize(meanBiomass = mean(Biomass), sdBiomass = sd(Biomass))


#ADD Mananara 2015 - from .xlsx files from Catherine
#use benthic script re: summarizing multiple xlsx files
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Catherine_ABS 2015/FISH DATA - Antongil Bay 2015")
getwd()
xls_files <- dir(pattern='*.xlsx$', recursive = T)
xls_files
no_files <- length(xls_files)

for ( i in 2:no_files) {
  excelFile <- paste(getwd(), xls_files[i], sep = "/")
  mySheet <- read.xls(excelFile, sheet="Données brutes")
  split <- strsplit(xls_files[i], split = "\\.")  
  name <- sapply(split, function(x) x[1])      
  mySheet$Year <- 2013
  mySheet$Area <- "Masoala"
  mySheet$Site <- name  
  write.csv(mySheet, file=paste(name, "csv", sep="."), row.names=FALSE)
}

csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files
abs2015 <- rbind_all(lapply(csv_files, read.csv))

names(abs2015)[2] <- "GENRE"
names(abs2015)[3] <- "species"
abs2015$count_abundance <- rowSums(abs2015[,4:12], na.rm = TRUE)
hist(abs2015$count_abundance)

#import dgs key
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar/Poisson")
dgs <- read.csv("dgs_key.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(dgs)
nrow(dgs)

#merge in DGS, keep all 1s, delete rest for spp richness counts comparable to Tim
clip2 <- left_join(abs2015, dgs, by = "GENRE")

abs2015_spp <- clip2 %>%
  select(Year,Area,Site,GENRE,species,count_abundance,DGS) %>%
  filter(GENRE != "" & count_abundance > 0)

#count DGS spp & total spp
abs2015_spp.rich <- abs2015_spp %>%
  group_by(Year,Area,Site) %>%
  mutate(ALLspp = n()) %>%
  ungroup() %>%
  filter(DGS == 1) %>%
  group_by(Year,Area,Site) %>%
  summarize(ALLspp = mean(ALLspp), DGSspp = n())
getwd()
write.csv(abs2015_spp.rich, "ABS2015 spp richness.csv", row.names= FALSE)

##Antongil Bay 2013 new LMMAs 
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/LMMA_2013_Antogil Bay/poissons")
getwd()
xls_files <- dir(pattern='*.xlsx$', recursive = T)
xls_files
no_files <- length(xls_files)

for ( i in 1:no_files) {
  excelFile <- paste(getwd(), xls_files[i], sep = "/")
  mySheet <- read.xls(excelFile, sheet="Données brutes")
  split <- strsplit(xls_files[i], split = "\\.")  
  name <- sapply(split, function(x) x[1])      
  mySheet$Year <- 2013
  mySheet$Area <- "LMMA"
  mySheet$Site <- name  
  write.csv(mySheet, file=paste(name, "csv", sep="."), row.names=FALSE)
}

csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files
abs2013 <- rbind_all(lapply(csv_files, read.csv))

names(abs2013)[2] <- "GENRE"
names(abs2013)[3] <- "species"
abs2013$count_abundance <- rowSums(abs2013[,4:12], na.rm = TRUE)
hist(abs2013$count_abundance)

#import dgs key
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar/Poisson")
dgs <- read.csv("dgs_key.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(dgs)
nrow(dgs)

#merge in DGS, keep all 1s, delete rest for spp richness counts comparable to Tim
clip2 <- left_join(abs2013, dgs, by = "GENRE")

abs2013 <- clip2 %>%
  select(Year,Area,Site,GENRE,species,count_abundance,DGS) %>%
  filter(GENRE != "" & count_abundance > 0)

#count DGS spp & total spp
abs2013_spp.rich <- abs2013 %>%
  group_by(Year,Area,Site) %>%
  mutate(ALLspp = n()) %>%
  ungroup() %>%
  filter(DGS == 1) %>%
  group_by(Year,Area,Site) %>%
  summarize(ALLspp = mean(ALLspp), DGSspp = n())
getwd()
write.csv(abs2013_spp.rich, "ABS2013 spp richness.csv", row.names= FALSE)



####SUMMARIZE poisson for REPORT table
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar/Poisson")
d <- read.csv("FISH summary 2013_2015.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)

#recode Mgmt to No-take / other
d$Mgmt2 <- ifelse(d$Mgmt == "No take", "No take", "Other")

d2 <- d %>% 
  select(Area,Site,Type,Mgmt2,Biomass,DGSspp) %>%
  group_by(Area,Mgmt2) %>%
  summarize(n_sites = n(),
            meanBiomass = round(mean(Biomass),2), sdBiomass = round(sd(Biomass),2),
            meanDGSspp = round(mean(DGSspp),1), sdDGSspp = round(sd(DGSspp),1)) %>%
  filter(Area != "")
head(d2)


write.csv(d2, "FISH summary 2013_2015 for REPORT.csv", row.names = FALSE)
  



#####################
#benthic summary - from bleaching susceptibility surveys
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar/Benthic")
bl <- read.csv("Bleaching RECAP_BEMAF.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(bl)
nrow(bl)

bl2 <- bl %>%
  filter(Year >= 2011) %>% 
  group_by(Area,Mgmt) %>%
  summarize(coral_cover = mean(Coral_cover), 
            algae_cover = mean(Erect.algae_cover),
            no_genera = mean(No_genera),
            site_suscept = mean(Site.susceptibility))  


#####################
#benthic summary - from benthic LITs 
#pull XLS syntheses files
installXLSXsupport()

#Masoala
## note that the perl scripts that gdata uses do not cope well will tilde expansion
## on *nix machines. So use the full path. 
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Suivi Dec 13/Masoala 13/PM Ambodilaitry Benthos 13")
getwd()
xls_files <- dir(pattern='*.xls$', recursive = T)
xls_files
no_files <- length(xls_files)

for ( i in 1:no_files) {
  excelFile <- paste(getwd(), xls_files[i], sep = "/")
  mySheet <- read.xls(excelFile, sheet="Synthese")
  split <- strsplit(xls_files[i], split = "\\.")  
  name <- sapply(split, function(x) x[1])      
  mySheet$Year <- 2013
  mySheet$Area <- "Masoala"
  mySheet$Site <- name  
  write.csv(mySheet, file=paste(name, "csv", sep="."), row.names=FALSE)
}

csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files
masoala <- rbind_all(lapply(csv_files, read.csv))

#Tampolo
## note that the perl scripts that gdata uses do not cope well will tilde expansion
## on *nix machines. So use the full path. 
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Suivi Dec 13/Tampolo 13/Benthos Tampolo 13")
getwd()
xls_files <- dir(pattern='*.xls$', recursive = T)
xls_files
no_files <- length(xls_files)

for ( i in 1:no_files) {
  excelFile <- paste(getwd(), xls_files[i], sep = "/")
  mySheet <- read.xls(excelFile, sheet="Synthese")
  split <- strsplit(xls_files[i], split = "\\.")  
  name <- sapply(split, function(x) x[1])      
  mySheet$Year <- 2013
  mySheet$Area <- "Tampolo"
  mySheet$Site <- name  
  write.csv(mySheet, file=paste(name, "csv", sep="."), row.names=FALSE)
}

csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files
tampolo <- rbind_all(lapply(csv_files, read.csv))

#Tanjona
## note that the perl scripts that gdata uses do not cope well will tilde expansion
## on *nix machines. So use the full path. 
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Suivi Dec 13/Tanjona 13/Benthos Tanjona 13")
getwd()
xls_files <- dir(pattern='*.xls$', recursive = T)
xls_files
no_files <- length(xls_files)

for ( i in 1:no_files) {
  excelFile <- paste(getwd(), xls_files[i], sep = "/")
  mySheet <- read.xls(excelFile, sheet="Synthese")
  split <- strsplit(xls_files[i], split = "\\.")  
  name <- sapply(split, function(x) x[1])      
  mySheet$Year <- 2013
  mySheet$Area <- "Tanjona"
  mySheet$Site <- name  
  write.csv(mySheet, file=paste(name, "csv", sep="."), row.names=FALSE)
}

csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files
tanjona <- rbind_all(lapply(csv_files, read.csv))

#Nosy Antafana
## note that the perl scripts that gdata uses do not cope well will tilde expansion
## on *nix machines. So use the full path. 
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Bemahafaly/Nosy Antafana 2013/PM Nosy Antafana Benthos 13")
getwd()
xls_files <- dir(pattern='*.xls$', recursive = T)
xls_files
no_files <- length(xls_files)

for ( i in 1:no_files) {
  excelFile <- paste(getwd(), xls_files[i], sep = "/")
  mySheet <- read.xls(excelFile, sheet="Synthese")
  split <- strsplit(xls_files[i], split = "\\.")  
  name <- sapply(split, function(x) x[1])      
  mySheet$Year <- 2013
  mySheet$Area <- "Nosy Antafana"
  mySheet$Site <- name  
  write.csv(mySheet, file=paste(name, "csv", sep="."), row.names=FALSE)
}

csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files
nosyantafana <- rbind_all(lapply(csv_files, read.csv))

abs_LITs <- bind_rows(masoala,tampolo,tanjona,nosyantafana)
#names(masoala)
#names(tampolo)
#names(tanjona)
#names(nosyantafana)
names(abs_LITs)[1] <- "Benthos"
names(abs_LITs)[12] <- "Perc_cover"

#remove total 1000 cm sums - in blank Benthos rows
levels(as.factor(abs_LITs$Benthos))
abs_LITs2 <- abs_LITs %>%
  filter(Benthos != "") %>%
  select(Year,Area,Site,Benthos,Perc_cover)  
  
min(abs_LITs2$Perc_cover); max(abs_LITs2$Perc_cover)
hist(abs_LITs2$Perc_cover)

key <- data.frame(Benthos = levels(as.factor(abs_LITs2$Benthos)))
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar/Benthic")
#write.csv(key, "WCS Mada key_Benthos.csv", row.names = FALSE)
key <- read.csv("WCS Mada key_Benthos.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(key)
nrow(key)

abs_LITs3 <- inner_join(abs_LITs2, key)
levels(as.factor(abs_LITs3$Type))

abs_benthic <- abs_LITs3 %>%
  group_by(Year,Area,Site,Type) %>%
  summarize(Cover = sum(Perc_cover)) 

abs_benthic <- dcast(abs_benthic, Year+Area+Site ~ Type)

abs_noGenera <- abs_LITs3 %>%
  filter(Type == "Hard coral" & Perc_cover > 0) %>%
  group_by(Year,Area,Site) %>%
  summarize(noGenera = n()) 

abs_benthic2 <- inner_join(abs_noGenera,abs_benthic)
getwd()
write.csv(abs_benthic2, "ABS benthic summary.csv", row.names = FALSE)


#last BENTHIC - merge site susceptibility WIO into genus LITs
#calculate LIT site susceptibility


#merge into Bleach_RECAP
#summarize for report
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar/Benthic")
d <- read.csv("ABS benthic summary.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)

d <- d %>%
  group_by(Year,Area,Site_original,Type,Mgmt) %>%
  summarize(no_genera = mean(noGenera), hardcoral = mean(Hard.coral), 
            macroalgae = mean(Macroalgae), suscept = mean(Susceptibility))

levels(as.factor(d$Mgmt))
d$Mgmt2 <- ifelse(d$Mgmt == "No take", "No take", "Other")

mean <- d %>%
  group_by(Area,Mgmt2) %>%
  summarize(no_genera = mean(no_genera), hardcoral = mean(hardcoral), 
   macroalgae = mean(macroalgae), suscept = mean(suscept, na.rm = TRUE))

sd <- d %>%
  group_by(Area,Mgmt2) %>%
  summarize(no_genera = sd(no_genera), hardcoral = sd(hardcoral), 
            macroalgae = sd(macroalgae), suscept = sd(suscept, na.rm = TRUE))

benthic_abs <- bind_rows(mean,sd)
benthic_abs$type <- c(rep("mean",12),rep("sd",12)) 

benthic_abs <- melt(benthic_abs, 
                    id.vars = c("Area","Mgmt2","type"))
benthic_abs2 <- dcast(benthic_abs, type+variable+Mgmt2~Area)
write.csv(benthic_abs2, "ABS benthic for report.csv", row.names = FALSE)






#NW - DONE
#merge Nya 2015 NW bleach suscept surveys - Ank/Ankivonj benthic indicators
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Madagascar MacMon data/NW monitoring 2015/BleachSuscept, Urchins_Madagascar 2015_Nya")
getwd()
xls_files <- dir(pattern='*.xlsx$', recursive = T)
xls_files
no_files <- length(xls_files)

for ( i in 2:15) {
  excelFile <- paste(getwd(), xls_files[i], sep = "/")
  mySheet <- read.xls(excelFile, sheet="BleachingTemplate")
  mySheet <- mySheet[,1:2]
  split <- strsplit(xls_files[i], split = "\\_")  
  name <- sapply(split, function(x) x[2])      
  mySheet$Year <- 2015
  mySheet$Area <- "NW MPAs"
  mySheet$Site <- name  
  write.csv(mySheet, file=paste(name, "csv", sep="."), row.names=FALSE)
}



csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files
nya_nw <- rbind_all(lapply(csv_files, read.csv))

clip <- nya_nw[which(nya_nw$BLEACHING.TEMPLATE == "Number of genera" |
  nya_nw$BLEACHING.TEMPLATE == "Site susceptibility" |
  nya_nw$BLEACHING.TEMPLATE == "% Coral cover" |
  nya_nw$BLEACHING.TEMPLATE == "% Erect algae" |
  nya_nw$BLEACHING.TEMPLATE == "Number of colonies"),]
 
names(clip)[1] <- "variable"
names(clip)[2] <- "value"
 
levels(as.factor(clip$Site))
clip <- clip[-which(clip$Site == "template"),]
clip$Area <- ifelse(clip$Site == "Ankivonjy plateau" | clip$Site == "Pan du Sucre" |
                      clip$Site == "Plage Noir" |clip$Site == "Kisimany","Ankivonjy",
                        "Ankarea")
clip$value <- as.numeric(clip$value)
clip$variable <- as.factor(clip$variable)
clip <- clip[,c(3:5,1:2)]

clip %>%
  group_by(Year,Area,variable) %>%
  summarize(value = mean(value), sdvalue = sd(value))
#write.csv(clip, "Nya bleach summary.csv", row.names = FALSE)  

#no of total colonies
clip %>%
  filter(variable == "Number of colonies") %>%
  summarize(sum = sum(value))

#import Nya summary with mgmt
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Madagascar/Benthic")
nya <- read.csv("Nya bleach summary.csv", 
                header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(nya)
nrow(nya)
nya <- nya %>%
  arrange(variable)
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/NW monitoring 2015_TimNya/Madagascar 2015_Nya")
getwd()
write.csv(nya, "Nya bleach summary by site.csv", row.names = FALSE)

nya2 <- nya %>%
  group_by(Area,Mgmt,variable) %>%
  summarize(meanvalue = mean(value), sd = sd(value))


nya_mean <- dcast(nya2, variable+Mgmt ~ Area, value.var = "meanvalue")
nya_sd <- dcast(nya2, variable+Mgmt ~ Area, value.var = "sd")
nya3 <- bind_rows(nya_mean, nya_sd)
write.csv(nya3, "Nya bleach summary_for MacMon table.csv", row.names= FALSE)

