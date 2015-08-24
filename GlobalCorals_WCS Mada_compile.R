
# Code to calculate genus-level percent cover for inclusion in Global Corals database
# August 2015

#Masoala
# all benthic files are now converted to .csvs
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Madagascar MacMon data/MNP Antongil Bay 2013/Masoala 13/PM Ambodilaitry Benthos 13")
getwd()
csv_files <- dir(pattern='.csv', recursive = FALSE)
csv_files
no_files <- length(csv_files)
no_files

for (i in 1:no_files) {
  csvFile <- paste(getwd(), csv_files[i], sep = "/")
  myFile <- read.csv(csvFile, header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
  head(myFile)  
  # clip to coral rows, reorganize columns
  myCorals <- myFile[18:74,c(17:19,1:11)]
  site <- myFile$Site[1]
  filename <- paste(site, "Corals", sep = "_")
    write.csv(myCorals, file=paste(filename, "csv", sep="."), row.names=FALSE)
}

setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Madagascar MacMon data/MNP Antongil Bay 2013/Masoala 13/PM Ambodilaitry Benthos 13/Corals")
coral_files <- dir(pattern='*.csv$', recursive = FALSE)
coral_files
masoala <- rbind_all(lapply(coral_files, read.csv))
head(masoala)
write.csv(masoala, "Masoala_corals_summmary.csv", row.names = FALSE)


#Tampolo
## note that the perl scripts that gdata uses do not cope well will tilde expansion
## on *nix machines. So use the full path. 
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Madagascar MacMon data/MNP Antongil Bay 2013/Tampolo 13/Benthos Tampolo 13")
getwd()
csv_files <- dir(pattern='.csv', recursive = FALSE)
csv_files
no_files <- length(csv_files)
no_files

for (i in 1:no_files) {
  csvFile <- paste(getwd(), csv_files[i], sep = "/")
  myFile <- read.csv(csvFile, header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
  head(myFile)  
  # clip to coral rows, reorganize columns
  myCorals <- myFile[18:74,c(17:19,1:11)]
  site <- myFile$Site[1]
  filename <- paste(site, "Corals", sep = "_")
  write.csv(myCorals, file=paste(filename, "csv", sep="."), row.names=FALSE)
}

setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Madagascar MacMon data/MNP Antongil Bay 2013/Tampolo 13/Benthos Tampolo 13/Corals")
coral_files <- dir(pattern='*.csv$', recursive = FALSE)
coral_files
tampolo <- rbind_all(lapply(coral_files, read.csv))
head(tampolo)
write.csv(tampolo, "Tampolo_corals_summmary.csv", row.names = FALSE)


#Tanjona
## note that the perl scripts that gdata uses do not cope well will tilde expansion
## on *nix machines. So use the full path. 
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Madagascar MacMon data/MNP Antongil Bay 2013/Tanjona 13/Benthos Tanjona 13")
getwd()
csv_files <- dir(pattern='.csv', recursive = FALSE)
csv_files
no_files <- length(csv_files)
no_files

for (i in 1:no_files) {
  csvFile <- paste(getwd(), csv_files[i], sep = "/")
  myFile <- read.csv(csvFile, header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
  head(myFile)  
  # clip to coral rows, reorganize columns
  myCorals <- myFile[18:74,c(17:19,1:11)]
  site <- myFile$Site[1]
  filename <- paste(site, "Corals", sep = "_")
  write.csv(myCorals, file=paste(filename, "csv", sep="."), row.names=FALSE)
}

setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Madagascar MacMon data/MNP Antongil Bay 2013/Tanjona 13/Benthos Tanjona 13/Corals")
coral_files <- dir(pattern='*.csv$', recursive = FALSE)
coral_files
tanjona <- rbind_all(lapply(coral_files, read.csv))
head(tanjona)
write.csv(tanjona, "Tanjona_corals_summmary.csv", row.names = FALSE)


#Nosy Antafana
## note that the perl scripts that gdata uses do not cope well will tilde expansion
## on *nix machines. So use the full path. 
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Madagascar MacMon data/MNP Antongil Bay 2013/Nosy Antafana 2013/PM Nosy Antafana Benthos 13")
getwd()
csv_files <- dir(pattern='.csv', recursive = FALSE)
csv_files
no_files <- length(csv_files)
no_files

for (i in 1:no_files) {
  csvFile <- paste(getwd(), csv_files[i], sep = "/")
  myFile <- read.csv(csvFile, header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
  head(myFile)  
  # clip to coral rows, reorganize columns
  myCorals <- myFile[18:74,c(17:19,1:11)]
  site <- myFile$Site[1]
  filename <- paste(site, "Corals", sep = "_")
  write.csv(myCorals, file=paste(filename, "csv", sep="."), row.names=FALSE)
}

setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/Madagascar MacMon data/MNP Antongil Bay 2013/Nosy Antafana 2013/PM Nosy Antafana Benthos 13/Corals")
coral_files <- dir(pattern='*.csv$', recursive = FALSE)
coral_files
antefana <- rbind_all(lapply(coral_files, read.csv))
head(antefana)
write.csv(antefana, "NosyAntefana_corals_summmary.csv", row.names = FALSE)


# READ IN coral summaries from each location and bind together for ABS coral summary
ABS_corals <- rbind.fill(masoala,tampolo,tanjona,antefana)
names(ABS_corals)[4] <- "Genus"
names(ABS_corals)[14] <- "sum_cm"
head(ABS_corals)
names(ABS_corals)

ABS_corals2 <- melt(ABS_corals[,-14], id.vars = 1:4, variable.name = "Transect", value.name = "cm")
head(ABS_corals2)
names(ABS_corals2)

# ADD in NW MPAs sites
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/ESD compiled files")
nwMPAs <- read.csv("Benthos_NW MPAs_2015_LITs.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
nwMPAs$Year <- 2015

nwMPAs <- subset(nwMPAs, Type == "Hard coral")

#rearrange to rbind into ABS
names(nwMPAs)[2] <- "Area"
names(nwMPAs)[6] <- "Genus"

head(nwMPAs)
names(nwMPAs)

# Merge in only on joining columns
ABS_NW <- rbind.fill(ABS_corals2, nwMPAs)
ABS_NW <- ABS_NW[,c(1:6)]
head(ABS_NW)
names(ABS_NW)

levels(as.factor(ABS_NW$Area))
levels(as.factor(ABS_NW$Genus))

#check how many transects completed at each site - cm summaries for each transect
count_transects <- ABS_NW %>%
  group_by(Year,Area,Site,Transect) %>%
  summarize(sum_cm = sum(cm))
count_transects <- count_transects[-which(count_transects$sum_cm == 0),]

count_transects2 <- count_transects %>%
  ungroup() %>%
  group_by(Year,Area,Site) %>%
  summarize(no_transects = n())

ABS_NW2 <- ABS_NW %>%
  group_by(Year,Area,Site,Genus) %>%
  summarize(sum_cm = sum(cm))
head(ABS_NW2)

ABS_NW3 <- subset(ABS_NW2, sum_cm > 0)
ABS_NW4 <- join(ABS_NW3, count_transects2)
ABS_NW4$Perc_cover <- (ABS_NW4$sum_cm / (ABS_NW4$no_transects * 1000)) * 100
head(ABS_NW4)

hist(ABS_NW4$Perc_cover)

write.csv(ABS_NW4, "ABS_NW Mada_coral genus_cover.csv", row.names = FALSE)

check <- ABS_NW4 %>%
  ungroup() %>%
  group_by(Year,Area,Site) %>%
  summarize(sum_cover = sum(Perc_cover)) %>%
  ungroup() %>%
  arrange(desc(sum_cover))
hist(check$sum_cover)

#will rescale 0 to 100 later with global database

#Make site summary for ABS_NW
head(ABS_NW4)
site_summary <- ABS_NW4 %>%
  group_by(Year,Area,Site,no_transects) %>%
  summarize(dummy = n())
site_summary  

setwd("/Users/emilydarling/Dropbox/1-On the go/Coral Database/1_FULLDATA/Final datasets/WCS Mada")
write.csv(site_summary, "ABS_NW site summary.csv", row.names = FALSE)

#read in edited site names.. 
ABS_NW5 <- read.csv("ABS_NW site summary_edit names.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
head(ABS_NW5)

ABS_NW5 <- merge(ABS_NW5, ABS_NW4)
head(ABS_NW5)
names(ABS_NW5)

ABS_NW6 <- ABS_NW5[,c(1,2,5,4,7:9)]
names(ABS_NW6)[3] <- "Site"
head(ABS_NW6)
ABS_NW6$Source <- "WCS Mada"

#Get to site-level percent cover, then
# Add in SW Soariake surveys
setwd("/Users/emilydarling/Dropbox/1-On the go/Coral Database/1_FULLDATA/Final datasets/WCS Mada")
Soariake <- read.csv("WCSMada_Soariake.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
head(Soariake)

Mada <- rbind.fill(ABS_NW6, Soariake)
Mada <- Mada[,c(8,1:3,5,7)]
head(Mada)

# Clean up genus names, make final site summary and percent cover estimates
levels(as.factor(Mada$Genus))

#copied in genus cleaning list from GlobalCorals code in Textmate
Mada$Genus <- CA(Mada$Genus)
Mada$Genus <- recode(Mada$Genus,
                "'Acanthastraea' = 'Acanthastrea';
                'Acathastrea' = 'Acanthastrea';
                'Acrop' = 'Acropora';
                'Astreo' = 'Astreopora';
                'Cyclosteris' = 'Cycloseris';      
                'Echino' = 'Echinopora';
                'Echinophilia' = 'Echinophyllia'; 
                'Gardi' = 'Gardineroseris';
                'Goniop' = 'Goniopora';      
                'herpo' = 'Herpolitha';
                'Hydnop' = 'Hydnophora';
                'Leptas' = 'Leptastrea';
                'Loboph' = 'Lobophyllia';   
                'Mille' = 'Millepora';
                'Millipora' = 'Millepora';    
                'Monti' = 'Montipora';
                'Platyg' = 'Platygyra';  
                'Poc' = 'Pocillopora';
                'Por' = 'Porites';
                'Psamacora' = 'Psammocora';      
                'Psamm' = 'Psammocora';
                'Psammacora' = 'Psammocora';
                'Seriatopo' = 'Seriatopora';
                'Styloph' = 'Stylophora';
                'Sympilia' = 'Symphyllia';
                'Tubip' = 'Tubipora';
                'Caulatrea' = 'Caulastrea';
                'Coscinarea' = 'Coscinaraea';
                'Coscinaria' = 'Coscinaraea';           
                'Cycloeseris' = 'Cycloseris';
                'Cyphasetrea' = 'Cyphastrea'; 
                'Diplioastrea' = 'Diploastrea';
                'Gardenoseris' = 'Gardineroseris';      
                'Gardinoseris' = 'Gardineroseris';
                'Hydenophora' = 'Hydnophora';
                'Hydonophora' = 'Hydnophora';
                'Lobophylia' = 'Lobophyllia';
                'Lobopyllia' = 'Lobophyllia';      
                'Milleopora' = 'Millepora';
                'Millipora' = 'Millepora';
                'Monastrea' = 'Montastrea';      
                'Monitpora' = 'Montipora';
                'Oulaphyllia' = 'Oulophyllia'; 
                'Pectina' = 'Pectinia';
                'PLeisastrea' = 'Plesiastrea';      
                'Pocilliopora' = 'Pocillopora';
                'Podobacia' = 'Podabacia';
                'Polyphillia' = 'Polyphyllia';
                'porites' = 'Porites';
                'Scapophyllia' = 'Scaphophyllia';      
                'Stylohpora' = 'Stylophora';
                'Symphilia' = 'Symphyllia';
                'Turbinaraea' = 'Turbinaria';
                'Tubinaria' = 'Turbinaria';
                'Fungiidae' = 'Fungia';
                'Gardinoseris' = 'Gardineroseris';
                'Hydenophora' = 'Hydnophora';
                'Hydonophora' = 'Hydnophora';
                'Lobophylia' = 'Lobophyllia';
                'Psamacora' = 'Psammocora';
                'Psammacora' = 'Psammocora';
                'Symphilia' = 'Symphyllia';
                'Balastomussa' = 'Blastomussa';
                'Favities' = 'Favites';
                'Gonipora' = 'Goniopora';
                'Herpolithia' = 'Herpolitha';
                'Lithophylon' = 'Lithophyllon';
                'Montastraea' = 'Montastrea';
                'Oulophylia' = 'Oulophyllia';
                'Oulphyllia' = 'Oulophyllia';   
                'Ouphyllia' = 'Oulophyllia';
                'Pleisiastrea' = 'Plesiastrea';
                'Sandolitha' = 'Sandalolitha';
                'Seriatopera' = 'Seriatopora';
                'Styllophora' = 'Stylophora';
                'Turbinarea' = 'Turbinaria';
                'Herpo' = 'Herpolitha';
                'Echynopora' = 'Echinopora';
                'Lithophyllum' = 'Lithophylum';
                'Paraclavurina' = 'Paraclavarina';
                'Echynophyllia' = 'Echinophyllia';
                'Playtgyra' = 'Platygyra';
                'Oxypora/echinophyllia' = 'Oxypora';
                'Acanthastria' = 'Acanthastrea';
                'Echnophyllia' = 'Echinophyllia';
                'Gyrosmilla' = 'Gyrosmilia';
                'Lithopyhllon' = 'Lithophyllon';
                'Pleisastrea' = 'Plesiastrea';
                'Sinarea' = 'Synarea';
                'Symphillia' = 'Symphyllia';
                'Coeleoseris' = 'Coeloseris';
                'Coscineraea' = 'Coscinaraea';
                'Plerogira' = 'Plerogyra';
                'Echinphora' = 'Echinopora';
                'Galaxea Astreata' = 'Galaxea';
                'Galaxea Fascicularis' = 'Galaxea';
                'Galaxea Green' = 'Galaxea';
                'Galaxea Red' = 'Galaxea'")            
levels(as.factor(Mada$Genus))

head(Mada)

#Save compiled genus database to MacMon ESD compile and Global Corals final database for WCS Mada  
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Madagascar/WCS/ESD compiled files")
write.csv(Mada, "Mada_Coral Genus compile_Aug2015.csv", row.names = FALSE)

setwd("/Users/emilydarling/Dropbox/1-On the go/Coral Database/1_FULLDATA/Final datasets/WCS Mada")
write.csv(Mada, "Mada_Coral Genus compile_Aug2015.csv", row.names = FALSE)

