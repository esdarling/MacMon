#note: packages loaded in .Rprofile

#code to calculate MacMon indicators
#======================================
#for Fiji year 0 baseline report


#bleaching susceptibility code
# - needs taxa susceptibilities
# - needs benthic data on coral community composition

#load taxa susceptibility
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Fiji/R_Emily")
d <- read.csv("FIJI_DataEntry_2012_TaxaSuscept.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
names(d)[1] <- "Genus" 
head(d)
nrow(d)
str(d) 

#recode Porites branching, massive to match benthic dataset
d$Genus <- recode(d$Genus, 
                  "'Porites (branching)' = 'Porites_branching';
                  'Porites (massive)' = 'Porites_massive'")
#create new dataframe for Porites to match benthic
#Porites suscept score is avg of Por_bra and Por_mas
Porites_newrow <- data.frame(Genus = "Porites", suscept = 1)
#add Faviidae new row - average of Faviids (moderate)
Faviidae_newrow <- data.frame(Genus = "Faviidae", suscept = 1)
#dplyr uses bind_rowa
d <- bind_rows(d, Porites_newrow,Faviidae_newrow)
head(d)

#recode suscept values into low, high moderate
#recode suscept score into values to multiple into susceptibility index
d$suscept_cat <- recode(d$suscept,"'0' = 'High';'1' = 'Moderate';'2' = 'Low'")
d$suscept_score <- recode(d$suscept_cat,"'High' = '2';'Moderate'='1';'Low' = '0'")


#load benthic community data
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Fiji/R_Emily")
benthic <- read.csv("FIJI_Benthic_Final_1042015.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 

#check n=100 Point USING PIPES!!!
#yes!
check <- benthic %>%
  group_by(Location,Site,Transect) %>%
  summarise(
    point_count = n())
hist(check$point_count)

#22 ecological sites
benthic %>%
  select(Location,Site) %>%
  distinct(Location, Site)

#clean hardcoral genus
head(benthic)
levels(as.factor(benthic$Genus))
benthic$Genus_clean <- CA(benthic$Genus)
benthic$Genus_clean <- recode(benthic$Genus_clean, 
                              "'Faviidae - Family' = 'Faviidae';
                              '' = 'Not_Identified'")
levels(as.factor(benthic$Genus_clean))

#PORITES is an issue - in hard coral need to us CM CB for Porites branching, Porites massive
head(benthic)
Porites_check <- head(subset(benthic, Genus_clean == "Porites"))
levels(as.factor(Porites_check$Life.Form))

benthic$Genus_clean <- ifelse(benthic$Genus_clean == "Porites" & benthic$Life.Form == "CB", "Porites_branching", 
                              ifelse(benthic$Genus_clean == "Porites" & benthic$Life.Form == "CM", "Porites_massive",
                                     benthic$Genus_clean))
levels(as.factor(benthic$Genus_clean))

#recode missing hardcoral w/genus to NA

#filter hardcorals out and calculate percent cover
#apple-shift-m is shortcut for pipes %>%
hc <- benthic %>%
  select(Location,Site,Transect,Point,Life.Form,Genus_clean,Coral) %>%
  filter(Coral == "Hardcoral") %>%
  group_by(Location,Site,Transect,Genus_clean) %>%
  summarize(sumPoint = n()) 
head(hc)

#need to calculate percent coral cover from sum(sum_Point) to calculate relative perc_cover
#pipes into different mutates are awesome
hc <- hc %>%
  group_by(Location,Site,Transect) %>%
  mutate(sumCover = sum(sumPoint)) %>%
  mutate(relCover = sumPoint / sumCover)
head(hc)

hist(hc$sumCover)
hist(hc$relCover)
names(hc)[4] <- "Genus"
head(hc)

#check genus merge from taxa susceptibility - names all good in taxa file? 
hc <- left_join(hc,d,by = "Genus")
head(hc)

#calculate site susceptibility index (SSI)
hc$relCover_x_suscept = hc$relCover * hc$suscept_score

hc <- hc %>%
  group_by(Location,Site,Transect) %>%
  mutate(SSI = sum(relCover_x_suscept, na.rm = TRUE))
head(hc)

ggplot(hc, aes(relCover, SSI)) + 
  geom_jitter(alpha = 0.5, aes(colour = suscept_cat)) + 
  stat_smooth(method = "gam") +
  facet_wrap(~ Genus, scales = "free") + 
  #theme(legend.position="none") 
ggsave("SSI x taxa.pdf")

SSI <- hc %>%
  group_by(Location,Site,Transect) %>%
  summarize(sumSuscept = sum(relCover_x_suscept, na.rm = TRUE))
head(SSI)            
hist(SSI$sumSuscept)   

#pipe to site-level SSI for MacMon indicator
#rescale SSI 0 to 1
SSI <- SSI %>%
  mutate(SSI = (sumSuscept - min(sumSuscept)) / (max(sumSuscept) - min(sumSuscept))) %>%
  group_by(Location, Site) %>%
  #summarize(SSI = mean(SSI)) %>%
  #group_by(Location) %>%
  summarize(meanSSI = mean(SSI), sdSSI = sd(SSI)) %>%
  ungroup() %>%
  arrange(desc(meanSSI))

write.csv(SSI, "SSI by site.csv", row.names = FALSE)





#fish bleaching susceptibility
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Fiji/R_Emily/susceptibility files")
d <- read.csv("Fish climate vulnerability for Emily.csv", 
              header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)
str(d) 
hist(d$Suscept_PopDecline)

#relative abundance of each species summed across Graham vulnerability index
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/Fiji/R_Emily/Fiji data")
fish <- read.csv("FIJI_Fish_Final.csv", 
              header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(fish)
nrow(fish)

#22 ecological sites
fish %>%
  select(Location,Site) %>%
  distinct(Location, Site)

#calculate spp relative abundance at each transect
fish <- fish %>%
  group_by(Location,Site,Transect) %>%
  mutate(totalAbund = sum(Abundance)) %>%
  mutate(relAbunda = Abundance / totalAbund)
head(fish)

#check genus merge from taxa susceptibility - names all good in taxa file? 
fish2 <- left_join(fish,d[,c(4,6)],by = "Species")
head(fish2)

#ungroup from Location, Site etc
#select columns I want to use
check <- fish2 %>%
  ungroup() %>%
  select(Species, Suscept_PopDecline) %>%
  distinct(Species) %>%
  arrange(Species)
#write.csv(check, "check Fiji fish merge with Graham.csv", row.names= FALSE)

#merge is good
#how much of community has a climate vuln score? 
fish3 <- fish2 %>%
  group_by(Location,Site,Transect) %>%
  filter(Suscept_PopDecline > 0) %>%
  mutate(Abund_withVuln = sum(Abundance)) %>%
  mutate(PercAbund_withVuln = Abund_withVuln / totalAbund)

#~25% of fish community included in climate vulnerability analysis (Fiji transects)
hist(fish3$PercAbund_withVuln)
mean(fish3$PercAbund_withVuln); sd(fish3$PercAbund_withVuln)

#calculate site susceptibility index (SSI)
fish3$relAbund_x_suscept = fish3$relAbund * fish3$Suscept_PopDecline

hist(fish3$relAbund_x_suscept)
min(fish3$relAbund_x_suscept); max(fish3$relAbund_x_suscept)

fishvuln <- fish3 %>%
  group_by(Location,Site,Transect) %>%
  #sum across species to a transect
  summarize(sumFishVuln = sum(relAbund_x_suscept, na.rm = TRUE)) %>%
  group_by(Location,Site)  %>%
  #average across transects to a site
  summarize(sumFishVuln = mean(sumFishVuln, na.rm = TRUE)) 

head(fishvuln)            
hist(fishvuln$sumFishVuln)   

write.csv(fishvuln, "fish vulnerability x site.csv", row.names= FALSE)

#catch sensitivity to bleaching
#WAY TOO COMPLICATED for a donor indicator?

#occupational mobility
#Ranking occupations is problematic


#MSL PCA







        












