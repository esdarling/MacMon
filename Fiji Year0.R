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




        












