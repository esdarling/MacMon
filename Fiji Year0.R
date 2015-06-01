#note: packages loaded in .Rprofile

#code to calculate MacMon indicators
#======================================
#for Fiji year 0 baseline report


#bleaching susceptibility code
# - needs taxa susceptibilities
# - needs benthic data on coral community composition

#load taxa susceptibility
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Fiji/susceptibility files")
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
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Fiji/data")
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
head(hc)
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
  ungroup() %>%
  mutate(SSI_scale = (sumSuscept - min(sumSuscept)) / (max(sumSuscept) - min(sumSuscept))) %>%
  group_by(Location) %>%
  summarize(meanSSI = mean(SSI_scale), sdSSI = sd(SSI_scale)) 


## SSI is coral bleaching susceptibility
SSI
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Fiji/output")
write.csv(SSI, "SSI by location.csv", row.names = FALSE)


#fish bleaching susceptibility
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Fiji/susceptibility files")
d <- read.csv("Fish climate vulnerability for Emily.csv", 
              header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)
str(d) 
hist(d$Suscept_PopDecline)

#relative abundance of each species summed across Graham vulnerability index
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Fiji/data")
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
  group_by(Location)  %>%
  #average across transects to a site
  summarize(sumFishVuln = mean(sumFishVuln, na.rm = TRUE)) 

fishvuln          
hist(fishvuln$sumFishVuln)   

write.csv(fishvuln, "fish vulnerability x site.csv", row.names= FALSE)

#catch sensitivity to bleaching
#WAY TOO COMPLICATED for a donor indicator?

#occupational mobility
#Ranking occupations is problematic


#MSL PCA

setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Fiji/data")
d <- read.csv("HouseholdSurveys_MSL question.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)

#remove other & non 
d <- d[,-c(16,17,24,25,31,36,40,47,53,59)]
d <- na.omit(d)
nrow(d)


levels(as.factor(d$Village))


indicators <- d[,c(3:44)]
#check indicators will all zeros - need to remove
#car battery, bicyclce, motorcycle, charcoal, tile, bamboo
names(indicators)
nrow(indicators)

indicators2 <- indicators[,c(1:2,5:7,10:12,16,20,22:25,27:30,32,34,35,36,39:42)]
names(indicators2)
wealth.pca<-prcomp(indicators2, scale = TRUE, na.action = na.omit)

summary(wealth.pca)
wealth.pca
biplot(wealth.pca)   

d$wealth_PCA <- predict(wealth.pca)[,1]
hist(d$wealth_PCA)   
head(d)

boxplot(d$wealth_PCA ~ d$Village)

MSL <- d %>%
  group_by(Village) %>%
  summarize(MSL = mean(wealth_PCA), sd = sd(wealth_PCA)) %>%
  ungroup() %>%
  arrange(MSL)
MSL$value <- paste(round(MSL$MSL,2), round(MSL$sd,2), sep = (" ("))
MSL$value <- paste(MSL$value, ")", sep = "")
MSL$value 

setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Fiji/output")
write.csv(MSL, "MSL by village.csv", row.names = FALSE)


##MAINA CLIMATE - pulled from coral database
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Fiji/data")
env <- read.csv("fromMaina_stressmodel.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(env)
nrow(env)

env <- env %>%
  group_by(Location) %>%
  summarize(mean_exp = mean(multivariate.stress.model), sd_exp = sd(multivariate.stress.model)) 
env
env$value <- paste(round(env$mean_exp,2), round(env$sd_exp,2), sep = (" ("))
env$value <- paste(env$value, ")", sep = "")
env$value 

setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Fiji/output")
write.csv(env, "Maina stressmodel by Location.csv", row.names= FALSE)


##Household dependance on livelihoods
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Fiji/data")
d <- read.csv("Household finances.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)
names(d)[1] <- "id"
names(d)[6] <- "propIncome"

levels(as.factor(d$Type))
d$Source <- CA(d$Source)
levels(as.factor(d$Source))

d$Source2 <- ifelse(d$Source == "Bdm collection (fishing)" | d$Source == "Bdm-middlemen" |
                    d$Source == "Fishing" | d$Source == "Pafco - Tuna Cannery" | d$Source == "Selling Frozen Fish", 
                      "Fishing", ifelse(d$Source == "Animal Husbandry" | d$Source == "Copra" | d$Source == "Farming" | 
                      d$Source == "Selling Food/crops", 
                       "Farming", 
                        "Other"))
head(d)
#select appropriate sources for fishing / ag
#cast out by id
#replace NAs with 0s for mean calculations
#avg across villages

d2 <- d %>%
  filter(Type == "Income") %>%
  group_by(Village,id,Source2) %>%
  summarize(propIncome = mean(propIncome, na.rm = TRUE))

d2_cast <- dcast(d2, Village + id ~ Source2)
d2_cast[is.na(d2_cast)] <- 0
head(d2_cast)

d3 <- melt(d2_cast[,1:4], id.var = 1:2)
head(d3)

d4 <- d3 %>%
  group_by(Village, variable) %>%
  summarize(mean_income = mean(value))

d4 <- subset(d4, Village == "Kiobo" | Village =="Navatu" | Village == "Bua-Lomanikoro" |
               Village == "Dalomo" | Village == "Arovudi" | Village == "Taviya" |
               Village == "Nabukadra" | Village == "Nadogoloa")
d4$Village <- factor(d4$Village, levels = c("Kiobo","Navatu","Bua-Lomanikoro","Dalomo","Arovudi","Taviya",
                                            "Nabukadra","Nadogoloa"))
levels(drop.levels(as.factor(d4$Village)))

d4$variable <- factor(d4$variable, levels = c("Fishing","Farming"))

d4_cast <- dcast(d4, variable ~ Village)
d4_cast

setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Fiji/output")
write.csv(d4_cast, "Income dependence.csv")









        












