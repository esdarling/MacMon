#code to calculate MacMon indicators
#======================================
#for KENYA year 0 baseline report


#Fish landings database
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/1_MacMon data/R_Emily/Kenya")
d <- read.csv("Fishlanding data All 2014.csv", header = TRUE, 
              stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)

levels(as.factor(d$Year))
d <- subset(d, Year >= 2012)
nrow(d)

#pull pelagic & 0 fishers
d <- d %>%
  filter(Catch_category != "Pelagic")  %>% 
  filter(No_fishers > 0)
nrow(d)

check <- d %>%
  select(Catch_category) %>%
  distinct(Catch_category)

#remove entries with no fishers entered
d[which(is.na(d$No_fishers)),]
d[which(d$No_fishers == 0),]

#start here
d2 <- d %>%
  select(Date,Year,Site,Sector,Gear_new,No_fishers,catch_kg) %>%
  group_by(Date,Year,Site,Sector,Gear_new) %>%
  mutate(total_catch = sum(catch_kg)) %>%
  mutate(CPUE = total_catch / No_fishers) %>%

head(d2)
min(d2$total_catch); max(d2$total_catch) 
subset(d2, total_catch > 500)

hist(d2$CPUE)  
min(d2$CPUE); max(d2$CPUE)  
check <- subset(d2, CPUE > 50)

#gear CPUE
CPUE_gear <- d2  %>% 
  group_by(Site,Sector,Gear_new) %>%
  summarise(gearCPUE = median(CPUE, na.rm = TRUE))
write.csv(CPUE_gear, "CPUE_gear.csv", row.names = FALSE)

#total CPUE by sites
CPUE_total <- d2  %>% 
  group_by(Site,Sector) %>%
  summarise(totalCPUE = median(CPUE))
write.csv(CPUE_total, "CPUE_total.csv", row.names = FALSE)

#total CPUE by sector
CPUE_sector <- d2  %>% 
  group_by(Sector) %>%
  summarise(totalCPUE = median(CPUE))
write.csv(CPUE_sector, "CPUE_sector.csv", row.names = FALSE)



