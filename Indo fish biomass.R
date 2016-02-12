#Indo fish biomass paper

#read in clean data at site level
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Seascapes/Indonesia MacMon/Indo Biomass paper/for R")

d <- read.csv("DATA_Fish_Transect_All_2015_v5.csv", 
              header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)

par(mar = rep(1, 4))
hist(d$latitude)
hist(d$longitude)

unique(d$Region2)
unique(d$Management.Rule)
unique(d$Area.Status)
nrow(d)
names(d)
#write.csv(d[,c(1:8)], "Site names and GPS from biomass_28Jan2016.csv", row.names =FALSE)


#map
# pull Indo ggmap and layer with points- of sites
map <- get_map(location=c(lon = 116, lat = -6), zoom=4, 
               maptype="satellite", source="google", crop = FALSE)         
p <- ggmap(map)
p   

#check point locations, fix any errant latitude and longitudes
names(d)
p + geom_point(aes(x = longitude, y = latitude, colour = Region2), 
               size = 2, alpha = 0.25, 
               position = position_jitter(width = 0.5, height = 0.55), 
               data = d)

#missing 37 sites with GPS information
#first take at fish biomass gradient by sites
names(d)
head(d)

par(mar=rep(1, 4))
hist(d$Biomass)
qplot(d$Biomass)

min(d$Biomass, na.rm = TRUE); max(d$Biomass, na.rm = TRUE)

ggplot(aes(x=Biomass), data = d) + 
  geom_histogram(binwidth = 250) + 
  scale_x_continuous(limits = c(0, 32000), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,350), expand = c(0,0)) +
  theme_bw()


#cut Biomass into discrete groups
#replace values already in column ordered in datasheet (hah, that was smart)
names(d)
d$Biomass_bins <- as.numeric(cut2(d$Biomass, g=10))
head(d$Biomass_bins)

#plot of biomass in each bin
ggplot(aes(x = Biomass_bins, y = Biomass), data = d) +
  geom_point(shape = 21, alpha = 0.2) + 
  scale_y_continuous(trans='log10') +
  scale_x_discrete(breaks = seq(1,10,1), labels = seq(1,10,1)) +
  theme_bw()

head(d)
bin_summary <- d %>%
  select(Biomass_bins, Biomass) %>%
  group_by(Biomass_bins) %>%
  summarise(mean_biom = mean(Biomass, na.rm = TRUE),
            sd_biom = sd(Biomass, na.rm = TRUE),
            min_biom = min(Biomass, na.rm = TRUE),
            max_biom = max(Biomass, na.rm = TRUE))
write.csv(bin_summary, "Biomass bin summary.csv", row.names = FALSE)


#melt biomass groups to long
names(d)
d2 <- melt(d, id.vars = c(1:9))
head(d2)
unique(d2$variable)

ggplot(aes(x = Biomass_bins, y = value), data = d2) +
  geom_point(shape = 21, alpha = 0.2) + 
  facet_wrap(~ variable, scales = "free") +
  #scale_y_log10() +
  geom_smooth(method = "loess", na.rm = TRUE) +
  scale_x_discrete(breaks = seq(1,10,1), labels = seq(1,10,1)) +
  theme_bw()
  #stat_smooth(method = "loess", na.rm = TRUE)

#average trophic biomass across bins
head(d2)  
d3 <- d2  %>% 
  group_by(Biomass_bins, variable) %>%
  summarise(mean.biomass = mean(value, na.rm = TRUE))

head(d3)
ggplot(aes(x = Biomass_bins, y = mean.biomass), data = d3) +
  geom_point(shape = 21, alpha = 0.5, aes(colour = variable)) + 
  scale_y_continuous(trans='log10')

#STACKED BAR PLOT!!
head(d3)
ggplot(aes(x = Biomass_bins, y = mean.biomass), 
       data = subset(d3, variable != "Biomass")) +
  geom_bar(aes(fill = variable), stat = "identity")

#Which sites are in bin 10? 
head(d)
bin10 <- subset(d, Biomass_bins == 10)
nrow(bin10)

p + geom_point(aes(x = longitude, y = latitude, colour = Region2), 
               size = 3, alpha = 0.8, 
               position = position_jitter(width = 0.5, height = 0.55), 
               data = bin10)

#Biomass bin 10: management / area status
head(d2)

ggplot(aes(x = Management.Rule, y = value),
       data = subset(d2, Biomass_bins == 10)) +
  #geom_point() +
  geom_boxplot(aes(colour = variable)) + 
  facet_wrap(~variable, scales = "free")

unique(d2$Area.Status)
ggplot(aes(x = Area.Status, y = mean.value),
       data = d) +
  #geom_point() +
  geom_boxplot() + 
  facet_wrap(~variable, scales = "free")


#Compare biomass in 1st and 10th bins
bin1_10 <- subset(d, Biomass_bins == 1 | Biomass_bins == 10)
nrow(bin1_10)

p + geom_point(aes(x = longitude, y = latitude, fill = as.factor(Biomass_bins)), 
               size = 3, alpha = 0.5, shape = 21, colour = "black",
               position = position_jitter(width = 0.5, height = 0.55), 
               data = bin1_10) +
  scale_fill_manual("Biomass bin", values = c("red","white"))

#management / area status across bins
head(d2)
unique(d2$variable)
library(car)
d2$variable <- recode(d2$variable, 
                      "'Benthic.invertivore.1' = 'Invertivore.1';
                      'Coralivore.1' = 'Corallivore.1'")

unique(d2$Management.Rule)
d2$Management.Rule <- factor(d2$Management.Rule, 
                      levels = c("open access","gear restriction",
                                 "no take zone", "remote"))

ggplot(aes(x = Management.Rule, y = value),
       data = subset(d2, Biomass_bins == 1)) +
  #geom_point() +
  geom_boxplot(aes(colour = variable)) + 
  facet_wrap(~variable, scales = "free") +
  theme_bw(base_size = 14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Biomass bin 1: <250 kg/ha")

ggplot(aes(x = Management.Rule, y = value),
       data = subset(d2, Biomass_bins == 10)) +
  #geom_point() +
  geom_boxplot(aes(colour = variable)) + 
  facet_wrap(~variable, scales = "free") +
  theme_bw(base_size = 14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Biomass bin 10: 1600 to 30,000+ kg/ha")

unique(d2$Area.Status)
d2$Area.Status <- factor(d2$Area.Status, levels  = c("open access", "community reserve",
                                                     "government reserve",
                                                     "remote"))

ggplot(aes(x = Area.Status, y = value),
       data = subset(d2, Biomass_bins == 1)) +
  #geom_point() +
  geom_boxplot(aes(colour = variable)) + 
  facet_wrap(~variable, scales = "free") +
  theme_bw(base_size = 14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Biomass bin 1: <250 kg/ha")

ggplot(aes(x = Area.Status, y = value),
       data = subset(d2, Biomass_bins == 10)) +
  #geom_point() +
  geom_boxplot(aes(colour = variable)) + 
  facet_wrap(~variable, scales = "free") +
  theme_bw(base_size = 14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Biomass bin 10: 1600 to 30,000+ kg/ha")








#code from Transect datasets - OLD
#melt site-level biomass and trophic group biomass to long
names(d)
d2 <- d %>%
  select(Region2,Year,Area.Status,Management.Rule,Location,Site.Name,
         Depth, Transect, latitude, longitude,
         Biomass, Benthic.invertivore.1, Carnivore.1, Coralivore.1, Detritivore.1,
         Herbivore.1,Omnivore.1,Planktivore.1)
names(d2)  

d3 <- melt(d2, id.vars = 1:10)
head(d3) 


#average up to site-level
#add in missing lat/long by sites.. (instead of matching into repeated transects)
d4 <- d3 %>%
  group_by(Region2,Year,Area.Status,Management.Rule,
           Location,Site.Name,latitude,longitude,variable) %>%
  summarize(mean.value = mean(value, na.rm = TRUE))
head(d4)  
names(d4)

d5 <- dcast(d4, Region2+Year+Area.Status+Management.Rule+
              Location+Site.Name+latitude+longitude ~ variable)
head(d5)

uniquesites <- unique(d5$Site.Name)
length(uniquesites)

#1017 sites; 632 unique sites
unique(d5$Region2)
write.csv(d5, "test.csv", row.names = FALSE)

#####
# check join with gps sites for missing lat/longs in test.csv
nrow(d5)
head(d5)
names(d5)

missing <- subset(d5, is.na(d5$latitude))
head(missing)
nrow(missing)

#675 sites missing lat/long
unique(missing$Region2)

#read cleaned gps locations from Shinta
gps <- read.csv("GPS Coordinate_Indo Sites_ESD.csv", 
              header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(gps)
nrow(gps)
names(gps)

#some sites are replicated.. 
names(gps)

gps2 <- gps %>%
  select(Region2,Site.Name,latitude_gps, longitude_gps) %>%
  group_by(Region2,Site.Name) %>%
  summarize(latitude_gps = mean(latitude_gps), 
            longitude_gps = mean(longitude_gps))

unique(gps2$Region2)
names(missing)
names(gps2)

subset(gps2, Site.Name == "Loh Lawi")

#406 match in by Site.Name out of 675
missing2 <- inner_join(missing, gps2)
nrow(missing2)

##merge into original sites file
nrow(d5)
head(d5)

head(gps2)
d6 <- left_join(d5, gps2)

head(d6)

#good checks on latitude and longitude
plot(d6$latitude ~ d6$latitude_gps)
plot(d6$longitude ~ d6$longitude_gps)

d6$latitude <- ifelse(is.na(d6$latitude), d6$latitude_gps, d6$latitude)
d6$longitude <- ifelse(is.na(d6$longitude), d6$longitude_gps, d6$longitude)

names(d6)
write.csv(d6[,-c(17:18)], "DATA_Fish_Transect_All_2015_STP_v3.csv", row.names = FALSE)

##CURRENTLY plugging in missing gps by hand



## cleaning original file
#recode region
d$Region <- tolower(d$Region)
unique(d$Region)

d$Region2 <- recode(d$Region, 
                    "'kayoa-halmahera' = 'halmahera';
  'morotai-halmahera' = 'halmahera';
  'kofiau-rajaampat' = 'raja ampat';
  'misool' = 'raja ampat';
  'komodo' = 'ntt';
  'north sulawesi' = 'sulawesi';
  'lembeh' = 'sulawesi';
  'tbr' = 'taka bonarate'")
unique(d$Region2)

#remove derawan and add in later..
d <- d[-which(d$Region2 == 'derawan'),]
unique(d$Region2)

#clean metadata  
names(d)
d$Area.Status <- tolower(d$Area.Status)
unique(d$Area.Status)

d$Management.Rule <- tolower(d$Management.Rule)
unique(d$Management.Rule)


#####





###### OLD CODE

#add in new decimal degree lat/longs
setwd("/Users/emilydarling/Dropbox/4_WCS MacMon/Seascapes/Indonesia MacMon/Indo Biomass paper/for R")

d <- read.csv("DATA_Fish_Transect_All_2015_STP.csv", 
              header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)


#replace with conversions to decimal degrees
new.dd <- read.csv("DD_for new lat_long.csv", 
            header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(new.dd)
nrow(new.dd)
names(new.dd)

#join to dataset
d2 <- left_join(d,new.dd[,c(9,14,19)])
nrow(d2)

d2$latitude <- ifelse(is.na(d2$latitude_dd), d2$Lattitude, d2$latitude_dd)
d2$longitude <- ifelse(is.na(d2$longitude_dd), d2$Longitude, d2$longitude_dd)
names(d2)

write.csv(d2, "DATA_Fish_Transect_All_2015_STP_v2.csv", row.names = FALSE)

