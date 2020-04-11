##Making a histogram of Surface Use Accross Time and Day
library(dplyr)
DSU.110496 <- X110496_Archive %>% 
  filter(Depth <= 0.5)

head(table(X110496_Archive$Depth))

library(hms)
daytime <- as_hms(c("00:00:00", "01:00:00", "02:00:00", "03:00:00", "04:00:00", "05:00:00", "06:00:00", "07:00:00", "08:00:00", "09:00:00", "10:00:00", "11:00:00", "12:00:00", "13:00:00", "14:00:00", "15:00:00", "16:00:00", "17:00:00", "18:00:00", "19:00:00", "20:00:00", "21:00:00",  "22:00:00",  "23:00:00"))

ggplot(data = DSU.100976) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime) +
  facet_wrap( ~Day) +
  ggtitle("Daily Surface Use _ 100976")
#OR
ggplot(data = DSU.100976) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime) +
  ggtitle("Daily Surface Use _ 100976")

ggplot(data = Swordfish.Surface) +
  geom_boxplot(mapping = aes(x = Age, y = log(Surface.Minutes), color = Age))

##1.17.20 - Using onewaytests
library(onewaytests)
homog.test(Surface.Minutes ~ Age, data = Swordfish.Surface, method = "Bartlett")
nor.test(Surface.Minutes ~ Age, data = Swordfish.Surface, method = "SW", plot = "qqplot-histogram")
welch.test(Surface.Minutes ~ Age, data = Swordfish.Surface, rate = 0.1)

##1.23.20 - Using separate()
library(tidyr)
name <- DSU.110496 %>% separate(Time, c("Time", "Day"), sep = "([// ])")

##Adding Age Column to DSU data.frames
age <- "Sub.Adult", "Juvenile", "Adult"
DSU.100976$Age <- age

##1.24.20 - Merging data.frames
##When adding rows us rbind()
DSU.SubAdult <- rbind(DSU.100976, DSU.100978, DSU.95975)
DSU.Juvenile <- rbind(DSU.98751, DSU.104668, DSU.104670, DSU.104671, DSU.104672, DSU.98721, DSU.98722)
DSU.Adult <- rbind(DSU.110490, DSU.110497, DSU.110498, DSU.106795)
DSU.All <- rbind(DSU.SubAdult, DSU.Juvenile, DSU.Adult)

##Ultimate Daily Surface Use Figure
ggplot(data = DSU.All) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime, show.legend = FALSE) +
  facet_grid(DeployID ~ Age) +
  coord_flip()
    
ggplot(data = DSU.SubAdult) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime, show.legend = FALSE) +
  facet_wrap(~ DeployID) + 
  coord_flip() +
  ggtitle("Daily Surface Use in Sub.adults")

ggplot(data = DSU.Adult) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime, show.legend = FALSE) +
  facet_wrap(~ DeployID) + 
  coord_flip() +
  ggtitle("Daily Surface Use in Adults")
    
ggplot(data = DSU.Juvenile) +
  geom_histogram(mapping = aes(x = Time, fill = Day), breaks = daytime, show.legend = FALSE) +
  facet_wrap(~ DeployID) + 
  coord_flip() +
  ggtitle("Daily Surface Use in Juveniles")   

#1.28.20 - Depth Distribution by proportion
ggplot(X104672_Series, aes(x = Depth, y = ..count../sum(..count..))) +
  geom_histogram(bins = 40, color = "navy", fill = NA) +
  coord_flip() + scale_x_reverse() +
  ylab("Pdepth") +
  theme_classic() +
  ggtitle("Depth Probability") +
  

sutable <- prop.table(table(X104672_Series$Depth))

#1.30.20 - Surface Use Histogram w/ Time Resolution
ggplot(data = DSU.98751) +
  +   geom_histogram(aes(x = Time, y = (..count..)*7.5, fill = Day), breaks = daytime) +
  +   ggtitle("Daily Surface Use _ 98751") +
  +     ylab("Duration (minutes)") +
  +     xlab("Time of Day")

#1.31.20/1.32.20 - Adding Resolution Column to DSU.[Individual] data.frames
DSU."Individual"$Resolution <- Time.Resolution("Individual")

Time.Resolution <- function(x) {
  as.numeric(x[2,7] - x[1,7])
}


hms.Resolution <- function(x) {
  tvalue <- hms(
    Time.Resolution(x),
    0, 0)
}

#correcting 100978 from data.frames
Swordfish.Surface <- Swordfish.Surface[-c(16),]
new <- data.frame(Age = 'SA', Individual = '100978', Surface.Prop = 0)
Swordfish.Surface <- rbind(Swordfish.Surface, new)
#AND
SA <- SA[-c(2),]
SA <- rbind(SA, new)

#re-do statistics 
xbar_0.5m <- mean(Swordfish.Surface$Surface.Prop)
# - SEE 1.17.20 AND ONEWAYTESTS - #

#2.4.20 - Streamlining Resolution function

DSU <- function(x) {
  as.name(paste0(DSU., x))
} #Non-functional because DSU.100976 is not a name

CallDSU <- function(x) {
  View(
    DSU(x)
  )
} #Disregard

#Juvenile Adjustment: 
DSU.98751$Seconds <- hms.Resolution(X98751_Series)
DSU.104668$Seconds <- hms.Resolution(X104668_Series)
DSU.104670$Seconds <- 300
DSU.104671$Seconds <- hms.Resolution(X104671_Series)
DSU.104672$Seconds <- hms.Resolution(X104672_Series)
DSU.98721$Seconds <- hms.Resolution(X98721_Series)
DSU.98722$Seconds <- hms.Resolution(X98722_Series)

#SA Adjustment
DSU.95975$Seconds <- hms.Resolution(X95975_Series)
DSU.100976$Seconds <- hms.Resolution(X100976_Series)

#Adult Adjustment
DSU.110490$Seconds <- hms.Resolution(X110490_Series)
DSU.110497$Seconds <- hms.Resolution(X110497_Series)
DSU.110498$Seconds <- hms.Resolution(X110498_Series)
DSU.106795$Seconds <- hms.Resolution(X106795_Series)
DSU.110496$Seconds <- 5

#Wrangling all_sword_hmmoce_locs
unique(all_sword_hmmoce_locs[["ptt"]])
all_sword_hmmoce_locs$ptt <- factor(all_sword_hmmoce_locs$ptt, levels = c("95975", "98721", "98722", "100976", "100980", seq(104668, 104672), "106788", "106795", "110490", "110491", "110496", "110497", "110498", "98751"))

#2.7.20 - Obective code for rough time resolution plots
#Template 
ggplot(data = DSU.98751) +
  geom_histogram(aes(x = Time, y = (..count..)*hms.Resolution(X98751_Series), fill = Day), breaks = daytime) +
  ggtitle("Daily Surface Use _ 98751") +
  ylab("Duration (seconds)") + 
  xlab("Time of Day")

Primary <- function(x) {
  sprintf("Daily Surface Use_ %s",x)
}


ggdsu <- function(x,y,z) {
  ggplot(data = x) +
    geom_histogram(aes(x = Time, y = (..count..)*hms.Resolution(y), fill = Day), breaks = daytime) +
    ggtitle(Primary(z)) +
    ylab("Duration (seconds)") + 
    xlab("Time of Day")
}

#Filtering for erroneous surface data

library(dplyr)
loc98751 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 98751)

ggplot(data = loc98751) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 12.18.2009 @ 00:20 #
Swordfish.Surface <- Swordfish.Surface[-c(8),]
new <- data.frame(Age = 'J', Individual = '98751', Surface.Prop = 0)
Swordfish.Surface <- rbind(Swordfish.Surface, new)
Juvenile <- Juvenile[-c(1),]
Juvenile <- rbind(Juvenile, new)
-----------------------------------------------------------------------------------------------------------
loc95975 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 95975)
ggplot(data = loc95975) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 7/19/2011 @ 23:50 #
-----------------------------------------------------------------------------------------------------------
loc98721 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 98721)
ggplot(data = loc98721) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 12.1.2011 @ 16:30 #
Swordfish.Surface <- Swordfish.Surface[-c(13),]
new <- data.frame(Age = 'J', Individual = '98721', Surface.Prop = 0)
Swordfish.Surface <- rbind(Swordfish.Surface, new)
Juvenile <- Juvenile[-c(11),]
Juvenile <- rbind(Juvenile, new)
-----------------------------------------------------------------------------------------------------------
loc98722 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 98722)
ggplot(data = loc98722) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 03.23.2012 @ 23:40 #
-----------------------------------------------------------------------------------------------------------
loc100976 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 100976)
ggplot(data = loc100976) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# Many repetitive depth recordings at +1900m around burn date #
#CUTOFF 12.19.2010 @ 11:00 #
-----------------------------------------------------------------------------------------------------------
loc104668 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 104668)
ggplot(data = loc104668) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 11.23.2011 @ 22:00 #

DSU.104668 <- DSU.104668[-c(30:471),]
Juvenile[1,3] <- 0.003645964
Swordfish.Surface[8,3] <- 0.003645964
-----------------------------------------------------------------------------------------------------------
loc104670 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 104670)
ggplot(data = loc104670) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF: 1.14.2011 @ 03:55 #
-----------------------------------------------------------------------------------------------------------
loc104671 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 104671)
ggplot(data = loc104671) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF: 4.13.2012 @ 22:40 #
----------------------------------------------------------------------------------------------------------
loc104672 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 104672)
ggplot(data = loc104672) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF: 1.05.2012 @ 16:40 # 
DSU.104672 <- DSU.104672[-c(1844:2384),]
1843/13833
Swordfish.Surface[11,3] <-  0.1332321
Juvenile[4,3] <- 0.1332321
----------------------------------------------------------------------------------------------------------
loc106788 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 106788)
ggplot(data = loc106788) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 03-25-2014 @ 05:52 #
----------------------------------------------------------------------------------------------------------
loc106795 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 106795)
ggplot(data = loc106795) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 11.02.2013 @ 20:07 #
DSU.106795 <- DSU.106795[-c(49:767),]
48/6932
Swordfish.Surface[7,3] <- 0.006924409
Adult[7,3] <- 0.006924409
----------------------------------------------------------------------------------------------------------
loc110490 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 110490)
ggplot(data = loc110490) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 12.14.2011 @ 07:55 #
----------------------------------------------------------------------------------------------------------
loc110491 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 110490)
ggplot(data = loc110491) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 12.14.2011 @ 07:55 #
----------------------------------------------------------------------------------------------------------
loc110496 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 110496)
ggplot(data = loc110496) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 03.14.2012 @ 17:15:10 #
DSU.110496 <- DSU.110496[-c(397119:435098),]
397118/2578721
new <- data.frame(Age = "A", 
                  Individual = "110496",
                  Surface.Prop = 0.153998)
Swordfish.Surface <- rbind(Swordfish.Surface, new)
----------------------------------------------------------------------------------------------------------
loc110497 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 110497)
ggplot(data = loc110497) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 12.18.2011 @ 22:50 #
----------------------------------------------------------------------------------------------------------
loc110498 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 110498)
ggplot(data = loc110498) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
# CUTOFF 12.21.2011 @ 22:00 #
----------------------------------------------------------------------------------------------------------

#2.24.20 - Workflow for new swords
1. View(x)                                                 #determine rough cutoff
2. head() %>% prop.table() %>% table(x)                    #determine surface.prop
3. Swordfish.Surface2 <- data.frame(Individual = c(), 
                                    Surface.Prop = c())    #combine into data.frme
4. DSU.x <- filter( , Depth < 1)                           #make dsu table
5. DSU.x $Seconds <- hms.Resolution(x)                     #add time resolution

ggplot(data = Swordfish.Surface) +
  geom_violin(mapping = aes(x = Age, y = Surface.Prop, color = Age))

#2.21-20 - Wrangling dates in DSU.data for mapview
DSU.100976$Day[DSU.100976$Day=="27-Nov-2010"] <- as.Date("2010-11-27")
DSU.100976$Day[DSU.100976$Day=="28-Nov-2010"] <- as.Date("2010-11-28")
DSU.100976$Day[DSU.100976$Day=="29-Nov-2010"] <- as.Date("2010-11-29")
DSU.100976$Day[DSU.100976$Day=="30-Nov-2010"] <- as.Date("2010-11-30")
DSU.100976$Day[DSU.100976$Day=="01-Dec-2010"] <- as.Date("2010-12-01")
DSU.100976$Day[DSU.100976$Day=="02-Dec-2010"] <- as.Date("2010-12-02")
DSU.100976$Day[DSU.100976$Day=="03-Dec-2010"] <- as.Date("2010-12-03")
DSU.100976$Day[DSU.100976$Day=="07-Dec-2010"] <- as.Date("2010-12-07")
DSU.100976$Day[DSU.100976$Day=="09-Dec-2010"] <- as.Date("2010-12-09")

DSU.100976$Day <- as.numeric(DSU.100976$Day)
DSU.100976$Day <- as.Date(DSU.100976$Day, origin = "1970-01-01")

lon <- c(-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,-67.64126,
           -67.39505, -67.39505,-67.39505,-67.39505,-67.39505,-67.39505, -67.39505,-67.39505,-67.39505,-67.39505,-67.39505, -67.39505,-67.39505,-67.39505,-67.39505,-67.39505, -67.39505,-67.39505,-67.39505,-67.39505,-67.39505, -67.39505,-67.39505,-67.39505,-67.39505,-67.39505, -67.39505,-67.39505,-67.39505,-67.39505,-67.39505,-67.39505,
           -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, -66.63085, 
           -66.67595, -66.67595, -66.67595, -66.67595, -66.67595, 
           -66.35331, -66.35331 ,-66.35331, -66.35331, 
           -66.04232, -66.04232 ,-66.04232, -66.04232, -66.04232, -66.04232 ,-66.04232, -66.04232, -66.04232, -66.04232, -66.04232, -66.04232, -66.04232,
           -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583, -65.73583,
           -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711, -65.57711)

lat <- c(27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048, 27.60048,
         26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 26.85869, 
         25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277, 25.98277,
         25.73542, 25.73542, 25.73542, 25.73542, 25.73542, 
         25.62716, 25.62716, 25.62716, 25.62716,
         24.89117, 24.89117, 24.89117, 24.89117, 24.89117, 24.89117, 24.89117, 24.89117, 24.89117, 24.89117, 24.89117, 24.89117, 24.89117,
         23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261, 23.49261,
         22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903, 22.77903)

DSU.100976$Longitude <- lon
DSU.100976$Latitude <- lat


#2.25.20 Creating DSU sf objects

DSU_sf <- st_as_sf(DSU.100976, coords = c("Latitude", "Longitude"))

ggplot(data = DSU_sf) +
  geom_sf(position = "dodge") + 
  coord_sf() 


#3.4.20 Programming world maps tutorial
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html 

library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")

world <- ne_countries(scale = "medium", returnclass = "sf")

class(world)

ggplot(data = world) +
  geom_sf(color = "black", fill = "darkgreen") +
  theme_bw() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

library("ggspatial")
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))

## Scale on map varies by more than 10%, scale bar may be inaccurate

library("sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
           fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

ggplot(data = world) + 
  geom_sf(fill= "antiquewhite") + 
  geom_text(data= world_points,aes(x=X, y=Y, label=name), color = "darkblue", fontface = "bold", check_overlap = FALSE) + 
  annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", fontface = "italic", color = "grey22", size = 6) + 
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) + xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Map of the Gulf of Mexico and the Caribbean Sea") + theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))
---------------------------------------------------------------------------------------------------
View(all_sword_hmmoce_locs)

uno <- filter(all_sword_hmmoce_locs, all_sword_hmmoce_locs$ptt == "95975")
dos <- filter(all_sword_hmmoce_locs, all_sword_hmmoce_locs$ptt == "100976")

SubA <- rbind(uno, dos)

ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = SubB, aes(lon, lat, fill = ptt), size = 4, shape = 23) +
  annotate(geom = "text", x = -58, y = 35, label = "Atlantic Ocean", fontface = "italic", color = "grey22", size = 6) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  coord_sf(xlim = c(-82, -50), ylim = c(15,45), expand = FALSE) +
  xlab("longitude") +
  ylab("Latitude") +
  ggtitle("Surface locations among sub adult swords") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))


minutes <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           10, 20, 10, 20, 0, 0, 0, 0, 10, 20, 40, 
           30, 30, 30, 20, 10, 110, 100, 0, 220, 170, 200, 150, 270, 60,
           110, 0, 0, 80, 0, 0, 280, 0, 160, 0, 140, 
           0, 0, 0, 0, 0, 0, 0, 0, 
         140, 
         0, 
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 
           0, 0, 217.5, 240, 195, 37.5, 30, 0, 97.5, 0, 0, 0, 255, 0, 187.5, 
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  
SubA$minutes <-  minutes 

SubA <- filter(SubA, SubA$minutes > 0)


ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = SubA, aes(lon, lat, fill = ptt, size = hours), shape = 23, na.rm = TRUE) +
  geom_point(data =  SubB, aes(lon, lat, color = ptt), size = 3, shape = 23, fill = NA, alpha = 0.25) +
  annotate(geom = "text", x = -57, y = 34, label = "Atlantic Ocean", fontface = "italic", color = "grey22", size = 5) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  coord_sf(xlim = c(-82, -50), ylim = c(15,45), expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Surface locations among sub adult swords") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "blank", size = 0.5), panel.background = element_rect(fill = "aliceblue"))
---------------------------------------------------------------------------------------------------

uno <- filter(all_sword_hmmoce_locs, ptt == 104668)
dos <- filter(all_sword_hmmoce_locs, ptt == 104670)
tres <- filter(all_sword_hmmoce_locs, ptt == 104671)
quatro <- filter(all_sword_hmmoce_locs, ptt == 104672)
cinco <- filter(all_sword_hmmoce_locs, ptt == 98722)

J <- rbind(uno, dos, tres, quatro, cinco)

minutes <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 15, 10, 
           0, 0, 0, 120, 0, 0,
           
           0, 5, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0,
           
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 20, 70, 0, 0, 255, 315, 
           0, 0, 155, 0, 0, 0, 0, 0, 0, 0,
           25, 0, 0, 125, 0, 0, 0, 0, 0, 0,
           360, 365, 0, 0, 0, 0, 230, 0, 0, 0,
           0, 145, 0, 0, 0, 0, 0, 0, 0, 0, 
           0, 0, 80, 0, 0, 0, 130, 105, 0, 165,
           0, 100, 125, 90, 0, 0, 100, 130, 125, 0, 
           0, 0, 0, 20, 5, 0, 0, 0, 15, 0,
           0, 35, 0, 60, 0, 0, 0, 0, 20, 0,
           0, 0, 0, 0, 0, 0, 0, 10, 0, 0,
           0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0,
           30, 
           
           0, 0, 0, 0, 25, 0, 0, 235, 235, 5,
           0, 200, 0, 445, 465, 485, 780, 350, 260, 440,
           305, 280, 340, 335, 585, 265, 165, 255, 175, 365, 
           0, 65, 5, 275, 215, 175, 150, 85, 5, 25,
           35, 165, 15, 215, 245, 140, 0, 0, 95, 50,
           0, 260, 0, 0, 0, 0, 
           
           0, 0, 0, 0, 0, 0, 0, 90, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 20, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           10, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 30,  0,
           10, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0)

J$minutes <- minutes
J$Hours <- J$minutes / 60
J <- filter(J, J$minutes > 0)

J2 <- rbind(uno, dos, tres, quatro, cinco)
J2$minutes <- minutes
J2 <- filter(J2, J2$minutes == 0)

ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = J, aes(lon, lat, fill = ptt, size = Hours), shape = 24, na.rm = TRUE) +
  geom_point(data =  J2, aes(lon, lat, color = ptt), size = 3, shape = 24, fill = NA, alpha = 0.25) +
  annotate(geom = "text", x = -24, y = 38.5, label = "Azores Islands", fontface = "italic", color = "grey22", size = 3) +
  annotation_scale(location = "br", width_hint = 0.5) +
  coord_sf(xlim = c(-38, -21.5), ylim = c(27,42), expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Surface locations among juvenile swords") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "blank", size = 0.5), panel.background = element_rect(fill = "aliceblue"))

ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = J, aes(lon, lat, fill = ptt, size = Hours), shape = 24, na.rm = TRUE) +
  annotate(geom = "text", x = -24, y = 38.5, label = "Azores Islands", fontface = "italic", color = "grey22", size = 3) +
  annotation_scale(location = "br", width_hint = 0.5) +
  coord_sf(xlim = c(-38, -21.5), ylim = c(27,42), expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Surface locations among juvenile swords") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "blank", size = 0.5), panel.background = element_rect(fill = "aliceblue"))
---------------------------------------------------------------------------------------------------
  
uno <- filter(all_sword_hmmoce_locs, ptt == "110490")
dos <- filter(all_sword_hmmoce_locs, ptt == "110497")
tres <- filter(all_sword_hmmoce_locs, ptt == "110496")
quatro <- filter(all_sword_hmmoce_locs, ptt == "110498")
cinco <- filter(all_sword_hmmoce_locs, ptt == "106795")

A <- rbind(uno, dos, tres, quatro, cinco )
View(A)

library(tidyr)
name <- DSU.110496 %>% separate(Time, c("Time", "Day"), sep = "([// ])")

minutes <- c(
  0, 0, 5, 0, 0, 0, 10,
  135, 165, 0, 35, 0, 5, 15,
  50, 5, 0, 0, 40, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  10, 5, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 5, 0, 0, 0, 0,
  0, 0, 5, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 
  
  0, 0, 0.166, 0.166, 0, 0, 0.583,
  1.166, 6.916, 1.25, 0.25, 1, 63.833, 11.916, 
  7.25,  8.833, 1.083, 0.083, 1.416, 7.25, 12.5, 
  4.75, 36.833, 8.583, 100.75, 6.583, 46.083, 56.583, 
  109.333, 204.5, 317.5, 295.583, 355.833, 286, 434.25, 
  368, 324.33, 568.833, 628.5, 576.666, 259.833, 301.833, 
  480.916, 561.416, 608.166, 648.333, 609.916, 548.666, 458.25, 
  413.083, 301.416, 284.416, 144.583, 60.916, 30.25, 121.166, 
  119.166, 147.667, 195.166, 297.75, 163.083, 292.25, 533.916, 
  565.25, 270.75, 36.416, 92.416, 311.75, 571, 632, 
  633.666, 639, 627.833, 649.666, 490.833, 546.833, 505.75, 
  365.583, 471, 428.75, 417.166, 197.916, 371.083, 154, 
  264.333, 125.5, 226.333, 176.666, 355.166, 168.666, 206.916, 
  348.416, 273.583, 423.75, 250.833, 311.75, 459.333, 379.083, 
  250.916, 286.833, 382.833, 597, 183.333, 320.75, 415.083,
  327.583, 355.416, 313.333, 155.583, 310.75, 103, 96.916,
  56.083, 1.083, 130.916, 189.666, 146.5, 309.833, 290.75,
  426.5, 298.083, 442.666, 427.916, 288.666, 228.5, 20.416, 
  15, 52.75, 6.75, 7.583, 1.916, 0, 2.75,
  0.75, 0.833, 0, 0.416, 0.083, 2.75, 1.1667, 9.083,
  6.416, 1.916, 2.333, 18.5, 15.5, 2.5, 0.916, 
  0, 0, 
  
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 5, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  
  0, 0, 0, 0, 0, 0, 0,
  2, 0, 1, 4, 3, 5, 0,
  5, 3, 6, 4, 5, 0, 0,
  0, 0, 0, 0, 1, 0, 0,
  1, 1, 5, 0, 0, 1, 0,
  0, 1, NA, NA, NA, NA
)

View(minutes)

A$Minutes <- minutes
A <- filter(A, A$Minutes > 0)

A3 <- rbind(uno, dos, tres, quatro, cinco )
A2$Minutes <- minutes
A2 <- filter(A2, A2$Minutes == 0 )


ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = A, aes(lon, lat, fill = ptt, size = Minutes), shape = 21, na.rm = TRUE) +
  geom_point(data =  A2, aes(lon, lat, color = ptt), size = 3, shape = 21, fill = NA, alpha = 0.25) +
  annotate(geom = "text", x = -65, y = 40, label = "Atlantic Ocean", fontface = "italic", color = "grey22", size = 3) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  coord_sf(xlim = c(-77, -35.5), ylim = c(18,47), expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Surface locations among adult swords") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "blank", size = 0.5), panel.background = element_rect(fill = "aliceblue"))

#3.13.20 surface use by weight and month

weight <- c(68.2, 81.8, 81.8, 65.9, 100.0, 90.9, 90.9, 19.5, 23.5, 17.5, 10.0, 50.0, 56.8, 45.5, 11.0, 12.0, 11.0, 61.4)
Swordfish.Surface$Weight <- weight

surftime <- c(0, 480, 0, 10, 5, 0, 360, 145, 5, 3365, 9215, 1260, 2440, 0, 0, 0, 360, 33093)
Swordfish.Surface$Surface.Minutes <- surftime

ggplot(data = Swordfish.Surface, aes(x = Weight, y = Surface.Minutes/60, color = Age)) +
  geom_boxplot(aes(group = Age, alpha = 10),color = "grey", width = 0.25, position = "dodge", outlier.shape = NA, show.legend = FALSE) +
  geom_point(aes(shape = Age), position = "jitter") +
  geom_text(aes(label = ifelse((Surface.Prop > 0.1), paste(Individual, "\n", Weight, ",", Surface.Minutes/60), "")), hjust = 0, nudge_x = 3, nudge_y = -0.005, show.legend = FALSE) +
  xlab("Weight(kg)") +
  ylab("Hours Spent Above 0.5m") +
  theme_classic()


jmonth <- c("OCT",  "OCT",  "OCT",  "OCT",  "OCT",  "OCT",  "OCT",
            "OCT",  "OCT",  "OCT",  "OCT",  "NOV", "NOV", "NOV", 
            "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
            "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
            "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
            
            "OCT",  "OCT",  "OCT",  "OCT",  "OCT",  "OCT",  "OCT",
            "OCT",  "OCT",  "OCT",  "OCT",  "NOV", "NOV", "NOV", 
            "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
            "NOV", "NOV", "NOV", "NOV", "NOV",
            
            "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV",
            "NOV", "NOV", "NOV", "NOV","NOV", "NOV", "NOV",
            "NOV", "NOV", "DEC", "DEC", "DEC", "DEC", "DEC", 
            "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
            "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
            "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
            "DEC", "DEC", "DEC", "DEC", "DEC", "JAN", "JAN", "JAN", 
            "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", 
            "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", 
            "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", 
            "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", 
            "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", 
            "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", 
            "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", 
            "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB",
            "FEB", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", 
            "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", 
            "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", 
            "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", 
            "MAR", "MAR", "MAR", "MAR", "APR", "APR", "APR", 
            "APR", "APR", "APR", "APR", "APR", "APR", "APR", 
            "APR", "APR", "APR", 
            
            "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV",
            "NOV", "NOV", "NOV", "NOV","NOV", "NOV", "NOV",
            "NOV", "NOV", "DEC", "DEC", "DEC", "DEC", "DEC", 
            "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
            "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
            "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
            "DEC", "DEC", "DEC", "DEC", "DEC", "JAN", "JAN", "JAN", 
            "JAN", "JAN", "JAN", "JAN", "JAN", "JAN",
            
            
            "OCT",  "OCT", "OCT",  "OCT",  "OCT",  "OCT",  "NOV",
            "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV",
            "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV",
            "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
            "NOV", "NOV", "NOV", "NOV", "NOV", "DEC", "DEC", "DEC",
            "DEC", "DEC","DEC", "DEC", "DEC", "DEC", "DEC", "DEC",
            "DEC","DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
            "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC",
            "DEC", "DEC", "DEC", "DEC", "JAN", "JAN", "JAN","JAN", "JAN",
            "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN",
            "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN",
            "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN",
            "JAN", "JAN", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB",
            "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", 
            "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB",
            "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "MAR",
            "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", 
            "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", 
            "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR")


J$Month <- jmonth
J$Month <- factor(J$Month, levels = c("OCT", "NOV", "DEC", "JAN", "FEB", "MAR", "APR"), ordered = TRUE)

uno <- filter(J, Month == "OCT")
dos <- filter(J, Month == "NOV")
tres <- filter(J, Month == "DEC")
quatro <- filter(J, Month == "JAN")
cinco <- filter(J, Month == "FEB")
seis <- filter(J, Month == "MAR")
siete <- filter(J, Month == "APR")

sum(uno$Hours)
sum(dos$Hours)
sum(tres$Hours)
sum(quatro$Hours)
sum(cinco$Hours)
sum(seis$Hours)
sum(siete$Hours)

month <- c("OCT", "NOV", "DEC", "JAN", "FEB", "MAR", "APR")
omonth <- factor(month, c("OCT", "NOV", "DEC", "JAN", "FEB", "MAR", "APR"), ordered = TRUE)

J <- data.frame(Month = omonth, 
                Hours = c(0.08333333, 39.16667, 116.8333, 37.41667, 20.16667, 4.833333, 0.5), 
                Obs = c(28, 103, 93, 71, 58, 55, 13))

ggplot(data = J, aes(x = Month, y = Hours)) +
  geom_col(aes(fill = Month)) +
  xlab("Month") + 
  ylab("Surface Hours per Day") +
  theme_classic()

#3.19.20 Monthly Surface Use for Adults and Sub.Adults

A3 <- rbind(uno, dos, tres, quatro, cinco)

amonth <- c(
  "SEP", "SEP","SEP", "SEP", "SEP", "SEP", "SEP", 
  "SEP", "SEP", "SEP", "SEP", "SEP", "SEP", "SEP", 
  "SEP", "SEP", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
  "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
  
  "SEP", "SEP", "SEP", "SEP", "SEP", "SEP", "SEP", 
  "SEP", "SEP", "SEP", "SEP", "SEP", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "DEC", "DEC", "DEC", "DEC", 
  "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
  "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
  
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "DEC", "DEC", "DEC", "DEC", 
  "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
  "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
  "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
  "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "JAN", 
  "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", 
  "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", 
  "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", 
  "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN", 
  "JAN", "JAN", "FEB", "FEB", "FEB", "FEB", "FEB", 
  "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", 
  "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", 
  "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", 
  "FEB", "FEB", "FEB", "MAR", "MAR", "MAR", "MAR", 
  "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", 
  "MAR", "MAR", "MAR", 
  
  "SEP", "SEP", "SEP", "SEP", "SEP", "SEP", "SEP", 
  "SEP", "SEP", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", 
  "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
  "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
  "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
  
  "SEP", "SEP", "SEP", "SEP", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", "OCT", 
  "NOV", "NOV", "NOV", "NOV", "NOV", "NOV"
)

A3$Month <- amonth
A3$Minutes <- minutes
A3$Month <- factor(A3$Month, levels = c("SEP", "OCT", "NOV", "DEC", "JAN", "FEB", "MAR"), ordered = TRUE)

cero <- filter(A3, Month == "SEP")
uno <- filter(A3, Month == "OCT")
dos <- filter(A3, Month == "NOV")
tres <- filter(A3, Month == "DEC")
quatro <- filter(A3, Month == "JAN")
cinco <- filter(A3, Month == "FEB")
seis <- filter(A3, Month == "MAR")

sum(cero$Minutes)
sum(uno$Minutes)
sum(dos$Minutes, na.rm = TRUE)
sum(tres$Minutes)
sum(quatro$Minutes)
sum(cinco$Minutes)
sum(seis$Minutes)

month <- c("SEP", "OCT", "NOV", "DEC", "JAN", "FEB", "MAR")
omonth <- factor(month, c("SEP", "OCT", "NOV", "DEC", "JAN", "FEB", "MAR"), ordered = TRUE)

A3 <- data.frame(Month = omonth, 
                Minutes = c(430, 206.496, 6973.822, 11437.41, 10001.32, 4325.492, 61.5797), 
                Obs = c(41, 139, 126, 84, 31, 29, 14))

ggplot(data = A3, aes(x = Month, y = (Minutes/60)/Obs)) +
  geom_col(aes(fill = Month)) +
  xlab("Month") + 
  ylab("Surface Hours per Day") +
  theme_classic()

---
uno <- filter(all_sword_hmmoce_locs, all_sword_hmmoce_locs$ptt == "95975")
dos <- filter(all_sword_hmmoce_locs, all_sword_hmmoce_locs$ptt == "100976")

unique(uno$date)
unique(dos$date)

samonth <- c("JAN", "JAN", "JAN", "JAN", "JAN", "JAN", "JAN",
             "JAN", "JAN", "JAN", "JAN", "JAN", "FEB", "FEB", "FEB", 
             "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", 
             "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", 
             "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", "FEB", 
             "FEB", "FEB", "FEB", "FEB", "MAR", "MAR", "MAR",
             "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", 
             "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", 
             "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", 
             "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", "MAR", 
             "APR", "APR", "APR", "APR", "APR", "APR", "APR", 
             "APR", "APR", "APR", "APR", "APR", "APR", "APR", 
             "APR", "APR", "APR", "APR", "APR", "APR", "APR", 
             "APR", "APR", "APR", "APR", "APR", "APR", "APR", 
             "APR", "APR", "MAY", "MAY", "MAY", "MAY", "MAY", 
             "MAY", "MAY", "MAY", "MAY", "MAY", "MAY", "MAY", 
             "MAY", "MAY", "MAY", "MAY", "MAY", "MAY", "MAY", 
             "MAY", "MAY", "MAY", "MAY", "MAY", "MAY", "MAY", 
             "MAY", "MAY", "MAY", "MAY", "MAY", "JUN", "JUN", 
             "JUN", "JUN", "JUN", "JUN", "JUN", "JUN", "JUN", 
             "JUN", "JUN", "JUN", "JUN", "JUN", "JUN", "JUN", 
             "JUN", "JUN", "JUN", "JUN", "JUN", "JUN", "JUN", 
             "JUN", "JUN", "JUN", "JUN", "JUN", "JUN", "JUN", 
             "JUL", "JUL", "JUL", "JUL", "JUL", "JUL", "JUL", 
             "JUL", "JUL", "JUL", "JUL", "JUL", "JUL", "JUL", 
             "JUL", "JUL", "JUL", "JUL", "JUL", "JUL", 
             
             "NOV", "NOV", "NOV", "NOV", "NOV", "NOV", "DEC",
             "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
             "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", "DEC", 
             "DEC", "DEC", "DEC", "DEC"
             )

SA <- rbind(uno, dos)
SA$Month <- samonth
SA$Minutes <- minutes
SA$Hours <- SA$Minutes/60
SA$Month <- factor(SA$Month, levels = c("NOV", "DEC", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL"))

uno <- filter(SA, Month == "NOV")
dos <- filter(SA, Month == "DEC")
tres <- filter(SA, Month == "JAN")
quatro <- filter(SA, Month == "FEB")
cinco <- filter(SA, Month == "MAR")
seis <- filter(SA, Month == "APR")
siete <- filter(SA, Month == "MAY")
ocho <- filter(SA, Month == "JUN")
nueve <- filter(SA, Month == "JUL")

sum(uno$Hours)
sum(dos$Hours)
sum(tres$Hours)
sum(quatro$Hours)
sum(cinco$Hours)
sum(seis$Hours)
sum(siete$Hours)
sum(ocho$Hours)
sum(nueve$Hours)

month <- c("NOV", "DEC", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL")
omonth <- factor(month, c("NOV", "DEC", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL"), ordered = TRUE)

SA <- data.frame(
  Month = omonth, 
  Hours = c(11.5, 9.5, 0, 0, 0, 2.166667, 36.16667, 2.333333, 0), 
  Obs = c(6, 19, 12, 28, 31, 30, 31, 30, 20)
)

ggplot(data = SA, aes(x = Month, y = Hours/Obs)) +
  geom_col(aes(fill = Month)) +
  xlab("Month") + 
  ylab("Surface Hours per Day") +
  theme_classic()
---
library("ggspatial")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")

SubB <- filter(SubA, SubA$minutes > 0)
SubB$Month <- factor(SubB$Month, c("NOV", "DEC", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL"), ordered = TRUE)

ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = SubB, aes(lon, lat, color = Month, size = Hours, shape = ptt), na.rm = TRUE) +
  annotate(geom = "text", x = -57, y = 34, label = "Atlantic Ocean", fontface = "italic", color = "grey22", size = 5) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  coord_sf(xlim = c(-82, -50), ylim = c(15,45), expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Surface locations among sub adult swords") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "blank", size = 0.5), panel.background = element_rect(fill = "aliceblue"))


J2 <- filter(J, J$minutes > 0)
J2$Month <- factor(J2$Month, c("OCT", "NOV", "DEC", "JAN", "FEB", "MAR", "APR"), ordered = FALSE)

ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = J2, aes(lon, lat, color = Month, size = Hours, shape = ptt), na.rm = TRUE) +
  annotate(geom = "text", x = -24, y = 38.5, label = "Azores Islands", fontface = "italic", color = "grey22", size = 3) +
  annotation_scale(location = "br", width_hint = 0.5) +
  coord_sf(xlim = c(-38, -21.5), ylim = c(27,42), expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Surface locations among juvenile swords") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "blank", size = 0.5), panel.background = element_rect(fill = "aliceblue"))

A2 <- filter(A, A$Minutes > 0)
A2$Month <- factor(A2$Month, levels = c("SEP", "OCT", "NOV", "DEC", "JAN", "FEB", "MAR"), ordered = FALSE)
A2$Hours <- A2$Minutes / 60

ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = A2, aes(lon, lat, color = Month, size = Hours), shape = 23, na.rm = TRUE) +
  geom_point(data = SubB, aes(lon, lat, color = Month, size = Hours), shape = 22, na.rm = TRUE) +
  annotate(geom = "text", x = -65, y = 40, label = "Atlantic Ocean", fontface = "italic", color = "grey22", size = 3) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  coord_sf(xlim = c(-77, -35.5), ylim = c(18,47), expand = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Surface locations among West Atlantic swords") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "blank", size = 0.5), panel.background = element_rect(fill = "aliceblue"))
