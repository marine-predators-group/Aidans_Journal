---
layout: post
title: Marine Predators Group: Swordfish Surface Use Dossier
date: '2020-06-11 00:00'
tags:
  - summary
categories:
  - Miscellaneous
---

This document contains a summary of the initial data analysis exploring surface use among different populations of North Atlantic Swordfish. Here surface use is defined by occupaton of the top 0.5 meters of the water column. Below is a detailed account of the workflow, analysis, and handling of this data, including the code used to produce all results.

The code in this document is intended to be implemented sequentially, with unique objects created, modified, and referenced throughout. 

Because the depth measurements for surface observations were unclear, data from tag 110496 was not included in this analysis. However, because this tag was physically recovered and contains a much higher data resolution, it should be of high priority in future analysis. 

---
Summary: Thus far, the analysis of the available archival tag data for this group of swordfish suggests that surface use is a multi-variate behavior which cannot be accurately described by individual factors. Here we consider the effect of ontogenetic stage on general surface use and vertical habitat across three life stages. Furthermore, we specifically consider variation in surface use with time of day, gregorian month, geographic location, and individual weight. 
 The results of our statistical analysis could not reject the null hypothesis that, in general, juvenile, sub-adult, and adult swordfish spend roughly equal time at the surface. This result however seems suspicious upon visual inspection of the data and a review of the methods used here is in order. Greater depth distributions confirm that the vertical habitat does change across the life stages considered here. 
 The data suggest that swordfish surface use, across each age group is largely curpuscular, occuring in the hours of dawn and dusk. However, our analysis suggests that within this time-frame surface use is heavily skewed toward the hours approaching dawn than the hours after dusk. Juveniles also demonstrated more temporal variablity in surface use, spending more time in the top 0.5 meters of the water column during the day than any other group. This suggests that basking behavior specifically is more common amongst juvenilies although further analysis is required to confirm this hypthesis. 
 Surface use appears to vary significantly relative to the month of the year. Surface use among juveniles, sub-adults, and adults was greatest in December, November, and January respectively. Post-hoc tests revealed curious results which were heavily influenced by variations in the frequency of observations for each month. Despite low statistical power, we envision that this information justifies the reconstruction of oceanographic conditions during these months in order to identify factors which could have influenced an increase in surface use. 
 Juvenile swordfish demonstrated the greatest overlap in geographic habitat between individuals and two centers of surface use are identified here. While several swordfish were observed at the surface in each location, it is worth noting that only two individuals contributed most of the observations in both areas. Only three sub-adult swordfish were considered in this data and of those, only two were ever recorded at the surface. Because of this it is difficult to make inferences about the geographic patterns of surface use for this group. Further sampling of sub-adult swordfish is necessary in this region. Lastly, adult swordfish demonstrated diffuse surface use throughout the most broad geographic habitat of any group. Surface use appeared to be decentralized occuring infrequently thoughout the range of most tracks.
 We hypothesized that larger swordfish would be observed in surface waters less frequently due to their greater thermal intertia. However, the results of a linear regression demonstrate that there is no significant linear correlation between surface time and individual size. This was true within each age class as well as between all available data. Future work may aim to examine this relationship from a non-linear perspective. 
 Future anlysis effort should be aimed specifically at the data from tag 110496. While this tag was physically recovered and contained the greatest resolution of data, differences in recorded depth measurements made the distinction of surface depths particularly challenging. Initially, the lowest depth measure recorded (-3 meters) was considered equivalent to 0 meters based on feedback form a Wildlife Computers representative. When considered from this perspecitve, the frequency of surface observations was relatively similar to those of other tagged adult swords. However, this surface cut-off was later changed to -0.5 meters based on a series of data entries at the end of the tags duty cycle which we believe to represent observations where the tag was at the surface after the swordfish had been caught (the tag was recovered by a spanish longline fishing vessel). With this new surface cut-off, the data from tag 110496 demonstrated significanlty more surface observations than any other tagged organism. It is unclear whether this difference is due to an incorrect surface cut-off, or if the increased resolution of the recovered data reveals more fine-scale patterns in surface use by an adult swordfish. Further consideration is imperative to answer this question. 
 The next step in this project is to construct a series of generalized addative models which describe the pattern of surface use according to several factors including individual weight, time of day, SST, and chlorophyl alpha. We believe that the procedure for this modeling could closely follow that described in Abecassis et al., 2012.  

---
1. Cut-off Determination

In the initial analysis of the data it became clear that some but not all of the PSAT tags used to collect this data continued to record after they had detatched from swordfish on their programmed "burn date". This resulted in thousands of erronious surfacing observations at the end of several datasets, in which the tag was floating at the surface and no longer represented the activity of the study organism. An example of this can be seen in the raw data collected from tag 104672. 

In order to exclude erronious surface observations, the time of the last observation before the tag surfaced permanently was used as the cut-off time after which no data were considered. Datasets in which the tag continued recording after surfacing were identified by plotting the geolocation tracks for each tag. Instances in which the tag recorded the same location accross multiple days at the end of the track were flagged and inspected manually. Instances in which the tag remained constantly at the surface for a period of more than two hours at the end of its duty cycle were identified as erronious.   

```r
library(dplyr)
loc98751 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 98751)

ggplot(data = loc98751) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
## CUTOFF 12.18.2009 @ 00:20 ##
-----------------------------------------------------------------------------------------------------------
loc95975 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 95975)
ggplot(data = loc95975) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
# CUTOFF 7/19/2011 @ 23:50 #
-----------------------------------------------------------------------------------------------------------
loc98721 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 98721)
ggplot(data = loc98721) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
## CUTOFF 12.1.2011 @ 16:30 ##
-----------------------------------------------------------------------------------------------------------
loc98722 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 98722)
ggplot(data = loc98722) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
# CUTOFF 03.23.2012 @ 23:40 #
-----------------------------------------------------------------------------------------------------------
loc100976 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 100976)
ggplot(data = loc100976) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
# Many repetitive depth recordings at +1900m around burn date #
#CUTOFF 12.19.2010 @ 11:00 #
-----------------------------------------------------------------------------------------------------------
loc100978 <- all_swordhmoce_locs %>%
  filter(ptt == 100978)
ggplot(data = loc100978) +
  geom_jitter(aes(x = date, y = lat), color = "navy")
#CUTOFF 01.14.2011 @ 05:01 #
-----------------------------------------------------------------------------------------------------------
loc104668 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 104668)
ggplot(data = loc104668) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
## CUTOFF 11.23.2011 @ 22:00 ##
-----------------------------------------------------------------------------------------------------------
loc104670 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 104670)
ggplot(data = loc104670) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
# CUTOFF: 1.14.2011 @ 03:55 #
-----------------------------------------------------------------------------------------------------------
loc104671 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 104671)
ggplot(data = loc104671) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
# CUTOFF: 4.13.2012 @ 22:40 #
----------------------------------------------------------------------------------------------------------
loc104672 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 104672)
ggplot(data = loc104672) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
## CUTOFF: 1.05.2012 @ 16:40 ## 
----------------------------------------------------------------------------------------------------------
loc106788 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 106788)
ggplot(data = loc106788) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
# CUTOFF 03-25-2014 @ 05:52 #
----------------------------------------------------------------------------------------------------------
loc106795 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 106795)
ggplot(data = loc106795) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
## CUTOFF 11.02.2013 @ 20:07 ##
----------------------------------------------------------------------------------------------------------
loc110490 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 110490)
ggplot(data = loc110490) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
# CUTOFF 12.14.2011 @ 07:55 #
----------------------------------------------------------------------------------------------------------
loc110491 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 110491)
ggplot(data = loc110491) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
# CUTOFF 03.13.2012 @ 22:30 #
----------------------------------------------------------------------------------------------------------
loc110496 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 110496)
ggplot(data = loc110496) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
## CUTOFF 03.14.2012 @ 17:15:10 ##
----------------------------------------------------------------------------------------------------------
loc110497 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 110497)
ggplot(data = loc110497) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
# CUTOFF 12.18.2011 @ 22:50 #
----------------------------------------------------------------------------------------------------------
loc110498 <- all_sword_hmmoce_locs %>% 
  filter(ptt == 110498)
ggplot(data = loc110498) +
  geom_jitter(aes(x = date, y = lon), color = "navy")
# CUTOFF 12.21.2011 @ 22:00 #
----------------------------------------------------------------------------------------------------------

cutoff <- data.frame(
  ptt = c(98751, 95975, 98721, 98722, 100976, 104668, 104670, 104671, 104672, 106788, 106795, 110490, 
          110491, 110496, 110497, 110498, 100978, 100980),
  Age = c("J", "SA", "J", "J", "SA", "J", "J", "J", "J", "A", "A", "A", "A", "A", "A", "A", "SA", "A"),
  Month = c(12, 07, 12, 03, 12, 11, 01, 04, 01, 03, 11, 12, NA, 03, 12, 12, 01, 02), 
  Day = c(18, 19, 01, 23, 19, 23, 14, 13, 05, 25, 02, 14, NA, 14, 18, 21, 14, 28), 
  Year = c(2009, 2011, 2011, 2012, 2010, 2011, 2011, 2012, 2012, 2014, 2013, 2011, NA, 2012, 2011, 2011, 2011, 2011),
  Time = c(0020, 2350, 1630, 2340, 1100, 2200, 0355, 2240, 1640, 0552, 2007, 0755, NA, 1715, 2250, 2200, 0501, 1752)
)

```

The "cutoff" data frame above contains the cut-off dates for all tracks. 

These times should be considered in future efforts which may aim to establish a blanket cut-off for all individuals (ie. dicount the last 2 days of data observations for all individuals).

---
2. Proportion of Time Spent at Surface

In order to determine whether swordfish of different age classes, or stages of ontogeny, spend different amounts of time at the surface, we considered the proportion of time spent at the surface. This metric was calculated as the proportion out of the total number observations for each tag which occured at or above 0.5 meters depth.

A Welch's Heteroscedastic F-test with trimmed means and winzorized variance was selected to evaluate the statistical significance of observed differences. This test was chosen for its robustness to small sample sizes and non-normal data. 

```r
Juvenile <- data.frame(
  ptt = as.factor(c(98751, 104668, 104670, 104671, 104672, 98721, 98722)), 
  surf.prop = c(0, 0.0007733952, 0.0002344666, 0.030701154, 0.14478749, 0, 0.005639098), 
  age = as.factor(c("J", "J", "J", "J", "J", "J", "J"))
)

X104668_Series <- X104668_Series[-c(7426:7954),]
X104670_Series <- X104670_Series[-c(5082:5109),]
X104671_Series <- X104671_Series[-c(5082:5109),]
X104672_Series <- X104672_Series[-c(13834:14413),]
X98751_Series <- X98751_Series[-c(2236:2384),]
X98721_Series <- X98721_Series[-c(5096:5387),]

head(prop.table(table(X98751_Series$Depth)))
head(prop.table(table(X104668_Series$Depth)))
head(prop.table(table(X104670_Series$Depth)))
head(prop.table(table(X104671_Series$Depth)))
head(prop.table(table(X104672_Series$Depth)))
head(prop.table(table(X98721_Series$Depth))) # no depths more shallow than 60 meters
head(prop.table(table(X98722_Series$Depth)))
  
Sub_Adult <- data.frame(
  ptt = as.factor(c(100976, 100978, 95975)), 
  surf.prop = c(0.0370125578, 0, 0.0123058301), 
  age = as.factor(c("SA", "SA", "SA"))
)
  
X100978_Series <- X100978_Series[-c(4322:6672),] 

head(prop.table(table(X100976_Series$Depth)))
head(prop.table(table(X100978_Series$Depth)))
head(prop.table(table(X95975_Series$Depth)))

Adult <- data.frame(
  ptt = as.factor(c(100980, 110490, 110491, 110497, 110498, 106788, 106795)), 
  surf.prop = c(0, 0.0047128130, 0, 0.0001083013, 4.793175e-05, 0, 0.006924409), 
  age = as.factor(c("A", "A", "A", "A", "A", "A", "A"))
)

X106795_Series <- X106795_Series[-c(6933:7681),]

head(prop.table(table(X100980_Series$Depth)))
head(prop.table(table(X110490_Series$Depth)))
head(prop.table(table(X110491_Series$Depth)))
head(prop.table(table(X110497_Series$Depth)))
head(prop.table(table(X110498_Series$Depth)))
head(prop.table(table(X106788_Series$Depth)))
head(prop.table(table(X106795_Series$Depth)))

Swordfish.Surface <- rbind(Juvenile, Sub_Adult, Adult)

library(onewaytests)
homog.test(surf.prop ~ age, data = Swordfish.Surface, method = "Bartlett")
nor.test(surf.prop ~ age, data = Swordfish.Surface, method = "SW", plot = "qqplot-histogram")
welch.test(surf.prop ~ age, data = Swordfish.Surface, rate = 0.1)

```

Interpretation: The results of a Bartlett's homogeneity test demonstrate that the variances are not homogenous between groups (p = 4.15e-06). The distribution of surface proportions were non-normal among juveniles and adults acording to a Shapiro-Wilk Normality Test. Finally, the results of our Welch's test demonstrate that the difference in surface use was not significant accross ontogenetic stages. 
 In other words, this data supports the null hypothesis that amount of time spent at the surface does not vary with swordfish age. However, this is not to say that surface use remains the same across ontogeny. Visual inspection of the data suggests that these results may be incorrect. This is especially true with reguard to the adult group of swords which seem to spend extremely little time at the surface compared to the other two groups. A review and reapplication of the statistical tests chosen here is in order.  

Future Directions: There are several time resolutions accross the datasets and thus, future analysis may seek to re-examine the relationship between overall surface use and onotgeny by incorporating the specific time spent at surface for each individual rather than the proportion of observations.

---
3. Greater Depth Distribution

For context, the general vertical habitat and depth distribution of swordfish was considered across ontogenetic stages.  

```r
Juvenile <- rbind(X98751_Series, X104668_Series, X104670_Series,
                  X104671_Series, X104672_Series, X98721_Series, X98722_Series)

ggplot(data = Juvenile) +
  geom_histogram(aes(x = Depth), binwidth = 50, fill = NA, color = "grey22", na.rm = TRUE) +
  scale_x_reverse() +
  coord_flip() +
  ggtitle("Juvenile") +
  ylab("observations") +
  theme_classic()

Sub_Adult <- rbind(X100976_Series, X100978_Series, X95975_Series)

ggplot(data = Sub_Adult) +
  geom_histogram(aes(x = Depth), binwidth = 50, fill = NA, color = "grey22", na.rm = TRUE) +
  scale_x_reverse() +
  coord_flip() +
  ggtitle("Sub-Adult") +
  ylab("observations") +
  theme_classic()

Adult <- rbind(X100980_Series, X110490_Series, X110491_Series, X110497_Series, X110498_Series, 
               X106788_Series, X106795_Series)

ggplot(data = Adult) +
  geom_histogram(aes(x = Depth), binwidth = 50, fill = NA, color = "grey22", na.rm = TRUE) +
  scale_x_reverse() +
  coord_flip() +
  ggtitle("Adult") +
  ylab("observations") +
  theme_classic()
  
  ```
These are the resulting products:
! [JDepth_Dist](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/jd.dist.png?raw=true)

! [SADepth_Dist](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/sad.dist.png?raw=true)

! [ADepth_Dist](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/ad.dist.png?raw=true)

In these plots, binwidth is homogenous and is set at 50 meter intervals. The x-axis represents the total number of observations which occured in each bin across all data within each ontogenetic stage.

Depth distributions for each individual sword, with specific time resolution, can be found at:
[https://github.com/marine-predators-group/Aidans_Journal/tree/master/images/Depth_Distributions](https://github.com/marine-predators-group/Aidans_Journal/tree/master/images/Depth_Distributions)

Interpretation: Based on these figures, it would appear that juvenile swordfish occupy the most narrow and shallow depth range. Sub_Adult swords were recorded at the deepest depths across our data, however adults spent the most time at depth. 

Future Directions: In future analysis it may be helpful to display this data with variable binwidth such that the 0-0.5 depth range can clearly be identified. Furthermore, as in the previous section, time resolution could be applied to the dependent axis. This is a relatively simple task for individuals, made slightly more complicated by the fact that within each age class there are different data resolutions. 

---
4. Surface Use By Time of Day

To examine surface use in relation to patterns such as diel vertical migration and basking behavior, the amount of time spent at the surface was compared relative to time of day across swordfish ontogeny. This was accomplished by plotting time spent at the surface during each half-hour of the day for all individuals within each age class. 

```r
library(hms)

daytime <- as_hms(c("00:00:00", "00:30:00", "01:00:00", "01:30:00", "02:00:00", "02:03:00", "03:00:00", "03:30:00", "04:00:00", "04:30:00",
                    "05:00:00", "05:30:00",  "06:00:00", "06:30:00",  "07:00:00", "07:30:00", "08:00:00", "08:30:00",  "09:00:00", "09:30:00",
                    "10:00:00", "10:30:00",  "11:00:00", "11:30:00", "12:00:00", "12:30:00", "13:00:00", "13:30:00", "14:00:00", "14:30:00",
                    "15:00:00", "15:30:00", "16:00:00", "16:30:00", "17:00:00", "17:30:00", "18:00:00", "18:30:00", "19:00:00", "19:30:00",
                    "20:00:00", "20:30:00",  "21:00:00", "21:30:00", "22:00:00", "22:30:00", "23:00:00", "23:30:00"))
                    
br <- seq(0, 86400, by = 1800)

#Juvenile 
DSU.104668 <- filter(X104668_Series, Depth <= 0.5)
DSU.104670 <- filter(X104670_Series, Depth <= 0.5)
DSU.104671 <- filter(X104671_Series, Depth <= 0.5)
DSU.104672 <- filter(X104672_Series, Depth <= 0.5)
DSU.98722 <- filter(X98722_Series, Depth <= 0.5)

DSU.104668$Time <- as.numeric(DSU.104668$Time)
DSU.104670$Time <- as.numeric(DSU.104670$Time)
DSU.104671$Time <- as.numeric(DSU.104671$Time)
DSU.104672$Time <- as.numeric(DSU.104672$Time)
DSU.98722$Time <- as.numeric(DSU.98722$Time)


freq1 <- hist(DSU.104668$Time, breaks = br, include.lowest = TRUE, plot = FALSE)
freq2 <- hist(DSU.104670$Time, breaks = br, include.lowest = TRUE, plot = FALSE)
freq3 <- hist(DSU.104671$Time, breaks = br, include.lowest = TRUE, plot = FALSE)
freq4 <- hist(DSU.104672$Time, breaks = br, include.lowest = TRUE, plot = FALSE)
freq5 <- hist(DSU.98722$Time, breaks = br, include.lowest = TRUE, plot = FALSE)

unique(X104668_Series$Day)
(freq1$counts * 5) / 34

unique(X104670_Series$Day)
(freq2$counts * 5) / 24

unique(X104671_Series$Day)
(freq3$counts * 5) / 125

unique(X104672_Series$Day)
(freq4$counts * 5) / 51

unique(X98722_Series$Day)
(freq5$counts * 10) / 85


avrg.surf_time <- data.frame(
  Time = daytime, 
  Surf_Time = ((((freq1$counts * 5) / 34) + ((freq2$counts * 5) / 24) + ((freq3$counts * 5) / 125) + 
    ((freq4$counts * 5) / 51) + ((freq5$counts * 10) / 85)) / 7 )
    
)

ggplot(avrg.surf_time) +
  geom_point(aes(x = Time, y = Surf_Time)) +
  ylab("Average Surface Time (minutes)") +
  theme_classic()
  
#Sub_Adult 

DSU.100976 <- filter(X100976_Series, Depth <= 0.5)
DSU.95975 <- filter(X95975_Series, Depth <= 0.5)
DSU.100978 <- filter (X100978_Series, Depth <= 0.5)

DSU.100976$Time <- as.numeric(DSU.100976$Time)
DSU.95975$Time <- as.numeric(DSU.95975$Time)
DSU.100978$Time <- as.numeric(DSU.100978$Time)

freq <- hist(DSU.100976$Time, breaks = br, include.lowest = TRUE, plot = FALSE)
freq2 <- hist(DSU.95975$Time, breaks = br, include.lowest = TRUE, plot = FALSE)
freq3 <- hist(DSU.100978$Time, breaks = br, include.lowest = TRUE, plot = FALSE)

avrg.surf_time <- data.frame(
  Time = daytime, 
  Surf_Time = c((((freq$counts * 7.5) / 25) + ((freq2$counts * 10) / 156) / 3))
)

ggplot(avrg.surf_time) +
  geom_point(aes(x = Time, y = Surf_Time)) +
  ylab("Average Surface Time (minutes)") +
  theme_classic()
  
#Adult  
br <- seq(0, 86400, by = 1800)

X106795_Series <- X106795_Series[-c(6933:7681),]
DSU.106795 <- filter(X106795_Series, Depth <= 0.5)
DSU.110490 <- filter(X110490_Series, Depth <= 0.5)
DSU.110497 <- filter(X110497_Series, Depth <= 0.5)
DSU.110498 <- filter(X110498_Series, Depth <= 0.5)
DSU.106788 <- filter(X106788_Series, Depth <= 0.5)

DSU.106795$Time <- as.numeric(DSU.106795$Time)
DSU.110490$Time <- as.numeric(DSU.110490$Time)
DSU.110497$Time <- as.numeric(DSU.110497$Time)
DSU.110498$Time <- as.numeric(DSU.110498$Time)
DSU.106788$Time <- as.numeric(DSU.106788$Time)

freq1 <- hist(DSU.106795$Time, breaks = br, include.lowest = TRUE, plot = FALSE)
freq2 <- hist(DSU.110490$Time, breaks = br, include.lowest = TRUE, plot = FALSE)
freq3 <- hist(DSU.110497$Time, breaks = br, include.lowest = TRUE, plot = FALSE)
freq4 <- hist(DSU.110498$Time, breaks = br, include.lowest = TRUE, plot = FALSE)

unique(X106795_Series$Day)
(freq1$count * 7.5) / 37

unique(X110490_Series$Day)
(freq2$count * 5) / 91

unique(X110497_Series$Day)
(freq3$count * 5) / 89

unique(X110498_Series$Day)
(freq4$count * 5) / 91

avrg.surf_time <- data.frame(
  Time = daytime, 
  Surf_Time = ((((freq1$count * 7.5) / 37) + ((freq2$count * 5) / 91) + ((freq3$count * 5) / 89) + ((freq4$count * 5) / 91)) / 7)
)

ggplot(avrg.surf_time) +
  geom_point(aes(x = Time, y = (Surf_Time * 60))) +
  ylab("Average Surface Time (seconds)") +
  theme_classic()

```

These are the resulting products:
! [J](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/posts/J_DSU.Point.png?raw=true)

! [SA](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/posts/SA_DSU.Point.png?raw=true)

! [A](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/posts/Adult_DSU.Point.png?raw=true)

Daily surface use plots for each individual swordfish, with specific time resolution, can be found at:
[https://github.com/marine-predators-group/Aidans_Journal/tree/master/images/Daily_Surfacing](https://github.com/marine-predators-group/Aidans_Journal/tree/master/images/Daily_Surfacing)

Interpretation: These plots demonstrate that the majority of surfacing behavior is curpuscular as previous research has shown. However, surfacing appears to be skewed, occuring much more frequently in the hours approaching dawn than dusk. This pattern is most clear in the Sub_Adult and Adult groups. Among the Juveniles, there is a much higher percentage of surfacing which occurs during the day and surfacing behavior appears to be more temporally variable in general among the members of this group. 

Future Directions: Future efforts might specifically focus on identifying basking behavior (daytime surfacing events proceeded by a slow ascent from depth and a rapid descent). This data might support the hypothesis that juvenile swordfish demonstrate basking behavior more often and thus coroborate the hypothesis that basking behavior is used as a behavioral thermoregulatory strategy among swordfish. 

---
5. Surface Use by Location

The spatial distribution of surfacing behavior was examined for individuals in each age class. This was done in order to determine if swordfish surface more frequently when in certain habitats. Whether surfacing locations overlapped among swordfish of different ages was also a subject of curiosity where general habitat overlap was present. 

I began by creating a data frame with track data for individuals in each age class. Next I created a vector composed of the time in minutes each individual was recorded at the surface daily, in the same order as the days appeared in the data frame of track data. I plotted days in which time spent at the surface > 0 as solid points, while days in which no surface observations were recorded were plotted as open points. The size of the filled point corresponds to the amount of time spent at the surface in that location. 

```r
#Juvenile
uno <- filter(all_sword_hmmoce_locs, ptt == 104668)
dos <- filter(all_sword_hmmoce_locs, ptt == 104670)
tres <- filter(all_sword_hmmoce_locs, ptt == 104671)
quatro <- filter(all_sword_hmmoce_locs, ptt == 104672)
cinco <- filter(all_sword_hmmoce_locs, ptt == 98722)

J <- rbind(uno, dos, tres, quatro, cinco)

jminutes <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
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

#Sub_Adult 
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


saminutes <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
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

#Adults
uno <- filter(all_sword_hmmoce_locs, ptt == "110490")
dos <- filter(all_sword_hmmoce_locs, ptt == "110497")
tres <- filter(all_sword_hmmoce_locs, ptt == "110496")
quatro <- filter(all_sword_hmmoce_locs, ptt == "110498")
cinco <- filter(all_sword_hmmoce_locs, ptt == "106795")

A <- rbind(uno, dos, tres, quatro, cinco )
View(A)

library(tidyr)
name <- DSU.110496 %>% separate(Time, c("Time", "Day"), sep = "([// ])")

aminutes <- c(
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

```

These are the resulting products: 
! [J_SurfMap](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/A_SurfMap.png?raw=true)

! [SA_SurfMap](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/SA_SurfMap.png?raw=true)

! [A_SurfMap](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/A_SurfMap.png?raw=true) 

Interpretation: Among the juvenile swords, there appear to be two locations in which most of the surface observations are concentrated. One centered at 39ºN 30ºW, the other centered at 33ºN 31ºW. It is worth noting that only two individuals, 104671 and 104672 comprise the majority of these observations. Among the sub-adults, there is less overlap between the tracks of the two individuals which displayed surfacing behavior. Both individuals appear to display a behavior shift in which they transition from no surface use to heavy, daily surface use for a series of days near the middle of their time at liberty. Lastly, surface use among the adult swordfish was much more variable across location, with individuals visiting the surface for only short periods on a handful of days. The exception to this pattern is individual 110496. In this graph the surface was arbitrarily set as -0.5 meters for this individual. This metric was selected based on a series of presumed surface observations at the end of the dataset. Individual 110496 was recorded at the surface during the majority of it's days at liberty, and spent more time there than any other conspecifics. This could be due to 1) the value selected to represent the surface was, in actuality too deep and captured more of the swordfishes behavior; or 2) the recovered tag contains sufficient temporal resolution to track the true surface use of adult swordfish. 

Future Directions: Future work should be conducted to examine the data for individual 110496 in more detail. Analysis should focus on determining an accurate depth cut-off for surface observations which would allow the data to be incorporated into further analysis. Future work on data from 110496 might focus on discerning the average duration of surface visits for this individual. This metric could be used to assess whether or not the resolution of the other tags, which approach 10 minute increments in some cases, is sufficient to provide an accurate representation of surface use among swordfish. 
 Future studies might further examine the offshore locations noted here in order to confirm that surface use is more common in these locations. Similarly, future work may focus on surface use within the Sargasso Sea, where most of the surface observations for individual 110496 were concentrated. This region has a shallow oxygen minimum zone which is known to compress the virtical habitat of swordfish. 

---
6. Surface Use by Month

The frequency of surface observations were compared across months to determine whether the amount of surface use was homogenous throughout the study period. This was done by calculating the sum of surface time during each month for each individual within an age class. These sums were then divided by the number of days in each month which the tags were at liberty to calculate the aveage surface time for each individual. The grand mean was calculated for each age class as the average across individuals. 

As in section 5. of this document, track data for individuals of each age class were isolated for this analysis. However, the tracks were filtered to include only days in which surface observations were observed. Next, a vector containing a three letter month code of each observation was applied as a new column to the data frame, folowing the order of dates in the track data.

```r
#Juveniles
uno <- filter(all_sword_hmmoce_locs, ptt == 104668)
dos <- filter(all_sword_hmmoce_locs, ptt == 104670)
tres <- filter(all_sword_hmmoce_locs, ptt == 104671)
quatro <- filter(all_sword_hmmoce_locs, ptt == 104672)
cinco <- filter(all_sword_hmmoce_locs, ptt == 98722)

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


J <- rbind(uno, dos, tres, quatro, cinco)
J$Month <- jmonth
J$Month <- factor(J$Month, levels = c("OCT", "NOV", "DEC", "JAN", "FEB", "MAR", "APR"), ordered = TRUE)
J$minutes <- jminutes
J <- filter(J, J$minutes > 0)

(out <- aov.test(minutes ~ Month, data = J))
paircomp(out, adjust.method = "bonferroni")

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
  
#Sub_Adults
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
SA$Minutes <- saminutes
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
  
#Adults
uno <- filter(all_sword_hmmoce_locs, ptt == "110490")
dos <- filter(all_sword_hmmoce_locs, ptt == "110497")
tres <- filter(all_sword_hmmoce_locs, ptt == "110496")
quatro <- filter(all_sword_hmmoce_locs, ptt == "110498")
cinco <- filter(all_sword_hmmoce_locs, ptt == "106795")

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
A3$Minutes <- aminutes
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

```

These were the products:
! [J_SurfMap](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/posts/JSurfMonth.pdf?raw=true)

! [SA_SurfMap](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/posts/SASurfMonth.pdf?raw=true)

! [A_SurfMap](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/posts/SurfMonth.pdf?raw=true) 

Interpretation: ANOVA revealed a significant difference between monthly surface time values in all age classes, juveniles (p = 1.529253e-05), sub-adults (7.753209e-13), and adults (p = 5.329852e-33). Juvenile swordfish spent the greatest time at the surface in the month of December, however the results of post-hoc tests show that this difference is only significant with regard to the months of November and March. Adult swordfish spent the most time at the surface in the month of January, significantly longer than any other month except November and December. Sub-adult swordfish demonstrated an inverse pattern in which time spent at the surface reached a relative maximum in November before dropping to zero from January - March. Among sub-adults November surface time was significantly greater than all months except for December, May, and January (although surface time was very low in January there were relatively few recorded days at liberty making statistical inference difficult).    

Future Directions: Future analysis might use this information to inform the reconstruction of oceanographic conditions such as SST, cholophyl alpha, and submesoscale features present during months when surface use was identified as being highest. 

---
7. Surface Use by Weight

Surface use was considered relative to swordfish size. Larger animals have a lower surface area to volume ratio and, in general, belonged to the adult and sub-adult age classes which have more developed physiolgical structures for thermoregulation. Both of these factors confer greater thermal intertia to larger organisms and thus, we hypothesize that larger swordfish will spend more time at depth and surface less frequently than smaller individuals.

```r
Swordfish.Surface <- data.frame(
  ptt = as.factor(c(98751, 104668, 104670, 98721, 98722, 104671, 104672, 100976, 100980, 100978, 95975, 
                    110490, 110491, 110497, 110498, 106788, 106795)), 
  age = c("J", "J", "J", "J", "J", "J", "J", "SA", "A", "SA", "SA", "A", "A", "A", "A", "A", "A"), 
  weight = c(11.0, 19.5, 23.5, 12.0, 11.0, 17.5, 10.0, 50.0, 68.2, 45.5, 56.8, 81.8, 81.8, 65.9, 100.0, 90.9, 90.9), 
  surf.time = c(0, 25, 5, 0, 360, 3365, 9215, 1260, 0, 0, 2440, 480, 0, 10, 5, 0, 360),
  surf.prop = c(0, 0.0007733952, 0.0002344666, 0, 0.005639098, 0.030701154, 0.14478749, 0.0370125578, 0, 0, 0.0123058301, 
                0.0047128130, 0, 0.0001083013, 4.793175e-05, 0, 0.006924409)
)

ggplot(data = Swordfish.Surface, aes(x = weight, y = surf.time/60, color = age)) +
  geom_boxplot(aes(group = age, alpha = 10),color = "grey", width = 0.25, position = "dodge", outlier.shape = NA, show.legend = FALSE) +
  geom_point(aes(shape = age), position = "jitter") +
  geom_text(aes(label = ifelse((surf.prop > 0.1), paste(ptt, "\n", weight, ",", surf.time/60), "")), hjust = 0, nudge_x = 3, nudge_y = -0.005, show.legend = FALSE) +
  xlab("Weight(kg)") +
  ylab("Hours Spent Above 0.5m") +
  theme_classic()

mod <- lm(Swordfish.Surface$surf.time ~ Swordfish.Surface$weight)
summary(mod)

#Juvenile
Juvenile <- data.frame(
  ptt = as.factor(c(98751, 104668, 104670, 104671, 104672, 98721, 98722)), 
  surf.prop = c(0, 0.0007733952, 0.0002344666, 0.030701154, 0.14478749, 0, 0.005639098),
  weight = c(11.0, 19.5, 23.5, 17.5, 10.0, 12.0, 11.0),
  surf.time = c(0, 25, 5, 3365, 9215, 0, 360), 
  age = as.factor(c("J", "J", "J", "J", "J", "J", "J"))
)

ggplot(data = Juvenile, aes(x = weight, y = surf.time/60, color = age)) +
  geom_point(position = "jitter") +
  xlab("Weight(kg)") +
  ylab("Hours Spent Above 0.5m") +
  theme_classic()

mod <- lm(Juvenile$surf.time ~ Juvenile$weight)
summary(mod)

#Sub_Adult
Sub_Adult <- data.frame(
  ptt = as.factor(c(100976, 100978, 95975)), 
  surf.prop = c(0.0370125578, 0, 0.0123058301), 
  weight = c(50.0, 45.5, 56.8), 
  surf.time = c(1260, 0, 2440), 
  age = as.factor(c("SA", "SA", "SA"))
)

ggplot(data = Sub_Adult, aes(x = weight, y = surf.time/60, color = age)) +
  geom_point(position = "jitter") +
  xlab("Weight(kg)") +
  ylab("Hours Spent Above 0.5m") +
  theme_classic()

mod <- lm(Sub_Adult$surf.time ~ Sub_Adult$weight)
summary(mod)

#Adult
Adult <- data.frame(
  ptt = as.factor(c(100980, 110490, 110491, 110497, 110498, 106788, 106795)), 
  surf.prop = c(0, 0.0047128130, 0, 0.0001083013, 4.793175e-05, 0, 0.006924409), 
  weight = c(68.2, 81.8, 81.8, 65.9, 100.0, 90.9, 90.9), 
  surf.time = c(0, 0.0047128130, 0, 0.0001083013, 4.793175e-05, 0, 0.006924409),
  age = as.factor(c("A", "A", "A", "A", "A", "A", "A"))
)

ggplot(data = Adult, aes(x = weight, y = surf.time/60, color = age)) +
  geom_point(position = "jitter") +
  xlab("Weight(kg)") +
  ylab("Hours Spent Above 0.5m") +
  theme_classic()

mod <- lm(Adult$surf.time ~ Adult$weight)
summary(mod)
```

This was the result: 
! [surface_use_by_weight](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/posts/surface_by_weight.png?raw=true)

Interpretation: Based on the results of a linear regression, the data does not support our hypothesis that larger swordfish use the surface less frequently than smaller swordfish (p = 0.0865). This suggests that surface use among swordfish is a multivariate dimention of behavior which cannot be accurately modeled by considering only one variable.  

Future Directions: Futre analysis may seek to apply other, non-linear models to this data in order to explore the relationship between weight and surface use in greater depth. It may be pertinent to re-examine this relationship once more tag data has been aquired since our p-value was relativivly close to 0.05 and a linear model was able to explain a high percentage of the variability in the data (r^2 = 0.9633). 

---
