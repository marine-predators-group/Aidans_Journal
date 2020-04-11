---
layout: post
title: Seasonal Surface Use and Location Interaction 
date: '2020-03-19'
tags:
  - markdown
  - R code
categories:
  - Miscellaneous
---

### Comparing Surface Use by Month (cont.)

Continuing from last week, I folowed the same procedures to generate bar plots of surface use across each month. I realized however, that the graphs did not provide an accurate comparison accross months because of the different number of observations (which consequently influenced the total observed surface time within that month). To resolve this issue, I divided the total number of observed surface hours by the total number of observed days in each month. Therefore, the axis technically represents the total number of surface hours per day; however, this is not an accurate metric because surface use varied significantly between individuals and days.

[a] Juvenile
```r
ggplot(data = J, aes(x = Month, y = Hours/Obs)) +
  geom_col(aes(fill = Month)) +
  xlab("Month") + 
  ylab("Surface Hours per Day") +
  theme_classic()
```
![Juvenile Surface use across months](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/JSurfMonth.pdf?raw=true)

[b] Sub-Adult
```r
ggplot(data = SA, aes(x = Month, y = Hours/Obs)) +
  geom_col(aes(fill = Month)) +
  xlab("Month") + 
  ylab("Surface Hours per Day") +
  theme_classic()
```
![Sub-Adult Surface use across months](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/SASurfMonth.pdf?raw=true)

[c] Adult
```r
ggplot(data = A3, aes(x = Month, y = (Minutes/60)/Obs)) +
  geom_col(aes(fill = Month)) +
  xlab("Month") + 
  ylab("Surface Hours per Day") +
  theme_classic()
```
![Adult Surface use across months](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/SurfMonth.pdf?raw=true)

---
### Location Track with Seasonal Aesthetic Mapping

To visualize the effect on surface use of the interaction between location and time of year, I decided to revisit my re-plot the location tracks and set the color aesthetic to month:

[a] Juvenile
```r
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
```
![Juvenile Surface location w/ month resolution](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/JMapMonth.pdf?raw=true)

[b] Sub-Adult
```r
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
```
![Sub-Adult Surface location w/ month resolution](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/MapMonth.pdf?raw=true)

[c] Adult
```r
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
```
![Adult Surface location w/ month resolution](https://github.com/marine-predators-group/Aidans_Journal/blob/master/images/AMonthMap.pdf?raw=true)

Note: When reading these plots, the symbol denotes the tag ID of individual fish. The color of the point denotes the month when that surface event was recorded (coloration is not consistent across plots unfortunately). Whereas previous plots included all track data, these plots only include surface observations. 

---



