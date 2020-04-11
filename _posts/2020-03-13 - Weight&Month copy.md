---
layout: post
title: Weight Driven and Seasonal Surface Use Patterns 
date: '2020-03-13'
tags:
  - markdown
  - R code
categories:
  - Miscellaneous
---

### Comparing Surface Use by Weight in Swordfish

I began by refencing the Braun et al. 2019 ICES paper and creating a vector with the weights of each individual. I then added these to my Swordfish.Surface data frame. 

This would allowed me to easily create a geom_point function comparing the two...

``` r
ggplot(data = Swordfish.Surface, aes(x = Weight, y = Surface.Prop, color = Age)) +
  geom_boxplot(aes(group = Age, alpha = 10),color = "grey", width = 0.25, position = "dodge", outlier.shape = NA, show.legend = FALSE) +
  geom_point(aes(shape = Age), position = "jitter") +
  geom_text(aes(label = ifelse((Surface.Prop > 0.1), paste(ptt, "\n", Weight, ",", Surface.Prop), "")), hjust = 0, nudge_x = 3, nudge_y = -0.005, show.legend = FALSE) +
  xlab("Weight(kg)") +
  ylab("Proportion of Time Spent Above 0.5m") +
  theme_classic()

```
![Surface Use By Weight](https://github.com/AidanCox12/Aidans_Journal/blob/master/images/Figures/SurfWeight.png?raw=true)

The grey box-and-whisker plots represent the distribution of surface use proportions within the age classes they are centered on; the two outliers in the data are labeled with their tag ID, weight (kg), and proportion of time spent at the surface

I did not run any regression analysis because there did not appear to be any new patterns in the data

After making this plot I realized that it would be more helpful to plot actual time spent at the surface rather than the proportion of time spent at the surface. This could be accomplished by:

1. Multiplying the time scale for each individual, recorded in the DSU. data frames, by the number of observations in the DSU. data frames
2. Applying the resultant values as a new column (or replacing Surface.Prop) in the Swordfish.Surface data frame

---
### Comparing Surface Use by Month

I began by working with data for the juvenile swords:

Similar to the proccess used to create SurfMap models, I manually entered a three leter month code for each date in the location track...

``` r
uno <- filter(all_sword_hmmoce_locs, ptt == 104668)
dos <- filter(all_sword_hmmoce_locs, ptt == 104670)
tres <- filter(all_sword_hmmoce_locs, ptt == 104671)
quatro <- filter(all_sword_hmmoce_locs, ptt == 104672)
cinco <- filter(all_sword_hmmoce_locs, ptt == 98722)

J <- rbind(uno, dos, tres, quatro, cinco)

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

```

Next I added the vector containing surface time in minutes for the juveniles, and another column with the minute scale converted to hours. I then tried to create a box plot using geom_bar using the non-functional code

``` r
ggplot(data = J) +
     geom_bar(aes(x = Month, y = Hours, color = Month))
```

This code failed because it attempted to apply a y aesthetic to geom_bar, which uses stat_count by default. 
In hindsight, I think I could have fixed this error by switching the "stat" argument from count to something else (but I'm not sure what I would have switched it to)...

Instead, I filtered the data points by month and summed the total hours spent at the surface for each month and stored these in a new data frame. I then used geom_col to directly reference the total surface time for each month:

```r
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

J <- data.frame(Month = as.factor(c("OCT", "NOV", "DEC", "JAN", "FEB", "MAR", "APR")), 
                Hours = c(0.08333333, 39.16667, 116.8333, 37.41667, 20.16667, 4.833333, 0.5))

ggplot(data = J, aes(x = reorder(Month, -Hours), y = Hours)) +
  geom_col(aes(fill = Month)) +
  xlab("Month") + 
  ylab("Hours spent at Surface") +
  theme_classic()
```
This is the resulting plot:
![IGV screencap showing Olurida_v081 intron/exon tracks](https://github.com/AidanCox12/Aidans_Journal/blob/master/images/Figures/SurfMonth.png?raw=true)

I could not figure out to order the x-axis chronologically, so I decided to order the columns by highest to lowest surface use.

It appears that surface use is dispraportionately higher in December for juveniles, however I have yet to perform any inferential statistics to confirm the significance of this difference. 

---

### Weekly Goals 

1. Finish Surface use by Month figures for the other age classes
2. Add specific time resolution to the Surface Use by Weight plot
3. Continue to develop inset plots for maps and DSU graphs



