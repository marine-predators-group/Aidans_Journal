---
author: aidancox12
comments: true
date: 2020-02-07 23:59:00+00:00
layout: post
title: Determining cutoffs for juvenile subgroup
tags:
  - markdown
  - R code
categories:
    - Misellaneous
---

After modifying the last of my data.frames earlier in the week, my original objective for the day 
was to write a general function for the time corrected daily surface use histograms. This was what
I came up with so far...

```r
Primary <- function(x) {
  sprintf("Daily Surface Use_ %s",x)
 
ggdsu <- function(x,y,z) {
  ggplot(data = x) +
    geom_histogram(aes(x = Time, y = (..count..)*hms.Resolution(y), fill = Day), breaks = daytime) +
    ggtitle(Primary(z)) +
    ylab("Duration (seconds)") + 
    xlab("Time of Day")
}

```

After writing this and testing it, I realized that it was necessary to go back and determine if there
were any other erroneous surface data (as was the case with 100978) before continuing making plots.

I determined that my workflow would be:
1. filter the all_sword_hmmoce_locs file into seperate data frames for each individual
2. make geom_jitter functions plotting Date x Latitude and look for recurring locations near the end of the data
  (I chose latitude of longitude so that I could more easily calculate the exact distance between values)
3. when recurring locations were present, go to the Series.csv file and determine the exact time the tag detatched
  (I chose to set this as the last reading before the tag registered at the surface and stayed there)
4. If necessary - recalculate the proportion of time spent at surface, excluding data after the cutoff, and edit my existing
  data.frames
  
Here is an example of the code that I used to accomplish this...
```r
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

```
I repeated this process for individuals:
- 98751
- 95975
- 98721
- 98722
- 100976
- 104668
- 104670
 
 # NOTE #
 Some of the tags continued to record location after detatchment, such as 95975, while others seemed to detatch
 (remained stationary for more than 24h) but did not surface, and continued to record depths of +1900 (indiv. 100976)
 
 Goals for next time:
 - Finish recording cutoffs for the rest of the individuals
 - Research code associated with map graphics in ggplot
 
 ---
 
