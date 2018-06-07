---
title: "The Effect of Transmission Type on MPG"
author: "Austin L. Bistline"
date: "May 18, 2018"
output: pdf_document
---

## Summary

This document seeks to answer the question of which transmission type is better for miles per gallon, or mpg.  We explore the 'cars' dataset (included in the data package of R), and create several models in order to answer this question.  

## Introduction

In 1974 data was extracted from Motor Trend, a magazine about the automobile industry.  From that data we explore the relationship between a set of variables and miles per gallon (MPG) (outcome) with a particular interest in answering the following two questions:

"Is an automatic or manual transmission better for MPG"
"Quantify the MPG difference between automatic and manual transmissions"

This report is the result of a peer-graded assignment, the Regression Models course project created by Johns Hopkins University and distributed through Coursera.  

### Exploring the Data

We first will become familiar with the data by taking a look at the variables and first 6 rows, and then determining the size of the dataset.       


```r
rm(list = ls())
head(mtcars)
```

```
##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```


```r
dim(mtcars)
```

```
## [1] 32 11
```
The mtcars dataset is very small, with 32 rows and 11 columns.  From the help file, the column names are interpreted as:

1. Miles/(US) gallon
2. Number of Cylinders
3. Displacement
4. Gross Horsepower
5. Rear Axle Ratio
6. Weight (*1000)
7. 1/4 mile time
8. Cylinder orientation ('V' or Straight)
9. Transmission type (0=Automatic, 1=Manual)
10. Number of forward gears
11. Number of carburetors

Next, we would like to examine how the variables are correlated with one another. The help file includes some example code that I will include below for experimental purposes.


```r
require(graphics)
pairs(mtcars[, c("mpg", "disp", "wt", "hp", "am")], main = "mtcars data")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
We see overall that displacement, weight, and hp have a negative correlation to mpg, and that automatic transmissions (0) tend to have lower mpg than manual transmissions (1).  It is important to examine the displacement, weight, and hp separately by transmission type.  A table of averages will first be examined to observe the differences.


```r
acars = mtcars[mtcars$am==0, ]
mcars = mtcars[mtcars$am==1, ]
trans = c("Automatic", "Manual")

rmean = function(x){round(mean(x), 1)}

mpg = c(rmean(acars$mpg), rmean(mcars$mpg))
disp = c(rmean(acars$disp), rmean(mcars$disp))
wt = c(rmean(acars$wt), rmean(mcars$wt)) * 1000
hp = c(rmean(acars$hp), rmean(mcars$hp))

data.frame(Transmission = trans, MPG = mpg, Disp = disp, WT = wt, HP = hp)
```

```
##   Transmission  MPG  Disp   WT    HP
## 1    Automatic 17.1 290.4 3800 160.3
## 2       Manual 24.4 143.5 2400 126.8
```
On average, automatics have lower mpg, but they also have twice the displacement, are heavier, and have more horsepower.   


```r
coplot(mpg ~ disp | as.factor(am), data = mtcars, panel = panel.smooth, rows = 1)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
coplot(mpg ~ wt | as.factor(am), data = mtcars, panel = panel.smooth, rows = 1)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-2.png)

```r
coplot(mpg ~ hp | as.factor(am), data = mtcars, panel = panel.smooth, rows = 1)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-3.png)

```r
coplot(mpg ~ drat | as.factor(am), data = mtcars, panel = panel.smooth, rows = 1)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-4.png)

```r
coplot(mpg ~ qsec | as.factor(am), data = mtcars, panel = panel.smooth, rows = 1)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-5.png)
From this alone, we might say that we have our answer, but we must also see how the engine sizes change.
