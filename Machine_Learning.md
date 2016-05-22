Machine Learning Project
================

Background
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <http://groupware.les.inf.puc-rio.br/har> (see the section on the Weight Lifting Exercise Dataset).

Data Processing
---------------

load req. packages
==================

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.2.4

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.2.4

``` r
library(randomForest)
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(Hmisc)
```

    ## Warning: package 'Hmisc' was built under R version 3.2.5

    ## Loading required package: survival

    ## Warning: package 'survival' was built under R version 3.2.5

    ## 
    ## Attaching package: 'survival'

    ## The following object is masked from 'package:caret':
    ## 
    ##     cluster

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     combine

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, round.POSIXt, trunc.POSIXt, units

``` r
library(foreach)
library(doParallel)
```

    ## Loading required package: iterators

    ## Loading required package: parallel

import data
===========

``` r
data <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv" , na.strings = c("NA", ""))
```

Analyse and Data Cleansing
==========================

``` r
#summary(data)
#describe(data)
sapply(data, class)
```

    ##                        X                user_name     raw_timestamp_part_1 
    ##                "integer"                 "factor"                "integer" 
    ##     raw_timestamp_part_2           cvtd_timestamp               new_window 
    ##                "integer"                 "factor"                 "factor" 
    ##               num_window                roll_belt               pitch_belt 
    ##                "integer"                "numeric"                "numeric" 
    ##                 yaw_belt         total_accel_belt       kurtosis_roll_belt 
    ##                "numeric"                "integer"                 "factor" 
    ##      kurtosis_picth_belt        kurtosis_yaw_belt       skewness_roll_belt 
    ##                 "factor"                 "factor"                 "factor" 
    ##     skewness_roll_belt.1        skewness_yaw_belt            max_roll_belt 
    ##                 "factor"                 "factor"                "numeric" 
    ##           max_picth_belt             max_yaw_belt            min_roll_belt 
    ##                "integer"                 "factor"                "numeric" 
    ##           min_pitch_belt             min_yaw_belt      amplitude_roll_belt 
    ##                "integer"                 "factor"                "numeric" 
    ##     amplitude_pitch_belt       amplitude_yaw_belt     var_total_accel_belt 
    ##                "integer"                 "factor"                "numeric" 
    ##            avg_roll_belt         stddev_roll_belt            var_roll_belt 
    ##                "numeric"                "numeric"                "numeric" 
    ##           avg_pitch_belt        stddev_pitch_belt           var_pitch_belt 
    ##                "numeric"                "numeric"                "numeric" 
    ##             avg_yaw_belt          stddev_yaw_belt             var_yaw_belt 
    ##                "numeric"                "numeric"                "numeric" 
    ##             gyros_belt_x             gyros_belt_y             gyros_belt_z 
    ##                "numeric"                "numeric"                "numeric" 
    ##             accel_belt_x             accel_belt_y             accel_belt_z 
    ##                "integer"                "integer"                "integer" 
    ##            magnet_belt_x            magnet_belt_y            magnet_belt_z 
    ##                "integer"                "integer"                "integer" 
    ##                 roll_arm                pitch_arm                  yaw_arm 
    ##                "numeric"                "numeric"                "numeric" 
    ##          total_accel_arm            var_accel_arm             avg_roll_arm 
    ##                "integer"                "numeric"                "numeric" 
    ##          stddev_roll_arm             var_roll_arm            avg_pitch_arm 
    ##                "numeric"                "numeric"                "numeric" 
    ##         stddev_pitch_arm            var_pitch_arm              avg_yaw_arm 
    ##                "numeric"                "numeric"                "numeric" 
    ##           stddev_yaw_arm              var_yaw_arm              gyros_arm_x 
    ##                "numeric"                "numeric"                "numeric" 
    ##              gyros_arm_y              gyros_arm_z              accel_arm_x 
    ##                "numeric"                "numeric"                "integer" 
    ##              accel_arm_y              accel_arm_z             magnet_arm_x 
    ##                "integer"                "integer"                "integer" 
    ##             magnet_arm_y             magnet_arm_z        kurtosis_roll_arm 
    ##                "integer"                "integer"                 "factor" 
    ##       kurtosis_picth_arm         kurtosis_yaw_arm        skewness_roll_arm 
    ##                 "factor"                 "factor"                 "factor" 
    ##       skewness_pitch_arm         skewness_yaw_arm             max_roll_arm 
    ##                 "factor"                 "factor"                "numeric" 
    ##            max_picth_arm              max_yaw_arm             min_roll_arm 
    ##                "numeric"                "integer"                "numeric" 
    ##            min_pitch_arm              min_yaw_arm       amplitude_roll_arm 
    ##                "numeric"                "integer"                "numeric" 
    ##      amplitude_pitch_arm        amplitude_yaw_arm            roll_dumbbell 
    ##                "numeric"                "integer"                "numeric" 
    ##           pitch_dumbbell             yaw_dumbbell   kurtosis_roll_dumbbell 
    ##                "numeric"                "numeric"                 "factor" 
    ##  kurtosis_picth_dumbbell    kurtosis_yaw_dumbbell   skewness_roll_dumbbell 
    ##                 "factor"                 "factor"                 "factor" 
    ##  skewness_pitch_dumbbell    skewness_yaw_dumbbell        max_roll_dumbbell 
    ##                 "factor"                 "factor"                "numeric" 
    ##       max_picth_dumbbell         max_yaw_dumbbell        min_roll_dumbbell 
    ##                "numeric"                 "factor"                "numeric" 
    ##       min_pitch_dumbbell         min_yaw_dumbbell  amplitude_roll_dumbbell 
    ##                "numeric"                 "factor"                "numeric" 
    ## amplitude_pitch_dumbbell   amplitude_yaw_dumbbell     total_accel_dumbbell 
    ##                "numeric"                 "factor"                "integer" 
    ##       var_accel_dumbbell        avg_roll_dumbbell     stddev_roll_dumbbell 
    ##                "numeric"                "numeric"                "numeric" 
    ##        var_roll_dumbbell       avg_pitch_dumbbell    stddev_pitch_dumbbell 
    ##                "numeric"                "numeric"                "numeric" 
    ##       var_pitch_dumbbell         avg_yaw_dumbbell      stddev_yaw_dumbbell 
    ##                "numeric"                "numeric"                "numeric" 
    ##         var_yaw_dumbbell         gyros_dumbbell_x         gyros_dumbbell_y 
    ##                "numeric"                "numeric"                "numeric" 
    ##         gyros_dumbbell_z         accel_dumbbell_x         accel_dumbbell_y 
    ##                "numeric"                "integer"                "integer" 
    ##         accel_dumbbell_z        magnet_dumbbell_x        magnet_dumbbell_y 
    ##                "integer"                "integer"                "integer" 
    ##        magnet_dumbbell_z             roll_forearm            pitch_forearm 
    ##                "numeric"                "numeric"                "numeric" 
    ##              yaw_forearm    kurtosis_roll_forearm   kurtosis_picth_forearm 
    ##                "numeric"                 "factor"                 "factor" 
    ##     kurtosis_yaw_forearm    skewness_roll_forearm   skewness_pitch_forearm 
    ##                 "factor"                 "factor"                 "factor" 
    ##     skewness_yaw_forearm         max_roll_forearm        max_picth_forearm 
    ##                 "factor"                "numeric"                "numeric" 
    ##          max_yaw_forearm         min_roll_forearm        min_pitch_forearm 
    ##                 "factor"                "numeric"                "numeric" 
    ##          min_yaw_forearm   amplitude_roll_forearm  amplitude_pitch_forearm 
    ##                 "factor"                "numeric"                "numeric" 
    ##    amplitude_yaw_forearm      total_accel_forearm        var_accel_forearm 
    ##                 "factor"                "integer"                "numeric" 
    ##         avg_roll_forearm      stddev_roll_forearm         var_roll_forearm 
    ##                "numeric"                "numeric"                "numeric" 
    ##        avg_pitch_forearm     stddev_pitch_forearm        var_pitch_forearm 
    ##                "numeric"                "numeric"                "numeric" 
    ##          avg_yaw_forearm       stddev_yaw_forearm          var_yaw_forearm 
    ##                "numeric"                "numeric"                "numeric" 
    ##          gyros_forearm_x          gyros_forearm_y          gyros_forearm_z 
    ##                "numeric"                "numeric"                "numeric" 
    ##          accel_forearm_x          accel_forearm_y          accel_forearm_z 
    ##                "integer"                "integer"                "integer" 
    ##         magnet_forearm_x         magnet_forearm_y         magnet_forearm_z 
    ##                "integer"                "numeric"                "numeric" 
    ##                   classe 
    ##                 "factor"

``` r
#str(data)
```

remove irrelvent column from equation
=====================================

``` r
data <- data[,-c(1:7)]
featurenames<- colnames(data[colSums(is.na(data))==0])
features <- data[featurenames]
```

After cleaning the dataset , divide data set in training and testing
====================================================================

``` r
partdata <- createDataPartition(y=features$classe, p =.7, list = FALSE)
training <- features[partdata,]
testing <- features[-partdata,]
```

Use prediction Algorithm on training dataset
============================================

``` r
registerDoParallel()
model <- foreach(ntree=rep(150, 4), .combine=randomForest::combine) %dopar% randomForest(training[-ncol(training)], training$classe, ntree=ntree)
```

Evaluate model
==============

``` r
predictionsTraining <- predict(model, newdata=training)
confusionMatrix(predictionsTraining,training$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 3906    0    0    0    0
    ##          B    0 2658    0    0    0
    ##          C    0    0 2396    0    0
    ##          D    0    0    0 2252    0
    ##          E    0    0    0    0 2525
    ## 
    ## Overall Statistics
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9997, 1)
    ##     No Information Rate : 0.2843     
    ##     P-Value [Acc > NIR] : < 2.2e-16  
    ##                                      
    ##                   Kappa : 1          
    ##  Mcnemar's Test P-Value : NA         
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
    ## Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
    ## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

Apply model on test set
=======================

``` r
predictionsTesting <- predict(model, newdata=testing)
confusionMatrix(predictionsTesting,testing$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1674   14    0    0    0
    ##          B    0 1124    5    0    0
    ##          C    0    1 1021    7    0
    ##          D    0    0    0  956    4
    ##          E    0    0    0    1 1078
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9946          
    ##                  95% CI : (0.9923, 0.9963)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9931          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   0.9868   0.9951   0.9917   0.9963
    ## Specificity            0.9967   0.9989   0.9984   0.9992   0.9998
    ## Pos Pred Value         0.9917   0.9956   0.9922   0.9958   0.9991
    ## Neg Pred Value         1.0000   0.9968   0.9990   0.9984   0.9992
    ## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
    ## Detection Rate         0.2845   0.1910   0.1735   0.1624   0.1832
    ## Detection Prevalence   0.2868   0.1918   0.1749   0.1631   0.1833
    ## Balanced Accuracy      0.9983   0.9929   0.9967   0.9954   0.9980
