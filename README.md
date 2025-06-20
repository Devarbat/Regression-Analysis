# Regression-Analysis for Sales Forecasting

## Project Overview
This project conducts statistical regression modeling to analyze the influence of multiple 5G-related variables on the number of units sold for Samsung smartphones in North America from 2021 to 2024. The analysis uses R-Studio to evaluate the significance of predictors and assess model assumptions.  

## Objectives
The primary objective is to model the sales performance (Units Sold) of Samsung smartphones based on explanatory variables such as 5G capability, market share, regional coverage, subscriber base, and user preferences. The goal is to identify which features significantly impact sales and how companies can leverage this insight for strategic decision-making.

## Project Structure

### 1. Database Setup

- **Database Description**:
The dataset contains 53 observations and the following key variables:

Response Variable:
UnitsSold (Discrete)

Explanatory Variables:

X5GCapabilityBool (Discrete)

MarketSharePer (Continuous)

Regional5GCoveragePer (Continuous)

X5GSubscribersMillions (Continuous)

Avg5GSpeedMbps (Continuous)

Preferencefor5GPer (Continuous)

- **Model Setup**:
- 
```R
	setwd("A:/Clemson/Spring 2025/STAT 8030/Project")
	Mobile <- read.csv(file="Samsung model sales in North America (0-1).csv")
	Mobile [1,]
  attach(Mobile)
	M1 <- lm(UnitsSold ~ X5GCapabilityBool + MarketSharePer + Regional5GCoveragePer + X5GSubscribersMillions + Avg5GSpeedMbps + Preferencefor5GPer, data = Mobile)
```
- **Checking the normality of data**:

```R
	library(car)
	qqPlot(M1)

```
Output:

![image](Normality.png)
Normality Plot

![image](https://github.com/user-attachments/assets/573715e2-3bd9-45d8-8ebc-4d93f0523e3c)

Residual Plot

- **Checking the Multicollinearity**:

```R
	vif_func<-function(in_frame,thresh=10,trace=T,...){
  #library(fmsb)
  VIF <- function(X) { 1/(1-summary(X)$r.squared) }
  percentile <- function(dat) { # convert numeric vector into percentiles
    pt1 <- quantile(dat, probs=seq(0, 1, by=0.01), type=7) # set minimum 0 percentile.
    pt2 <- unique(as.data.frame(pt1), fromLast=TRUE)
    pt3 <- rownames(pt2)
    pt4 <- as.integer(strsplit(pt3, "%"))
    datp <- pt4[as.integer(cut(dat, c(0, pt2$pt1), labels=1:length(pt3)))]
    return(datp)
  }
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    in_dat<-in_fram
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh)
      vif_vals<-NULL
      var_names <- names(in_dat)
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      vif_max<-as.numeric(vif_vals[max_row,2])
      if(vif_max<thresh) break
      if(trace==T){ #print output of each iteration prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      } 
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]  
    }   
    return(names(in_dat))  
  }
}
	m <- Mobile[,-(1:4)]
	round(cor(m[,c(2,1,4:8)]),2)

```  
Output:

![image](https://github.com/user-attachments/assets/3ff83114-c7cd-434c-ae52-cf59dc4e6003)


```R
	vif(M1)
X5GCapabilityBool      MarketSharePer  Regional5GCoveragePer X5GSubscribersMillions         

2.838115               2.509538               1.211113               1.168005               

Avg5GSpeedMbps     Preferencefor5GPer 

1.102620               1.126178 
```

### 2. Data Analysis

-  **Full Model**

```R
M1 <- lm(UnitsSold ~ X5GCapabilityBool + MarketSharePer + Regional5GCoveragePer + X5GSubscribersMillions + Avg5GSpeedMbps + Preferencefor5GPer, data = Mobile)
	summary(M1)
```

Output:

```R
Call:
lm(formula = UnitsSold ~ X5GCapabilityBool + MarketSharePer + 
    Regional5GCoveragePer + X5GSubscribersMillions + Avg5GSpeedMbps + 
    Preferencefor5GPer, data = Mobile)
Residuals:
   Min     1Q Median     3Q    Max 
-34897 -12087   2336  13683  27537 
Coefficients:
                       Estimate Std. Error t value Pr(>|t|)   
(Intercept)            45408.55   16214.95   2.800  0.00744 **
X5GCapabilityBool      18362.05    8057.17   2.279  0.02736 * 
MarketSharePer         -3132.31    1729.86  -1.811  0.07672 . 
Regional5GCoveragePer    -86.08     155.45  -0.554  0.58245   
X5GSubscribersMillions   159.37     170.18   0.936  0.35391   
Avg5GSpeedMbps           -42.78      37.09  -1.153  0.25469   
Preferencefor5GPer       -51.31     155.79  -0.329  0.74339   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 16880 on 46 degrees of freedom
Multiple R-squared:  0.1815,	Adjusted R-squared:  0.07472 
F-statistic:   1.7 on 6 and 46 DF,  p-value: 0.1425
```

-  **Stepwise Method for model Selection**

 ```R
 	library(MASS)

	stp <- stepAIC(M1, 
             scope = list(upper = ~X5GCapabilityBool + MarketSharePer + Regional5GCoveragePer + X5GSubscribersMillions + Avg5GSpeedMbps + Preferencefor5GPer, lower = ~1),
               direction = "both")
```

Output:

```R
•	Start:  AIC=1038.27
UnitsSold ~ X5GCapabilityBool + MarketSharePer + Regional5GCoveragePer + 
    X5GSubscribersMillions + Avg5GSpeedMbps + Preferencefor5GPer
                         Df  Sum of Sq        RSS    AIC
- Preferencefor5GPer      1   30895616 1.3134e+10 1036.4
- Regional5GCoveragePer   1   87334130 1.3190e+10 1036.6
- X5GSubscribersMillions  1  249815408 1.3353e+10 1037.3
- Avg5GSpeedMbps          1  378957321 1.3482e+10 1037.8
<none>                                 1.3103e+10 1038.3
- MarketSharePer          1  933925082 1.4037e+10 1039.9
- X5GCapabilityBool       1 1479383223 1.4582e+10 1041.9

•	Step:  AIC=1036.39
UnitsSold ~ X5GCapabilityBool + MarketSharePer + Regional5GCoveragePer + 
    X5GSubscribersMillions + Avg5GSpeedMbps
                         Df  Sum of Sq        RSS    AIC
- Regional5GCoveragePer   1   97598586 1.3231e+10 1034.8
- X5GSubscribersMillions  1  235964559 1.3370e+10 1035.3
- Avg5GSpeedMbps          1  365599478 1.3499e+10 1035.8
<none>                                 1.3134e+10 1036.4
- MarketSharePer          1  905322951 1.4039e+10 1037.9
+ Preferencefor5GPer      1   30895616 1.3103e+10 1038.3
- X5GCapabilityBool       1 1464681762 1.4598e+10 1040.0

•	Step:  AIC=1034.78
UnitsSold ~ X5GCapabilityBool + MarketSharePer + X5GSubscribersMillions + 
    Avg5GSpeedMbps
                         Df  Sum of Sq        RSS    AIC
- X5GSubscribersMillions  1  335280143 1.3566e+10 1034.1
<none>                                 1.3231e+10 1034.8
- Avg5GSpeedMbps          1  524366453 1.3756e+10 1034.8
- MarketSharePer          1  829756498 1.4061e+10 1036.0
+ Regional5GCoveragePer   1   97598586 1.3134e+10 1036.4
+ Preferencefor5GPer      1   41160072 1.3190e+10 1036.6
- X5GCapabilityBool       1 1367742599 1.4599e+10 1038.0

•	Step:  AIC=1034.11
UnitsSold ~ X5GCapabilityBool + MarketSharePer + Avg5GSpeedMbps
                         Df  Sum of Sq        RSS    AIC
<none>                                 1.3566e+10 1034.1
- Avg5GSpeedMbps          1  601112002 1.4168e+10 1034.4
+ X5GSubscribersMillions  1  335280143 1.3231e+10 1034.8
+ Regional5GCoveragePer   1  196914171 1.3370e+10 1035.3
- MarketSharePer          1  964422053 1.4531e+10 1035.8
+ Preferencefor5GPer      1   25825710 1.3541e+10 1036.0
- X5GCapabilityBool       1 1841156721 1.5408e+10 1038.9
```

- **Reduced Model**

```R
	M2 <- lm(UnitsSold ~ X5GCapabilityBool + MarketSharePer 
+ Avg5GSpeedMbps, data = Mobile)
	summary(M2)
```

Output:

```R
Call:
lm(formula = UnitsSold ~ X5GCapabilityBool + MarketSharePer + 
    Avg5GSpeedMbps, data = Mobile)

Residuals:
   Min     1Q Median     3Q    Max 
-38505 -15559   3018  13381  30199 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)       42098.91    8032.22   5.241 3.37e-06 ***
X5GCapabilityBool 18677.20    7242.73   2.579    0.013 *  
MarketSharePer    -3086.72    1653.87  -1.866    0.068 .  
Avg5GSpeedMbps      -51.32      34.83  -1.473    0.147    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 16640 on 49 degrees of freedom
Multiple R-squared:  0.1525,	Adjusted R-squared:  0.1006 
F-statistic: 2.939 on 3 and 49 DF,  p-value: 0.04225
```

- **Comparing Full Model and Reduced Model**

```R
#Full Model
	extractAIC(M1)
[1]    7.000 1038.267 #AIC value

	extractAIC(M1, k=log(length(UnitsSold)))
[1]    7.000 1052.059 #BIC value



	Press <- resid(M1)/(1 - lm.influence(M1)$hat);sum(Press^2)
[1] 17876755195 #PRESS value

#Reduced Model
	extractAIC(M2)
[1]    4.000 1034.111 #AIC value

	extractAIC(M2, k=log(length(UnitsSold)))
[1]    4.000 1041.992 #BIC value

	Press2 <- resid(M2)/(1 - lm.influence(M2)$hat);sum(Press^2)
[1] 17876755195 #PRESS value

```

### 3. Questions

- 1] **How does each predictor uniquely contribute to predicting units sold? Which predictors significantly impact the number of units sold?**

Answer:Using the Full model, we can conclude that the predictor 5GCapabilityBool significantly contributes to predicting units sold, with a positive coefficient of 18,362 and a p-value of 0.027, indicating that 5G capable phones are associated with a higher number of units sold. MarketSharePer also shows a marginally significant negative impact (p = 0.077), suggesting that an increase in market share percentage may be linked to a slight reduction in units sold, possibly due to other influencing market dynamics. 
However, Regional5GCoveragePer, 5GSubscribersMillions, Avg5GSpeedMbps, and Preferencefor5GPer - do not have statistically significant effects on units sold, as their p-values are all well above the 0.05 threshold.
Also, as p-value for overall F-test is 0.1425 i.e > 0.05. Thus, we fail to reject the null hypothesis. Hence, the overall model is not statistically significant. The predictor may not reliably predict units sold. 

-- 2] **Does model capability, with or without 5G, impact on the units sold?**

Answer: Yes, model capability—specifically whether a phone has 5G or not—does impact the number of units sold. Using the reduced model, the variable 5GCapabilityBool has a statistically significant positive coefficient of 18,677 with a p-value of 0.013, which is below the 0.05 threshold. This means that, on average, models with 5G capability are associated with 18,677 more units sold compared to those without 5G, holding other factors constant. Therefore, the presence of 5G in a mobile device significantly increases its sales performance.

-- 3] **Does the increase in 5G subscribers impact Samsung models' sales and market share?**

Answer: Based on the full model, the p-value for X5GSubscribersMillions is 0.35391, which is greater than the 0.05 significance level. Thus, we fail to reject the null hypothesis, indicating that the number of 5G subscribers does not significantly contribute to predicting Units Sold. Therefore, we conclude that an increase in 5G subscribers does not have a significant impact on Samsung models’ sales or market share within this dataset.


### 3. Summary

This analysis explored the impact of various 5G and non-5G Samsung phone sales in North America from 2021 to 2024. Using multiple linear regression, the study assessed how predictors such as 5G capability, market share, regional 5G coverage, 5G subscribers, average 5G speed, and customer preference for 5G influence the number of units sold.
Initially, the full model was used. It revealed that among all predictors, only 5G capability showed a statistically significant positive effect on units sold. Market share displayed a marginally significant effect, while other variables did not show meaningful contributions. The overall model, however, was not statistically significant, indicating that the full set of predictors may not reliably explain the variability in unit sales.
To improve model efficiency, a stepwise regression was performed, resulting in a reduced model that retained only three predictors: 5G capability, market share, and average 5G speed. Further analysis of the reduced model showed that only 5G capability impacted units sold. Then, comparing the reduced model with the full model, the reduced model showed improved AIC and BIC values, suggesting a better balance of accuracy and simplicity. 

### 4. Key Takeaways

The key takeaway from this project is that Samsung’s 5G capability is a major driver of phone sales in the North American market. In contrast, factors such as regional 5G coverage, subscriber base, and average 5G speed do not significantly influence sales. Focusing on enhancing and promoting 5G-enabled models is likely to yield better sales performance for Samsung.

