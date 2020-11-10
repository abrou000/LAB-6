# LAB-6
> pick_use2 <- (acs2017_ny$AGE >25) & (acs2017_ny$AGE <= 55)
> dat_use2 <- subset(acs2017_ny, pick_use2)
> 
the OLS model
> model_lm1 <- lm(LABFORCE ~ AGE + I(AGE^2) + female+ I(female*AfAm) + AfAm + Asian + race_oth + Hispanic + I(female*white) +I(female*Asian)
+             + educ_hs + educ_somecoll + educ_college + educ_advdeg 
+             + MARST + FAMSIZE+in_Bronx+in_Brooklyn+in_Manhattan+in_Queens+I(female*in_Bronx)+I(female*in_Brooklyn)+I(female*in_Manhattan)+I(female*in_Queens), data = dat_use2)
> summary(model_lm1)

Call:
lm(formula = LABFORCE ~ AGE + I(AGE^2) + female + I(female * 
    AfAm) + AfAm + Asian + race_oth + Hispanic + I(female * white) + 
    I(female * Asian) + educ_hs + educ_somecoll + educ_college + 
    educ_advdeg + MARST + FAMSIZE + in_Bronx + in_Brooklyn + 
    in_Manhattan + in_Queens + I(female * in_Bronx) + I(female * 
    in_Brooklyn) + I(female * in_Manhattan) + I(female * in_Queens), 
    data = dat_use2)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.07049  0.02017  0.12904  0.20444  0.60957 

Coefficients:
                           Estimate Std. Error t value Pr(>|t|)    
(Intercept)               1.603e+00  3.383e-02  47.395  < 2e-16 ***
AGE                       1.455e-03  1.664e-03   0.875 0.381844    
I(AGE^2)                 -2.793e-05  2.028e-05  -1.377 0.168490    
female                   -8.347e-02  8.679e-03  -9.617  < 2e-16 ***
I(female * AfAm)          1.566e-01  1.088e-02  14.401  < 2e-16 ***
AfAm                     -1.318e-01  6.150e-03 -21.422  < 2e-16 ***
Asian                     2.977e-03  7.362e-03   0.404 0.685990    
race_oth                 -1.742e-02  5.610e-03  -3.105 0.001906 ** 
Hispanic                  1.774e-02  4.529e-03   3.917 8.96e-05 ***
I(female * white)        -1.200e-02  8.615e-03  -1.393 0.163479    
I(female * Asian)        -7.077e-02  1.175e-02  -6.025 1.70e-09 ***
educ_hs                   1.943e-01  5.333e-03  36.439  < 2e-16 ***
educ_somecoll             2.817e-01  5.631e-03  50.024  < 2e-16 ***
educ_college              3.356e-01  5.605e-03  59.880  < 2e-16 ***
educ_advdeg               3.734e-01  5.889e-03  63.410  < 2e-16 ***
MARST                    -7.837e-03  6.951e-04 -11.274  < 2e-16 ***
FAMSIZE                   4.864e-03  8.731e-04   5.571 2.54e-08 ***
in_Bronx                  1.955e-02  9.242e-03   2.115 0.034397 *  
in_Brooklyn               3.588e-02  5.901e-03   6.080 1.21e-09 ***
in_Manhattan             -1.282e-02  8.368e-03  -1.532 0.125476    
in_Queens                 6.721e-02  6.340e-03  10.601  < 2e-16 ***
I(female * in_Bronx)     -4.495e-02  1.255e-02  -3.580 0.000343 ***
I(female * in_Brooklyn)  -3.330e-02  8.172e-03  -4.075 4.61e-05 ***
I(female * in_Manhattan)  1.942e-02  1.158e-02   1.677 0.093538 .  
I(female * in_Queens)    -5.927e-02  8.843e-03  -6.702 2.06e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3678 on 74910 degrees of freedom
Multiple R-squared:  0.09685,	Adjusted R-squared:  0.09656 
F-statistic: 334.7 on 24 and 74910 DF,  p-value: < 2.2e-16

the logit model
> acs2017_ny$LABFORCE <- as.factor(acs2017_ny$LABFORCE)
> levels(acs2017_ny$LABFORCE) <- c("NA","Not in LF","in LF")
> 
> acs2017_ny$MARST <- as.factor(acs2017_ny$MARST)
> levels(acs2017_ny$MARST) <- c("married spouse present","married spouse absent","separated","divorced","widowed","never married")
> 
> pick_use1 <- (acs2017_ny$AGE >25) & (acs2017_ny$AGE <= 55)
> dat_use1 <- subset(acs2017_ny, pick_use1)
> model_logit1 <- glm(LABFORCE ~ AGE,
+             family = binomial, data = dat_use1)
> acs2017_ny$age_bands <- cut(acs2017_ny$AGE,breaks=c(0,25,35,45,55,65,100))
> table(acs2017_ny$age_bands,acs2017_ny$LABFORCE)
          
              NA Not in LF in LF
  (0,25]   31680     11717 13256
  (25,35]      0      4271 20523
  (35,45]      0      4064 18924
  (45,55]      0      5406 21747
  (55,65]      0     10563 18106
  (65,100]     0     28701  5880
> pick_use1 <- (acs2017_ny$AGE >25) & (acs2017_ny$AGE <= 55)
> dat_use1 <- subset(acs2017_ny, pick_use1)
> 
> dat_use1$LABFORCE <- droplevels(dat_use1$LABFORCE) 
> # actually not necessary since logit is smart enough to drop unused levels, but helps my personal sense of order
> 
the baseline model1
> model_logit1 <- glm(LABFORCE ~ AGE + I(AGE^2) + female+ I(female*AfAm) + AfAm + Asian + race_oth + Hispanic + I(female*white) +I(female*Asian)
+             + educ_hs + educ_somecoll + educ_college + educ_advdeg 
+             + MARST + FAMSIZE+in_Bronx+in_Brooklyn+in_Manhattan+in_Queens+I(female*in_Bronx)+I(female*in_Brooklyn)+I(female*in_Manhattan)+I(female*in_Queens),
+             family = binomial, data = dat_use1)
> summary(model_logit1)

Call:
glm(formula = LABFORCE ~ AGE + I(AGE^2) + female + I(female * 
    AfAm) + AfAm + Asian + race_oth + Hispanic + I(female * white) + 
    I(female * Asian) + educ_hs + educ_somecoll + educ_college + 
    educ_advdeg + MARST + FAMSIZE + in_Bronx + in_Brooklyn + 
    in_Manhattan + in_Queens + I(female * in_Bronx) + I(female * 
    in_Brooklyn) + I(female * in_Manhattan) + I(female * in_Queens), 
    family = binomial, data = dat_use1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.8179   0.3112   0.4865   0.6488   1.6580  

Coefficients:
                             Estimate Std. Error z value Pr(>|z|)    
(Intercept)                 0.6603044  0.2467332   2.676 0.007446 ** 
AGE                         0.0091904  0.0122202   0.752 0.452013    
I(AGE^2)                   -0.0002146  0.0001486  -1.444 0.148798    
female                     -0.5443439  0.0603056  -9.026  < 2e-16 ***
I(female * AfAm)            0.9905662  0.0735524  13.467  < 2e-16 ***
AfAm                       -0.8968155  0.0419622 -21.372  < 2e-16 ***
Asian                       0.1142356  0.0671677   1.701 0.088989 .  
race_oth                   -0.1881902  0.0420738  -4.473 7.72e-06 ***
Hispanic                    0.1063801  0.0328660   3.237 0.001209 ** 
I(female * white)          -0.2235240  0.0599467  -3.729 0.000192 ***
I(female * Asian)          -0.6176920  0.0900000  -6.863 6.73e-12 ***
educ_hs                     0.9019487  0.0314136  28.712  < 2e-16 ***
educ_somecoll               1.4572879  0.0354379  41.122  < 2e-16 ***
educ_college                1.9524510  0.0377351  51.741  < 2e-16 ***
educ_advdeg                 2.3991469  0.0444752  53.943  < 2e-16 ***
MARSTmarried spouse absent -0.5119020  0.0529215  -9.673  < 2e-16 ***
MARSTseparated             -0.1050754  0.0583999  -1.799 0.071981 .  
MARSTdivorced               0.1007671  0.0385529   2.614 0.008956 ** 
MARSTwidowed               -0.2772601  0.0938662  -2.954 0.003139 ** 
MARSTnever married         -0.3607954  0.0261593 -13.792  < 2e-16 ***
FAMSIZE                     0.0296996  0.0063499   4.677 2.91e-06 ***
in_Bronx                    0.1536379  0.0654705   2.347 0.018942 *  
in_Brooklyn                 0.3440424  0.0500304   6.877 6.13e-12 ***
in_Manhattan               -0.0306836  0.0713433  -0.430 0.667135    
in_Queens                   0.6291748  0.0567268  11.091  < 2e-16 ***
I(female * in_Bronx)       -0.2570742  0.0855534  -3.005 0.002657 ** 
I(female * in_Brooklyn)    -0.3199232  0.0638158  -5.013 5.35e-07 ***
I(female * in_Manhattan)    0.0165528  0.0923269   0.179 0.857714    
I(female * in_Queens)      -0.5563385  0.0706910  -7.870 3.55e-15 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 71408  on 74934  degrees of freedom
Residual deviance: 64097  on 74906  degrees of freedom
AIC: 64155

Number of Fisher Scoring iterations: 5

#we see that a woman who is  white or Asian has a less likely to be in the laborforce than African American due to necessity or low skill which can caused them to work lest they lose their job.
however the woman with the more degree is most likely to be in the labor force.
also, the entrance in labor force is correalted to the borough you belong to. we see that living in brooklyn and queens are correleated negatively to the appartenace at the labor force with a respective -0.32 and -0.55 which is considerable. this can be explained to high presence of buisness hiring in the black.
we see age is not anymore significant, though he has positive influence over the presence in the labor force. this could be due to the number of variables to explain.

the analysis OLS Vs Logit
the significant variables are the same for both models. the logit models has higher estimates most of the time than the OLS model.



