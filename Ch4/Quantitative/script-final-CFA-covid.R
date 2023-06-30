library(dplyr)
library(tidyr)
library(skimr)
library(purrr)
library(lavaan)
library(semPlot)
library(semTools)
library(nonnest2)
library(ggplot2)

d <- readRDS("DB-final-paper.rds")

skim(d)


model <- '
  # measurement
  # Tr = trust
   # Govt gov
   # Univ uni/educ
   # Comp priv
  # DP proteccion datos
  # OS open source
  Tr =~ Govt + Univ + Comp
  # reg
  Down ~ Tr + DP + OS
  # q30 ~ Down % tdr 0.4
  # residual correlations
  # Govt ~~ Univ
  # Govt ~~ Comp
  # Univ ~~ Comp
'

fit <- cfa(model, data = d)

summary(fit, fit.measures = T)
#lavaan 0.6-8 ended normally after 38 iterations

#Estimator                                         ML
#Optimization method                           NLMINB
#Number of model parameters                        10

#Number of observations                           521

#Model Test User Model:
  
  #Test statistic                                35.130
#Degrees of freedom                                 8
#P-value (Chi-square)                           0.000

#Model Test Baseline Model:
  
 # Test statistic                               355.777
#Degrees of freedom                                14
#P-value                                        0.000

#User Model versus Baseline Model:
  
#  Comparative Fit Index (CFI)                    0.921
#Tucker-Lewis Index (TLI)                       0.861

#Loglikelihood and Information Criteria:
  
 # Loglikelihood user model (H0)              -3798.530
#Loglikelihood unrestricted model (H1)      -3780.965

#Akaike (AIC)                                7617.059
#Bayesian (BIC)                              7659.617
#Sample-size adjusted Bayesian (BIC)         7627.874

#Root Mean Square Error of Approximation:
  
 # RMSEA                                          0.081
#90 Percent confidence interval - lower         0.055
#%90 Percent confidence interval - upper         0.109
#P-value RMSEA <= 0.05                          0.028

#Standardized Root Mean Square Residual:
  
 # SRMR                                           0.054

#Parameter Estimates:
  
 # Standard errors                             Standard
#Information                                 Expected
#Information saturated (h1) model          Structured

#Latent Variables:
  #         Estimate  Std.Err  z-value  P(>|z|)
#Tr =~                                               
#  Govt              1.000                           
#Univ              0.574    0.063    9.102    0.000
#Comp              0.500    0.060    8.302    0.000

#Regressions:
 
#          Estimate  Std.Err  z-value  P(>|z|)
#Down ~                                              
 # Tr              0.078    0.011    6.821    0.000
#DP                0.008    0.010    0.805    0.421
#OS               -0.014    0.008   -1.773    0.076

#Variances:
 # Estimate  Std.Err  z-value  P(>|z|)
#.Govt              2.279    0.528    4.312    0.000
#.Univ              3.041    0.256   11.885    0.000
#.Comp              4.182    0.295   14.170    0.000
#.Down              0.197    0.013   15.252    0.000
#Tr                5.366    0.681    7.880    0.000

parameterEstimates(fit, ci = TRUE, level = 0.95)
#lhs op  rhs    est    se      z pvalue ci.lower ci.upper
#1    Tr =~ Govt  1.000 0.000     NA     NA    1.000    1.000
#2    Tr =~ Univ  0.574 0.063  9.102  0.000    0.450    0.697
#3    Tr =~ Comp  0.500 0.060  8.302  0.000    0.382    0.618
#4  Down  ~   Tr  0.078 0.011  6.821  0.000    0.055    0.100
#5  Down  ~   DP  0.008 0.010  0.805  0.421   -0.012    0.028
#6  Down  ~   OS -0.014 0.008 -1.773  0.076   -0.030    0.002
#7  Govt ~~ Govt  2.279 0.528  4.312  0.000    1.243    3.314
#8  Univ ~~ Univ  3.041 0.256 11.885  0.000    2.539    3.542
#9  Comp ~~ Comp  4.182 0.295 14.170  0.000    3.603    4.760
#10 Down ~~ Down  0.197 0.013 15.252  0.000    0.172    0.223
#11   Tr ~~   Tr  5.366 0.681  7.880  0.000    4.031    6.700
#12   DP ~~   DP  4.256 0.000     NA     NA    4.256    4.256
#13   DP ~~   OS  1.720 0.000     NA     NA    1.720    1.720
#14   OS ~~   OS  6.612 0.000     NA     NA    6.612    6.612


semPaths(fit, style = "lisrel",
         whatLabels = "std", edge.label.cex = 1,
         label.prop = 0.9, edge.label.color = "black", rotation = 2,
         equalizeManifests = FALSE, optimizeLatRes = TRUE, node.width = 1.5,
         edge.width = 0.5, shapeMan = "rectangle", shapeLat = "ellipse",
         shapeInt = "triangle", sizeMan = 8, sizeInt = 8, sizeLat = 8,
         curve = 2, unCol = "#070b8c")

fitMeasures(fit, c("chisq", "rmsea", "srmr", "gfi", "ecvi"))
#chisq  rmsea   srmr    gfi   ecvi 
#35.130  0.081  0.054  0.968  0.106 
dplot <- standardizedSolution(fit, ci = TRUE, level = 0.95)

dplot <- dplot %>% 
  filter(!grepl("~~", op)) %>% 
  mutate(rel = paste(lhs, op, rhs)) %>% 
  select(rel, est.std, ci.lower, ci.upper)


p <- ggplot(dplot, aes(rel, est.std))
p + 
  geom_pointrange(aes(ymin = ci.lower, ymax = ci.upper)) +
  labs(x = "Relation", y = "Estimate", title = "Estimated Paths Effects on Q29") +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed")
