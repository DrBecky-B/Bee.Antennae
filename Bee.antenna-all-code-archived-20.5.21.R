##### Bee.Antennae ######
# R Code and data for analysis of antennal sensilla densities from Halictus rubicundus
# updated 2.2.2022 

library(lme4)
library(mgcv)
library(ggplot2)
library(multcomp)
library(car)
library(binom)
library(irr)
library(DHARMa)
library(beeswarm)

# Intra-rater Repeatability
# Full antennae - all samples (3 or 2 replicates for all) of type iii
IOR.full.2 <- read.delim("~/IOR-full-2.txt")
IOR.full.3 <- read.delim("~/IOR-full-3.txt")

# Quads of antennae - all samples (3 or 2 replicates for all) of type i, type ii, type iii
IOR.Q.Sp.2 <- read.delim("~/IOR-Q-Sp2.txt")
IOR.Q.Sp.3 <- read.delim("~/IOR-Q-Sp-3.txt")
IOR.Q.Stb.2 <- read.delim("~/IOR-Q-Stb-2.txt")
IOR.Q.Stb.3 <- read.delim("~/IOR-Q-Stb-3.txt")
IOR.Q.Sacc.2 <- read.delim("~/IOR-Q-Sacc-2.txt")
IOR.Q.Sacc.3 <- read.delim("~/IOR-Q-Sacc-3.txt")


# ICC for type i on quads
icc(IOR.Q.Sp.2, model="twoway", type="agreement")
#Single Score Intraclass Correlation
#Model: twoway 
#Type : agreement 
#Subjects = 33 
#Raters = 2 
#ICC(A,1) = 0.888 (very good)
#F-Test, H0: r0 = 0 ; H1: r0 > 0 
#F(32,33) = 16.9 , p = 6.28e-13 
#95%-Confidence Interval for ICC Population Values:
#  0.788 < ICC < 0.943

icc(IOR.Q.Sp.3, model="twoway", type="agreement")
#Single Score Intraclass Correlation
#Model: twoway 
#Type : agreement 
#Subjects = 30 
#Raters = 3 
#ICC(A,1) = 0.987 (excellent)
#F-Test, H0: r0 = 0 ; H1: r0 > 0 
#F(29,58.3) = 218 , p = 1.46e-49 
#95%-Confidence Interval for ICC Population Values:
#  0.976 < ICC < 0.993

# ICC for type ii on quads
icc(IOR.Q.Stb.2 , model="twoway", type="agreement")
#Single Score Intraclass Correlation
#Model: twoway 
#Type : agreement 
#Subjects = 33 
#Raters = 2 
#ICC(A,1) = 0.854 (very good)
#F-Test, H0: r0 = 0 ; H1: r0 > 0 
#F(32,32) = 12.3 , p = 1.02e-10 
#95%-Confidence Interval for ICC Population Values:
#  0.724 < ICC < 0.925

icc(IOR.Q.Stb.3, model="twoway", type="agreement")
#Single Score Intraclass Correlation
#Model: twoway 
#Type : agreement 
#Subjects = 30 
#Raters = 3 
#ICC(A,1) = 0.801 (good)
#F-Test, H0: r0 = 0 ; H1: r0 > 0 
#F(29,33.1) = 15.7 , p = 2.5e-12 
#95%-Confidence Interval for ICC Population Values:
#  0.648 < ICC < 0.896

# ICC for type iii on quads
icc(IOR.Q.Sacc.2, model="twoway", type="agreement")
#Single Score Intraclass Correlation
#Model: twoway 
#Type : agreement 
#Subjects = 33 
#Raters = 2 
#ICC(A,1) = 0.994 (excellent)
#F-Test, H0: r0 = 0 ; H1: r0 > 0 
#F(32,32.5) = 312 , p = 1.28e-32 
#95%-Confidence Interval for ICC Population Values:
#  0.987 < ICC < 0.997

icc(IOR.Q.Sacc.3, model="twoway", type="agreement")
#Single Score Intraclass Correlation
#Model: twoway 
#Type : agreement 
#Subjects = 30 
#Raters = 3 
#ICC(A,1) = 0.822 (good)
#F-Test, H0: r0 = 0 ; H1: r0 > 0 
#F(29,49.3) = 16.3 , p = 1.23e-16 
#95%-Confidence Interval for ICC Population Values:
#  0.698 < ICC < 0.905

# ICC for type i on full antennae
icc(IOR.full.2, model="twoway", type="agreement")
#Single Score Intraclass Correlation
#Model: twoway 
#Type : agreement 
#Subjects = 19 
#Raters = 2 
#ICC(A,1) = 0.937 (excellent)
#F-Test, H0: r0 = 0 ; H1: r0 > 0 
#F(18,19) = 30.6 , p = 2.43e-10 
#95%-Confidence Interval for ICC Population Values:
#  0.847 < ICC < 0.975

icc(IOR.full.3, model="twoway", type="agreement")
# Single Score Intraclass Correlation
#Model: twoway 
#Type : agreement 
#Subjects = 10 
#Raters = 3 
#ICC(A,1) = 0.785 (good)
#F-Test, H0: r0 = 0 ; H1: r0 > 0 
#F(9,18.7) = 11.1 , p = 8.3e-06 
#95%-Confidence Interval for ICC Population Values:
#  0.507 < ICC < 0.935

#Based on Koo & Li 2016
#ICC < 0.5 = poor
#ICC 0.5-0.75 = moderate
#ICC 0.75-0.9 = good
#ICC > 0.9 = excellent

##### Regional models ######
# Dataset without Knepp (SE) bees
# Test for an effect of region (South-West, North, Scotland) on density of 3 sensilla types

# Type i sensilla (olfactory plates)
# check most appropriate error structure
Full.antennae.all <- read.delim("~/Full-antennae-all.txt")
Full.antennae.all$SegF <- as.factor(Full.antennae.all$Seg)
Reg.mod.i <- lmer(Sp.sum ~ Region + SegF + Region*SegF + (1|Individual), data = Full.antennae.all)
simulationOutput.reg.i <- simulateResiduals(fittedModel = Reg.mod.i, plot = T, use.u = T)
car::Anova(Reg.mod.i)

#Analysis of Deviance Table (Type II Wald chisquare tests)

#Response: Sp.sum
#               Chisq Df   Pr(>Chisq)    
#Region       2.3118  2     0.3148    
#SegF        19.4819  1  1.016e-05 ***
#Region:SegF  0.4550  2     0.7965    

Reg.mod.i.no.int <- lmer(Sp.sum ~ Region + SegF + (1|Individual),data = Full.antennae.all)
simulationOutput.reg.i.no.int <- simulateResiduals(fittedModel = Reg.mod.i.no.int, plot = T, use.u = T)
post.hoc.reg.i <- glht(Reg.mod.i.no.int, linfct = mcp(Region = "Tukey"))
summary(post.hoc.reg.i)

#Multiple Comparisons of Means: Tukey Contrasts
#Fit: lmer(formula = Sp.sum ~ Region + SegF + (1 | Individual), data = Full.antennae.all)
#Linear Hypotheses:
#  Estimate Std. Error z value Pr(>|z|)
#Scotland - North == 0         1.402      1.917   0.731    0.745
#South-West - North == 0       2.856      1.859   1.536    0.274
#South-West - Scotland == 0    1.453      1.930   0.753    0.732
#(Adjusted p values reported -- single-step method)

#Figure 2a
cc <- palette()
palette(c(cc,"black","grey"))
boxplot(Sp.sum ~ Region, data = Full.antennae.all,
        outline = FALSE,     
        ylab = "# Olfactory plates (i)")
beeswarm(Sp.sum ~ Region, data = Full.antennae.all, 
         pwcol = as.character(Full.antennae.all$SegF), 
         pch = 16, add = TRUE)



# Type ii sensilla (olfactory hairs)
Reg.mod.ii <- lmer(Stb.sum ~ Region + SegF + Region*SegF + (1|Individual), data = Full.antennae.all)
simulationOutput.reg.ii <- simulateResiduals(fittedModel = Reg.mod.ii, plot = T, use.u = T)
car::Anova(Reg.mod.ii)
#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Stb.sum
#               Chisq Df Pr(>Chisq)    
#Region       7.8165  2    0.02008 *  
#  SegF        41.7450  1   1.04e-10 ***
#  Region:SegF  2.7015  2    0.25905    
Reg.mod.ii.no.int <- lmer(Stb.sum ~ Region + SegF + (1|Individual), data = Full.antennae.all)
simulationOutput.reg.ii.no.int <- simulateResiduals(fittedModel = Reg.mod.ii.no.int, plot = T, use.u = T)
car::Anova(Reg.mod.ii.no.int)
post.hoc.reg.ii <- glht(Reg.mod.ii.no.int, linfct = mcp(Region = "Tukey"))
summary(post.hoc.reg.ii)

#Multiple Comparisons of Means: Tukey Contrasts
#Fit: lmer(formula = Stb.sum ~ Region + SegF + (1 | Individual), data = Full.antennae.all)
#Linear Hypotheses:
#                             Estimate  Std.Error z value Pr(>|z|)  
#Scotland - North == 0        -9.798      3.866  -2.534   0.0302 *
#  South-West - North == 0      -8.462      3.745  -2.259   0.0617 .
#South-West - Scotland == 0    1.336      3.883   0.344   0.9368  


#Figure 2b
boxplot(Stb.sum ~ Region, data = Full.antennae.all,
        outline = FALSE,     
        ylab = "# Olfactory hairs (ii)")
beeswarm(Stb.sum ~ Region, data = Full.antennae.all, 
         pwcol = as.character(Full.antennae.all$SegF), 
         pch = 16, add = TRUE)

# Type iii sensilla (thermo/hygrosense pores)
Reg.mod.iii <- lmer(Scac ~ Region + SegF + Region*SegF + (1|Individual), data = Full.antennae.all)
simulationOutput.reg.iii <- simulateResiduals(fittedModel = Reg.mod.iii, plot = T, use.u = T)
car::Anova(Reg.mod.iii)
#Response: Scac
#               Chisq Df Pr(>Chisq)    
#Region       9.1374  2    0.01037 *  
#  SegF        19.1706  1  1.195e-05 ***
#  Region:SegF  0.3541  2    0.83772    

Reg.mod.iii.no.int <- lmer(Scac ~ Region + SegF + (1|Individual), data = Full.antennae.all)
simulationOutput.reg.iii.no.int <- simulateResiduals(fittedModel = Reg.mod.iii.no.int, plot = T, use.u = T)
car::Anova(Reg.mod.iii.no.int)
post.hoc.reg.iii <- glht(Reg.mod.iii.no.int, linfct = mcp(Region = "Tukey"))
summary(post.hoc.reg.iii)
#Multiple Comparisons of Means: Tukey Contrasts
#Fit: lmer(formula = Scac ~ Region + SegF + (1 | Individual), data = Full.antennae.all)

#Linear Hypotheses:
#  Estimate Std. Error z value Pr(>|z|)  
#Scotland - North == 0         1.322      2.790   0.474   0.8836  
#South-West - North == 0      -6.779      2.738  -2.476   0.0355 *
# South-West - Scotland == 0   -8.101      2.898  -2.796   0.0143 *

#Figure 2c
boxplot(Scac ~ Region, data = Full.antennae.all,
        outline = FALSE,     
        ylab = "# Hygro/thermo receptors (iii)")
beeswarm(Scac ~ Region, data = Full.antennae.all, 
         pwcol = as.character(Full.antennae.all$SegF), 
         pch = 16, add = TRUE)


##### Transplant models ######         
# Dataset with Knepp (SE) bees and Migdale (Scotland) bees only
# Test for an effect of where bees developed (Scotland or Knepp) on density of 3 sensilla types
Knepp.transplant.full <- read.delim("~/Knepp-transplant-full.txt")
Knepp.transplant.full$SegF <- as.factor(Knepp.transplant.full$Seg)

# Type i sensilla (olfactory plates)
Origin.mod.i <- lmer(Sp.sum ~ Origin + SegF + Origin*SegF + (1|Individual), data = Knepp.transplant.full)
simulationOutput.origin.i <- simulateResiduals(fittedModel = Origin.mod.i, plot = T, use.u = T)
car::Anova(Origin.mod.i)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Sp.sum
#               Chisq Df Pr(>Chisq)    
#Origin       3.4394  1    0.06366 .  
#SegF        34.8291  1    3.6e-09 ***
#  Origin:SegF  0.6104  1    0.43464    

#Fig 3a
boxplot(Sp.sum ~ Origin, data = Knepp.transplant.full,
        outline = FALSE,     
        ylab = "# Olfactory plates (i)")
beeswarm(Sp.sum ~ Origin, data = Knepp.transplant.full, 
         pwcol = as.character(Knepp.transplant.full$SegF), 
         pch = 16, add = TRUE)


# Type ii sensilla (olfactory hairs)
Origin.mod.ii <- lmer(Stb.sum ~ Origin + SegF + Origin*SegF + (1|Individual), data = Knepp.transplant.full)
simulationOutput.origin.ii <- simulateResiduals(fittedModel = Origin.mod.ii, plot = T, use.u = T)
car::Anova(Origin.mod.ii)
#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Stb.sum
#Chisq Df Pr(>Chisq)    
#Origin       6.4113  1    0.01134 *  
#  SegF        61.3464  1  4.787e-15 ***
#  Origin:SegF  1.5723  1    0.20988   

#Fig 3b
boxplot(Stb.sum ~ Origin, data = Knepp.transplant.full,
        outline = FALSE,     
        ylab = "# Olfactory hairs (ii)")
beeswarm(Stb.sum ~ Origin, data = Knepp.transplant.full, 
         pwcol = as.character(Knepp.transplant.full$SegF), 
         pch = 16, add = TRUE)



# Type iii sensilla (thermo/hygrosense pores)
Origin.mod.iii <- lmer(Scac ~ Origin + SegF + Origin*SegF + (1|Individual), data = Knepp.transplant.full)
simulationOutput.origin.iii <- simulateResiduals(fittedModel = Origin.mod.iii, plot = T, use.u = T)
#best option Gaussian, ks = 0.04 but fit is worse if Poisson or nbinom; results unaffected
car::Anova(Origin.mod.iii)
#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Scac
#             Chisq Df Pr(>Chisq)    
#Origin       0.7400  1     0.3897    
#SegF        33.2461  1   8.12e-09 ***
#  Origin:SegF  0.1555  1     0.6933    


#Fig 3c
boxplot(Scac ~ Origin, data = Knepp.transplant.full,
        outline = FALSE,     
        ylab = "# Hygro/thermo receptors (iii)")
beeswarm(Scac ~ Origin, data = Knepp.transplant.full, 
         pwcol = as.character(Knepp.transplant.full$SegF), 
         pch = 16, add = TRUE)


##### Social phenotype models ######         
# Dataset with Knepp (SE) bees (that originated in Scotland) only
# Test for an effect of social phenotype of nest (social or solitary) on the density of 3 sensilla types  

Knepp.2020.soc.sol <- read.delim("~/Knepp-2020-soc-sol.txt")
Knepp.2020.soc.sol$SegF <- as.factor(Knepp.2020.soc.sol$Seg)

# First test for an effect of age - bees either fresh (B2) or old (B1 provisioned/original foundresses)
# Type i sensilla (olfactory plates)
Age.mod.i <- lmer(Sp.sum ~ Age + SegF + Age*SegF + (1|Individual), data = Knepp.2020.soc.sol)
simulationOutput.age.i <- simulateResiduals(fittedModel = Age.mod.i, plot = T, use.u = T)
car::Anova(Age.mod.i)
#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Sp.sum
#           Chisq Df Pr(>Chisq)    
#Age       0.2519  1     0.6157    
#SegF     25.0701  1  5.528e-07 ***
#  Age:SegF  1.5101  1     0.2191   

# Type ii sensilla (olfactory hairs)
Age.mod.ii <- lmer(Stb.sum ~ Age + SegF + Age*SegF + (1|Individual), data = Knepp.2020.soc.sol)
simulationOutput.age.ii <- simulateResiduals(fittedModel = Age.mod.ii, plot = T, use.u = T)
car::Anova(Age.mod.ii)
#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Stb.sum
#           Chisq Df Pr(>Chisq)    
#Age       0.0108  1     0.9174    
#SegF     56.2388  1  6.418e-14 ***
#  Age:SegF  0.1380  1     0.7103    

# Type iii sensilla (Hygro/thermo receptors)
Age.mod.iii <- lmer(Scac~ Age + SegF + Age*SegF + (1|Individual), data = Knepp.2020.soc.sol)
simulationOutput.age.iii <- simulateResiduals(fittedModel = Age.mod.iii, plot = T, use.u = T)
car::Anova(Age.mod.iii)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Scac
#Chisq Df Pr(>Chisq)    
#Age       0.0911  1     0.7628    
#SegF     31.4975  1  1.997e-08 ***
#  Age:SegF  0.2421  1     0.6227   

#Test for an effect of bee caste (worker, future-reproductive,
#old foundress or B1 foundress

Bee.phen <- read.csv("~/Bee.phen.txt")
Bee.phen$SegF <- as.factor(Bee.phen$Seg)

# Type i sensilla (olfactory plates)
Bee.phen.mod.i <- lmer(Sp.sum ~ Phenotype.2 + SegF + Phenotype.2*SegF + (1|Individual), data = Bee.phen)
simulationOutput.bee.phen.i <- simulateResiduals(fittedModel = Bee.phen.mod.i, plot = T, use.u = T)
car::Anova(Bee.phen.mod.i)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Sp.sum
#Chisq Df Pr(>Chisq)    
#Phenotype.2       1.5153  3     0.6787    
#SegF             22.3386  1  2.286e-06 ***
# Phenotype.2:SegF  1.8197  3     0.6107    

boxplot(Sp.sum ~ Phenotype.2, data = Bee.phen,
        outline = FALSE,     
        ylab = "# Olfactory plates (i)")
beeswarm(Sp.sum ~ Phenotype.2, data = Bee.phen, 
         pwcol = as.character(Bee.phen$SegF), 
         pch = 16, add = TRUE)


# Type ii sensilla (olfactory hairs)
Bee.phen.mod.ii <- lmer(Stb.sum ~ Phenotype.2 + SegF + Phenotype.2*SegF + (1|Individual), data = Bee.phen)
simulationOutput.bee.phen.ii <- simulateResiduals(fittedModel = Bee.phen.mod.ii, plot = T, use.u = T)
car::Anova(Bee.phen.mod.ii)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Stb.sum
#Chisq Df Pr(>Chisq)    
#Phenotype.2       1.2480  3     0.7415    
#SegF             42.1784  1  8.332e-11 ***
# Phenotype.2:SegF  0.8482  3     0.8379    


boxplot(Stb.sum ~ Phenotype.2, data = Bee.phen,
        outline = FALSE,     
        ylab = "# Olfactory hairs (ii)")
beeswarm(Stb.sum ~ Phenotype.2, data = Bee.phen, 
         pwcol = as.character(Bee.phen$SegF), 
         pch = 16, add = TRUE)

# Type iii sensilla (hygro/thermoreceptors)
Bee.phen.mod.iii <- lmer(Scac ~ Phenotype.2 + SegF + Phenotype.2*SegF + (1|Individual), data = Bee.phen)
simulationOutput.bee.phen.iii <- simulateResiduals(fittedModel = Bee.phen.mod.iii, plot = T, use.u = T)
#poss non-homog var but using weights makes fit worse
plot(fitted(Bee.phen.mod.iii), resid(Bee.phen.mod.iii))
leveneTest(Scac ~ Phenotype.2, data = Knepp.2020.soc.sol)
car::Anova(Bee.phen.mod.iii)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Scac
#                 Chisq Df Pr(>Chisq)    
#Phenotype.2       3.3862  3     0.3358    
#SegF             30.1754  1  3.947e-08 ***
#  Phenotype.2:SegF  2.2577  3     0.5207    

boxplot(Scac ~ Phenotype.2, data = Bee.phen,
        outline = FALSE,     
        ylab = "# Hygro/thermoreceptors (iii)")
beeswarm(Scac ~ Phenotype.2, data = Bee.phen, 
         pwcol = as.character(Bee.phen$SegF), 
         pch = 16, add = TRUE)

#re-run with no foundresses from 2019 (which developed and emerged in SCO)

Bee.phen.2020 <- Bee.phen %>% filter(Phenotype.2 == "Sol-B1" | Phenotype.2 == "Future-rep" | Phenotype.2 == "Worker")

# Type i sensilla (olfactory plates)
Bee.phen.mod.i.2020 <- lmer(Sp.sum ~ Phenotype.2 + SegF + Phenotype.2*SegF + (1|Individual), data = Bee.phen.2020)
simulationOutput.bee.phen.i.2020 <- simulateResiduals(fittedModel = Bee.phen.mod.i.2020, plot = T, use.u = T)
car::Anova(Bee.phen.mod.i.2020)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Sp.sum
#Chisq Df Pr(>Chisq)    
#Phenotype.2       1.3784  2     0.5020    
#SegF             21.4697  1  3.595e-06 ***
#  Phenotype.2:SegF  1.8065  2     0.4053    


# Type ii sensilla (olfactory hairs)
Bee.phen.mod.ii.2020 <- lmer(Stb.sum ~ Phenotype.2 + SegF + Phenotype.2*SegF + (1|Individual), data = Bee.phen.2020)
simulationOutput.bee.phen.ii <- simulateResiduals(fittedModel = Bee.phen.mod.ii.2020, plot = T, use.u = T)
car::Anova(Bee.phen.mod.ii.2020)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Stb.sum
#Chisq Df Pr(>Chisq)    
#Phenotype.2       1.0502  2     0.5915    
#SegF             37.2708  1  1.028e-09 ***
#  Phenotype.2:SegF  0.0160  2     0.9920     


boxplot(Stb.sum ~ Phenotype.2, data = Bee.phen.2020,
        outline = FALSE,     
        ylab = "# Olfactory hairs (ii)")
beeswarm(Stb.sum ~ Phenotype.2, data = Bee.phen.2020, 
         pwcol = as.character(Bee.phen$SegF), 
         pch = 16, add = TRUE)

# Type iii sensilla (hygro/thermoreceptors)
Bee.phen.mod.iii.2020 <- lmer(Scac ~ Phenotype.2 + SegF + Phenotype.2*SegF + (1|Individual), data = Bee.phen.2020)
simulationOutput.bee.phen.iii.2020 <- simulateResiduals(fittedModel = Bee.phen.mod.iii.2020, plot = T, use.u = T)
car::Anova(Bee.phen.mod.iii.2020)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Scac
#Chisq Df Pr(>Chisq)    
#Phenotype.2       2.4757  2     0.2900    
#SegF             21.0657  1  4.438e-06 ***
#  Phenotype.2:SegF  1.7760  2     0.4115    

boxplot(Scac ~ Phenotype.2, data = Bee.phen.2020,
        outline = FALSE,     
        ylab = "# Hygro/thermoreceptors (iii)")
beeswarm(Scac ~ Phenotype.2, data = Bee.phen.2020, 
         pwcol = as.character(Bee.phen$SegF), 
         pch = 16, add = TRUE)

# Test for an effect of social phenotype of nest (social or solitary)
# on the density of 3 sensilla types   
# remove any bees of unknown origin, any bees that have not provisoned yet
# (as a worker or a foundress)

No.unknowns <- read.csv("~/No.unknowns.txt")
No.unknowns$SegF <- as.factor(No.unknowns$Seg)

# Type i sensilla (olfactory plates)
Nest.phen.mod.i <- lmer(Sp.sum ~ Nest.phenotype + SegF + Nest.phenotype*SegF + (1|Individual), data = No.unknowns)
simulationOutput.nest.phen.i <- simulateResiduals(fittedModel = Nest.phen.mod.i, plot = T, use.u = T)
car::Anova(Nest.phen.mod.i)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Sp.sum
#                       Chisq Df Pr(>Chisq)    
#Nest.phenotype       2.9107  1  0.0879939 .  
#SegF                11.8006  1  0.0005921 ***
#  Nest.phenotype:SegF  1.0783  1  0.2990698    

boxplot(Sp.sum ~ Nest.phenotype, data = No.unknowns,
        outline = FALSE,     
        ylab = "# olfactory plates (i)")
beeswarm(Sp.sum ~ Nest.phenotype, data = No.unknowns, 
         pwcol = as.character(No.unknowns$SegF), 
         pch = 16, add = TRUE)

# Type ii sensilla (olfactory hairs)
Nest.phen.mod.ii <- lmer(Stb.sum ~ Nest.phenotype + SegF + Nest.phenotype*SegF + (1|Individual), data = No.unknowns)
simulationOutput.nest.phen.ii <- simulateResiduals(fittedModel = Nest.phen.mod.ii, plot = T, use.u = T)
car::Anova(Nest.phen.mod.ii)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Stb.sum
#                   Chisq Df Pr(>Chisq)    
#Nest.phenotype       0.0808  1     0.7762    
#SegF                42.0320  1  8.979e-11 ***
#  Nest.phenotype:SegF  1.2897  1     0.2561  

boxplot(Stb.sum ~ Nest.phenotype, data = No.unknowns,
        outline = FALSE,     
        ylab = "# olfactory plates (i)")
beeswarm(Stb.sum ~ Nest.phenotype, data = No.unknowns, 
         pwcol = as.character(No.unknowns$SegF), 
         pch = 16, add = TRUE)

# Type iii sensilla (hygro/thermoreceptors)
Nest.phen.mod.iii <- lmer(Scac ~ Nest.phenotype + SegF + Nest.phenotype*SegF + (1|Individual), data = No.unknowns)
simulationOutput.nest.phen.iii <- simulateResiduals(fittedModel = Nest.phen.mod.iii, plot = T, use.u = T)
car::Anova(Nest.phen.mod.iii)

#Analysis of Deviance Table (Type II Wald chisquare tests)
#Response: Scac
#                       Chisq Df Pr(>Chisq)    
#Nest.phenotype       0.2917  1     0.5891    
#SegF                33.0758  1  8.864e-09 ***
# Nest.phenotype:SegF  0.0980  1     0.7542    

boxplot(Scac ~ Nest.phenotype, data = No.unknowns,
        outline = FALSE,     
        ylab = "# Hygro/thermoreceptors (iii)")
beeswarm(Scac ~ Nest.phenotype, data = No.unknowns, 
         pwcol = as.character(No.unknowns$SegF), 
         pch = 16, add = TRUE)


