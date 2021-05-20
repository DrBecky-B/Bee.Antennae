##### Bee.Antennae ######
# R Code and data for analysis of antennal sensilla densities from Halictus rubicundus

library(lme4)
library(nlme)
library(mgcv)
library(itsadug)
library(qcc)
library(sjPlot)
library(Rmisc)
library(ggplot2)
library(car)
library(blmeco)
library(MASS)
library(vcd)
library(countreg)
library(mctest)
library(multcomp)
library(glmmTMB)
library(dplyr)
library(emmeans)
library(car)
library(Hmisc)
library(binom)
library(irr)

# Intra-rater Repeatability
# Full antennae - all samples (3 or 2 replicates for all) of type iii
IOR.full.2 <- read.delim("~/Documents/UoE Documents /Sweat bee project/Antennae/Final antenna/Results/IOR-data/IOR-full-2.txt")
IOR.full.3 <- read.delim("~/Documents/UoE Documents /Sweat bee project/Antennae/Final antenna/Results/IOR-data/IOR-full-3.txt")

# Quads of antennae - all samples (3 or 2 replicates for all) of type i, type ii, type iii
IOR.Q.Sp.2 <- read.delim("~/Documents/UoE Documents /Sweat bee project/Antennae/Final antenna/Results/IOR-data/IOR-Q-Sp2.txt")
IOR.Q.Sp.3 <- read.delim("~/Documents/UoE Documents /Sweat bee project/Antennae/Final antenna/Results/IOR-data/IOR-Q-Sp-3.txt")
IOR.Q.Stb.2 <- read.delim("~/Documents/UoE Documents /Sweat bee project/Antennae/Final antenna/Results/IOR-data/IOR-Q-Stb-2.txt")
IOR.Q.Stb.3 <- read.delim("~/Documents/UoE Documents /Sweat bee project/Antennae/Final antenna/Results/IOR-data/IOR-Q-Stb-3.txt")
IOR.Q.Sacc.2 <- read.delim("~/Documents/UoE Documents /Sweat bee project/Antennae/Final antenna/Results/IOR-data/IOR-Q-Sacc-2.txt")
IOR.Q.Sacc.3 <- read.delim("~/Documents/UoE Documents /Sweat bee project/Antennae/Final antenna/Results/IOR-data/IOR-Q-Sacc-3.txt")


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
#icc(IOR.full.2, model="twoway", type="agreement")
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

##### Population models ######
# Dataset without Knepp (SE) bees
# Test for an effect of population (South-West, North, Scotland) on density of 3 sensilla types

Full.antennae.all <- read.delim("~/Full-antenna-all-3.txt")
Full.antennae.all$SegF <- as.factor(Full.antennae.all$Seg)

# Type i sensilla (olfactory plates)
# check most appropriate error structure
distplot(Full.antennae.all$Sp.sum, type="poisson")
distplot(Full.antennae.all$Sp.sum, type="nbinom")
Full.antennae.all$SegF <- as.factor(Full.antennae.all$Seg)
Full.pop.mod <- glmer(Sp.sum ~ Region + SegF + Region*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Full.antennae.all)
dispersion_glmer(Full.pop.mod) #1.05 (disp ok)
car::Anova(Full.pop.mod)
Pop.mod <- glmer(Sp.sum ~ Region + SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Full.antennae.all)
post.hoc <- glht(Pop.mod, linfct = mcp(Region = "Tukey"))
summary(post.hoc)

# Figure 2.a
Sp.means <- group.CI(Sp.sum ~ Region*SegF, Full.antennae.all, ci = 0.95)
Sp.fig <- ggplot(Sp.means, aes(x=factor(Region, level = c("South-West", "North", "Scotland")), y=Sp.sum.mean, fill=SegF)) + 
  geom_bar(color="black",position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Sp.sum.lower, ymax=Sp.sum.upper),
                width=.2,                   
                position=position_dodge(.9)) + xlab("Population") + ylab("# Olfactory plate sensillae (i)")
Sp.fig <- Sp.fig + theme_classic()
Sp.fig + theme(axis.title=element_text(face="bold.italic",
                                        size="12", color="black"), legend.position="top") + scale_fill_viridis_d(option = "inferno")

# Type ii sensilla (olfactory hairs)
distplot(Full.antennae.all$Sp.sum, type="poisson")
distplot(Full.antennae.all$Stb.sum, type="nbinom")
Full.pop.mod <- glmer(Stb.sum ~ Region + SegF + Region*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Full.antennae.all)
dispersion_glmer(Full.pop.mod) # 1.00 (disp ok)
car::Anova(Full.pop.mod)
Pop.mod <- glmer(Stb.sum ~ Region + SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Full.antennae.all)
post.hoc <- glht(Pop.mod, linfct = mcp(Region = "Tukey"))
summary(post.hoc)


# Figure 2.b
Stb.means <- group.CI(Stb.sum ~ Region*SegF, Full.antennae.all, ci = 0.95)
Stb.fig <- ggplot(Stb.means, aes(x=factor(Region, level = c("South-West", "North", "Scotland")), y=Stb.sum.mean, fill=SegF)) + 
  geom_bar(color="black",position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Stb.sum.lower, ymax=Stb.sum.upper),
                width=.2,                   
                position=position_dodge(.9)) + xlab("Population") + ylab("# Olfactory hair-like sensillae (ii)")
Stb.fig <- Stb.fig + theme_classic()
Stb.fig + theme(axis.title=element_text(face="bold.italic",
                                         size="12", color="black"), legend.position="top") + scale_fill_viridis_d(option = "inferno")

# Type iii sensilla (thermo/hygrosense pores)
distplot(Full.antennae.all$Scac, type="poisson")
distplot(Full.antennae.all$Scac, type="nbinom")
Full.pop.mod <- glmer(Scac ~ Population + SegF + Population*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Full.antennae.all)
dispersion_glmer(Full.pop.mod) #1.13 (disp ok)
car::Anova(Full.pop.mod)
Pop.mod <- glmer(Scac ~ Population + SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Full.antennae.all)
post.hoc <- glht(Pop.mod, linfct = mcp(Population = "Tukey"))
summary(post.hoc)

# Figure 2.c
Scac.means <- group.CI(Scac ~ Region*SegF, Full.antennae.clean, ci = 0.95)

Scac.fig <- ggplot(Scac.means, aes(x=factor(Region, level = c("South-West", "North", "Scotland")), y=Scac.mean, fill=SegF)) + 
  geom_bar(color="black",position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Scac.lower, ymax=Scac.upper),
                width=.2,                   
                position=position_dodge(.9)) + xlab("Population") + ylab("# Thermo/hygroreceptive sensillae (iii)")
Scac.fig <- Scac.fig + theme_classic()
Scac.fig + theme(axis.title=element_text(face="bold.italic",
                                               size="12", color="black"), legend.position="top") + scale_fill_viridis_d(option = "inferno")
                                               

##### Transplant models ######         
# Dataset with Knepp (SE) bees and Migdale (Scotland) bees only
# Test for an effect of where bees developed (Scotland or Knepp) on density of 3 sensilla types
Knepp.transplant.full <- read.delim("~/Knepp-transplant-full.txt")
Knepp.transplant.full$SegF <- as.factor(Knepp.transplant.full$Seg)

# Type i sensilla (olfactory plates)
Full.origin.mod.sp <- glmer(Sp.sum ~ Origin + SegF + Origin*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Knepp.transplant.full)
car::Anova(Full.origin.mod.sp)
dispersion_glmer(Full.origin.mod.sp) #0.99 (disp ok)

# Fig 3.a
Sp.origin <- group.CI(Sp.sum ~ Origin*SegF, data = Full.antennae.all.Knepp, ci = 0.95)
Sp.origin.fig <- ggplot(Sp.origin, aes(x=Origin, y=Sp.sum.mean, fill=SegF)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  geom_errorbar(aes(ymin=Sp.sum.lower, ymax=Sp.sum.upper),
                width=.2,                   
                position=position_dodge(.9)) + xlab("Origin") + ylab("# Olfactory plate sensillae (i)")
Sp.origin.fig <- Sp.origin.fig + theme_classic()
Sp.origin.fig + theme(axis.title=element_text(face="bold.italic",
                                                size="12", color="black"), legend.position="top") + scale_fill_viridis_d(option = "inferno")


# Type ii sensilla (olfactory hairs)
Full.origin.mod.stb <- glmer(Stb.sum ~ Origin + SegF + Origin*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Knepp.transplant.full)
car::Anova(Full.origin.mod.stb) 
dispersion_glmer(Full.origin.mod.sp) #0.98 (disp ok)

# Fig 3.b
group.CI(Stb.sum ~ Origin, data = Full.antennae.all.Knepp, ci = 0.95)
Stb.origin <- group.CI(Stb.sum ~ Origin*SegF, data = Full.antennae.all.Knepp, ci = 0.95)
Stb.origin.fig <- ggplot(Stb.origin, aes(x=Origin, y=Stb.sum.mean, fill=SegF)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  geom_errorbar(aes(ymin=Stb.sum.lower, ymax=Stb.sum.upper),
                width=.2,                   
                position=position_dodge(.9)) + xlab("Origin") + ylab("# Olfactory hair-like sensillae (ii)")
Stb.origin.fig <- Stb.origin.fig + theme_classic()
Stb.origin.fig + theme(axis.title=element_text(face="bold.italic",
                                              size="12", color="black"), legend.position="top") + scale_fill_viridis_d(option = "inferno")


# Type iii sensilla (thermo/hygrosense pores)
Full.origin.mod <- glmer(Scac ~ Origin + SegF + Origin*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Knepp.transplant.full)
car::Anova(Full.origin.mod)
dispersion_glmer(Full.origin.mod) #1.16 (disp ok)

# Fig 3.c

Scac.origin <- group.CI(Scac ~ Origin*SegF, Knepp.transplant.full, ci = 0.95)
Scac.origin.fig <- ggplot(Scac.origin, aes(x=Origin, y=Scac.mean, fill=SegF)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  geom_errorbar(aes(ymin=Scac.lower, ymax=Scac.upper),
                width=.2,                   
                position=position_dodge(.9)) + xlab("Origin") + ylab("# Thermo/hygroreceptive sensillae (iii)")
Scac.origin.fig <- Scac.origin.fig + theme_classic()
Scac.origin.fig + theme(axis.title=element_text(face="bold.italic",
                                        size="12", color="black"), legend.position="top") + scale_fill_viridis_d(option = "inferno")
                                        
                                        
##### Social phenotype models ######         
# Dataset with Knepp (SE) bees (that originated in Scotland) only
# Test for an effect of social phenotype of nest (social or solitary) on the density of 3 sensilla types    

Full.antenna.all.3 <- read.delim("~/Knepp.2020.all.3.txt")
Knepp.2020.all.3$SegF <- as.factor(Knepp.2020.all.3$Seg)
# Exclude unknowns
Sol <- subset(Knepp.2020.all.3, Nest.phenotype=="Solitary")
Soc <- subset(Knepp.2020.all.3, Nest.phenotype=="Social")
No.unknowns <- rbind(Sol, Soc)
summary(No.unknowns)

# Type i sensilla (olfactory plates)
Phen.mod.sp <- glmer(Sp.sum ~ Nest.phenotype + SegF + Nest.phenotype*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = No.unknowns)
dispersion_glmer(Phen.mod.sp) #0.98 (disp ok)
car::Anova(Phen.mod.sp)

# Type ii sensilla (olfactory hairs)
Phen.mod.stb <- glmer(Stb.sum ~ Nest.phenotype + SegF + Nest.phenotype*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = No.unknowns)
dispersion_glmer(Phen.mod.sp) #0.90 (disp ok)
car::Anova(Phen.mod.stb)

# Type iii sensilla (thermo/hygrosense pores)
Phen.mod.Scac <- glmer(Scac ~ Nest.phenotype + SegF + Nest.phenotype*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = No.unknowns)
dispersion_glmer(Phen.mod.Scac) #0.95 (disp ok)
car::Anova(Phen.mod.Scac)


##### Age/wear models ######         
# Dataset with Knepp (SE) bees (that originated in Scotland) only   
# Test for effects of age related wear and tear - does the density of the 3 sensilla types change with time since emergence (freshly emerged individuals vs individuals that have provisioned a nest as a worker or a foundress)

# Type i sensilla (olfactory plates)
Age.mod.sp <- glmer(Sp.sum ~ Age + SegF + Age*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Knepp.2020.all.3)
car::Anova(Age.mod.sp)
dispersion_glmer(Age.mod.sp) #0.98 (disp ok)

# Type ii sensilla (olfactory hairs)
Age.mod.stb <- glmer(Stb.sum ~ Age + SegF + Age*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Knepp.2020.all.3)
dispersion_glmer(Age.mod.stb) #0.97 (disp ok)
car::Anova(Age.mod.stb)

# Type iii sensilla (thermo/hygrosense pores)
Age.mod.scac <- glmer(Scac ~ Age + SegF + Age*SegF + (1|Individual) + (1|OLRE), family=poisson(link = "log"), data = Knepp.2020.all.3)
dispersion_glmer(Age.mod.scac) #1.21 (disp slightly overdispersed, probs ok)
car::Anova(Age.mod.stb)



 

