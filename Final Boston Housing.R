library(MASS)
library(tidyverse)
library(corrplot)
library(GGally)

View(Boston)
#write.csv(Boston, "Boston Housing.csv")

Boston$chas <- as.factor(Boston$chas)
chas <- summary(Boston$chas)
pie(chas)

#### Scatter Plot ####
chas_0 <- dplyr::filter(Boston, chas==0)
chas_1 <- dplyr::filter(Boston, chas==1)

lm_crim0 <- lm(medv~crim,chas_0)
lm_crim1 <- lm(medv~crim,chas_1)
lm_zn0 <- lm(medv~zn,chas_0)
lm_zn1 <- lm(medv~zn,chas_1)
lm_indus0 <- lm(medv~indus,chas_0)
lm_indus1 <- lm(medv~indus,chas_1)
lm_nox0 <- lm(medv~nox,chas_0)
lm_nox1 <- lm(medv~nox,chas_1)
lm_rm0 <- lm(medv~rm,chas_0)
lm_rm1 <- lm(medv~rm,chas_1)
lm_age0 <- lm(medv~age,chas_0)
lm_age1 <- lm(medv~age,chas_1)
lm_dis0 <- lm(medv~dis,chas_0)
lm_dis1 <- lm(medv~dis,chas_1)
lm_rad0 <- lm(medv~rad,chas_0)
lm_rad1 <- lm(medv~rad,chas_1)
lm_tax0 <- lm(medv~tax,chas_0)
lm_tax1 <- lm(medv~tax,chas_1)
lm_ptratio0 <- lm(medv~ptratio,chas_0)
lm_ptratio1 <- lm(medv~ptratio,chas_1)
lm_black0 <- lm(medv~black,chas_0)
lm_black1 <- lm(medv~black,chas_1)
lm_lstat0 <- lm(medv~lstat,chas_0)
lm_lstat1 <- lm(medv~lstat,chas_1)

par(mfrow=c(3,4))
crim_plot <- plot(Boston$crim,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "CRIM");abline(lm_crim0, lwd=2, col="#839EAA");abline(lm_crim1, lwd=2, col="#6B1441")
zn_plot <- plot(Boston$zn,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "ZN");abline(lm_zn0, lwd = 2, col="#839EAA");abline(lm_zn1, lwd=2, col="#6B1441")
indus_plot <- plot(Boston$indus,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "INDUS");abline(lm_indus0, lwd = 2, col="#839EAA");abline(lm_indus1, lwd=2, col="#6B1441")
nox_plot <- plot(Boston$nox,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "NOX");abline(lm_nox0, lwd = 2, col="#839EAA");abline(lm_nox1, lwd=2, col="#6B1441")
rm_plot <- plot(Boston$rm,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "RM");abline(lm_rm0, lwd = 2, col="#839EAA");abline(lm_rm1, lwd=2, col="#6B1441")
age_plot <- plot(Boston$age,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "AGE");abline(lm_age0, lwd = 2, col="#839EAA");abline(lm_age1, lwd=2, col="#6B1441")
dis_plot <- plot(Boston$dis,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "DIS");abline(lm_dis0, lwd = 2, col="#839EAA");abline(lm_dis1, lwd=2, col="#6B1441")
rad_plot <- plot(Boston$rad,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "RAD");abline(lm_rad0, lwd = 2, col="#839EAA");abline(lm_rad1, lwd=2, col="#6B1441")
tax_plot <- plot(Boston$tax,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "TAX");abline(lm_tax0, lwd = 2, col="#839EAA");abline(lm_tax1, lwd=2, col="#6B1441")
ptratio_plot <- plot(Boston$ptratio,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "PTRATIO");abline(lm_ptratio0, lwd = 2, col="#839EAA");abline(lm_ptratio1, lwd=2, col="#6B1441")
black_plot <- plot(Boston$black,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "BLACK");abline(lm_black0, lwd = 2, col="#839EAA");abline(lm_black1, lwd=2, col="#6B1441")
lstat_plot <- plot(Boston$lstat,Boston$medv, col=ifelse(Boston$chas==0,alpha("#839EAA", 0.4),alpha("#6B1441",1)), pch=19, main= "LSTAT");abline(lm_lstat0, lwd = 2, col="#839EAA");abline(lm_lstat1, lwd=2, col="#6B1441")
par(mfrow=c(1,1))

#### Correlation Matrix ####
ggcorr(Boston, label = TRUE, label_alpha = 0.9, label_color = "white", low="#6B1441", high = "#27566B")

#### MODEL BUILDING ####
m1 <- lm(medv~lstat, data=Boston);summary(m1) ##  R-sq: 0.5441  Sig of F: < 2.2e-16

m2 <- lm(medv~.,data=Boston); summary(m2) ##  Adj. R-sq: 0.7338  Sig of F: < 2.2e-16
m3 <- lm(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat, data=Boston); summary(m3) ##  Adj. R-sq: 0.7406  Sig of F: < 2.2e-16

m4 <- lm(medv~crim+chas+chas*crim, data=Boston);summary(m4)
m5 <- lm(medv~chas+indus+chas*indus, data=Boston); summary(m5)
m6 <- lm(medv~chas+rad+chas*rad, data=Boston);summary(m6) 
m7 <- lm(medv~chas+tax+chas*tax, data=Boston);summary(m7) 
m8 <- lm(medv~chas+ptratio+chas*ptratio, data=Boston);summary(m8) 
m9 <- lm(medv~chas+black+chas*black, data=Boston);summary(m9) 
m10 <- lm(medv~chas+lstat+chas*lstat, data=Boston);summary(m10)

m11 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat+chas*rad+chas*tax+chas*ptratio+chas*lstat, data=Boston); summary(m11) 
m12 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat+chas*crim+chas*lstat, data=Boston); summary(m12)
m12 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat, data=Boston); summary(m12)

m14 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat+chas*crim, data=Boston);summary(m14) 
m14_2 <- lm(medv~crim+zn+chas+nox+rm+dis+ptratio+black+lstat+chas*crim, data=Boston);summary(m14_2) 
m15 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat+chas*indus, data=Boston);summary(m15)
m16 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat+chas*rad, data=Boston);summary(m16)
m16_2 <- lm(medv~crim+zn+chas+nox+rm+dis+rad+ptratio+black+lstat+chas*rad, data=Boston);summary(m16_2)
m17 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat+chas*tax, data=Boston);summary(m17)
m18 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat+chas*ptratio, data=Boston);summary(m18)
m18_2 <- lm(medv~zn+chas+nox+rm+dis+ptratio+black+lstat+chas*ptratio, data=Boston);summary(m18_2)
m19 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat+chas*black, data=Boston);summary(m19)
m20 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat+chas*lstat, data=Boston);summary(m20)
m20_2 <- lm(medv~crim+zn+chas+nox+rm+dis+ptratio+black+lstat+chas*lstat, data=Boston);summary(m20_2)

m21 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat+chas*crim+chas*tax+chas*ptratio+chas*lstat, data=Boston);summary(m21)
m22 <- lm(medv~crim+zn+chas+nox+rm+dis+tax+ptratio+black+lstat+chas*crim+chas*tax+chas*lstat, data=Boston);summary(m22)
m23 <- lm(medv~crim+zn+chas+nox+rm+dis+ptratio+black+lstat+chas*crim+chas*tax+chas*lstat, data=Boston);summary(m23)

finalModel <- m22
summary(finalModel)

plot(finalModel)

