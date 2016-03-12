## ----load_packages, include=FALSE----------------------------------------
# Naser Ameen
# 3/11/2016
# Regression of mtcars to determine:
# 1. If automatic/manual transmission is a signifcant predictor of mpg?
# 2. What is the best model to predict mpg?

# Get required pacakges
library(dplyr)
library(ggplot2)
cars <-tbl_df(mtcars)
head(cars,5)

## ----var_descriptors_conversions-----------------------------------------
mpg   <- cars$mpg               # mpg Miles/(US) gallon - numerical
cyl   <- factor(cars$cyl)       # Number of cylinders - categorical
disp  <- cars$disp              # Displacement (cu.in.) - numerical
hp    <- cars$hp                # Gross horsepower - numerical
drat  <- cars$drat              # Rear axle ratio - numerical
wt    <- cars$wt                # Weight (1000 lbs or 1 ton) - numerical
qsec  <- cars$qsec              # 1/4 mile time -numerical
vs    <- cars$vs                # V/S -numerical
am    <- factor(cars$am)        # Transmission (0 = automatic, 1 = manual) - categorical
gear  <- factor(cars$gear)      # Number of forward gears - categorical
carb  <- factor(cars$carb)      # Number of carbureators -categorical

## ----auto_man_regress, include=FALSE-------------------------------------
fit1 <- lm(mpg~am)
summary(fit1)

## ----full_regress, include=FALSE-----------------------------------------
fit2 = lm(mpg~.,data=cars)
summary(fit2)

## ----backward_regress, include=FALSE-------------------------------------
 mydf = data.frame(mpg=mpg, cyl=cyl, disp=disp, hp=hp, drat=drat, wt=wt, qsec=qsec, vs=vs, am=am, gear=gear, carb=carb)
fit3 = step(lm(mydf$mpg~.,data=mydf),direction = "backward")

## ----best_coeff, echo=FALSE----------------------------------------------
coef(fit3)

## ----sum_auto_man, echo= FALSE-------------------------------------------
summary(fit1)

## ----plot_auto_man, echo=FALSE,fig.width=8, fig.height=6-----------------
par(mfrow=c(2,2))
plot(fit1)

## ----sum_best_fit, echo=FALSE--------------------------------------------
summary(fit3)

## ----plot_best, echo=FALSE,fig.width=8, fig.height=6---------------------
par(mfrow=c(2,2))
plot(fit3)

## ----cor_factor----------------------------------------------------------
cor_df <- data.frame(mpg=mpg,wt=wt,hp=hp,cyl=as.numeric(cyl))
cor(cor_df)

