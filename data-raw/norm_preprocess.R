
#libraries
library(tidyr)
library(plyr)
library(reshape2)

library(lme4)
library(sjPlot)

library(ggplot2)
library(cowplot)

#read data
data_wide <- read.csv("norm_val.csv", header=TRUE, sep=";")
head(data_wide)

#data to long format
data_long <- gather(data_wide, position, measurement, X0_0:X10_180)

#splitting columns, removing "X" in the "position" variable
data_long <- data_long %>% separate(position, sep = "_", into = c("eccentricity", "angle"))
data_long$eccentricity <- gsub("X", "", data_long$eccentricity)
data_long$angle <- as.numeric(data_long$angle)
data_long <- data_long %>% separate(Type, sep =  "_", into  = c("Examtype", "Examnumber"))
data_long$Examtype <- as.factor(data_long$Examtype)
data_long$Eye.ID <- paste(data_long$Patient.ID, data_long$Eye, sep="_")

#data(non-pooled)reshaping (long to wide) for icc or kendall calculation
data_long_temp <- data_long
data_long_temp$Examnumber <- paste("E", data_long$Examnumber, sep="")
data_long_temp$Examination.ID <- NULL
data_long_temp$Type <- NULL
data_long_temp$Date.Time <- NULL
data_long_temp$Exam.duration <- NULL
data_long_temp$FPrate <- NULL
data_long_temp$WrongPressureEvents <- NULL
data_long_temp$Average.reaction.time <- NULL
data_long_temp$Fixation.area.0.632 <- NULL
data_long_temp$Fixation.area.0.950 <- NULL
data_long_temp$Fixation.angle <- NULL

data_reshaped <- spread(data_long_temp, Examnumber, measurement)
head(data_reshaped)

#<0 dB to zero
data_zero <- data_reshaped
data_zero$E1 <- as.numeric(ifelse(data_zero$E1=="<0", "0", data_zero$E1))
data_zero$E2 <- as.numeric(ifelse(data_zero$E2=="<0", "0", data_zero$E2))

#data diff cyan_red
data_diff <- left_join(data_zero[data_zero$Examtype == "Cyan",], data_zero[data_zero$Examtype=="Red",],
                       by=c("Patient.ID", "Eye", "Family.name", "Name", "DOB", "Sex", "eccentricity", "angle", "Eye.ID"))
data_diff$Examtype <- "CRdiff"
data_diff$E1 <- as.numeric(data_diff$E1.x-data_diff$E1.y)
data_diff$E2 <- as.numeric(data_diff$E2.x-data_diff$E2.y)
data_diff$Examtype.x <- NULL
data_diff$E1.x <- NULL
data_diff$E2.x <- NULL
data_diff$Examtype.y <- NULL
data_diff$E1.y <- NULL
data_diff$E2.y <- NULL

data_diff$Age <- data_diff$Age.x
data_diff$Age.x <- NULL
data_diff$Age.y <- NULL

data_diff$Lens <- data_diff$Lens.x
data_diff$Lens.x <- NULL
data_diff$Lens.y <- NULL

data_zero <- rbind(data_zero, data_diff)


########
# Plot #
########

#Coefficient of Repeatability = 1.96*sqrt(2)*sqrt(within-subject-variance)
#Please note, the "within-subject variance" from the anova is equal to the residual variance in the mixed-effect model
#fit <- aov(value~position+Error(Patient.ID/position), data=data_for_model)
#lmm_test <- lmer(value ~ position + (1|Patient.ID/position), data=data_for_model, REML = FALSE)

data_for_model <- gather(data_zero, testrun, value, E1:E2)
data_for_model$Patient.ID <- as.factor(data_for_model$Patient.ID)
data_for_model$eccentricity <- as.factor(data_for_model$eccentricity)
data_for_model$testrun <- as.factor(data_for_model$testrun)

data_for_model$pp <- ifelse(is.na(data_for_model$Lens)|data_for_model$Lens!="pp", "Phakic", "Pseudophakic")

data_for_model$Examtype <- ordered(data_for_model$Examtype, levels = c("Mesopic", "Cyan", "Red", "CRdiff"))

ggplot(data_for_model, aes(x=Age, y=value)) + geom_point(aes(x=Age, y=value, shape=pp), alpha = 1/20) + facet_wrap(~ Examtype) + ylab('Sensitivity [dB]') + xlab('Age [years]') + geom_smooth(aes(color=Examtype), method=lm) + scale_color_manual(values=c("#009E73", "#56B4E9", "#D55E00", "#CC79A7")) + theme(legend.position="none")





###########################
# generating centile data #
###########################

newdata <- data.frame(Age=77)

results <- NULL
predictions <- NULL
position <- NULL
Examtype <- NULL

for(j in levels(data_zero$Examtype)) {

  sub_dat <- subset(data_zero, Examtype == j)
  sub_dat$MeanSens <- (sub_dat$E1 + sub_dat$E2)/2
  sub_dat$position <- paste0(sub_dat$eccentricity, '_', sub_dat$angle)
  sub_dat$position <- as.factor(sub_dat$position)

  for(i in levels(sub_dat$position)) {
    lm_age <- lm(MeanSens ~ Age, data = subset(sub_dat, position == i))
    predictions <- rbind(predictions, predict(lm_age, newdata = newdata, interval = 'prediction'))
    position <- append(position, i)
    Examtype <- append(Examtype, j)

  }
}


results <- data.frame(Examtype, position, predictions)

#convert 95% CI to 90% CI
results$sd <- abs((results$lwr-results$fit)/1.96)
results$lwr <- results$fit-1.64*results$sd
results$upr <- results$fit+1.64*results$sd

#remove <0 dB
results_temp <- subset(results, Examtype!="CRdiff")
results_temp$fit <- ifelse(results_temp$fit<=0, 0, results_temp$fit)
results_temp$lwr <- ifelse(results_temp$lwr<=0, 0, results_temp$lwr)
results <- rbind(results_temp, subset(results, Examtype=="CRdiff"))
results_temp <- NULL

results  <- cbind(results,  colsplit(results$position, "_", c("eccentricity", "angle")))
results$eccentricity <- gsub("X", "", results$eccentricity)
results$angle <- as.numeric(results$angle)
results$eccentricity <- as.numeric(results$eccentricity)

#results$fit <- ifelse(results$fit<0, 0, results$fit)
#$lwr <- ifelse(results$lwr<0, 0, results$lwr)

write.table(results, "grid_norm_data.csv", sep=";", row.names=FALSE, col.names=TRUE)




#################
# Interpolation #
#################


#x - y coordinates
results$x <- cos(results$angle*pi/180) * as.numeric(results$eccentricity)
results$y <- sin(results$angle*pi/180) * as.numeric(results$eccentricity)


#gstat
library(gstat)
library(sp)
library(automap)

#create grid
grid <- expand.grid(x=seq(-20,20,by=.05), y=seq(-20,20,by=.05))
grid$eccentricity <- sqrt(grid$x^2+grid$y^2)
coordinates(grid) <- ~x+y
gridded(grid) <- TRUE

output <- NULL

#Mesopic
interpol_dat <- subset(results, Examtype=="Mesopic")
coordinates(interpol_dat) <- ~x+y

  #fit
  vgm.fit <- NULL
  vgm <- variogram(fit~1, interpol_dat)
  vgm.fit <- fit.variogram(vgm, vgm(c("Sph")), fit.kappa=TRUE, fit.ranges=TRUE, fit.sills=TRUE,   fit.method=1)

  x = krige.cv(fit~1, interpol_dat, interpol_dat, model=vgm.fit, nmax = 40, nfold=71)
  sqrt(mean((x$var1.pred-x$observed)^2))

  vgm.fit
  plot(vgm, vgm.fit)

  mes_ok_mean <- krige(fit~1, interpol_dat, grid, vgm.fit)
  spplot(mes_ok_mean ["var1.pred"])

  #lwr
  vgm.fit <- NULL
  vgm <- variogram(lwr~1, interpol_dat)
  vgm.fit <- fit.variogram(vgm, vgm(c("Sph")), fit.kappa=TRUE, fit.ranges=TRUE, fit.sills=TRUE,   fit.method=1)

  x = krige.cv(lwr~1, interpol_dat, interpol_dat, model=vgm.fit, nmax = 40, nfold=71)
  sqrt(mean((x$var1.pred-x$observed)^2))

  vgm.fit
  plot(vgm, vgm.fit)

  mes_ok_lwr <- krige(lwr~1, interpol_dat, grid, vgm.fit)
  spplot(mes_ok_lwr["var1.pred"])

  output <- data.frame(Examtype="Mesopic", X_corr=grid$x, Y_corr=grid$y, mean=mes_ok_mean$var1.pred, lwr=mes_ok_lwr$var1.pred)


#Cyan
interpol_dat <- subset(results, Examtype=="Cyan")
coordinates(interpol_dat) <- ~x+y

  #fit
  vgm.fit <- NULL
  vgm <- variogram(fit~1, interpol_dat)
  vgm.fit <- vgm(17.01, "Sph", 5.03)
  x = krige.cv(fit~1, interpol_dat, interpol_dat, model=vgm.fit, nmax = 40, nfold=71)
  sqrt(mean((x$var1.pred-x$observed)^2))

  vgm.fit
  plot(vgm, vgm.fit)

  cyan_ok_mean <- krige(fit~1, interpol_dat, grid, vgm.fit)
  spplot(cyan_ok_mean["var1.pred"])


  #lwr
  vgm.fit <- NULL
  vgm <- variogram(lwr~1, interpol_dat)
  vgm.fit <- fit.variogram(vgm, vgm(c("Sph")), fit.kappa=TRUE, fit.ranges=TRUE, fit.sills=TRUE,   fit.method=1)

  x = krige.cv(lwr~1, interpol_dat, interpol_dat, model=vgm.fit, nmax = 40, nfold=71)
  sqrt(mean((x$var1.pred-x$observed)^2))

  vgm.fit
  plot(vgm, vgm.fit)

  cyan_ok_lwr <- krige(lwr~1, interpol_dat, grid, vgm.fit)
  spplot(cyan_ok_lwr["var1.pred"])

  output <- rbind(output, data.frame(Examtype="Cyan", X_corr=grid$x, Y_corr=grid$y, mean=cyan_ok_mean$var1.pred, lwr=cyan_ok_lwr$var1.pred))


#Red
interpol_dat <- subset(results, Examtype=="Red")
coordinates(interpol_dat) <- ~x+y

  #fit
  vgm.fit <- NULL
  vgm <- variogram(fit~1, interpol_dat)
  vgm.fit <- fit.variogram(vgm, vgm(c("Sph")), fit.kappa=TRUE, fit.ranges=TRUE, fit.sills=TRUE,   fit.method=1)

  x = krige.cv(fit~1, interpol_dat, interpol_dat, model=vgm.fit, nmax = 40, nfold=71)
  sqrt(mean((x$var1.pred-x$observed)^2))

  vgm.fit
  plot(vgm, vgm.fit)

  red_ok_mean <- krige(fit~1, interpol_dat, grid, vgm.fit)
  spplot(red_ok_mean["var1.pred"])


  #lwr
  vgm.fit <- NULL
  vgm <- variogram(lwr~1, interpol_dat)
  vgm.fit <- fit.variogram(vgm, vgm(c("Sph")), fit.kappa=TRUE, fit.ranges=TRUE, fit.sills=TRUE,   fit.method=1)

  x = krige.cv(lwr~1, interpol_dat, interpol_dat, model=vgm.fit, nmax = 40, nfold=71)
  sqrt(mean((x$var1.pred-x$observed)^2))

  vgm.fit
  plot(vgm, vgm.fit)

  red_ok_lwr <- krige(lwr~1, interpol_dat, grid, vgm.fit)
  spplot(red_ok_lwr["var1.pred"])

  output <- rbind(output, data.frame(Examtype="Red", X_corr=grid$x, Y_corr=grid$y, mean=red_ok_mean$var1.pred, lwr=red_ok_lwr$var1.pred))

#CRdiff
interpol_dat <- subset(results, Examtype=="CRdiff")
coordinates(interpol_dat) <- ~x+y

  #fit
  vgm.fit <- NULL
  vgm <- variogram(fit~1, interpol_dat)
  vgm.fit <- fit.variogram(vgm, vgm(c("Ste")), fit.kappa = TRUE, fit.method=1)

  x = krige.cv(fit~1, interpol_dat, interpol_dat, model=vgm.fit, nmax = 40, nfold=71)
  sqrt(mean((x$var1.pred-x$observed)^2))

  vgm.fit
  plot(vgm, vgm.fit)

  CRdiff_ok_mean <- krige(fit~1, interpol_dat, grid, vgm.fit)
  spplot(CRdiff_ok_mean["var1.pred"])


  #lwr
  vgm.fit <- NULL
  vgm <- variogram(lwr~1, interpol_dat)
  vgm.fit <- fit.variogram(vgm, vgm(c("Ste")), fit.kappa = TRUE, fit.method=1)

  x = krige.cv(lwr~1, interpol_dat, interpol_dat, model=vgm.fit, nmax = 40, nfold=71)
  sqrt(mean((x$var1.pred-x$observed)^2))

  vgm.fit
  plot(vgm, vgm.fit)

  CRdiff_ok_lwr <- krige(lwr~1, interpol_dat, grid, vgm.fit)
  spplot(CRdiff_ok_lwr["var1.pred"])

  output <- rbind(output, data.frame(Examtype="CRdiff", X_corr=grid$x, Y_corr=grid$y, mean=CRdiff_ok_mean$var1.pred, lwr=CRdiff_ok_lwr$var1.pred))


#Cyan
C <- ggplot(subset(output, Examtype=="Cyan"), aes(x=X_corr, y=Y_corr, z=mean)) + geom_tile(aes(fill = mean)) + stat_contour(binwidth = 1, size = 0.5, colour = "black") + scale_fill_gradient2(limits=c(0, 25), low="black", high="cyan", mid="blue", midpoint=10, expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

D <- ggplot(subset(output, Examtype=="Cyan"), aes(x=X_corr, y=Y_corr, z=lwr)) + geom_tile(aes(fill = lwr)) + stat_contour(binwidth = 1, size = 0.5, colour = "black") + scale_fill_gradient(expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

#Red
E <- ggplot(subset(output, Examtype=="Red"), aes(x=X_corr, y=Y_corr, z=mean)) + geom_tile(aes(fill = mean)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient(limits=c(0, 25), low="white", high="red", expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

F <- ggplot(subset(output, Examtype=="Red"), aes(x=X_corr, y=Y_corr, z=lwr)) + geom_tile(aes(fill = lwr)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient(low="white", high="red", expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

#Mesopic
A <- ggplot(subset(output, Examtype=="Mesopic"), aes(x=X_corr, y=Y_corr, z=mean)) + geom_tile(aes(fill = mean)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient2(limits=c(18, 28), low="orange", high="green", mid="yellow", midpoint=22, expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

B <- ggplot(subset(output, Examtype=="Mesopic"), aes(x=X_corr, y=Y_corr, z=lwr)) + geom_tile(aes(fill = lwr)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient(low="orange", high="green", expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)


#CRdiff
G <- ggplot(subset(output, Examtype=="CRdiff"), aes(x=X_corr, y=Y_corr, z=mean)) + geom_tile(aes(fill = mean)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient2(limits=c(-20, 5), low="red", high="cyan", mid="purple", midpoint=-10, expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

H <- ggplot(subset(output, Examtype=="CRdiff"), aes(x=X_corr, y=Y_corr, z=lwr)) + geom_tile(aes(fill = lwr)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient(limits=c(-20, 5), low="red", high="cyan", expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

plot_grid(A, C, E, G)
write.table(output, "interpol_data.csv", sep=";", row.names=FALSE, col.names=TRUE)


plot_grid(B, D, F, H)

