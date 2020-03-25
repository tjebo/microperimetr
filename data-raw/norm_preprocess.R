
#libraries
library(reshape2)
library(sjPlot)
library(cowplot)
library(tidyverse)


# Coefficient of repeatability ----
# CoR = 1.96*sqrt(2)*sqrt(within-subject-variance)
# Please note, the "within-subject variance" from the anova is equal to the residual variance in the mixed-effect model
# fit <- aov(value~position+Error(Patient.ID/position), data=data_for_model)
# lmm_test <- lme4::lmer(value ~ position + (1|Patient.ID/position), data=data_for_model, REML = FALSE)

## Example for plotting regression lines for normal data
ggplot(norm_data_pfau, aes(x = Age, y = value)) +
  geom_point(aes(x = Age, y = value, shape = lens), alpha = 0.05, show.legend = FALSE) +
  facet_wrap(~ testtype) +
  ylab("Sensitivity [dB]") +
  xlab("Age [years]") +
  geom_smooth(aes(color = testtype, linetype = lens), method = 'lm', formula = y ~ x) +
  scale_color_manual(name = 'Regression lines', values = maia_palette)



#################
# Interpolation #
#################


#x - y coordinates
results$x <- cos(results$angle*pi/180) * as.numeric(results$eccent)
results$y <- sin(results$angle*pi/180) * as.numeric(results$eccent)


#gstat
library(gstat)
library(sp)
library(automap)

#create grid
grid <- expand.grid(x=seq(-20,20,by=.05), y=seq(-20,20,by=.05))
grid$eccent <- sqrt(grid$x^2+grid$y^2)
coordinates(grid) <- ~x+y
gridded(grid) <- TRUE

output <- NULL

#Mesopic
interpol_dat <- subset(results, testtype=="Mesopic")
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

  output <- data.frame(testtype="Mesopic", X_corr=grid$x, Y_corr=grid$y, mean=mes_ok_mean$var1.pred, lwr=mes_ok_lwr$var1.pred)


#Cyan
interpol_dat <- subset(results, testtype=="Cyan")
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

  output <- rbind(output, data.frame(testtype="Cyan", X_corr=grid$x, Y_corr=grid$y, mean=cyan_ok_mean$var1.pred, lwr=cyan_ok_lwr$var1.pred))


#Red
interpol_dat <- subset(results, testtype=="Red")
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

  output <- rbind(output, data.frame(testtype="Red", X_corr=grid$x, Y_corr=grid$y, mean=red_ok_mean$var1.pred, lwr=red_ok_lwr$var1.pred))

#CRdiff
interpol_dat <- subset(results, testtype=="CRdiff")
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

  output <- rbind(output, data.frame(testtype="CRdiff", X_corr=grid$x, Y_corr=grid$y, mean=CRdiff_ok_mean$var1.pred, lwr=CRdiff_ok_lwr$var1.pred))


#Cyan
C <- ggplot(subset(output, testtype=="Cyan"), aes(x=X_corr, y=Y_corr, z=mean)) + geom_tile(aes(fill = mean)) + stat_contour(binwidth = 1, size = 0.5, colour = "black") + scale_fill_gradient2(limits=c(0, 25), low="black", high="cyan", mid="blue", midpoint=10, expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

D <- ggplot(subset(output, testtype=="Cyan"), aes(x=X_corr, y=Y_corr, z=lwr)) + geom_tile(aes(fill = lwr)) + stat_contour(binwidth = 1, size = 0.5, colour = "black") + scale_fill_gradient(expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

#Red
E <- ggplot(subset(output, testtype=="Red"), aes(x=X_corr, y=Y_corr, z=mean)) + geom_tile(aes(fill = mean)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient(limits=c(0, 25), low="white", high="red", expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

F <- ggplot(subset(output, testtype=="Red"), aes(x=X_corr, y=Y_corr, z=lwr)) + geom_tile(aes(fill = lwr)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient(low="white", high="red", expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

#Mesopic
A <- ggplot(subset(output, testtype=="Mesopic"), aes(x=X_corr, y=Y_corr, z=mean)) + geom_tile(aes(fill = mean)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient2(limits=c(18, 28), low="orange", high="green", mid="yellow", midpoint=22, expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

B <- ggplot(subset(output, testtype=="Mesopic"), aes(x=X_corr, y=Y_corr, z=lwr)) + geom_tile(aes(fill = lwr)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient(low="orange", high="green", expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)


#CRdiff
G <- ggplot(subset(output, testtype=="CRdiff"), aes(x=X_corr, y=Y_corr, z=mean)) + geom_tile(aes(fill = mean)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient2(limits=c(-20, 5), low="red", high="cyan", mid="purple", midpoint=-10, expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

H <- ggplot(subset(output, testtype=="CRdiff"), aes(x=X_corr, y=Y_corr, z=lwr)) + geom_tile(aes(fill = lwr)) + stat_contour(binwidth = 1, size = 0.5, colour = "black")  + scale_fill_gradient(limits=c(-20, 5), low="red", high="cyan", expression("Sensitivity [dB]")) + xlab('Temporal - nasal [?]') + ylab('Inferior - superior [?]') + xlim(-10, 10) + ylim(-10, 10)

plot_grid(A, C, E, G)
write.table(output, "interpol_data.csv", sep=";", row.names=FALSE, col.names=TRUE)


plot_grid(B, D, F, H)

