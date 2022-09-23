# This script has suggested figures for my TAFS publicaton

library(plotrix)



# Going to plot model averaged coefficients for TR, Producitivty and Marine Survival Models



### Taku

# Data
Taku.TR.MA = Taku.Model.Avg.TR.Std$coefTable
Taku.TR.MA.coef = Taku.TR.MA[2:6, 1] # So stupid...ranked not in order
Taku.TR.MA.coef.Final = c(-0.13895834, 0.22548064, -0.05557903, 0.14571790, 0.05002149)
Taku.TR.MA.stand.errors = Taku.TR.MA[2:6, 2]
Taku.TR.MA.stand.errors.Final = c(0.1151175, 0.1284838, 0.1311272, 0.1218631, 0.1328205)

Taku.Model.Avg.Prod.Std$coefTable
Taku.Prod.MA.coef.Final = c(0.1314038, 0.3303561, -0.3902406, 0.1980340, 0.0373109)
Taku.Prod.MA.stand.errors.Final = c(0.1820391, 0.2013866, 0.1896134, 0.1880635, 0.2169567)

Taku.Model.Avg.MS.Std$coefTable
Taku.MS.MA.coef.Final = c(0.31910348, -0.02769687, 0.24854203)
Taku.MS.MA.stand.errors.Final = c(0.1617479, 0.1653005, 0.1329164)

par(mar=c(5,7,0,3))

tiff("Taku.MA.tiff", width = 10, height = 10, units = 'in', res = 700)
par(family = "Times")
# TR
plotCI(x = Taku.TR.MA.coef.Final, y = c(1.64, 1.48, 1.33, 1.18, 1.03), uiw = 1.96*Taku.TR.MA.stand.errors.Final, err = "x", pch = 16, lwd = 3, cex = 1.5,
       xlim = c(-1, 1), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 1.5, yaxt = "n", family = "Times")
axis(1, lwd = 2.5, tck=-.01, mgp=c(1.80,1,0), cex.axis = 1.5)
text(x = 0, y = 1.75, " A) Taku River", cex = 1.5, family = "Times", font = 2)
mtext(c("FW1","SW2", "SW4"), side = 2, line = 1, at = c(1.60, 1.30, 1), las = 1, cex = 1.5, family = "Times")
mtext("SW1", side = 2, line = 1, at = (1.45), las = 1, cex = 1.5, family = "Times")
mtext("SW3", side = 2, line = 1, at = (1.15), las = 1, cex = 1.5, family = "Times")
clip(-30, 30, 0, 1.68)
abline(v=0, lwd = 3, lty = 2)

# Prod
plotCI(x = Taku.Prod.MA.coef.Final, y = c(1.60, 1.45, 1.30, 1.15, 1), uiw = 1.96*Taku.Prod.MA.stand.errors.Final, err = "x", pch = 15, lwd = 3, cex = 1.5,
       xlim = c(-1, 1), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 1.5, yaxt = "n", family = "Times", add = TRUE)

# MS
plotCI(x = Taku.MS.MA.coef.Final, y = c(1.41, 1.26, 1.11), uiw = 1.96*Taku.MS.MA.stand.errors.Final, err = "x", pch = 17, lwd = 3, cex = 1.5,
       xlim = c(-1, 1), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 1.5, yaxt = "n", family = "Times", add = TRUE)
dev.off()


### Unuk

# Data

Unuk.Model.Avg.TR.Std$coefTable
Unuk.TR.MA.coef.Final = c(0.06070596, 0.33256483, -0.01932104, 0.08414466, 0.09917489)
Unuk.TR.MA.stand.errors.Final = c(0.11775232, 0.09769285, 0.11837568, 0.10074456, 0.09647408)

Unuk.Model.Avg.Prod.Std$coefTable
Unuk.Prod.MA.coef.Final = c(0.08545404, 0.34866562, 0.05545430, 0.03854285, 0.13610123)
Unuk.Prod.MA.stand.errors.Final = c(0.1882184, 0.1505817, 0.1854497, 0.1635247, 0.1531175)

Unuk.Model.Avg.MS.Std$coefTable
Unuk.MS.MA.coef.Final = c(0.08332176, 0.30541793, 0.07500613)
Unuk.MS.MA.stand.errors.Final = c(0.1464401, 0.1152740, 0.1192712)


tiff("Unuk.MA.tiff", width = 10, height = 10, units = 'in', res = 700)
par(family = "Times")

# TR
plotCI(x = Unuk.TR.MA.coef.Final, y = c(1.68, 1.52, 1.37, 1.22, 1.07), uiw = 2*Unuk.TR.MA.stand.errors.Final, err = "x", pch = 16, lwd = 3, cex = 1.5,
       xlim = c(-1, 1), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 1.5, yaxt = "n", family = "Times")
axis(1, lwd = 2.5, tck=-.01, mgp=c(1.80,1,0), cex.axis = 1.5)
text(x = 0, y = 1.75, " B) Unuk River", cex = 1.5, family = "Times", font = 2)
mtext(c("FW1","SW2", "SW4"), side = 2, line = 1, at = c(1.64, 1.34, 1.04), las = 1, cex = 1.5, family = "Times")
mtext("SW1", side = 2, line = 1, at = (1.49), las = 1, cex = 1.5, family = "Times")
mtext("SW3", side = 2, line = 1, at = (1.19), las = 1, cex = 1.5, family = "Times")
clip(-30, 30, 0, 1.72)
abline(v=0, lwd = 3, lty = 2)

# Prod
plotCI(x = Unuk.Prod.MA.coef.Final, y = c(1.64, 1.49, 1.34, 1.19, 1.04), uiw = 2*Unuk.Prod.MA.stand.errors.Final, err = "x", pch = 15, lwd = 3, cex = 1.5,
       xlim = c(-1, 1), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 1.5, yaxt = "n", family = "Times", add = TRUE)

# MS
plotCI(x = Unuk.MS.MA.coef.Final, y = c(1.60, 1.45, 1), uiw = 2*Unuk.MS.MA.stand.errors.Final, err = "x", pch = 17, lwd = 3, cex = 1.5,
       xlim = c(-1, 1), ylim = c( 1, 2), axes = F, xlab = "Regression Coefficients", ylab = "", cex.lab = 1.5, yaxt = "n", family = "Times", add = TRUE)

dev.off()






## Annual growth versus BY

# Taku
Taku.Growth = read.csv("data/Tak.ASubs.TR.csv")
Taku.Growth

Taku.Growth.Long = read.csv("data/Taku.Growth.Long.csv")

tiff("Plot3.72.tiff", width = 10, height = 10, units = 'in', res = 700)


par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE, family = "Times", mfrow = c(2,1)) 
plot(Taku.Growth$cFW1.W ~ Taku.Growth$BY, type = "o", lty = 5, pch = 1, lwd = 2, ylab = "", xlab = "", main = "A) Taku River", axes = F, ylim = c(0, 1.5)) 
lines(Taku.Growth$cSW1.W ~ Taku.Growth$BY, lty = 1, type = "o", pch = 16, lwd = 2) 
lines(Taku.Growth$cSW2.W ~ Taku.Growth$BY, lty = 4, type = "o", pch = 22, lwd = 2) 
lines(Taku.Growth$cSW3.W ~ Taku.Growth$BY, lty = 6, type = "o", pch = 2, lwd = 2) 
lines(Taku.Growth$cSW4.W ~ Taku.Growth$BY, lty = 2, type = "o", pch = 15, lwd = 2) 
title(main = "", xlab = "Brood Year", ylab = expression(paste("Annual Scale Growth " (mu*m))), cex.lab=1.25, cex.main= 1.5, mgp=c(1.5,3,0)) 
axis(1, pos = 0, at = seq(1978, 2005, by = 3), lwd = 2, tck = -.01, mgp=c(1.80,.5,0)) 
axis(2, pos = 1978, lwd = 2, tck = -.01, ylim = c(0, 0.06), las = 1, mgp=c(1.80,.5,0)) 
legend(2006, 1.5, legend = c("FW1", "SW1", "SW2", "SW3", "SW4"), lty = c(5, 1, 4, 6, 2), pch = c(1, 16, 22, 2, 15), cex = .8)

# Unuk 
Unuk.Growth = read.csv("data/Unk.ASubs.TR.csv")
Unuk.Growth
Unuk.BYs = seq(1980, 2006, by = 2)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE, family = "Times")
plot(Unuk.Growth$cFW1.W ~ Unuk.Growth$BY, type = "o", lty = 5, pch = 1, lwd = 2, ylab = "", xlab = "", main = "B) Unuk River", axes = F, ylim = c(0, 1.5), xlim = c(1980, 2006))
lines(Unuk.Growth$cSW1.W ~ Unuk.Growth$BY, lty = 1, type = "o", pch = 16, lwd = 2)
lines(Unuk.Growth$cSW2.W ~ Unuk.Growth$BY, lty = 4, type = "o", pch = 22, lwd = 2)
lines(Unuk.Growth$cSW3.W ~ Unuk.Growth$BY, lty = 6, type = "o", pch = 2, lwd = 2)
lines(Unuk.Growth$cSW4.W ~ Unuk.Growth$BY, lty = 2, type = "o", pch = 15, lwd = 2)
title(main = "", xlab = "Brood Year", ylab = expression(paste("Annual Scale Growth " (mu*m))), cex.lab=1.25, cex.main= 1.5, mgp=c(1.5,3,0))
axis(1, pos = 0, lwd = 2, tck = -.01, mgp=c(1.80,.5,0), at = Unuk.BYs, labels = Unuk.BYs)
axis(2, pos = 1980, lwd = 2, tck = -.01, ylim = c(0, 0.06), las = 1, mgp=c(1.80,.5,0))
legend(2007, 1.5, legend = c("FW1", "SW1", "SW2", "SW3", "SW4"), lty = c(5, 1, 4, 6, 2), pch = c(1, 16, 22, 2, 15), cex = .8)

png("Plot.png")
dev.off()

## Recruitment benchmarks


tiff(file = "temp.tiff", width = 32, height = 3200, units = "in", res = 600)
par(mfrow=c(2,1), mai = c(.7, 0.7, 0.7, 0.7))
## Taku
# Total Return
yticks = seq(0, 200000, 20000)
plot(TR.BY ~ BY, main="", xlab="", ylab="", pch=16, axes=F, xlim=c(1975, 2010), ylim=c(0, 200000), mgp=c(1,3,0), font = 2, data = Weight.avg.data) 
title(main=, ylab="Total Return", cex.lab=1.15, cex.main=1.25, mgp=c(2.5,3,0), family = "Times")
axis(side=1,pos = 0, lwd=2, tck=-.01, mgp=c(1.80,.5,0))
axis(side=2, at = yticks, pos = 1975, lwd=2, las=1, tck=-.01,mgp=c(1.8,.5,0))
mtext(side = 1, line = 1, 'Brood Year', at = 1992.5, family = "Times", cex = 1.15)
mtext(side = 3, line = 1.15, " A) Taku River", cex = 1.5, at = 1992.5, family = "Times")

# Productivity
par(new = T)
yticks.prod = seq(0, 5, .5)
plot(Prod.Taku ~ BY, main="", xlab="", ylab="", pch = 0,  axes=F, xlim=c(1975, 2010), ylim=c(0, 5), mgp=c(1,3,0), font = 2, data = Weight.avg.data) 
axis(side=4, at = yticks.prod, pos = 2010, lwd=2, las=1, tck=-.01,mgp=c(1.8,.5,0))
mtext(side = 4, line = .9, 'Productivty', family = "Times", cex = 1.15)




## Unuk
# Total Return
yticks = seq(0, 20000, 2000)
plot(TR.1.2_1.5 ~ BY, main="", xlab="", ylab="", pch=16, axes=F, xlim=c(1975, 2010), ylim=c(0, 20000), mgp=c(1,3,0), font = 2, data = Weight.avg.data.Unuk.FINAL) 
title(main=, ylab="Total Return", cex.lab=1.15, cex.main=1.25, mgp=c(2.5,3,0), family = "Times")
axis(side=1,pos = 0, lwd=2, tck=-.01, mgp=c(1.80,.5,0))
axis(side=2, at = yticks, pos = 1975, lwd=2, las=1, tck=-.01,mgp=c(1.8,.5,0))
mtext(side = 1, line = 1, 'Brood Year', at = 1992.5, family = "Times", cex = 1.15)
mtext(side = 3, line = 1.15, " B) Unuk River", cex = 1.5, at = 1992.5, family = "Times")

# Productivity
par(new = T)
yticks.prod = seq(0, 5, .5)
plot(Prod.1.2_1.5 ~ BY, main="", xlab="", ylab="", pch = 0,  axes=F, xlim=c(1980, 2010), ylim=c(0, 5), mgp=c(1,3,0), font = 2, data = Weight.avg.data.Unuk.FINAL) 
axis(side=4, at = yticks.prod, pos = 2010, lwd=2, las=1, tck=-.01,mgp=c(1.8,.5,0))
mtext(side = 4, line = .9, 'Productivty', family = "Times", cex = 1.15)
dev.off()
# MS
## Taku 
# Marine Survival
Weight.avg.data.MS

yticks.MS = seq(0, .075, .015)
plot(MS ~ BY, main="", xlab="", ylab="", pch=16, axes=F, xlim=c(1990, 2005), ylim=c(0, .075), mgp=c(1,3,0), font = 2, data = Weight.avg.data.MS) 
title(main=, ylab="Marine Survival", cex.lab=1.15, cex.main=1.25, mgp=c(2.5,3,0), family = "Times")
axis(side=1,pos = 0, lwd=2, tck=-.01, mgp=c(1.80,.5,0))
axis(side=2, at = yticks.MS, pos = 1990, lwd=2, las=1, tck=-.01,mgp=c(1.8,.5,0))
mtext(side = 1, line = 1.2, 'Brood Year', at = 1997.5, family = "Times", cex = 1.15)
mtext(side = 3, line = 1.15, " A) Taku River", cex = 1.5, at = 1997.5, family = "Times")
Unuk.MS.abr

## Unuk
yticks.MS = seq(0, .075, .015)
plot(Marine.Sur.U ~ BY, main="", xlab="", ylab="", pch=16, axes=F, xlim=c(1980, 2010), ylim=c(0, .075), mgp=c(1,3,0), font = 2, data = Unuk.MS.abr) 
title(main=, ylab="Marine Survival", cex.lab=1.15, cex.main=1.25, mgp=c(2.5,3,0), family = "Times")
axis(side=1,pos = 0, lwd=2, tck=-.01, mgp=c(1.80,.5,0))
axis(side=2, at = yticks.MS, pos = 1980, lwd=2, las=1, tck=-.01,mgp=c(1.8,.5,0))
mtext(side = 1, line = 1.2, 'Brood Year', at = 1995, family = "Times", cex = 1.15)
mtext(side = 3, line = 1.15, " B) Unuk River", cex = 1.5, at = 1995, family = "Times")



