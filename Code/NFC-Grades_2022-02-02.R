# NFC AND ABILITY SELF-CONCEPTS AS PREDICTORS OF CHANGES IN SCHOOL GRADES =====

# required packages -----------------------------------------------------------

library(haven)      # for reading SPSS data file
library(lavaan)     # for latent change score modeling
library(papaja)     # for easier reporting
library(psych)      # for correlation analysis, Mardia test etc.
library(robustbase) # for robust regression
library(shape)      # for plotting

# data ------------------------------------------------------------------------
d = read_sav("KoopRicarda/2006_2007_nfc.sav")
d.var.labels = data.frame(attr(d, "variable.labels"))

# sample description ----------------------------------------------------------

# age and gender
age = rbind(describe(d$alter_I), describe(d$alter_II))
rownames(age) = c("T1", "T2")
gender = cbind(rbind(table(d$sex), table(d$sex_II)), round(rbind(prop.table(table(d$sex)), prop.table(table(d$sex_II))),2))
colnames(gender) = c("female", "male", "%female", "%male")
rownames(gender) = c("T1", "T2")

# date and time interval
T1 = as.POSIXct(d$datum_I)
T2 = as.POSIXct(d$datum_II)
Tdiff = round(range(T2 - T1, na.rm = T)/7)
attr(Tdiff, "units") = "weeks"

# variables -------------------------------------------------------------------

# GRades (Noten allgemein und fächerspezifisch)
# O = overall, G = German, M = math, P = physics, C = chemistry 
GRO1 = 6 - d$note_all    
GRO2 = 6 - d$note_all_2
GRG1 = 6 - d$note_d      
GRG2 = 6 - d$note_d_2
GRM1 = 6 - d$not_m       
GRM2 = 6 - d$not_m_2
GRP1 = 6 - d$note_p      
GRP2 = 6 - d$note_p_2
GRC1 = 6 - d$note_c      
GRC2 = 6 - d$note_c_2

# Academic Selfconcept (Fähigkeitsselbstkonzept allgemein/fächerspezifisch)
ASO1 = d$fsk_al
ASO2 = d$fsk_al_2
ASG1 = d$fsk_d
ASG2 = d$fsk_d_2
ASM1 = d$fsk_m
ASM2 = d$fsk_m_2
ASP1 = d$fsk_p
ASP2 = d$fsk_p_2
ASC1 = d$fsk_c
ASC2 = d$fsk_c_2

# INterest (Interesse allgemein und fächerspezifisch)
INO1 = d$in_al
INO2 = d$in_al_2
ING1 = d$in_d
ING2 = d$in_d_2
INM1 = d$in_m
INM2 = d$in_m_2
INP1 = d$in_p
INP2 = d$in_p_2
INC1 = d$in_c
INC2 = d$in_c_2

# Hope For Success (Hoffnung auf Erfolg)
HFS1 = d$hae
HFS2 = d$hae_2

# Fear Of Failure (Furcht vor Misserfolg)
FOF1 = d$fvm
FOF2 = d$fvm_2

# Need For Cognition
NFC1 = d$ska_nfc
NFC2 = d$ska_nfc_2
data = data.frame(GRO1, GRO2, GRG1, GRG2, GRM1, GRM2, GRP1, GRP2, GRC1, GRC2, ASO1, ASO2, ASG1, ASG2, ASM1, ASM2, ASP1, ASP2, ASC1, ASC2, INO1, INO2, ING1, ING2, INM1, INM2, INP1, INP2, INC1, INC2, HFS1, HFS2, FOF1, FOF2, NFC1, NFC2)
final = data.frame(DAT1 = d$datum_I, DAT2 = d$datum_II, AGE1 = d$alter_I, AGE2 = d$alter_II, SEX1 = d$sex, SEX2 = d$sex_II)

# boxplots 4 attachment ----

data4boxplots = data.frame(GRO1, GRO2, GRG1, GRG2, GRM1, GRM2, GRP1, GRP2, GRC1, GRC2, HFS1, HFS2, ASO1, ASO2, ASG1, ASG2, ASM1, ASM2, ASP1, ASP2, ASC1, ASC2, FOF1, FOF2, INO1, INO2, ING1, ING2, INM1, INM2, INP1, INP2, INC1, INC2, NFC1, NFC2)
par(mfrow = c(3, 6), mar = c(2,2,2,2))
for (i in seq(1, ncol(data4boxplots), 2)) {
  data2plot = data4boxplots[,c(i, i + 1)]
  mean.sd = cbind(describe(data2plot)$mean, describe(data2plot)$sd)
  varlabel  = substr(colnames(data2plot)[1], 1, 3)
  plot(c(0, 3), c(0, 7), type = "n", xaxt = "n", ylim = c(0, 7), las = 1, main = varlabel)
  axis(1, 1:2, c("T1", "T2"))
  boxplot(data2plot, add = T, at = 1:2, axes = F, lty = 1, lwd = 1.5, notch = T, boxwex = .2,  staplewex = NA)
  lines(1:2, colMeans(data2plot, na.rm = T), type = "o", lwd = 1.5, pch = 19, cex = 0.75, col = 2)
  lines(rep(1, 2), c(mean.sd[1, 1] - mean.sd[1, 2], mean.sd[1, 1] + mean.sd[1, 2]), col = 2, lwd = 1.5)
  lines(rep(2, 2), c(mean.sd[2, 1] - mean.sd[2, 2], mean.sd[2, 1] + mean.sd[2, 2]), col = 2, lwd = 1.5)
}
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# Multiple regression to select variables for latent change score modeling ----

# data.frames
df.overall    = data.frame(grd.1 = gro.1, asc.1 = aso.1, int.1 = ino.1, hfs.1, fof.1, nfc.1, grd.2 = gro.2, asc.2 = aso.2, int.2 = ino.2, hfs.2, fof.2, nfc.2)
df.german     = data.frame(grd.1 = grg.1, asc.1 = asg.1, int.1 = ing.1, hfs.1, fof.1, nfc.1, grd.2 = grg.2, asc.2 = asg.2, int.2 = ing.2, hfs.2, fof.2, nfc.2)
df.math       = data.frame(grd.1 = grm.1, asc.1 = asm.1, int.1 = inm.1, hfs.1, fof.1, nfc.1, grd.2 = grm.2, asc.2 = asm.2, int.2 = inm.2, hfs.2, fof.2, nfc.2)
df.physics    = data.frame(grd.1 = grp.1, asc.1 = asp.1, int.1 = inp.1, hfs.1, fof.1, nfc.1, grd.2 = grp.2, asc.2 = asp.2, int.2 = inp.2, hfs.2, fof.2, nfc.2)
df.chemistry  = data.frame(grd.1 = grc.1, asc.1 = asc.1, int.1 = inc.1, hfs.1, fof.1, nfc.1, grd.2 = grc.2, asc.2 = asc.2, int.2 = inc.2, hfs.2, fof.2, nfc.2)

# multivariate nor,ality
mardia.overall    = mardia(df.overall, plot = F)
mardia.german     = mardia(df.german, plot = F)
mardia.math       = mardia(df.math, plot = F)
mardia.physics    = mardia(df.physics, plot = F)
mardia.chemistry  = mardia(df.chemistry, plot = F)

# regressions
mr <- function(data) {
  
  # models
  m.null.mod = 'grd.2 ~ 0 * grd.1 + 0 * asc.1 + 0 * int.1 + 0 * hfs.1 + 0 * fof.1 + 0 * nfc.1 \n grd.2 ~ 1'
  m.full.mod = 'grd.2 ~ a * grd.1 + b * asc.1 + c * int.1 + d * hfs.1 + e * fof.1 + f * nfc.1 \n grd.2 ~ 1'
  m.only.grd = 'grd.2 ~ a * grd.1 + 0 * asc.1 + 0 * int.1 + 0 * hfs.1 + 0 * fof.1 + 0 * nfc.1 \n grd.2 ~ 1'
  m.with.nfc = 'grd.2 ~ a * grd.1 + b * asc.1 + 0 * int.1 + 0 * hfs.1 + 0 * fof.1 + f * nfc.1 \n grd.2 ~ 1'
  m.wout.nfc = 'grd.2 ~ a * grd.1 + b * asc.1 + 0 * int.1 + 0 * hfs.1 + 0 * fof.1 + 0 * nfc.1 \n grd.2 ~ 1'

  # fitted models 
  f.null.mod = sem(m.null.mod, data = data, fixed.x = F, missing = "fiml", estimator = "ml")
  f.full.mod = sem(m.full.mod, data = data, fixed.x = F, missing = "fiml", estimator = "ml")
  f.only.grd = sem(m.only.grd, data = data, fixed.x = F, missing = "fiml", estimator = "ml")
  f.with.nfc = sem(m.with.nfc, data = data, fixed.x = F, missing = "fiml", estimator = "ml")
  f.wout.nfc = sem(m.wout.nfc, data = data, fixed.x = F, missing = "fiml", estimator = "ml")
  
  # summaries
  s.null.mod = summary(f.null.mod, fit.measures = T, standardized = T, rsquare = T)
  s.full.mod = summary(f.full.mod, fit.measures = T, standardized = T, rsquare = T)
  s.only.grd = summary(f.only.grd, fit.measures = T, standardized = T, rsquare = T)
  s.with.nfc = summary(f.with.nfc, fit.measures = T, standardized = T, rsquare = T)
  s.wout.nfc = summary(f.wout.nfc, fit.measures = T, standardized = T, rsquare = T)
  
  # parameter estimates
  p.null.mod = parameterEstimates(f.null.mod, standardized = T, rsquare = T)[c(7, 1:6, 36), -c(4, 11, 13)]
  p.full.mod = parameterEstimates(f.full.mod, standardized = T, rsquare = T)[c(7, 1:6, 36), -c(4, 11, 13)]
  p.only.grd = parameterEstimates(f.only.grd, standardized = T, rsquare = T)[c(7, 1:6, 36), -c(4, 11, 13)]
  p.with.nfc = parameterEstimates(f.with.nfc, standardized = T, rsquare = T)[c(7, 1:6, 36), -c(4, 11, 13)]
  p.wout.nfc = parameterEstimates(f.wout.nfc, standardized = T, rsquare = T)[c(7, 1:6, 36), -c(4, 11, 13)]
  
  # model comparison
  compare.models = anova(f.null.mod, f.full.mod, f.only.grd, f.with.nfc, f.wout.nfc)
  
  # output
  return = list(f = list(f.null.mod = f.null.mod,
                         f.full.mod = f.full.mod,
                         f.only.grd = f.only.grd,
                         f.with.nfc = f.with.nfc,
                         f.wout.nfc = f.wout.nfc),
                s = list(s.null.mod = s.null.mod,
                         s.full.mod = s.full.mod,
                         s.only.grd = s.only.grd,
                         s.with.nfc = s.with.nfc,
                         s.wout.nfc = s.wout.nfc),
                p = list(p.null.mod = p.null.mod,
                         p.full.mod = p.full.mod,
                         p.only.grd = p.only.grd,
                         p.with.nfc = p.with.nfc,
                         p.wout.nfc = p.wout.nfc),
                a = compare.models)
}

sink("mr.overall.txt")
mr.overall = mr(df.overall)
sink()

sink("mr.german.txt")
mr.german = mr(df.german)
sink()

sink("mr.math.txt")
mr.math = mr(df.math)
sink()

sink("mr.physics.txt")
mr.physics = mr(df.physics)
sink()

sink("mr.chemistry.txt")
mr.chemistry = mr(df.chemistry)
sink()


# Latent change score modeling ------------------------------------------------

lcsm <- '
# --------------------- VARIABLE LABELS
# grd                   School Grades 
# nfc                   Need for Cognition
# asc                   Academic Self Concept
# .1                    First Assessment
# .2                    Second Assessment
# .d                    Latent Change Score

# --------------------- GRADES REGRESSIONS
grd.2  ~ 1 * grd.1      # This parameter regresses grd.2 perfectly on grd.1
grd.d =~ 1 * grd.2      # This defines the latent change score factor as measured perfectly by scores on grd.2

# --------------------- GRADES INTERCEPTS
grd.d  ~ 1              # This estimates the intercept of the grd change score
grd.1  ~ 1              # This estimates the intercept of grd.1
grd.2  ~ 0 * 1          # This constrains the intercept of grd.2 to 0

# --------------------- GRADES VARIANCES
grd.1 ~~ grd.1          # This estimates the variance of the grd.1
grd.2 ~~ 0 * grd.2      # This fixes the variance of the grd.2 to 0
grd.d ~~ grd.d          # This estimates the variance of the change scores

# --------------------- ABILITY SELF-CONCEPT REGRESSIONS
asc.2  ~ 1 * asc.1      # This parameter regresses asc.2 perfectly on asc.1
asc.d =~ 1 * asc.2      # This defines the latent change score factor as measured perfectly by scores on asc.2

# --------------------- ABILITY SELF-CONCEPT INTERCEPTS
asc.1  ~ 1              # This estimates the intercept of asc.1
asc.2  ~ 0 * 1          # This line constrains the intercept of asc.2 to 0
asc.d  ~ 1              # This estimates the intercept of the change score

# --------------------- ABILITY SELF-CONCEPT VARIANCES
asc.1 ~~ asc.1          # This estimates the variance of asc.1
asc.2 ~~ 0 * asc.2      # This fixes the variance of the asc.2 to 0
asc.d ~~ asc.d          # This estimates the variance of the change scores

# --------------------- NEED FOR COGNITION REGRESSIONS
nfc.2  ~ 1 * nfc.1      # This parameter regresses nfc.2 perfectly on nfc.1
nfc.d =~ 1 * nfc.2      # This defines the latent change score factor as measured perfectly by scores on nfc.2

# --------------------- NEED FOR COGNITION INTERCEPTS
nfc.1  ~ 1              # This estimates the intercept of nfc.1
nfc.2  ~ 0 * 1          # This constrains the intercept of nfc.2 to 0
nfc.d  ~ 1              # This estimates the intercept of the change score

# --------------------- NEED FOR COGNITION VARIANCES
nfc.1 ~~ nfc.1          # This estimates the variance of nfc.1
nfc.2 ~~ 0 * nfc.2      # This fixes the variance of the nfc.2 to 0
nfc.d ~~ nfc.d          # This estimates the variance of the change scores

# --------------------- SELF-FEEDBACK & CROSS-DOMAIN COUPLING
grd.d  ~ beta1 * grd.1 + gamma12 * asc.1 + gamma13 * nfc.1 
asc.d  ~ gamma21 * grd.1 + beta2 * asc.1 + gamma23 * nfc.1
nfc.d  ~ gamma31 * grd.1 + gamma32 * asc.1 + beta3 * nfc.1 

# --------------------- COVARIANCES
grd.1 ~~  phi12 * asc.1 # This estimates the covariances at T1
grd.1 ~~  phi13 * nfc.1
asc.1 ~~  phi23 * nfc.1
grd.d ~~  rho12 * asc.d # This estimates the change score covariances
grd.d ~~  rho13 * nfc.d
asc.d ~~  rho23 * nfc.d
'

# Overall Grades and Academic Self Concept together with NFC ----
fit.overall <- lavaan(lcsm, data = df.overall, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
# summary(fit.overall, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# German Grades and Academic Self Concept together with NFC ----
fit.german <- lavaan(lcsm, data = df.german, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
# summary(fit.german, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE) 

# Math Grades and Academic Self Concept together with NFC ----
fit.math <- lavaan(lcsm, data = df.math, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
# summary(fit.math, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE) 

# Physics Grades and Academic Self Concept together with NFC ----
fit.physics <- lavaan(lcsm, data = df.physics, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
# summary(fit.physics, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE) 

# Chemistry Grades and Academic Self Concept together with NFC ----
fit.chemistry <- lavaan(lcsm, data = df.chemistry, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
# summary(fit.chemistry, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE) 

# get parameter estimates from LCSMs
sf_cdc = list(sf_cdc_overall    = parameterEstimates(fit.overall,   standardized = T)[25:33, -c(10:11)],
              sf_cdc_german     = parameterEstimates(fit.german,    standardized = T)[25:33, -c(10:11)],
              sf_cdc_math       = parameterEstimates(fit.math,      standardized = T)[25:33, -c(10:11)],
              sf_cdc_physics    = parameterEstimates(fit.physics,   standardized = T)[25:33, -c(10:11)],
              sf_cdc_chemnistry = parameterEstimates(fit.chemistry, standardized = T)[25:33, -c(10:11)])


# plot LCSM -------------------------------------------------------------------

# function for plotting ellipses at specified positions
circle = function(x, y, scale = 1, angle = 1:360, plot = T, col = 1) {
  v = seq(0, 2 * pi, length.out = 360)
  coords = cbind(sin(v) * scale + x, cos(v) * scale + y)
  coords = rbind(coords, coords[1, ])
  if (plot == T) { lines(coords[angle,], col = col) }
  invisible(coords[angle, ])
}
triangle = function(x, y, h = 1, w = 1, fill = 0) {
  X = seq(x, x + w, length.out = 100)
  Y = c(seq(y, y + h, length.out = 50), seq(y + h, y, length.out = 50))
  polygon(c(X, rev(X)), c(Y, rep(y, 100)), col = fill)
  invisible(cbind(X, Y))
}
# adds coefficients to plots
add.coef = function(x, y, coef, gt1 = FALSE, enlarge = .06, font = 1, cex = 1, pos = NULL, col = 1) {
  coef = format(round(coef, 2), nsmall = 2)
  coef = sub("0.00", "0", coef)
  coef = sub("1.00", "1", coef)
  #coef=sub(".00","0",coef)
  coef = sub("> .99", "1", coef)
  w = enlarge + strwidth(coef) * cex / 2
  h = enlarge + strheight(coef) * cex / 2
  rect(x - w, y - h, x + w, y + h, border = NA, col = "#FFFFFF")
  #points(x,y,pch=19,cex=cex,col=0)
  text(x, y, coef, font = font, cex = cex, pos = pos, col = col)
}

# plots trivariate latent change score model
plot.lcsm <- function(fit, index = "", title = "") {
  
  # plot omits the intercepts as well as the manifest variables at T2 
  # because this would make the plot too hard to decipher
  
  # parameter estimates and their significance (for plotting sig. coefs bold-face)
  pe = parameterEstimates(fit, standardized = T)[25:39, ]
  sig = (pe$pvalue < .05) + 1
  
  # setup plot
  plot(c(0, 12), c(0, 12), type = "n", axes = F, xlab = "", ylab = "")
  
  # manifest T1
  rect(2, 1, 4,  3)
  rect(2, 5, 4,  7)
  rect(2, 9, 4, 11)
  # latent change scores
  c1 = circle(9,  2, scale = 1.1)
  c2 = circle(9,  6, scale = 1.1)
  c3 = circle(9, 10, scale = 1.1)
  
  # labels
  text(c(3, 3, 3, 9, 9, 9), c(2, 6, 10, 2, 6, 10),
       c('NFC.1', 'ASC.1', 'GRD.1', expression(Delta ~ NFC), expression(Delta ~ ASC), expression(Delta ~ GRD)))
  
  # arrows from lower left
  Arrows(4, 2.0,  7.9, 2, code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#993333')
  Arrows(4, 2.1, c2[247,1], c2[247,2], code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#336699')
  Arrows(4, 2.2, c3[225,1], c3[225,2], code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#336699')
  
  # arrows from middle left
  Arrows(4, 6.0, 7.9, 6, code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#993333')
  Arrows(4, 5.9, c1[292,1], c1[292,2], code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#336699')
  Arrows(4, 6.1, c3[247,1], c3[247,2], code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#336699')
  
  # arrows from top left
  Arrows(4, 10, 7.9, 10, code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#993333')
  Arrows(4, 9.9, c2[292,1], c2[292,2], code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#336699')
  Arrows(4, 9.8, c1[315,1], c1[315,2], code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#336699')
  
  # correlations left
  Arrows(3, 3, 3, 5, code = 3, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = grey(.6))
  Arrows(3, 7, 3, 9, code = 3, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = grey(.6))
  lines(getellipse(rx = 1, ry = 4, mid = c(2, 6), from = 1/2 * pi, to = -1/2 * pi), col = grey(.6))
  Arrowhead(2,  2, arr.length = .2, arr.adj = 1, arr.type = "triangle", lcol = grey(.6), angle = -20)
  Arrowhead(2, 10, arr.length = .2, arr.adj = 1, arr.type = "triangle", lcol = grey(.6), angle =  20)
  
  # correlations right
  Arrows(9, 3.1, 9, 4.9,  code = 3, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#339933')
  Arrows(9, 7.1, 9, 8.9,  code = 3, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#339933')
  lines(getellipse(rx = 1, ry = 4, mid = c(10.1, 6), from = -1/2 * pi, to = 1/2 * pi), col = "#339933")
  Arrowhead(10.1,  2, arr.length = .2, arr.adj = 1, arr.type = "triangle", lcol = "#339933", angle = 200)
  Arrowhead(10.1, 10, arr.length = .2, arr.adj = 1, arr.type = "triangle", lcol = "#339933", angle = 160)
  
  # paths originating from GPA
  add.coef(4.8, 10.0, pe[which(pe$label == 'beta1'),   'std.nox'], font = sig[which(pe$label == 'beta1')],   col = '#993333')
  add.coef(4.8,  9.2, pe[which(pe$label == 'gamma21'), 'std.nox'], font = sig[which(pe$label == 'gamma21')], col = '#336699')
  add.coef(4.8,  8.5, pe[which(pe$label == 'gamma31'), 'std.nox'], font = sig[which(pe$label == 'gamma31')], col = '#336699')
  
  # paths originating from ASC
  add.coef(4.8,  6.0,pe[which(pe$label == 'beta2'),   'std.nox'], font = sig[which(pe$label == 'beta2')],   col = '#993333')
  add.coef(4.8,  6.8,pe[which(pe$label == 'gamma12'), 'std.nox'], font = sig[which(pe$label == 'gamma12')], col = '#336699')
  add.coef(4.8,  5.2,pe[which(pe$label == 'gamma32'), 'std.nox'], font = sig[which(pe$label == 'gamma32')], col = '#336699')
  
  # paths originating from NFC
  add.coef(4.8,  2.0,pe[which(pe$label == 'beta3'),   'std.nox'], font = sig[which(pe$label == 'beta3')],   col = '#993333')
  add.coef(4.8,  3.5,pe[which(pe$label == 'gamma13'), 'std.nox'], font = sig[which(pe$label == 'gamma13')], col = '#336699')
  add.coef(4.8,  2.8,pe[which(pe$label == 'gamma23'), 'std.nox'], font = sig[which(pe$label == 'gamma23')], col = '#336699')
    
  # correlations T1  
  add.coef(3.0,  8.0,pe[which(pe$label == 'phi12'),   'std.nox'], font = sig[which(pe$label == 'phi12')], col = grey(.6))
  add.coef(1.0,  6.0,pe[which(pe$label == 'phi13'),   'std.nox'], font = sig[which(pe$label == 'phi13')], col = grey(.6))
  add.coef(3.0,  4.0,pe[which(pe$label == 'phi23'),   'std.nox'], font = sig[which(pe$label == 'phi23')], col = grey(.6))
    
  # correlations change scores  
  add.coef(9.0,  8.0,pe[which(pe$label == 'rho12'),   'std.nox'], font = sig[which(pe$label == 'rho12')], col = '#339933')
  add.coef(11.,  6.0,pe[which(pe$label == 'rho13'),   'std.nox'], font = sig[which(pe$label == 'rho13')], col = '#339933')
  add.coef(9.0,  4.0,pe[which(pe$label == 'rho23'),   'std.nox'], font = sig[which(pe$label == 'rho23')], col = '#339933')
  
  # add panel index and title
  text(1, 12, index, cex = 2)
  text(6, 12, title, cex = 2)
  
  invisible(pe)
  
}

demo.lcsm <-  function(index, title) {
  plot(c(0, 12), c(0, 12), type = "n", axes = F, xlab = "", ylab = "")
  
  # manifest T1
  rect(2, 1, 4,  3)
  rect(2, 9, 4, 11)
  rect(8, 1, 10,  3)
  rect(8, 9, 10, 11)
  
  # latent change scores
  c1 = circle(9,  7.25, scale = 1.1)
  c2 = circle(9,  4.75, scale = 1.1)
  
  # labels
  text(c(3, 3, 9, 9, 9, 9), c(2, 10, 2, 4.75, 7.25, 10), 
       c('Y.1', 'X.1', 'Y.2', expression(Delta ~ Y), expression(Delta ~ X), 'X.2'))
  
  # arrows bottom
  lines(c(4, 8), rep(2, 2), lty = 3)
  Arrowhead(8, 2.0, arr.length = .2, arr.type = "triangle", arr.adj = 1)
  Arrows(4, 2.2,  c2[247,1], c2[247,2], code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#993333')
  lines(rep(9, 2), c(3, 3.65), lty = 3)
  Arrowhead(9,3.65, arr.length = .2, arr.type = "triangle", arr.adj = 2, angle = 90)
  Arrows(4, 2.4,  c1[247,1], c1[247,2], code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#336699')
  
  # arrows top
  lines(c(4, 8), rep(10, 2), lty = 3)
  Arrowhead(8, 10, arr.length = .2, arr.type = "triangle", arr.adj = 1)
  Arrows(4, 9.8, c1[292,1], c1[292,2], code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2)
  lines(c(9, 9), c(9.0, 8.35), lty = 3)
  Arrowhead(9,8.35, arr.length = .2, arr.type = "triangle", arr.adj = 2, angle = 270)
  Arrows(4, 9.6, c2[292,1], c2[292,2], code = 2, arr.length = .2, arr.type = "triangle", arr.adj = 2, col = '#336699')
  
  # correlations left
  lines(getellipse(rx = 1, ry = 4, mid = c(2, 6), from = 1/2 * pi, to = -1/2 * pi), col = grey(.6))
  Arrowhead(2,  2, arr.length = .2, arr.adj = 1, arr.type = "triangle", lcol = grey(.6), angle = -10)
  Arrowhead(2, 10, arr.length = .2, arr.adj = 1, arr.type = "triangle", lcol = grey(.6), angle =  10)
  
  # correlations right
  lines(getellipse(rx = 1, ry = 1.35, mid = c(10.1, 6), from = -1/2 * pi, to = 1/2 * pi), col = "#339933")
  Arrowhead(10.1, 4.65, arr.length = .2, arr.adj = 1, arr.type = "triangle", lcol = "#339933", angle = 180)
  Arrowhead(10.1, 7.35, arr.length = .2, arr.adj = 1, arr.type = "triangle", lcol = "#339933", angle = 180)
  
  # add labels
  add.label = function(x, y, label, font = 1, cex = 1, pos = NULL, col = 1) {
    points(x, y, pch = 19, cex = cex + 3, col = 0)
    text(x, y, label, font = font, cex = cex, pos = pos, col = col)
  }
  
  # fixed regressions
  # left
  add.label(1.0,6.0, expression(phi), cex = 1.75, col = grey(.6))
  
  # self-feedback and cross-domain coupling
  # middle top
  add.label(6, 8.75, expression(beta[X]),   cex = 1.5, col = '#993333')
  add.label(6, 7.40, expression(lambda[X]), cex = 1.5, col = '#336699')
  # middle bottom
  add.label(6, 4.60, expression(lambda[Y]), cex = 1.5, col = '#336699')
  add.label(6, 3.25, expression(beta[Y]),   cex = 1.5, col = '#993333')
  
  # correlations change scores
  add.label(11.2, 6.0, expression(rho), cex = 1.75, col = '#339933')
  
  # add panel index and title
  text(1, 12, index, cex = 2)
  text(6, 12, title, cex = 2)
}

# in RStudio, play around a bit with the exact plot window size to get best results 
par(mfrow = c(3, 2), mar = c(0,0,0,0))
demo.lcsm("A","Example of Bivariate Model")
plot.lcsm(fit.overall,   index = "B", title = "GPA")
plot.lcsm(fit.german,    index = "D", title = "German")
plot.lcsm(fit.math,      index = "C", title = "Math")
plot.lcsm(fit.physics,   index = "E", title = "Physics")
plot.lcsm(fit.chemistry, index = "F", title = "Chemistry")
par(mfrow = c(1, 1), mar = c(5,4,4,2))
# now save plot in the plot window via Export > Save as Image... and choose
# desired output format

# save plot automatically as EPS (it won't look nice, though)
# dev.copy2eps(file="Fig1_auto.eps",width=mm2in(190), height = mm2in(250))

# save all variables for use im R Markdown document
save.image("~/Documents/R/nfc_rf/NFC_ASC_Grades.RData")
