# NFC AND ABILITY SELF-CONCEPTS AS PREDICTORS OF CHANGES IN SCHOOL GRADES =====

# activate and restore project------------------------------------------------
source('renv/activate.R')
renv::activate(getwd())
renv::restore()

# required packages -----------------------------------------------------------
library(haven)      # for reading SPSS data file
library(lavaan)     # for latent change score modeling
library(naniar)     # test data with missings for MCAR
library(papaja)     # for easier reporting
library(psych)      # for correlation analysis, Mardia test etc.
library(semTools)   # for various SEM related tools
library(shape)      # for plotting

# global functions ------------------------------------------------------------

# function for reporting p-values in main text (wrapper for papaja::printp)
rp <- function(p, bound = "max") {
  if (length(p) == 1) {
    # single p-value is passed
    if (p >= .001) {
      # returns "p = ..."
      p_out = paste0("$p=", papaja::printp(p), "$")
  } else {
      # returns "p < .001"
      p_out = paste0("$p", papaja::printp(p), "$")
  } 
} else if (length(p) > 1) {
    # vector of p-values is passed
    if (bound == "max") {
      # returns "p <= ..."
      p_out = paste0("$p", "\\le", papaja::printp(max(p)), "$")
  } else if (bound == "min") {
      # returns "p >= ..."
      p_out = paste0("$p", "\\ge", papaja::printp(min(p)), "$")
  }
    
}
  return(p_out)
}

# function for reporting correlations in main text (wrapper for papaja::printnum)
rr <-  function(r, symbol = "$r_{s}", bound = "max", absval = F) {
  # per default assumes Spearman correlation, set symbol to "$r" for Pearson 
  
  # shall absolute value of correlations be returned?
  if (absval == T) {
    symbol = paste0("$|", sub("[$]", "", symbol), "|")
}
  
  if (length(r) == 1) {
    # single correlation is passed
    if (r < .01) {
      # returns "r < .01"
      r_out = paste0(symbol, "<", papaja::printnum(r, gt1 = F), "$")
  } else {
      # returns "r = ..."
      r_out = paste0(symbol, "=", papaja::printnum(r, gt1 = F), "$")
  }  
} else {
    # vector of correlations is passed
    if (bound == "max") {
      # returns "r <= ..."
      r_out = paste0(symbol, "\\le", papaja::printnum(max(r), gt1 = F), "$")
  } else if (bound == "min") {
      # returns "r >= ..."
      r_out = paste0(symbol, "\\ge", papaja::printnum(min(r), gt1 = F), "$")
  }
}
  return(r_out)
}

# function for reporting regression coefficients based on multiple regression using lavaan
rb <- function(parameter_estimates, pred = "") {
  # shorten varname for easier coding
  pe = parameter_estimates
  
  # which row contains predictor?
  pe_row = which(pe$rhs == pred)
  
  # create report string
  report = paste0("$B=$ ", printnum(pe$est[pe_row]), 
                  ", 95% CI [", paste0(printnum(pe[pe_row, c('ci.lower', 'ci.upper')]), collapse = ', '), "], "
                  , rp(pe[pe_row, 'pvalue']))
  
  return(report)
}

# function for reporting LCSM results 
rl <- function(fit, label = "") {
  
  # get all parameter estimates
  parameter_estimates = parameterEstimates(fit, standardized = T)
  
  # get relevant parameter estimates
  pe = parameter_estimates[parameter_estimates$label != "", ]
  
  # prepare output
  out = paste0("$B=$ ", printnum(pe[pe$label == label, 'est']), 
               ", 95% CI [", paste0(printnum(pe[pe$label == label, c('ci.lower', 'ci.upper')]), collapse = ", "), "], ",
               rp(pe[pe$label == label, 'pvalue']), ", ",
               "$\\beta=$ ", printnum(pe[pe$label == label, 'std.all'], gt1 = F))
  
  return(out)
  
}


# set root directory ----------------------------------------------------------
here::i_am("flag_root_for_NFC-Grades.txt")

# data ------------------------------------------------------------------------
d = read.csv(here::here("Data", "NFC-Grades.csv"))
d.codebook = read.csv(here::here("Data", "NFC-Grades_Codebook.csv"))

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
# O = overall, G = German, M = Math, P = Physics, C = Chemistry 
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


# internal consistencies ------------------------------------------------------

# function to extract internal consistencies
raw.alpha <- function(df) {
  alpha.1 = printnum(alpha(df[, 1:(ncol(df)/2)], max = 30, warnings = F)$total$raw_alpha, gt1 = F) 
  alpha.2 = printnum(alpha(df[, (ncol(df)/2 + 1):ncol(df)], max = 30, warnings = F)$total$raw_alpha, gt1 = F)
  return(c(alpha.1, alpha.2))
}

# Grades (singular values, no reliability computation possible)
alpha.gro = rep(NA, 2)
alpha.grg = rep(NA, 2)
alpha.grm = rep(NA, 2)
alpha.grp = rep(NA, 2)
alpha.grc = rep(NA, 2)

# Ability self-concept
asc.items = d[, grep("kom", colnames(d))]
aso.items = asc.items[, grep("_a", colnames(asc.items))]
asg.items = asc.items[, grep("_d", colnames(asc.items))]
asm.items = asc.items[, grep("_m", colnames(asc.items))]
asp.items = asc.items[, grep("_p", colnames(asc.items))]
asc.items = asc.items[, grep("_c", colnames(asc.items))]
alpha.aso = raw.alpha(aso.items)
alpha.asg = raw.alpha(asg.items)
alpha.asm = raw.alpha(asm.items)
alpha.asp = raw.alpha(asp.items)
alpha.asc = raw.alpha(asc.items)

# Interest
int.items = d[, grep("in_", colnames(d))]
ino.items = int.items[, grep("_a", colnames(int.items))][,c(1:3,5:7)]
ing.items = int.items[, grep("_d", colnames(int.items))][,c(1:3,5:7)]
inm.items = int.items[, grep("_m", colnames(int.items))][,c(1:3,5:7)]
inp.items = int.items[, grep("_p", colnames(int.items))][,c(1:3,5:7)]
inc.items = int.items[, grep("_c", colnames(int.items))][,c(1:3,5:7)]
alpha.ino = raw.alpha(ino.items)
alpha.ing = raw.alpha(ing.items)
alpha.inm = raw.alpha(inm.items)
alpha.inp = raw.alpha(inp.items)
alpha.inc = raw.alpha(inc.items)

# Hope for Success
hfs.items = d[, grep("lm_", colnames(d))]
alpha.hfs = raw.alpha(hfs.items)

# Fear of Failure
fof.items = d[, grep("an_", colnames(d))]
alpha.fof = raw.alpha(fof.items)

# Need for Cognition
nfc.items = d[, grep("nfc", colnames(d)[-grep("ska",colnames(d))])]
alpha.nfc = raw.alpha(nfc.items)

# alphas for correlation tables
alpha.overall   = c(alpha.gro[1], alpha.aso[1], alpha.ino[1], alpha.hfs[1], alpha.fof[1], alpha.nfc[1],
                    alpha.gro[2], alpha.aso[2], alpha.ino[2], alpha.hfs[2], alpha.fof[2], alpha.nfc[2])
alpha.german    = c(alpha.grg[1], alpha.asg[1], alpha.ing[1], alpha.hfs[1], alpha.fof[1], alpha.nfc[1],
                    alpha.grg[2], alpha.asg[2], alpha.ing[2], alpha.hfs[2], alpha.fof[2], alpha.nfc[2])
alpha.math      = c(alpha.grm[1], alpha.asm[1], alpha.inm[1], alpha.hfs[1], alpha.fof[1], alpha.nfc[1],
                    alpha.grm[2], alpha.asm[2], alpha.inm[2], alpha.hfs[2], alpha.fof[2], alpha.nfc[2])
alpha.physics   = c(alpha.grp[1], alpha.asp[1], alpha.inp[1], alpha.hfs[1], alpha.fof[1], alpha.nfc[1],
                    alpha.grp[2], alpha.asp[2], alpha.inp[2], alpha.hfs[2], alpha.fof[2], alpha.nfc[2])
alpha.chemistry = c(alpha.grc[1], alpha.asc[1], alpha.inc[1], alpha.hfs[1], alpha.fof[1], alpha.nfc[1],
                    alpha.grc[2], alpha.asc[2], alpha.inc[2], alpha.hfs[2], alpha.fof[2], alpha.nfc[2])


# data.frames for correlation and regression analysis ----

df.overall    = data.frame(grd.1 = GRO1, asc.1 = ASO1, int.1 = INO1, hfs.1 = HFS1, fof.1 = FOF1, nfc.1 = NFC1, grd.2 = GRO2, asc.2 = ASO2, int.2 = INO2, hfs.2 = HFS2, fof.2 = FOF2, nfc.2 = NFC2)
df.german     = data.frame(grd.1 = GRG1, asc.1 = ASG1, int.1 = ING1, hfs.1 = HFS1, fof.1 = FOF1, nfc.1 = NFC1, grd.2 = GRG2, asc.2 = ASG2, int.2 = ING2, hfs.2 = HFS2, fof.2 = FOF2, nfc.2 = NFC2)
df.math       = data.frame(grd.1 = GRM1, asc.1 = ASM1, int.1 = INM1, hfs.1 = HFS1, fof.1 = FOF1, nfc.1 = NFC1, grd.2 = GRM2, asc.2 = ASM2, int.2 = INM2, hfs.2 = HFS2, fof.2 = FOF2, nfc.2 = NFC2)
df.physics    = data.frame(grd.1 = GRP1, asc.1 = ASP1, int.1 = INP1, hfs.1 = HFS1, fof.1 = FOF1, nfc.1 = NFC1, grd.2 = GRP2, asc.2 = ASP2, int.2 = INP2, hfs.2 = HFS2, fof.2 = FOF2, nfc.2 = NFC2)
df.chemistry  = data.frame(grd.1 = GRC1, asc.1 = ASC1, int.1 = INC1, hfs.1 = HFS1, fof.1 = FOF1, nfc.1 = NFC1, grd.2 = GRC2, asc.2 = ASC2, int.2 = INC2, hfs.2 = HFS2, fof.2 = FOF2, nfc.2 = NFC2)

# test whether missings are MCAR ----

mcar = as.data.frame(rbind(mcar_test(df.overall),
                           mcar_test(df.german),
                           mcar_test(df.math),
                           mcar_test(df.chemistry),
                           mcar_test(df.physics)))

rownames(mcar) = c("GPA", "German", "Math", "Physics", "Chemistry")
# deviation from normality ----------------------------------------------------

# univariate normality

# wrapper function for shapiro.test
sw <- function(x) {
  x = data.frame(x)
  d = dim(x)
  n = names(x)
  r = NULL
  for (i in 1:dim(x)[2]) {
    y = shapiro.test(x[,i])
    v = cbind(y$statistic,y$p.value)
    colnames(v) = c("W","p")
    rownames(v) = n[i]
    r = rbind(r,v)
}
  return(round(r,3))
}

shapiro.tests = data.frame(overall   = sw(df.overall)[ ,2],
                           german    = sw(df.german)[ ,2],
                           math      = sw(df.math)[ ,2], 
                           physics   = sw(df.physics)[ ,2],
                           chemistry = sw(df.chemistry)[, 2])

# multivariate normality
mardia.overall    = mardia(df.overall, plot = F)
mardia.german     = mardia(df.german, plot = F)
mardia.math       = mardia(df.math, plot = F)
mardia.physics    = mardia(df.physics, plot = F)
mardia.chemistry  = mardia(df.chemistry, plot = F)

mardia.tests = data.frame(overall    = c(mardia.overall$p.skew, mardia.overall$p.kurt),
                          german     = c(mardia.german$p.skew, mardia.german$p.kurt),
                          math       = c(mardia.math$p.skew, mardia.math$p.kurt),
                          physics    = c(mardia.physics$p.skew, mardia.physics$p.kurt),
                          chemistry  = c(mardia.chemistry$p.skew, mardia.chemistry$p.kurt))
rownames(mardia.tests) = c("p.skew", "p.kurt")

# correlation analysis --------------------------------------------------------

# function for setting colnames uppercase
col.toupper <- function(df) {
  return(toupper(sub("[.]", "", colnames(df))))
}

# function for creating a custom correlation table
corr.report <- function(df, method = "Spearman", diagonal = NULL, Tdiff = c(53, 59), what.measures = "variables in the study", measure.names = "", what.diagonal = "Cronbach", rtt.bold = T, descriptives = T, p.note = "gt") {
  
  # number of variables
  nvars = ncol(df) / 2
  
  # rename variables
  colnames(df) = col.toupper(df)
  
  # internal consistency and retest reliability
  t1 = data.frame(df[, grep("1", colnames(df))])
  t2 = data.frame(df[, grep("2", colnames(df))])
  ci.rows = seq(1, ncol(df) * 2, by = nvars + 1)
  retest = corr.test(t1, t2)
  rtt.ci = retest$ci[ci.rows, ]
  min.rtt = printnum(min(rtt.ci[, 2]), gt1 = F)
  
  # correlation table incl. reliabilities (if provided) prepared for printing in table 
  corr.tab = corr.test(df, method = tolower(method))
  if (length(corr.tab$n) == 1) {
    n = corr.tab$n
    n.note = ""
} else {
    n = paste0(range(corr.tab$n), collapse = "-")
    n.note = " due to missings"
}
  ct = printnum(corr.tab$r, gt1 = F)
  ct[lower.tri(ct)] = ""
  
  if (!is.null(diagonal)) {
    min.ic = printnum(min(diagonal, na.rm = T), gt1 = F)
    diag(ct) = paste("\\textit{", sub("NA", "—", printnum(diagonal, gt1 = F)), "}", sep = "") # removes leading zero
} else {
    min.ic = NA
    diag(ct) = rep("—", ncol(df))
}
  
  # set retest reliabilities to bold italic 
  if (rtt.bold == T) {
    for (i in 1:nvars) {
      ct[i, i + nvars] = paste("\\textbf{\\textit{", ct[i, i + nvars], "}}", sep = "")
  }
    retest.note = paste(
      "bold-faced coefficients give the",
      paste(Tdiff, collapse = "-"),
      "week retest reliability; "
    )
} else {
    retest.note = ""
}
  
  if (descriptives == T) {
    # get descriptives 
    scale.descriptives = data.frame(describe(df))[, c("mean", "sd", "min", "max", "skew", "kurtosis")]
    
    # format descriptives
    dt = round(t(scale.descriptives), 2)
    dt = rbind(format(dt[1:2, ], nsmall = 2),
               format(dt[3:4, ], nsmall = 0),
               format(dt[5:6, ], nsmall = 2))
    rownames(dt) = c("Mean", "SD", "Min", "Max", "Skew", "Kurtosis")
    caption = paste(method, "correlations and descriptive statistics of the", what.measures)
} else {
    dt = NA
    caption = paste(method, "correlations of the", what.measures)
}
  
  # create caption and note to correlation table 
  measure.description = paste(unique(sub("1|2", "", colnames(df))), measure.names, sep = " = ", collapse = ", ")
  if (what.diagonal == "Cronbach") {
    diagonal.description = "coefficients in the diagonal are Cronbach’s $\\alpha$, "
} else if (what.diagonal == "MacDonald") {
    diagonal.description = "coefficients in the diagonal are MacDonald’s $\\omega$, with the entries for CEI1 and CEI2 containing the total $\\omega$ of the factor model, "
} else {
    diagonal.description = ""
}
  
  if (p.note == "le") {
    p.note = paste("all coefficients significant at \\textit{p} $\\leq$ ", sub("< ", "", printp(max(corr.tab$ci$p))), "; ", sep = "")
} else if (p.note == "gt") {
    if (method == "Pearson") {
      p.note = paste0("$p < .05$ for $|r|$ > ", printnum(abs(corr.tab$r[which.min(abs(.05 - corr.tab$p))]), gt1 = F), "; ")
  } else {
      p.note = paste0("$p < .05$ for $|r_{s}|$ > ", printnum(abs(corr.tab$r[which.min(abs(.05 - corr.tab$p))]), gt1 = F), "; ")
  }
    
}
  
  note = paste("\\textit{N} = ", n, n.note, "; ",
               p.note, diagonal.description, retest.note, 
               measure.description," at measurement occasion 1, and 2, respectively",
               sep = "")
  
  # return results
  return(list(ct      = ct,         # correlation table
              dt      = dt,         # descriptives
              caption = caption,
              note    = note,
              method  = method,
              min.ic  = min.ic,
              min.rtt = min.rtt))
}

# descriptives and scale intercorrelations separately for overall grades 
# (reported in main ms.) and subject grades (reported in the supplement)  

# correlations overall
corr.overall = corr.report(df.overall,
                           diagonal = alpha.overall,
                           what.measures = "variables in the analyses on Grade Point Average",
                           measure.names = c("Grade Point Average", "Overall Ability Self-Concept", "Overall Interest in School", "Hope for Success", "Fear of Failure", "Need for Cognition"))
# correlations German
corr.german  = corr.report(df.german,
                           diagonal = alpha.german,
                           what.measures = "variables in the analyses on German grades",
                           measure.names = c("Grade German", "Ability Self-Concept German", "Interest in German", "Hope for Success", "Fear of Failure", "Need for Cognition"))
# correlations Math
corr.math    = corr.report(df.math,
                           diagonal = alpha.math,
                           what.measures = "variables in the analyses on Math grades",
                           measure.names = c("Grade Math", "Ability Self-Concept Math", "Interest in Math", "Hope for Success", "Fear of Failure", "Need for Cognition"))
# correlations Physics
corr.physics = corr.report(df.physics,
                           diagonal = alpha.physics,
                           what.measures = "variables in the analyses on Physics grades",
                           measure.names = c("Grade Physics", "Ability Self-Concept Physics", "Interest in Physics", "Hope for Success", "Fear of Failure", "Need for Cognition"))
# correlations Chemistry
corr.chemistry = corr.report(df.chemistry,
                             diagonal = alpha.chemistry,
                             what.measures = "variables in the analyses on Chemistry grades",
                             measure.names = c("Grade Chemistry", "Ability Self-Concept Chemistry", "Interest in Chemistry", "Hope for Success", "Fear of Failure", "Need for Cognition"))


# multiple regression to determine variables for latent change score models ----

mr <- function(data) {
  
  # models
  m.full.mod = 'grd.2 ~ a * grd.1 + b * asc.1 + c * int.1 + d * hfs.1 + e * fof.1 + f * nfc.1 \n grd.2 ~ 1'
  m.with.nfc = 'grd.2 ~ a * grd.1 + b * asc.1 + 0 * int.1 + 0 * hfs.1 + 0 * fof.1 + f * nfc.1 \n grd.2 ~ 1'
  m.wout.nfc = 'grd.2 ~ a * grd.1 + b * asc.1 + 0 * int.1 + 0 * hfs.1 + 0 * fof.1 + 0 * nfc.1 \n grd.2 ~ 1'
  
  # fitted models 
  f.full.mod = sem(m.full.mod, data = data, fixed.x = F, missing = "fiml", estimator = "mlr")
  f.with.nfc = sem(m.with.nfc, data = data, fixed.x = F, missing = "fiml", estimator = "mlr")
  f.wout.nfc = sem(m.wout.nfc, data = data, fixed.x = F, missing = "fiml", estimator = "mlr")
  
  # summaries
  s.full.mod = summary(f.full.mod, fit.measures = T, standardized = T, rsquare = T)
  s.with.nfc = summary(f.with.nfc, fit.measures = T, standardized = T, rsquare = T)
  s.wout.nfc = summary(f.wout.nfc, fit.measures = T, standardized = T, rsquare = T)
  
  # parameter estimates
  p.full.mod = parameterEstimates(f.full.mod, standardized = T, rsquare = T)[c(7, 1:6, 36), -c(4, 11, 13)]
  p.with.nfc = parameterEstimates(f.with.nfc, standardized = T, rsquare = T)[c(7, 1:6, 36), -c(4, 11, 13)]
  p.wout.nfc = parameterEstimates(f.wout.nfc, standardized = T, rsquare = T)[c(7, 1:6, 36), -c(4, 11, 13)]
  
  # model comparison
  compare.models = anova(f.with.nfc, f.wout.nfc)
  
  # output
  return = list(f = list(f.full.mod = f.full.mod,
                         f.with.nfc = f.with.nfc,
                         f.wout.nfc = f.wout.nfc),
                s = list(s.full.mod = s.full.mod,
                         s.with.nfc = s.with.nfc,
                         s.wout.nfc = s.wout.nfc),
                p = list(p.full.mod = p.full.mod,
                         p.with.nfc = p.with.nfc,
                         p.wout.nfc = p.wout.nfc),
                a = compare.models)
}

mr.overall   = mr(df.overall)
mr.german    = mr(df.german)
mr.math      = mr(df.math)
mr.physics   = mr(df.physics)
mr.chemistry = mr(df.chemistry)


# retrieve fit measures from lavaan fit
fit.results <- function(fit, suffix = ".scaled") {
  # get fit measures
  fm = fitMeasures(fit)
  
  # extract relevant fit measures
  N.orig = inspect(fit, 'norig')
  n.obs  = inspect(fit, 'nobs')
  Chi.Sq = fm[which(names(fm) == paste0("chisq", suffix))]
  df     = fm[which(names(fm) == paste0("df", suffix))]
  P      = fm[which(names(fm) == paste0("pvalue", suffix))]
  CFI    = fm[which(names(fm) == paste0("cfi", suffix))]
  RMSEA  = fm[which(names(fm) == paste0("rmsea", suffix))]
  CI.lo  = fm[which(names(fm) == paste0("rmsea.ci.lower", suffix))]
  CI.hi  = fm[which(names(fm) == paste0("rmsea.ci.upper", suffix))]
  SRMR   = fm[which(names(fm) == "srmr")]
  
  # create table of fit measures
  fm     = data.frame(N.orig, n.obs, Chi.Sq, df, P, CFI, RMSEA, CI.lo, CI.hi, SRMR)
  
  # create string for reporting
  rf     = paste0("$\\chi^2(",fm$df,")$ = ", printnum(fm$Chi.Sq), 
                  ", $p$ ", printp(fm$P),
                  ", CFI = ", printnum(fm$CFI),
                  ", RMSEA = ", printnum(fm$RMSEA, gt1 = F),
                  " with 90% CI [", paste0(printnum(c(fm$CI.lo, fm$CI.hi)), collapse = ", "), "]",
                  ", SRMR = ", printnum(fm$SRMR, gt1 = F))
  
  # get parameter estimates
  pe     = parameterEstimates(fit, standardized = T)
  pe     = pe[, c(1:6, 13, 8)]
  names(pe) = c("Crit", "op", "Pred", "Label", "Est", "SE", "Std", "P")
  
  return(list(parameter.estimates = pe, fit.measures = fm, fit.report = rf))
}

fit.mr.overall.with.nfc = fit.results(mr.overall$f$f.with.nfc)$fit.report
fit.mr.overall.wout.nfc = fit.results(mr.overall$f$f.wout.nfc)$fit.report

fro.c2.diff     = anova(mr.overall$f$f.with.nfc,mr.overall$f$f.wout.nfc)
fro.chi.sq.diff = paste0("$\\chi^2$(", fro.c2.diff$`Df diff`[2],") = ", printnum(fro.c2.diff$`Chisq diff`[2]), 
                         ", $p$ = ", printp(fro.c2.diff$`Pr(>Chisq)`[2]))

# report overall regression results (main text)

mr.col.names = c("$B$", "$SE$", "CI.LB", "CI.UB", "$b$", "$p$")

mr.report.overall = cbind(printnum(mr.overall$p$p.full.mod[1:7, c(4:5, 8:9, 10)], digits = 3, gt1 = c(T, T, T, T, F)), printp(mr.overall$p$p.full.mod[1:7, 7]))
colnames(mr.report.overall) = mr.col.names
rownames(mr.report.overall) = c("Intercept", "GPA", "Ability Self-Concept", "Interest", "Hope for Success", "Fear of Failure", "Need for Cognition")

# report subject-specific regression results
mr.report.german = cbind(printnum(mr.german$p$p.full.mod[1:7, c(4:5, 8:9, 10)], digits = 3, gt1 = c(T, T, T, T, F)), printp(mr.german$p$p.full.mod[1:7, 7]))
colnames(mr.report.german) = mr.col.names
rownames(mr.report.german) = c("Intercept", "Grade German", "Ability Self-Concept German", "Interest in German", "Hope for Success", "Fear of Failure", "Need for Cognition")

frg.c2.diff     = anova(mr.german$f$f.with.nfc,mr.german$f$f.wout.nfc)
frg.chi.sq.diff = paste0("$\\chi^2$(", frg.c2.diff$`Df diff`[2],") = ", printnum(frg.c2.diff$`Chisq diff`[2]), 
                         ", $p$ = ", printp(frg.c2.diff$`Pr(>Chisq)`[2]))

mr.report.math = cbind(printnum(mr.math$p$p.full.mod[1:7, c(4:5, 8:9, 10)], digits = 3, gt1 = c(T, T, T, T, F)), printp(mr.math$p$p.full.mod[1:7, 7]))
colnames(mr.report.math) = mr.col.names
rownames(mr.report.math) = c("Intercept", "Grade Math", "Ability Self-Concept Math", "Interest in Math", "Hope for Success", "Fear of Failure", "Need for Cognition")

frm.c2.diff     = anova(mr.math$f$f.with.nfc,mr.math$f$f.wout.nfc)
frm.chi.sq.diff = paste0("$\\chi^2$(", frm.c2.diff$`Df diff`[2],") = ", printnum(frm.c2.diff$`Chisq diff`[2]), 
                         ", $p$ = ", printp(frm.c2.diff$`Pr(>Chisq)`[2]))

mr.report.physics = cbind(printnum(mr.physics$p$p.full.mod[1:7, c(4:5, 8:9, 10)], digits = 3, gt1 = c(T, T, T, T, F)), printp(mr.physics$p$p.full.mod[1:7, 7]))
colnames(mr.report.physics) = mr.col.names
rownames(mr.report.physics) = c("Intercept", "Grade Physics", "Ability Self-Concept Physics", "Interest in Physics", "Hope for Success", "Fear of Failure", "Need for Cognition")

frp.c2.diff     = anova(mr.physics$f$f.with.nfc,mr.physics$f$f.wout.nfc)
frp.chi.sq.diff = paste0("$\\chi^2$(", frp.c2.diff$`Df diff`[2],") = ", printnum(frp.c2.diff$`Chisq diff`[2]), 
                         ", $p$ = ", printp(frp.c2.diff$`Pr(>Chisq)`[2]))

mr.report.chemistry = cbind(printnum(mr.chemistry$p$p.full.mod[1:7, c(4:5, 8:9, 10)], digits = 3, gt1 = c(T, T, T, T, F)), printp(mr.chemistry$p$p.full.mod[1:7, 7]))
colnames(mr.report.chemistry) = mr.col.names
rownames(mr.report.chemistry) = c("Intercept", "Grade Chemistry", "Ability Self-Concept Chemistry", "Interest in Chemistry", "Hope for Success", "Fear of Failure", "Need for Cognition")

frc.c2.diff     = anova(mr.chemistry$f$f.with.nfc,mr.chemistry$f$f.wout.nfc)
frc.chi.sq.diff = paste0("$\\chi^2$(", frc.c2.diff$`Df diff`[2],") = ", printnum(frc.c2.diff$`Chisq diff`[2]), 
                         ", $p$ = ", printp(frc.c2.diff$`Pr(>Chisq)`[2]))


mr.nobs = paste0(range(c(fit.results(mr.overall$f$f.full.mod)$fit.measures$n.obs,
                         fit.results(mr.german$f$f.full.mod)$fit.measures$n.obs,
                         fit.results(mr.math$f$f.full.mod)$fit.measures$n.obs,
                         fit.results(mr.physics$f$f.full.mod)$fit.measures$n.obs,
                         fit.results(mr.chemistry$f$f.full.mod)$fit.measures$n.obs)), collapse = "-")

# latent change score modeling ------------------------------------------------

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

# overall grades and ability self-concept together with NFC
fit.overall <- lavaan(lcsm, data = df.overall, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(fit.overall, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# German grades and ability self-concept together with NFC
fit.german <- lavaan(lcsm, data = df.german, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
# summary(fit.german, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE) 

# Math grades and ability self-concept together with NFC
fit.math <- lavaan(lcsm, data = df.math, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
# summary(fit.math, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE) 

# Physics grades and ability self-concept together with NFC
fit.physics <- lavaan(lcsm, data = df.physics, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
# summary(fit.physics, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE) 

# Chemistry grades and ability self-concept together with NFC
fit.chemistry <- lavaan(lcsm, data = df.chemistry, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
# summary(fit.chemistry, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE) 

# get parameter estimates from LCSMs
sf_cdc = list(sf_cdc_overall    = parameterEstimates(fit.overall,   standardized = T)[25:39, -c(10:11)],
              sf_cdc_german     = parameterEstimates(fit.german,    standardized = T)[25:39, -c(10:11)],
              sf_cdc_math       = parameterEstimates(fit.math,      standardized = T)[25:39, -c(10:11)],
              sf_cdc_physics    = parameterEstimates(fit.physics,   standardized = T)[25:39, -c(10:11)],
              sf_cdc_chemnistry = parameterEstimates(fit.chemistry, standardized = T)[25:39, -c(10:11)])


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

# save all variables for use in R Markdown document ---------------------------
save.image(here::here("Data","NFC-Grades.RData"))

# knit document ---------------------------------------------------------------
# now open "NFC-Grades.Rmd" in the "Manuscript" folder and hit the Knit button
# on top of your RStudio window

# delete unnecessary output of papaja either by hand or via
dir_manuscript  = dir(here::here("Manuscript"))
files_to_delete = dir_manuscript[grep("fff|log|tex|ttt", dir_manuscript)]
eval(parse(text = paste0("unlink('Manuscript/", files_to_delete, "')")))



# new analyses based on comments during peer-review ---------------------------

# measurement model (?) NFC

# get variable names of nfc items and paste them to console,
# 1) all items at each time point
# 2) separately for positive and negative formulated items
nfc_items_1 = colnames(d)[grep("nfc", colnames(d))][1:16]
nfc_posit_1 = nfc_items_1[-grep("g", nfc_items_1)]
nfc_negat_1 = nfc_items_1[ grep("g", nfc_items_1)]
nfc_items_2 = colnames(d)[grep("nfc", colnames(d))][18:33]
nfc_posit_2 = nfc_items_2[-grep("g", nfc_items_2)]
nfc_negat_2 = nfc_items_2[ grep("g", nfc_items_2)]
paste(nfc_items_1, collapse = " + ")
paste(nfc_posit_1, collapse = " + ")
paste(nfc_negat_1, collapse = " + ")
paste(nfc_items_2, collapse = " + ")
paste(nfc_posit_2, collapse = " + ")
paste(nfc_negat_2, collapse = " + ")

# simple model
m_nfc = '
NFC1 =~ nfc_01 + nfc_02 + nfc_03 + nfc_05 + nfc_13 + nfc_14 + nfc_04g + nfc_06g + nfc_07g + nfc_08g + nfc_09g + nfc_10g + nfc_11g + nfc_12g + nfc_15g + nfc_16g 
NFC2 =~ nfc_01_2 + nfc_02_2 + nfc_03_2 + nfc_05_2 + nfc_13_2 + nfc_14_2 + nfc_04g_2 + nfc_06g_2 + nfc_07g_2 + nfc_08g_2 + nfc_09g_2 + nfc_10g_2 + nfc_11g_2 + nfc_12g_2 + nfc_15g_2 + nfc_16g_2
NFC1 ~~ NFC2
'

# models with method factors
m_nfc_1 = '
NFC1 =~ nfc_01 + nfc_02 + nfc_03 + nfc_05 + nfc_13 + nfc_14 + nfc_04g + nfc_06g + nfc_07g + nfc_08g + nfc_09g + nfc_10g + nfc_11g + nfc_12g + nfc_15g + nfc_16g 
M_pos1 =~ nfc_01 + nfc_02 + nfc_03 + nfc_05 + nfc_13 + nfc_14
M_neg1 =~ nfc_04g + nfc_06g + nfc_07g + nfc_08g + nfc_09g + nfc_10g + nfc_11g + nfc_12g + nfc_15g + nfc_16g
M_pos1 ~~ 0*M_neg1
'

m_nfc_2 = '
NFC2 =~ nfc_01_2 + nfc_02_2 + nfc_03_2 + nfc_05_2 + nfc_13_2 + nfc_14_2 + nfc_04g_2 + nfc_06g_2 + nfc_07g_2 + nfc_08g_2 + nfc_09g_2 + nfc_10g_2 + nfc_11g_2 + nfc_12g_2 + nfc_15g_2 + nfc_16g_2
M_pos2 =~ nfc_01_2 + nfc_02_2 + nfc_03_2 + nfc_05_2 + nfc_13_2 + nfc_14_2
M_neg2 =~ nfc_04g_2 + nfc_06g_2 + nfc_07g_2 + nfc_08g_2 + nfc_09g_2 + nfc_10g_2 + nfc_11g_2 + nfc_12g_2 + nfc_15g_2 + nfc_16g_2
M_pos2 ~~ 0*M_neg2
'

f_nfc = cfa(m_nfc, data = d, estimator = "MLR", missing = "FIML")
summary(f_nfc, fit.measures = T, standardized = T)

f_nfc_1 = cfa(m_nfc_1, data = d, estimator = "MLR", missing = "FIML")
summary(f_nfc_1, fit.measures = T, standardized = T)

f_nfc_2 = cfa(m_nfc_2, data = d, estimator = "MLR", missing = "FIML")
summary(f_nfc_2, fit.measures = T, standardized = T)

# measurement models with parcels 

source("~/Documents/R/functions/parcel.gen.R")
df_nfc_1 = d[grep("nfc", colnames(d))][ 1:16]
df_nfc_2 = d[grep("nfc", colnames(d))][18:33]

nfc_parcels_1 = parcel.gen(as.numeric(principal(df_nfc_1, nfactors = 1)$loadings), parcels = 4)
nfc_parcels_2 = parcel.gen(as.numeric(principal(df_nfc_2, nfactors = 1)$loadings), parcels = 4)

df_nfc_p = data.frame(nfc_p1_1 = rowMeans(df_nfc_1[, nfc_parcels_1$items[, 1]]),
                      nfc_p2_1 = rowMeans(df_nfc_1[, nfc_parcels_1$items[, 2]]),
                      nfc_p3_1 = rowMeans(df_nfc_1[, nfc_parcels_1$items[, 3]]),
                      nfc_p4_1 = rowMeans(df_nfc_1[, nfc_parcels_1$items[, 4]]),
                      nfc_p1_2 = rowMeans(df_nfc_2[, nfc_parcels_1$items[, 1]]),
                      nfc_p2_2 = rowMeans(df_nfc_2[, nfc_parcels_1$items[, 2]]),
                      nfc_p3_2 = rowMeans(df_nfc_2[, nfc_parcels_1$items[, 3]]),
                      nfc_p4_2 = rowMeans(df_nfc_2[, nfc_parcels_1$items[, 4]]))

mp_nfc = '
NFC1 =~ nfc_p1_1 + nfc_p2_1 + nfc_p3_1 + nfc_p4_1
NFC2 =~ nfc_p1_2 + nfc_p2_2 + nfc_p3_2 + nfc_p4_2
NFC1 ~~ NFC2
nfc_p1_1 ~~ nfc_p1_2
nfc_p2_1 ~~ nfc_p2_2
nfc_p3_1 ~~ nfc_p3_2
nfc_p4_1 ~~ nfc_p4_2
'
fp_nfc = cfa(mp_nfc, data = df_nfc_p, estimator = "MLR", missing = "FIML")
summary(fp_nfc, fit.measures = T, standardized = T)

# Ability Self-Concept overall

paste(colnames(d)[grep("kom_al", colnames(d))], collapse = " + ")

m_asc = '
ASC1 =~ kom_al1 + kom_al2 + kom_al3 + kom_al4 
ASC2 =~ kom_al1_2 + kom_al2_2 + kom_al3_2 + kom_al4_2
kom_al1 ~~ kom_al1_2
kom_al2 ~~ kom_al2_2
kom_al3 ~~ kom_al3_2
kom_al4 ~~ kom_al4_2
ASC1 ~~ ASC2
'

f_asc = cfa(m_asc, data = d, estimator = "MLR", missing = "FIML")
summary(f_asc, fit.measures = T, standardized = T)

# Ability Self-Concept combined

paste(colnames(d)[grep("kom_", colnames(d))], collapse = " + ")

m_asc = '
ASO1 =~ kom_al1 + kom_al2 + kom_al3 + kom_al4 
ASG1 =~ kom_d1 + kom_d2 + kom_d3 + kom_d4 
ASM1 =~ kom_m1 + kom_m2 + kom_m3 + kom_m4 
ASP1 =~ kom_p1 + kom_p2 + kom_p3 + kom_p4 
ASC1 =~ kom_c1 + kom_c2 + kom_c3 + kom_c4
ASO2 =~ kom_al1_2 + kom_al2_2 + kom_al3_2 + kom_al4_2
ASG2 =~ kom_d1_2 + kom_d2_2 + kom_d3_2 + kom_d4_2
ASM2 =~ kom_m1_2 + kom_m2_2 + kom_m3_2 + kom_m4_2
ASP2 =~ kom_p1_2 + kom_p2_2 + kom_p3_2 + kom_p4_2
ASC2 =~ kom_c1_2 + kom_c2_2 + kom_c3_2 + kom_c4_2
ASO1 ~~ ASO2
ASG1 ~~ ASG2
ASM1 ~~ ASM2
ASP1 ~~ ASP2
ASC1 ~~ ASC2
'

f_asc = cfa(m_asc, data = d, estimator = "MLR", missing = "FIML")
summary(f_asc, fit.measures = T, standardized = T)

#  NFC and ASC combined

m_comb = '
# Ability Self Concept
ASO1 =~ kom_al1 + kom_al2 + kom_al3 + kom_al4 
ASG1 =~ kom_d1 + kom_d2 + kom_d3 + kom_d4 
ASM1 =~ kom_m1 + kom_m2 + kom_m3 + kom_m4 
ASP1 =~ kom_p1 + kom_p2 + kom_p3 + kom_p4 
ASC1 =~ kom_c1 + kom_c2 + kom_c3 + kom_c4
ASO2 =~ kom_al1_2 + kom_al2_2 + kom_al3_2 + kom_al4_2
ASG2 =~ kom_d1_2 + kom_d2_2 + kom_d3_2 + kom_d4_2
ASM2 =~ kom_m1_2 + kom_m2_2 + kom_m3_2 + kom_m4_2
ASP2 =~ kom_p1_2 + kom_p2_2 + kom_p3_2 + kom_p4_2
ASC2 =~ kom_c1_2 + kom_c2_2 + kom_c3_2 + kom_c4_2
ASO1 ~~ ASO2
ASG1 ~~ ASG2
ASM1 ~~ ASM2
ASP1 ~~ ASP2
ASC1 ~~ ASC2

# Need for Cognition
NFC1 =~ nfc_p1_1 + nfc_p2_1 + nfc_p3_1 + nfc_p4_1
NFC2 =~ nfc_p1_2 + nfc_p2_2 + nfc_p3_2 + nfc_p4_2
NFC1 ~~ NFC2

note_all_2 ~ note_all + ASO1 + NFC1
note_d_2  ~ note_d + ASG1 + NFC1
not_m_2   ~ not_m  + ASM1 + NFC1
note_p_2  ~ note_p + ASP1 + NFC1
note_c_2  ~ note_c + ASC1 + NFC1
'

f_comb = cfa(m_comb, data = cbind(d, df_nfc_p), estimator = "MLR", missing = "FIML", fixed.x = F)
summary(f_comb, fit.measures = T, standardized = T)


mo = '
# Need for Cognition
nfc.1 =~ nfc_p1_1 + nfc_p2_1 + nfc_p3_1 + nfc_p4_1
nfc.2 =~ nfc_p1_2 + nfc_p2_2 + nfc_p3_2 + nfc_p4_2
nfc.1 ~~ nfc.2

'

f0 = cfa(m0, data = dfo, estimator = "MLR", missing = "FIML", fixed.x = F)
summary(f0, fit.measures = T, standardized = T)



# latent change score modeling ------------------------------------------------

dfo = data.frame(grd.1 = d$note_all, grd.2 = d$note_all_2, d, df_nfc_p)
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
# 
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



fo = cfa(lcsm, data = df.nfc_asc_grd, estimator = "MLR", missing = "FIML", fixed.x = F)
summary(fo, fit.measures = T, standardized = T)


m_comb = '

# Need for Cognition
NFC1 =~ nfc_p1_1 + nfc_p2_1 + nfc_p3_1 + nfc_p4_1
NFC2 =~ nfc_p1_2 + nfc_p2_2 + nfc_p3_2 + nfc_p4_2
NFC1 ~~ NFC2

# Hope for Success
HFS1 =~ lm_1 + lm_2 + lm_3 + lm_4 + lm_5 + lm_6 + lm_7
HFS2 =~ lm_1_2 + lm_2_2 + lm_3_2 + lm_4_2 + lm_5_2 + lm_6_2 + lm_7_2
HFS1 ~~ HFS2

# Fear of Failure
FOF1 =~ lm_1 + lm_2 + lm_3 + lm_4 + lm_5 + lm_6 + lm_7
FOF2 =~ an_1_2 + an_2_2 + an_3_2 + an_4_2 + an_5_2 + an_6_2 + an_7_2
FOF1 ~~ FOF2

# Ability Self Concept Overall
ASO1 =~ kom_al1 + kom_al2 + kom_al3 + kom_al4 
ASO2 =~ kom_al1_2 + kom_al2_2 + kom_al3_2 + kom_al4_2
ASO1 ~~ ASO2

# Ability Self Concept German
ASG1 =~ kom_d1 + kom_d2 + kom_d3 + kom_d4 
ASG2 =~ kom_d1_2 + kom_d2_2 + kom_d3_2 + kom_d4_2
ASG1 ~~ ASG2

# Ability Self Concept Math
ASM1 =~ kom_m1 + kom_m2 + kom_m3 + kom_m4 
ASM2 =~ kom_m1_2 + kom_m2_2 + kom_m3_2 + kom_m4_2
ASM1 ~~ ASM2

# Ability Self Concept Physics
ASP1 =~ kom_p1 + kom_p2 + kom_p3 + kom_p4 
ASP2 =~ kom_p1_2 + kom_p2_2 + kom_p3_2 + kom_p4_2
ASP1 ~~ ASP2

# Ability Self Concept Chemistry
ASC1 =~ kom_c1 + kom_c2 + kom_c3 + kom_c4 
ASC2 =~ kom_c1_2 + kom_c2_2 + kom_c3_2 + kom_c4_2
ASC1 ~~ ASC2

# Interest Overall
INO1 =~ in_al1 + in_al2 + in_al3
INO2 =~ in_al1_2 + in_al2_2 + in_al3_2
INO1 ~~ INO2

# Interest German
ING1 =~ in_d1 + in_d2 + in_d3
ING2 =~ in_d1_2 + in_d2_2 + in_d3_2
ING1 ~~ ING2

# Interest Math
INM1 =~ in_m1 + in_m2 + in_m3
INM2 =~ in_m1_2 + in_m2_2 + in_m3_2
INM1 ~~ INM2

# Interest Physics
INP1 =~ in_p1 + in_p2 + in_p3
INP2 =~ in_p1_2 + in_p2_2 + in_p3_2
INP1 ~~ INP2

# Interest Chemistry
INC1 =~ in_c1 + in_c2 + in_c3
INC2 =~ in_c1_2 + in_c2_2 + in_c3_2
INC1 ~~ INC2

#note_all   ~ ASO1 + NFC1 + INO1
#note_d   ~ ASG1 + NFC1 + ING1

note_all_2 ~ note_all + ASO1 + NFC1 + INO1 + HFS1 + FOF1
note_d_2   ~ note_d   + ASG1 + NFC1 + ING1 + HFS1 + FOF1
not_m_2    ~ not_m    + ASM1 + NFC1 + INM1 + HFS1 + FOF1
note_p_2   ~ note_p   + ASP1 + NFC1 + INP1 + HFS1 + FOF1
note_c_2   ~ note_c   + ASC1 + NFC1 + INC1 + HFS1 + FOF1

'

f_comb = cfa(m_comb, data = cbind(d, df_nfc_p), estimator = "MLR", missing = "FIML", fixed.x = F)
summary(f_comb, fit.measures = T, standardized = T)

m_c = '
# Interest Chemistry
INC1 =~ in_c1 + in_c2 + in_c3
INC2 =~ in_c1_2 + in_c2_2 + in_c3_2
INC1 ~~ INC2

'

f_c = cfa(m_c, data = cbind(d, df_nfc_p), estimator = "MLR", missing = "FIML", fixed.x = F)
summary(f_c, fit.measures = T, standardized = T)

# measurement models for the constructs of interest ---------------------------

# Need for Cognition
mm_nfc = '
NFC1 =~ nfc_p1_1 + nfc_p2_1 + nfc_p3_1 + nfc_p4_1
NFC2 =~ nfc_p1_2 + nfc_p2_2 + nfc_p3_2 + nfc_p4_2
NFC1 ~~ NFC2
'

f_nfc = cfa(mm_nfc, data = df_nfc_p, estimator = "MLR", missing = "FIML", fixed.x = F)
summary(f_nfc, fit.measures = T, standardized = T)

# Hope for Success
mm_hfs = '
HFS1 =~ lm_1 + lm_2 + lm_3 + lm_4 + lm_5 + lm_6 + lm_7
HFS2 =~ lm_1_2 + lm_2_2 + lm_3_2 + lm_4_2 + lm_5_2 + lm_6_2 + lm_7_2
HFS1 ~~ HFS2
'

f_hfs = cfa(mm_hfs, data = d, estimator = "MLR", missing = "FIML", fixed.x = F)
summary(f_hfs, fit.measures = T, standardized = T)

# Fear of Failure
mm_fof = '
FOF1 =~ an_1 + an_2 + an_3 + an_4 + an_5 + an_6 + an_7
FOF2 =~ an_1_2 + an_2_2 + an_3_2 + an_4_2 + an_5_2 + an_6_2 + an_7_2
FOF1 ~~ FOF2
'

# --------------------------------------------------------------------
# Need for Cognition
mm_trt = '
NFC1 =~ nfc_p1_1 + nfc_p2_1 + nfc_p3_1 + nfc_p4_1
NFC2 =~ nfc_p1_2 + nfc_p2_2 + nfc_p3_2 + nfc_p4_2
NFC1 ~~ NFC2

HFS1 =~ lm_1 + lm_2 + lm_3 + lm_4 + lm_5 + lm_6 + lm_7
HFS2 =~ lm_1_2 + lm_2_2 + lm_3_2 + lm_4_2 + lm_5_2 + lm_6_2 + lm_7_2
HFS1 ~~ HFS2

FOF1 =~ an_1 + an_2 + an_3 + an_4 + an_5 + an_6 + an_7
FOF2 =~ an_1_2 + an_2_2 + an_3_2 + an_4_2 + an_5_2 + an_6_2 + an_7_2
FOF1 ~~ FOF2
'

f_trt = cfa(mm_trt, data = cbind(d, df_nfc_p), estimator = "MLR", missing = "FIML")
summary(f_trt, fit.measures = T, standardized = T)


#--------------------------------------------------------------------

f_fof = cfa(mm_fof, data = d, estimator = "MLR", missing = "FIML")
summary(f_fof, fit.measures = T, standardized = T)

# derive factor scores
fs_trt = data.frame(cbind(lavPredict(f_nfc), lavPredict(f_hfs), lavPredict(f_fof)))

# Ability Self Concept
mm_asc = '
# Ability Self Concept Overall
ASO1 =~ kom_al1 + kom_al2 + kom_al3 + kom_al4 
ASO2 =~ kom_al1_2 + kom_al2_2 + kom_al3_2 + kom_al4_2
ASO1 ~~ ASO2

# Ability Self Concept German
ASG1 =~ kom_d1 + kom_d2 + kom_d3 + kom_d4 
ASG2 =~ kom_d1_2 + kom_d2_2 + kom_d3_2 + kom_d4_2
ASG1 ~~ ASG2

# Ability Self Concept Math
ASM1 =~ kom_m1 + kom_m2 + kom_m3 + kom_m4 
ASM2 =~ kom_m1_2 + kom_m2_2 + kom_m3_2 + kom_m4_2
ASM1 ~~ ASM2

# Ability Self Concept Physics
ASP1 =~ kom_p1 + kom_p2 + kom_p3 + kom_p4 
ASP2 =~ kom_p1_2 + kom_p2_2 + kom_p3_2 + kom_p4_2
ASP1 ~~ ASP2

# Ability Self Concept Chemistry
ASC1 =~ kom_c1 + kom_c2 + kom_c3 + kom_c4 
ASC2 =~ kom_c1_2 + kom_c2_2 + kom_c3_2 + kom_c4_2
ASC1 ~~ ASC2
'

f_asc = cfa(mm_asc, data = d, estimator = "MLR", missing = "FIML")

# Interest 
mm_int = '
# Interest Overall
INO1 =~ in_al1 + in_al2 + in_al3
INO2 =~ in_al1_2 + in_al2_2 + in_al3_2
INO1 ~~ INO2

# Interest German
ING1 =~ in_d1 + in_d2 + in_d3
ING2 =~ in_d1_2 + in_d2_2 + in_d3_2
ING1 ~~ ING2

# Interest Math
INM1 =~ in_m1 + in_m2 + in_m3
INM2 =~ in_m1_2 + in_m2_2 + in_m3_2
INM1 ~~ INM2

# Interest Physics
INP1 =~ in_p1 + in_p2 + in_p3
INP2 =~ in_p1_2 + in_p2_2 + in_p3_2
INP1 ~~ INP2

# Interest Chemistry
INC1 =~ in_c1 + in_c2 + in_c3
INC2 =~ in_c1_2 + in_c2_2 + in_c3_2
INC1 ~~ INC2
'

f_int = cfa(mm_int, data = d, estimator = "MLR", missing = "FIML")

# combine factor scores
fs_tot = data.frame(cbind(fs_trt,
                          lavPredict(f_asc),
                          lavPredict(f_int),
                          GRO1 = 6 - d$note_all,
                          GRO2 = 6 - d$note_all_2,
                          GRG1 = 6 - d$note_d,
                          GRG2 = 6 - d$note_d_2,
                          GRM1 = 6 - d$not_m,
                          GRM2 = 6 - d$not_m_2,
                          GRP1 = 6 - d$note_p,
                          GRP2 = 6 - d$note_p_2,
                          GRC1 = 6 - d$note_c,
                          GRC2 = 6 - d$note_c_2))

# regressions of grades on motivational variables incl. NFC
m_reg = ' 
GRO2 ~ GRO1 + ASO1 + INO1 + HFS1 + FOF1 + NFC1
GRG2 ~ GRG1 + ASG1 + ING1 + HFS1 + FOF1 + NFC1
GRM2 ~ GRM1 + ASM1 + INM1 + HFS1 + FOF1 + NFC1
GRP2 ~ GRP1 + ASP1 + INP1 + HFS1 + FOF1 + NFC1
GRC2 ~ GRC1 + ASC1 + INC1 + HFS1 + FOF1 + NFC1
'

f_reg = sem(m_reg, data = fs_tot, estimator = "MLR", missing = "FIML", fixed.x = F)
summary(f_reg, fit.measures = T, standardized = T)

# regressions of grades on motivational variables excl. NFC
m_reg_0 = ' 
GRO2 ~ GRO1 + ASO1 + INO1 + HFS1 + FOF1 + 0*NFC1
GRG2 ~ GRG1 + ASG1 + ING1 + HFS1 + FOF1 + 0*NFC1
GRM2 ~ GRM1 + ASM1 + INM1 + HFS1 + FOF1 + 0*NFC1
GRP2 ~ GRP1 + ASP1 + INP1 + HFS1 + FOF1 + 0*NFC1
GRC2 ~ GRC1 + ASC1 + INC1 + HFS1 + FOF1 + 0*NFC1
'

f_reg_0 = sem(m_reg_0, data = fs_tot, estimator = "MLR", missing = "FIML", fixed.x = F)
summary(f_reg_0, fit.measures = T, standardized = T)

# chi-square difference test
anova(f_reg, f_reg_0)

# latent change score modeling ------------------------------------------------

lcsm_ext <- '
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
  
  
  # --------------------- INTEREST REGRESSIONS
  int.2  ~ 1 * int.1      # This parameter regresses int.2 perfectly on int.1
  int.d =~ 1 * int.2      # This defines the latent change score factor as measured perfectly by scores on int.2
  
  # --------------------- INTEREST INTERCEPTS
  int.1  ~ 1              # This estimates the intercept of int.1
  int.2  ~ 0 * 1          # This line constrains the intercept of int.2 to 0
  int.d  ~ 1              # This estimates the intercept of the change score
  
  # --------------------- INTEREST VARIANCES
  int.1 ~~ int.1          # This estimates the variance of int.1
  int.2 ~~ 0 * int.2      # This fixes the variance of the int.2 to 0
  int.d ~~ int.d          # This estimates the variance of the change scores
  
  
  # --------------------- HOPE FOR SUCCESS REGRESSIONS
  hfs.2  ~ 1 * hfs.1      # This parameter regresses hfs.2 perfectly on hfs.1
  hfs.d =~ 1 * hfs.2      # This defines the latent change score factor as measured perfectly by scores on hfs.2
  
  # --------------------- HOPE FOR SUCCESS INTERCEPTS
  hfs.1  ~ 1              # This estimates the intercept of hfs.1
  hfs.2  ~ 0 * 1          # This line constrains the intercept of hfs.2 to 0
  hfs.d  ~ 1              # This estimates the intercept of the change score
  
  # --------------------- HOPE FOR SUCCESS VARIANCES
  hfs.1 ~~ hfs.1          # This estimates the variance of hfs.1
  hfs.2 ~~ 0 * hfs.2      # This fixes the variance of the hfs.2 to 0
  hfs.d ~~ hfs.d          # This estimates the variance of the change scores
  
  
  # --------------------- FEAR OF FAILURE REGRESSIONS
  fof.2  ~ 1 * fof.1      # This parameter regresses fof.2 perfectly on fof.1
  fof.d =~ 1 * fof.2      # This defines the latent change score factor as measured perfectly by scores on fof.2
  
  # --------------------- FEAR OF FAILURE INTERCEPTS
  fof.1  ~ 1              # This estimates the intercept of fof.1
  fof.2  ~ 0 * 1          # This line constrains the intercept of fof.2 to 0
  fof.d  ~ 1              # This estimates the intercept of the change score
  
  # --------------------- FEAR OF FAILURE VARIANCES
  fof.1 ~~ fof.1          # This estimates the variance of fof.1
  fof.2 ~~ 0 * fof.2      # This fixes the variance of the fof.2 to 0
  fof.d ~~ fof.d          # This estimates the variance of the change scores
  
  
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
  grd.d  ~ beta1   * grd.1 + gamma21 * asc.1 + gamma31 * int.1 + gamma41 * hfs.1 + gamma51 * fof.1 + gamma61 * nfc.1  
  asc.d  ~ gamma12 * grd.1 + beta2   * asc.1 + gamma32 * int.1 + gamma42 * hfs.1 + gamma52 * fof.1 + gamma62 * nfc.1
  int.d  ~ gamma13 * grd.1 + gamma23 * asc.1 + beta3   * int.1 + gamma43 * hfs.1 + gamma53 * fof.1 + gamma63 * nfc.1
  hfs.d  ~ gamma14 * grd.1 + gamma24 * asc.1 + gamma34 * int.1 + beta4   * hfs.1 + gamma54 * fof.1 + gamma64 * nfc.1
  fof.d  ~ gamma15 * grd.1 + gamma25 * asc.1 + gamma35 * int.1 + gamma45 * hfs.1 + beta5   * fof.1 + gamma65 * nfc.1
  nfc.d  ~ gamma16 * grd.1 + gamma26 * asc.1 + gamma36 * int.1 + gamma46 * hfs.1 + gamma56 * fof.1 + beta6   * nfc.1 
  
  
  # --------------------- COVARIANCES
  grd.1 ~~ phi2  * asc.1  # This estimates the covariances at T1
  grd.1 ~~ phi3  * int.1
  grd.1 ~~ phi4  * hfs.1
  grd.1 ~~ phi5  * fof.1
  grd.1 ~~ phi6  * nfc.1
  asc.1 ~~ phi23 * int.1
  asc.1 ~~ phi24 * hfs.1
  asc.1 ~~ phi25 * fof.1
  asc.1 ~~ phi26 * nfc.1
  int.1 ~~ phi34 * hfs.1
  int.1 ~~ phi35 * fof.1
  int.1 ~~ phi36 * nfc.1
  hfs.1 ~~ phi45 * fof.1
  hfs.1 ~~ phi46 * nfc.1
  fof.1 ~~ phi56 * nfc.1
  
  
  grd.d ~~ rho2 * asc.d # This estimates the change score covariances
  grd.d ~~ rho3 * int.d
  grd.d ~~ rho4 * hfs.d
  grd.d ~~ rho5 * fof.d
  grd.d ~~ rho6 * nfc.d
  asc.d ~~ rho23 * int.d
  asc.d ~~ rho24 * hfs.d
  asc.d ~~ rho25 * fof.d
  asc.d ~~ rho26 * nfc.d
  int.d ~~ rho34 * hfs.d
  int.d ~~ rho35 * fof.d
  int.d ~~ rho36 * nfc.d
  hfs.d ~~ rho45 * fof.d
  hfs.d ~~ rho46 * nfc.d
  fof.d ~~ rho56 * nfc.d
  
  '

# overall grades
df.overall = with(fs_tot, data.frame(grd.1 = GRO1, grd.2 = GRO2, 
                                     asc.1 = ASO1, asc.2 = ASO2, 
                                     int.1 = INO1, int.2 = INO2, 
                                     hfs.1 = HFS1, hfs.2 = HFS2,
                                     fof.1 = FOF1, fof.2 = FOF2,
                                     nfc.1 = NFC1, nfc.2 = NFC2))

fit.overall <- lavaan(lcsm_ext, data = df.overall, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(fit.overall, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

format.pe <- function(fit) {
  pe = parameterestimates(fit, standardized = T)[49:84, c("est", "se", "pvalue", "ci.lower", "ci.upper", "std.nox")]
  pe = cbind(round(pe[, c(1,2,4:6)], 2), round(pe[, 3], 3))
  colnames(pe) = c("B", "SE", "lower", "upper", "beta", "p")
  pe$sig = rep("", nrow(pe))
  pe$sig[which(pe$p <  .05)] = "*"
  pe$sig[which(pe$p <  .01)] = "**"
  pe$sig[which(pe$p < .001)] = "***"
  return(pe)
}

pe.overall=format.pe(fit.overall)
write.csv(pe.overall, file = "pe.overall.csv", row.names = F)

format.cor <- function(fit) {
  pe = parameterestimates(fit, standardized = T)[85:114, c("est", "pvalue", "std.nox")]
  cor = pval = data.frame(matrix(rep("—", 36), nrow = 6))
  colnames(cor) = colnames(pval) = c("GRD", "ASC", "INT", "HfS", "FoF", "NFC")
  rownames(cor) = rownames(pval) = c("GRD", "ASC", "INT", "HfS", "FoF", "NFC")
  # parameter estimates ------------------------------------------------------------
  # T1 correlations
  cor[1, 2:6] = sub("0.", ".", format(round(pe$std.nox[ 1: 5], 2), nsmall = 2))
  cor[2, 3:6] = sub("0.", ".", format(round(pe$std.nox[ 6: 9], 2), nsmall = 2))
  cor[3, 4:6] = sub("0.", ".", format(round(pe$std.nox[10:12], 2), nsmall = 2))
  cor[4, 5:6] = sub("0.", ".", format(round(pe$std.nox[13:14], 2), nsmall = 2))
  cor[5, 6:6] = sub("0.", ".", format(round(pe$std.nox[15:15], 2), nsmall = 2))
  # Correlated Change
  cor[2:6, 1] = sub("0.", ".", format(round(pe$std.nox[15 +  1: 5], 2), nsmall = 2))
  cor[3:6, 2] = sub("0.", ".", format(round(pe$std.nox[15 +  6: 9], 2), nsmall = 2))
  cor[4:6, 3] = sub("0.", ".", format(round(pe$std.nox[15 + 10:12], 2), nsmall = 2))
  cor[5:6, 4] = sub("0.", ".", format(round(pe$std.nox[15 + 13:14], 2), nsmall = 2))
  cor[6:6, 5] = sub("0.", ".", format(round(pe$std.nox[15 + 15:15], 2), nsmall = 2))
  # p-values -----------------------------------------------------------------------
  # T1 correlations
  pval[1, 2:6] = sub("0.", ".", format(round(pe$pval[ 1: 5], 3), nsmall = 3))
  pval[2, 3:6] = sub("0.", ".", format(round(pe$pval[ 6: 9], 3), nsmall = 3))
  pval[3, 4:6] = sub("0.", ".", format(round(pe$pval[10:12], 3), nsmall = 3))
  pval[4, 5:6] = sub("0.", ".", format(round(pe$pval[13:14], 3), nsmall = 3))
  pval[5, 6:6] = sub("0.", ".", format(round(pe$pval[15:15], 3), nsmall = 3))
  # Correlated Change
  pval[2:6, 1] = sub("0.", ".", format(round(pe$pval[15 +  1: 5], 3), nsmall = 3))
  pval[3:6, 2] = sub("0.", ".", format(round(pe$pval[15 +  6: 9], 3), nsmall = 3))
  pval[4:6, 3] = sub("0.", ".", format(round(pe$pval[15 + 10:12], 3), nsmall = 3))
  pval[5:6, 4] = sub("0.", ".", format(round(pe$pval[15 + 13:14], 3), nsmall = 3))
  pval[6:6, 5] = sub("0.", ".", format(round(pe$pval[15 + 15:15], 3), nsmall = 3))  
  return(list(rho = cor, p = pval))
  
}

format.cor(fit.overall)

# German grades
df.german  = with(fs_tot, data.frame(grd.1 = GRG1, grd.2 = GRG2, 
                                     asc.1 = ASG1, asc.2 = ASG2, 
                                     int.1 = ING1, int.2 = ING2, 
                                     hfs.1 = HFS1, hfs.2 = HFS2,
                                     fof.1 = FOF1, fof.2 = FOF2,
                                     nfc.1 = NFC1, nfc.2 = NFC2))

fit.german <- lavaan(lcsm_ext, data = df.german, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(fit.german, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

pe.german=format.pe(fit.german)
write.csv(pe.german, file = "pe.german.csv", row.names = F)

format.cor(fit.german)

# Math grades
df.math    = with(fs_tot, data.frame(grd.1 = GRM1, grd.2 = GRM2, 
                                     asc.1 = ASM1, asc.2 = ASM2, 
                                     int.1 = INM1, int.2 = INM2, 
                                     hfs.1 = HFS1, hfs.2 = HFS2,
                                     fof.1 = FOF1, fof.2 = FOF2,
                                     nfc.1 = NFC1, nfc.2 = NFC2))

fit.math <- lavaan(lcsm_ext, data = df.math, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(fit.math, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
pe.math=format.pe(fit.math)
write.csv(pe.math, file = "pe.math.csv", row.names = F)

format.cor(fit.math)

# Physics grades
df.physics = with(fs_tot, data.frame(grd.1 = GRP1, grd.2 = GRP2, 
                                     asc.1 = ASP1, asc.2 = ASP2, 
                                     int.1 = INP1, int.2 = INP2, 
                                     hfs.1 = HFS1, hfs.2 = HFS2,
                                     fof.1 = FOF1, fof.2 = FOF2,
                                     nfc.1 = NFC1, nfc.2 = NFC2))

fit.physics <- lavaan(lcsm_ext, data = df.physics, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(fit.physics, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
pe.physics = format.pe(fit.physics)
write.csv(pe.physics, file = "pe.physics.csv", row.names = F)

format.cor(fit.physics)

# Chemistry grades
df.chemistry = with(fs_tot, data.frame(grd.1 = GRC1, grd.2 = GRC2, 
                                       asc.1 = ASC1, asc.2 = ASC2, 
                                       int.1 = INC1, int.2 = INC2, 
                                       hfs.1 = HFS1, hfs.2 = HFS2,
                                       fof.1 = FOF1, fof.2 = FOF2,
                                       nfc.1 = NFC1, nfc.2 = NFC2))
   
fit.chemistry <- lavaan(lcsm_ext, data = df.chemistry, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
summary(fit.chemistry, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
pe.chemistry = format.pe(fit.chemistry)
write.csv(pe.chemistry, file = "pe.chemistry.csv", row.names = F)

fcc = format.cor(fit.chemistry)

asterisk = "***"
for (i in 1:6) {
  for (j in 1:6) {
    if (fcc$p[i, j] != "—") {
      sig = sum(as.numeric(fcc$p[i, j]) < c(.05, .01, .001))
      fcc$rho[i, j] = paste0(fcc$rho[i, j], substr(asterisk, 1, sig))
    }
  }
}
