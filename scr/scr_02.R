############################################
##                SCRIPT 02               ##
##               CALIBRATION              ##
############################################

# Source setup scripts:
source(here::here("scr","scr_00.R"))

# Load data:
load("data/lobby_dat.Rda")

###Calibration: --------------------------------------------------------------
### list of variables to calibrate: 
# Judicial independence
# Administrative burden
# Trade openness
# Budget transparency
# E-citizenship
# Freedom of press
# Mean trust in government
# TRUST 2019
# Mean CPI
# CPI 2019

#-------------------------------

### JUDICIAL INDEPENDENCE: --------------------------------------------------
CASES <- dat$CNTRY
JUD <- dat$judicial_ind
JUD_DF <- cbind.data.frame(CASES, JUD)

# Plot:
JUD_DF <- JUD_DF[order(JUD, decreasing = FALSE), ]
JUD_DF
dotchart(JUD_DF$JUD, labels = JUD_DF$CASES, cex = .7, 
         color = "black", xlab = "Raw Judicial Independence", main="a. Calibration Thresholds - Judicial Independence")

# Thresholds: 
findTh(JUD_DF$JUD, n = 3, method = "complete")
findTh(JUD_DF$JUD, n = 5, method = "complete")
findTh(JUD_DF$JUD, n = 7, method = "complete")
findTh(JUD_DF$JUD, n = 9, method = "complete")

abline(v = 5.516814, untf = FALSE, col = "firebrick") # excl
abline(v = 6.851813, untf = FALSE, col = "orange2") # 0.5
abline(v = 8.163596, untf = FALSE, col = "orange2") # incl

# Calibration:
JUD_Z <- calibrate(dat$judicial_ind, type = "fuzzy",
                   method = "direct", thresholds = "e=5.516814, c=6.851813, i=8.163596",
                   logistic = TRUE, idm = 0.947, ecdf = FALSE)

plot(JUD, JUD_Z, cex = .7, col = "cadetblue4", main="b. Calibration plot - Judicial Independence")
abline(h = 0.50, v = 6.851813, untf = FALSE, col = "grey")
text(JUD, JUD_Z, labels = CASES, cex = .6, pos = 4)

### ADMINISTRATIVE BURDEN: -----------------------------------------------------

ADM <- dat$adm_burden
ADM_DF <- cbind.data.frame(CASES, ADM)

# Plot:
ADM_DF <- ADM_DF[order(ADM, decreasing = FALSE), ]
ADM_DF
dotchart(ADM_DF$ADM, labels = ADM_DF$CASES, cex = .7, 
         color = "black", xlab = "Raw Administrative Burden", main="a. Calibration Thresholds - Administrative Burden")

# Threshold
findTh(ADM_DF$ADM, n = 3, method = "complete")
findTh(ADM_DF$ADM, n = 5, method = "complete")
findTh(ADM_DF$ADM, n = 7, method = "complete")
findTh(ADM_DF$ADM, n = 9, method = "complete")

abline(v = 8.771003, untf = FALSE, col = "gold") # excl
abline(v = 9.152657, untf = FALSE, col = "firebrick") # 0.5
abline(v = 9.554932, untf = FALSE, col = "orange2") # incl

# Calibration:
ADM_Z <- calibrate(dat$adm_burden, type = "fuzzy",
                   method = "direct", thresholds = "e=8.771003, c=9.152657, i=9.554932",
                   logistic = TRUE, idm = 0.947, ecdf = FALSE)

plot(ADM, ADM_Z, cex = .7, col = "cadetblue4", main="b. Calibration plot - Asministrative Burden")
abline(h = 0.50, v = 9.152657, untf = FALSE, col = "grey")
text(ADM, ADM_Z, labels = CASES, cex = .6, pos = 4)

### TRADE OPENNESS: -------------------------------------------------------------

TRADE <- dat$trade_ope
TRADE_DF <- cbind.data.frame(CASES, TRADE)

# Plot:
TRADE_DF <- TRADE_DF[order(TRADE, decreasing = FALSE), ]
TRADE_DF
dotchart(TRADE_DF$TRADE, labels = TRADE_DF$CASES, cex = .7, 
         color = "black", xlab = "Raw Trade Openness", main="a. Calibration Thresholds - Trade Openness")

# Thresholds:
findTh(TRADE_DF$TRADE, n = 3, method = "complete")
findTh(TRADE_DF$TRADE, n = 5, method = "complete")
findTh(TRADE_DF$TRADE, n = 7, method = "complete")
findTh(TRADE_DF$TRADE, n = 9, method = "complete")

abline(v = 9.495119, untf = FALSE, col = "gold") # excl
abline(v = 9.637369, untf = FALSE, col = "firebrick") # 0.5
abline(v = 9.960356, untf = FALSE, col = "orange2") # incl


# Calibration:
TRADE_Z <- calibrate(dat$trade_ope, type = "fuzzy",
                     method = "direct", thresholds = "e=9.495119, c=9.637369, i=9.960356",
                     logistic = TRUE, idm = 0.947, ecdf = FALSE)

plot(TRADE, TRADE_Z, cex = .7, col = "cadetblue4", main="b. Calibration Plot - Trade Openness")
abline(h = 0.50, v = 9.637369, untf = FALSE, col = "grey")
text(TRADE, TRADE_Z, labels = CASES, cex = .6, pos = 4)

### BUDGET TRANSPARENCY: -----------------------------------------------------------

BUDGET <- dat$budget_trans
BUDGET_DF <- cbind.data.frame(CASES, BUDGET)

# Plot:
BUDGET_DF <- BUDGET_DF[order(BUDGET, decreasing = FALSE), ]
BUDGET_DF
dotchart(BUDGET_DF$BUDGET, labels = BUDGET_DF$CASES, cex = .7, 
         color = "black", xlab = "Raw Budget Transparency", main="a. Calibration Thresholds - Budget Transparency")

# Thresholds:
findTh(BUDGET_DF$BUDGET, n = 3, method = "complete")
findTh(BUDGET_DF$BUDGET, n = 5, method = "complete")
findTh(BUDGET_DF$BUDGET, n = 7, method = "complete")
findTh(BUDGET_DF$BUDGET, n = 9, method = "complete")

abline(v = 7.107142, untf = FALSE, col = "gold") # excl
abline(v = 7.856071, untf = FALSE, col = "firebrick") # 0.5
abline(v = 8.820357, untf = FALSE, col = "orange2") # incl


# Calibration:
BUDGET_Z <- calibrate(dat$budget_trans, type = "fuzzy",
                      method = "direct", thresholds = "e=7.107142, c=7.856071, i=8.820357",
                      logistic = TRUE, idm = 0.947, ecdf = FALSE)

plot(BUDGET, BUDGET_Z, cex = .7, col = "cadetblue4", main="b. Calibration Plot - Budget Transparency")
abline(h = 0.50, v = 7.856071, untf = FALSE, col = "grey")
text(BUDGET, BUDGET_Z, labels = CASES, cex = .6, pos=4)

### E-CITIZENSHIP: ------------------------------------------------------------------

E_CIT <- dat$e_cit
E_CIT_DF <- cbind.data.frame(CASES, E_CIT)

# Plot:
E_CIT_DF <- E_CIT_DF[order(E_CIT, decreasing = FALSE), ]
E_CIT_DF
dotchart(E_CIT_DF$E_CIT, labels = E_CIT_DF$CASES, cex = .7, 
         color = "black", xlab = "Raw e-citizenship", main="a. Calibration Thresholds - e-citizenship")

# Thresholds:
findTh(E_CIT_DF$E_CIT, n = 3, method = "complete")
findTh(E_CIT_DF$E_CIT, n = 5, method = "complete")
findTh(E_CIT_DF$E_CIT, n = 7, method = "complete")
findTh(E_CIT_DF$E_CIT, n = 9, method = "complete")

abline(v = 6.831558, untf = FALSE, col = "gold") # excl
abline(v = 8.149668, untf = FALSE, col = "firebrick") # 0.5
abline(v = 9.062029, untf = FALSE, col = "orange2") # incl


#Calibration:
E_CIT_Z <- calibrate(dat$e_cit, type = "fuzzy",
                     method = "direct", thresholds = "e=6.831558, c=8.149668, i=9.062029",
                     logistic = TRUE, idm = 0.947, ecdf = FALSE)

plot(E_CIT, E_CIT_Z, cex = .7, col = "cadetblue4", main="b. Calibration Plot - e-citizenship")
abline(h = 0.50, v = 8.149668, untf = FALSE, col = "grey")
text(E_CIT, E_CIT_Z, labels = CASES, cex = .6, pos = 4)

### FREEDOM OF THE PRESS: ------------------------------------------------------------

PRESS <- dat$free_press
PRESS_DF <- cbind.data.frame(CASES, PRESS)

# Plot:
PRESS_DF <- PRESS_DF[order(PRESS, decreasing = FALSE), ]
PRESS_DF
dotchart(PRESS_DF$PRESS, labels = PRESS_DF$CASES, cex = .7, 
         color = "black", xlab = "Raw Freedom of the Press", main="a. Calibration Thresholds - Freedom of the Press")

# Thresholds:
findTh(PRESS_DF$PRESS, n = 3, method = "complete")
findTh(PRESS_DF$PRESS, n = 5, method = "complete")
findTh(PRESS_DF$PRESS, n = 7, method = "complete")
findTh(PRESS_DF$PRESS, n = 9, method = "complete")

abline(v = 7.914634, untf = FALSE, col = "gold") # excl
abline(v = 8.243903, untf = FALSE, col = "firebrick") # 0.5
abline(v = 8.518292, untf = FALSE, col = "orange2") # incl

# Calibration:
PRESS_Z <- calibrate(dat$free_press, type = "fuzzy",
                     method = "direct", thresholds = "e=7.914634, c=8.243903, i=8.518292",
                     logistic = TRUE, idm = 0.947, ecdf = FALSE)

plot(PRESS, PRESS_Z, cex = .7, col = "cadetblue4", main="b. Calibration Plot - Freedom of the Press")
abline(h = 0.50, v = 8.243903, untf = FALSE, col = "grey")
text(PRESS, PRESS_Z, labels = CASES, cex = .6, pos = 4)

### MEAN CPI: -------------------------------------------------------------------
MEAN_CPI <- dat$MEAN_CPI
MEAN_CPI_DF <- cbind.data.frame(CASES, MEAN_CPI)

# Plot:
MEAN_CPI_DF <- MEAN_CPI_DF[order(MEAN_CPI, decreasing = FALSE), ]
MEAN_CPI_DF
dotchart(MEAN_CPI_DF$MEAN_CPI, labels = MEAN_CPI_DF$CASES, cex = .7, 
         color = "black", xlab = "RAW CLEAN")

# Thresholds: 
findTh(MEAN_CPI_DF$MEAN_CPI, n = 3, method = "complete")
findTh(MEAN_CPI_DF$MEAN_CPI, n = 5, method = "complete")
findTh(MEAN_CPI_DF$MEAN_CPI, n = 7, method = "complete")
findTh(MEAN_CPI_DF$MEAN_CPI, n = 9, method = "complete")

abline(v = 54.50000 , untf = FALSE, col = "firebrick") # excl
abline(v = 66.66667, untf = FALSE, col = "orange2") # 0.5
abline(v = 77.83333, untf = FALSE, col = "orange2") # incl

# Calibration:
MEAN_CPI_Z <- calibrate(dat$MEAN_CPI, type = "fuzzy",
                   method = "direct", thresholds = "e=54.50000, c=66.66667, i=77.83333",
                   logistic = TRUE, idm = 0.947, ecdf = FALSE)

plot(MEAN_CPI, MEAN_CPI_Z, cex = .7, col = "cadetblue4")
abline(h = 0.50, v = 65.5, untf = FALSE, col = "grey")
text(MEAN_CPI, MEAN_CPI_Z, labels = CASES, cex = .7, pos = 4)

### CPI 2019: -------------------------------------------------------------------
CPI <- dat$CPI
CPI_DF <- cbind.data.frame(CASES, CPI)

# Plot:
CPI_DF <- CPI_DF[order(CPI, decreasing = FALSE), ]
CPI_DF
dotchart(CPI_DF$CPI, labels = CPI_DF$CASES, cex = .7, 
         color = "black", xlab = "Raw Corruption Perceptions Index", main="a. Calibration Thresholds - CPI")

# Thresholds: 
findTh(CPI_DF$CPI, n = 3, method = "complete")
findTh(CPI_DF$CPI, n = 5, method = "complete")
findTh(CPI_DF$CPI, n = 7, method = "complete")
findTh(CPI_DF$CPI, n = 9, method = "complete")

abline(v = 54.5 , untf = FALSE, col = "gold") # excl
abline(v = 65.5, untf = FALSE, col = "firebrick") # 0.5
abline(v = 78.5, untf = FALSE, col = "orange2") # incl

# Calibration:
CPI_Z <- calibrate(dat$CPI, type = "fuzzy",
                        method = "direct", thresholds = "e=54.5, c=65.5, i=78.5",
                        logistic = TRUE, idm = 0.947, ecdf = FALSE)

plot(CPI, CPI_Z, cex = .7, col = "cadetblue4", main="b. Calibration plot - CPI")
abline(h = 0.50, v = 65.5, untf = FALSE, col = "grey")
text(CPI, CPI_Z, labels = CASES, cex = .6, pos = 4)


### MEAN TRUST IN THE GOVERNMENT: ------------------------------------------------------------

MEAN_TRUST <- dat$MEAN_TRUST
MEAN_TRUST_DF <- cbind.data.frame(CASES, MEAN_TRUST)

# Plot:
MEAN_TRUST_DF <- MEAN_TRUST_DF[order(MEAN_TRUST, decreasing = FALSE), ]
MEAN_TRUST_DF
dotchart(MEAN_TRUST_DF$MEAN_TRUST, labels = MEAN_TRUST_DF$CASES, cex = .7, 
         color = "black", xlab = "RAW CLEAN")

# Thresholds:
findTh(MEAN_TRUST_DF$MEAN_TRUST, n = 3, method = "complete")
findTh(MEAN_TRUST_DF$MEAN_TRUST, n = 5, method = "complete")
findTh(MEAN_TRUST_DF$MEAN_TRUST, n = 7, method = "complete")
findTh(MEAN_TRUST_DF$MEAN_TRUST, n = 9, method = "complete")

abline(v = 32.91369, untf = FALSE, col = "firebrick") # excl
abline(v = 50, untf = FALSE, col = "orange2") # 0.5
abline(v = 71.14906, untf = FALSE, col = "orange2") # incl

# Calibration:
MEAN_TRUST_Z <- calibrate(dat$MEAN_TRUST, type = "fuzzy",
                     method = "direct", thresholds = "e=32.91369, c=50, i=71.14906",
                     logistic = TRUE, idm = 0.947, ecdf = FALSE)

# calibrazione stretta: "e=44.62888, c=50.40773, i=55.85878"
# calibrazione larga: 32.91369 55.85878 71.14906

plot(MEAN_TRUST, MEAN_TRUST_Z, cex = .7, col = "cadetblue4")
abline(h = 0.50, v = 50, untf = FALSE, col = "grey")
text(MEAN_TRUST, MEAN_TRUST_Z, labels = CASES, cex = .7, pos = 4)

### TRUST IN THE GOVERNMENT: ------------------------------------------------------------

TRUST <- dat$TRUST
TRUST_DF <- cbind.data.frame(CASES, TRUST)

# Plot:
TRUST_DF <- TRUST_DF[order(TRUST, decreasing = FALSE), ]
TRUST_DF
dotchart(TRUST_DF$TRUST, labels = TRUST_DF$CASES, cex = .7, 
         color = "black", xlab = "Raw Trust in government", main="a. Calibration Thresholds - Trust in government")

# Thresholds:
findTh(TRUST_DF$TRUST, n = 3, method = "complete")
findTh(TRUST_DF$TRUST, n = 5, method = "complete")
findTh(TRUST_DF$TRUST, n = 7, method = "complete")
findTh(TRUST_DF$TRUST, n = 9, method = "complete")

abline(v = 28.36544, untf = FALSE, col = "gold") # excl
abline(v = 50, untf = FALSE, col = "firebrick") # 0.5
abline(v = 70.96796, untf = FALSE, col = "orange2") # incl  

# Calibration:
TRUST_Z <- calibrate(dat$TRUST, type = "fuzzy",
                         method = "direct", thresholds = "e=28.36544, c=50, i=70.96796",
                          logistic = TRUE, idm = 0.947, ecdf = FALSE)


plot(TRUST, TRUST_Z, cex = .6, col = "cadetblue4", main="b. Calibration plot - Trust in government")
abline(h = 0.50, v = 50, untf = FALSE, col = "grey")
text(TRUST, TRUST_Z, labels = CASES, cex = .6, pos = 4)

### Saving calibrated variables:--------------------------------------

# Binding the calibrated variables with the other ones:

lobby <- dat %>% 
  select (LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET, COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, 
          DISTANCE2019) 

lobby <- cbind.data.frame(lobby, JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z, CPI_Z, MEAN_CPI_Z, TRUST_Z, MEAN_TRUST_Z) 

row.names(lobby) <- dat$CNTRY

#Saving dataset:
save(lobby, file="data/lobby.Rda")




