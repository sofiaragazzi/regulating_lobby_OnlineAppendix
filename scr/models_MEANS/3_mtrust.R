############################################     OUTCOME = MEAN_TRUST, AND ONLY LOBBY VARIABLES
##                  APPENDIX              ##
##                   MODEL 3              ##
############################################
# Source setup scripts:
source(here::here("Desktop", "TGL", "research", "scr","scr_00.R"))

# Load data:
load("data/lobby.Rda")

### ASSESS THE CLAIM OF INDIVIDUAL NECESSITY --------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "MEAN_TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019",
       relation = "nec") 

# with negative outcome
pofind(lobby, outcome = "~MEAN_TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
       relation = "nec")

### ASSESS THE CLAIM OF JOINT SUFFICIENCY-------------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "MEAN_TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
       relation = "suff")

# with negative outcome
pofind(lobby, outcome = "~MEAN_TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
       relation = "suff")

### The TRUTH TABLE: ----------------------------------------------

# positive truth table for model 3:
TTP3 <- truthTable(lobby, outcome = "MEAN_TRUST_Z",
                   conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019",
                   incl.cut = 0.85, n.cut = 1, complete = F, use.letters = F,
                   show.cases = TRUE, dcc = T, sort.by = "incl, n")

TTP3

# negative truth table for model 3:
lobby$X_MEAN_TRUST_Z <- 1 - lobby$MEAN_TRUST_Z

TTN3 <- truthTable(lobby, outcome = "X_MEAN_TRUST_Z", 
                   conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
                   incl.cut = 0.85, n.cut = 1, complete = FALSE, use.letters = F,
                   show.cases = TRUE, dcc = TRUE, sort.by = "incl, n")
TTN3

### MINIMISATION: ----------------------------------------------------------------

# 1. COMPLEX SOLUTION: --------------------------------------
path3.1 <- minimize(TTP3, 
                    details = TRUE, method = "QMC", row.dom = T)
path3.1
path3.1$PIchart
path3.1$SA

# Plot 1: 
XYplot(path3.1$solution[[1]], MEAN_TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Trust in government", jitter = F, amount = 0.04)
# Incl: 0.910; Cov: 0.150; PRI: 0.796

# COMPLEX-NEGATIVE:
path3.2 <- minimize(TTN3, 
                    details = TRUE, method = "QMC", row.dom = T)
path3.2
path3.2$PIchart
path3.2$SA

# Plot 1: 
XYplot(path3.2$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Distrust in government", jitter = F, amount = 0.04)

# Incl: 0.993; Cov: 0.444; PRI: 0.991


# PARSIMONIOUS SOLUTION:------------------------------------------

path2 <- minimize(TTP, include = "?", 
                  details = TRUE, method = "QMC")
path2
path2$solution[[1]]
path2$SA

# Plot 2:
XYplot(path2$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive parsimonious", ylab = "TRUST in government", jitter = T, amount = 0.04)


# INTERMEDIATE SOLUTION:-------------------

path3 <- minimize(TTP, include = "?", dir.exp = "1,1,1,1,1,1,1,1,1,1,1",
                  details = TRUE, method = "CCubes", row.dom = T)
path3
path3$i.sol$C1P1$solution[[1]]

path3$PIchart

path4 <- minimize(TTN, include = "?", dir.exp = "1,1,1,1,1,1,1,1,1,1,1",
                  details = TRUE, method = "CCubes", row.dom = T)

path4

# Plot 3:
XYplot(path3$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive intermediate", ylab = "TRUST in government", jitter = T, amount = 0.04)

