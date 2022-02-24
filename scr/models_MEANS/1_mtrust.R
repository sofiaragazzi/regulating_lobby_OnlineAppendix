############################################     VERSION WITH OUTCOME = MEAN_TRUST
##                APPENDIX                ##
##                MODEL    1              ##
############################################

# Source setup scripts:
source(here::here("Desktop", "TGL", "research", "scr","scr_00.R"))

# Load data:
load("data/lobby.Rda")

### ASSESS THE CLAIM OF INDIVIDUAL NECESSITY --------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "MEAN_TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                        JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z, MEAN_CPI_Z",
       relation = "nec") 


# with negative outcome 
pofind(lobby, outcome = "~MEAN_TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                        JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z, MEAN_CPI_Z", 
       relation = "nec")

### ASSESS THE CLAIM OF JOINT SUFFICIENCY-------------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "MEAN_TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                        JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z, MEAN_CPI_Z", 
       relation = "suff")

# with negative outcome
pofind(lobby, outcome = "~MEAN_TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                        JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z, MEAN_CPI_Z", 
       relation = "suff")


### The TRUTH TABLE: ----------------------------------------------

# positive truth table for model 1:
TTP1 <- truthTable(lobby, outcome = "MEAN_TRUST_Z",
                   conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                        JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
                   incl.cut = 0.85, n.cut = 1, complete = F, use.letters = F,
                   show.cases = TRUE, dcc = T, sort.by = "incl, n")

TTP1

# negative truth table for model 1: 
lobby$X_MEAN_TRUST_Z <- 1 - lobby$MEAN_TRUST_Z

TTN1 <- truthTable(lobby, outcome = "X_MEAN_TRUST_Z", 
                   conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                        JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
                   incl.cut = 0.85, n.cut = 1, complete = FALSE, use.letters = F,
                   show.cases = TRUE, dcc = TRUE, sort.by = "incl, n")
TTN1


### MINIMISATION: ----------------------------------------------------------------

# 1. COMPLEX SOLUTION: --------------------------------------
path1.1 <- minimize(TTP1, 
                    details = TRUE, method = "QMC", row.dom = T)
path1.1
path1.1$PIchart
path1.1$SA

# Plot 1: 
XYplot(path1.1$solution[[1]], MEAN_TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Trust in government", jitter = F, amount = 0.04)

# Incl: 0.974; Cov: 0.661; PRI: 0.958

# COMPLEX-NEGATIVE:
path1.2 <- minimize(TTN1, 
                    details = TRUE, method = "QMC", row.dom = T)
path1.2
path1.2$PIchart
path1.2$SA

# Plot 1: 
XYplot(path1.2$solution[[1]], X_MEAN_TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Distrust in government", jitter = F, amount = 0.04)

# Incl: 0.995; Cov: 0.627; PRI: 0.994


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

#POSITIVE
path3 <- minimize(TTP, include = "?", dir.exp = c(""),
                  details = TRUE, method = "CCubes", row.dom = T)
path3
path3$i.sol$C1P1$solution[[1]]
path3$PIchart

path3$solution

XYplot(path3$i.sol$C1P1$solution[[1]], MEAN_TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "darkorange", 
       xlab = "positive intermediate", ylab = "TRUST")
# Incl: 0.956; Cov: 0.347; PRI: 0.935

#NEGATIVE
path3_neg <- minimize(TTN, include = "?", dir.exp = c(""),
                      details = TRUE, method = "CCubes", row.dom = T)

path3_neg$
  
  XYplot(path3_neg$i.sol$C1P1$solution[[1]], MEAN_TRUST_Z, lobby, 
         clabels = row.names(lobby), cex = 0.7, col = "darkorange", 
         xlab = "negative intermediate", ylab = "TRUST")
# Incl: 1; Cov: 0.048; PRI: 1
