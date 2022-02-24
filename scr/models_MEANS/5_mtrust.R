############################################     OUTCOME = MEAN_TRUST, AND ONLY CONTEXT VARIABLES
##                APPENDIX                ##
##                   MODEL 5              ##
############################################
# Source setup scripts:
source(here::here("Desktop", "TGL", "research", "scr","scr_00.R"))

# Load data:
load("data/lobby.Rda")

### ASSESS THE CLAIM OF INDIVIDUAL NECESSITY --------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "MEAN_TRUST_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
       relation = "nec") 

# with negative outcome
pofind(lobby, outcome = "~MEAN_TRUST_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "nec")

### ASSESS THE CLAIM OF JOINT SUFFICIENCY-------------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "MEAN_TRUST_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")

# with negative outcome
pofind(lobby, outcome = "~MEAN_TRUST_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")

### The TRUTH TABLE: ----------------------------------------------

# positive truth table for model 5:
TTP5 <- truthTable(lobby, outcome = "MEAN_TRUST_Z",
                   conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
                   incl.cut = 0.85, n.cut = 1, complete = F, use.letters = F,
                   show.cases = TRUE, dcc = T, sort.by = "incl, n")
TTP5

# negative truth table for model 5:
lobby$X_MEAN_TRUST_Z <- 1 - lobby$MEAN_TRUST_Z

TTN5 <- truthTable(lobby, outcome = "X_MEAN_TRUST_Z", 
                   conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
                   incl.cut = 0.85, n.cut = 1, complete = FALSE, use.letters = F,
                   show.cases = TRUE, dcc = TRUE, sort.by = "incl, n")
TTN5

### MINIMISATION: ----------------------------------------------------------------

# 1. COMPLEX SOLUTION: --------------------------------------
path5.1 <- minimize(TTP5, 
                    details = TRUE, method = "QMC", row.dom = T)
path5.1
path5.1$PIchart
path5.1$SA

# Plot 1: 
XYplot(path1$solution[[1]], MEAN_TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Trust in government", jitter = F, amount = 0.04)
# Incl: 0.910; Cov: 0.150; PRI: 0.796

# COMPLEX-NEGATIVE:
path5.2 <- minimize(TTN5, 
                    details = TRUE, method = "QMC", row.dom = T)
path5.2
path5.2$PIchart
path5.2$SA

# Plot 1: 
XYplot(path5.2$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Distrust in government", jitter = F, amount = 0.04)

# Incl: 0.993; Cov: 0.444; PRI: 0.991
