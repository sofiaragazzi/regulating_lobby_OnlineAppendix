############################################     VERSION WITH OUTCOME = MEAN_CPI_Z
##                   APPENDIX             ##
##                   MODEL 2              ##
############################################
# Source setup scripts:
source(here::here("Desktop", "TGL", "research", "scr","scr_00.R"))

# Load data:
load("data/lobby.Rda")

### ASSESS THE CLAIM OF INDIVIDUAL NECESSITY --------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "MEAN_CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
       relation = "nec") 

# with negative outcome
pofind(lobby, outcome = "~MEAN_CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "nec")

### ASSESS THE CLAIM OF JOINT SUFFICIENCY-------------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "MEAN_CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")

# with negative outcome
pofind(lobby, outcome = "~MEAN_CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")

### The TRUTH TABLE: ----------------------------------------------

# positive truth table for model 2:
TTP2 <- truthTable(lobby, outcome = "MEAN_CPI_Z",
                   conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
                   incl.cut = 0.85, n.cut = 1, complete = F, use.letters = F,
                   show.cases = TRUE, dcc = T, sort.by = "incl, n")

TTP2

# negative truth table for model 2:
lobby$X_CPI_Z<- 1 - lobby$MEAN_CPI_Z

TTN2 <- truthTable(lobby, outcome = "X_CPI_Z", 
                   conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
                   incl.cut = 0.85, n.cut = 1, complete = FALSE, use.letters = F,
                   show.cases = TRUE, dcc = TRUE, sort.by = "incl, n")
TTN2

### MINIMISATION: ----------------------------------------------------------------

# 1. COMPLEX SOLUTION: --------------------------------------
path2.1 <- minimize(TTP2, 
                    details = TRUE, method = "QMC", row.dom = T)
path2.1
path2.1$PIchart
path2.1$SA
path2.1$essential

# Plot 1: 
XYplot(path2.1$solution[[1]], MEAN_CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Lower perceived corruption", jitter = F, amount = 0.05)

# Incl: 0.989; Cov: 0.674; PRI: 0.987

# COMPLEX-NEGATIVE:
path2.2 <- minimize(TTN2, 
                    details = TRUE, method = "QMC", row.dom = T)
path2.2
path2.2$PIchart
path2.2$SA

# Plot 1: 
XYplot(path2.2$solution[[1]], MEAN_CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Higher perceived corruption", jitter = F, amount = 0.01)

# Incl: 0.996; Cov: 0.636; PRI: 0.996

