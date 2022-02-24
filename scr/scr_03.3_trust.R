############################################     
##                SCRIPT 3.3              ##
##            MODEL 3: R = TRUST          ##
############################################


# Source setup scripts:
source(here::here("scr","scr_00.R"))

# Load data:
load("data/lobby.Rda")

### ASSESS THE CLAIM OF INDIVIDUAL NECESSITY --------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019",
       relation = "nec") 

# with negative outcome
pofind(lobby, outcome = "~TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
       relation = "nec")

### ASSESS THE CLAIM OF JOINT SUFFICIENCY-------------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
       relation = "suff")

# with negative outcome
pofind(lobby, outcome = "~TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
       relation = "suff")

### The TRUTH TABLE: ----------------------------------------------

# positive truth table for model 3:
TTP3 <- truthTable(lobby, outcome = "TRUST_Z",
                  conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019",
                  incl.cut = 0.85, n.cut = 1, complete = F, use.letters = T,
                  show.cases = TRUE, dcc = T, sort.by = "incl, n")

TTP3

#saving the output:
stargazerTT(TTP3,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.1.2 - Positive outcome",
            out = "ttp3.html",
            digits = 3)

# negative truth table for model 3:
lobby$X_TRUST_Z <- 1 - lobby$TRUST_Z

TTN3 <- truthTable(lobby, outcome = "X_TRUST_Z", 
                  conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
                  incl.cut = 0.85, n.cut = 1, complete = FALSE, use.letters = T,
                  show.cases = TRUE, dcc = TRUE, sort.by = "incl, n")
TTN3

#saving the output:
stargazerTT(TTN3,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.1.2 - Negative outcome",
            out = "ttn3.html",
            digits = 3)

### MINIMISATION: ----------------------------------------------------------------

# 1. COMPLEX SOLUTION: --------------------------------------
path3.1 <- minimize(TTP3, 
                  details = TRUE, method = "QMC", row.dom = T)
path3.1
path3.1$PIchart
path3.1$SA

# Plot 1: 
XYplot(path3.1$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Trust in government", jitter = F,
       enhance = T)

#saving the output:
stargazerSol(path3.1,
             outcome = "TRUST_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.1.2",
             out = "hp1.2_pos.html",
             digits = 3)

# COMPLEX-NEGATIVE:
path3.2 <- minimize(TTN3, 
                     details = TRUE, method = "QMC", row.dom = T)
path3.2
path3.2$PIchart
path3.2$SA

# Plot 1: 
XYplot(path3.2$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Distrust in government", jitter = F,
       enhance = T)

#saving the output:
stargazerSol(path3.2,
             outcome = "X_TRUST_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.1.2",
             out = "hp1.2_neg.html",
             digits = 3)

