############################################     
##                SCRIPT 3.1              ##
##        MODEL 1: OUTCOME= TRUST         ##
############################################

# Source setup scripts:
source(here::here("scr","scr_00.R"))

# Load data:
load("data/lobby.Rda")

### ASSESS THE CLAIM OF INDIVIDUAL NECESSITY --------------------------------------------------

# with positive outcome
pofind(lobby, 
       outcome = "TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                     COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                     JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
       relation = "nec") 

# with negative outcome 
pofind(lobby, outcome = "~TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                        JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "nec")

### ASSESS THE CLAIM OF JOINT SUFFICIENCY-------------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                        JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")

# with negative outcome
pofind(lobby, outcome = "~TRUST_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                        JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")


### The TRUTH TABLE: ----------------------------------------------

# positive truth table for model 1:
TTP1 <- truthTable(lobby, outcome = "TRUST_Z",
                  conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                        JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
                  incl.cut = 0.85 , n.cut = 1, complete = F, use.letters = T,
                  show.cases = TRUE, dcc = T, sort.by = "incl, n")

TTP1

#saving the output:
stargazerTT(TTP1,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.1 - Positive outcome",
            out = "ttp1.html",
            digits = 3)


# negative truth table for model 1: 
lobby$X_TRUST_Z <- 1 - lobby$TRUST_Z

TTN1 <- truthTable(lobby, outcome = "X_TRUST_Z", 
                  conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                        JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
                  incl.cut = 0.85, n.cut = 1, complete = FALSE, use.letters = T,
                  show.cases = TRUE, dcc = TRUE, sort.by = "incl, n")
TTN1

#saving the output:
stargazerTT(TTN1,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.1 - Negative outcome",
            out = "ttn1.html",
            digits = 3)

### MINIMISATION: ----------------------------------------------------------------

# 1. COMPLEX SOLUTION: --------------------------------------
path1.1 <- minimize(TTP1, 
                  details = TRUE, method = "QMC", row.dom = T)
path1.1
path1.1$PIchart
path1.1$SA

#saving the output:
stargazerSol(path1.1,
             outcome = "TRUST_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.1",
             out = "hp1_pos.html",
             digits = 3)


# Plot 1: 
XYplot(path1.1$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Trust in government", jitter = F, amount = 0.04, 
       enhance = T, main= "Hp.1")


# COMPLEX-NEGATIVE:
path1.2 <- minimize(TTN1, 
                     details = TRUE, method = "QMC", row.dom = T)
path1.2
path1.2$PIchart
path1.2$SA

# Plot 2: 
XYplot(path1.2$solution[[1]], X_TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Distrust in government", jitter = F, amount = 0.04)

#saving the output:
stargazerSol(path1.2,
             outcome = "X_TRUST_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.1",
             out = "hp1_neg.html",
             digits = 3)


# plotting the two XYplot: 
par(mfrow=c(1,2))

XYplot(path1.1$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Trust in government", jitter = F, 
       enhance = T)

XYplot(path1.2$solution[[1]], X_TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Distrust in government", jitter = F, 
       enhance = T)

