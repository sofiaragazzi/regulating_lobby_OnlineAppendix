############################################     
##                SCRIPT 3.5              ##
##            MODEL 5: C = TRUST          ##
############################################


# Source setup scripts:
source(here::here("Desktop", "TGL", "research", "scr","scr_00.R"))

# Load data:
load("data/lobby.Rda")

### ASSESS THE CLAIM OF INDIVIDUAL NECESSITY --------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "TRUST_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
       relation = "nec") 

# with negative outcome
pofind(lobby, outcome = "~TRUST_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "nec")

### ASSESS THE CLAIM OF JOINT SUFFICIENCY-------------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "TRUST_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")

# with negative outcome
pofind(lobby, outcome = "~TRUST_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")

### The TRUTH TABLE: ----------------------------------------------

# positive truth table for model 5:
TTP5 <- truthTable(lobby, outcome = "TRUST_Z",
                  conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
                  incl.cut = 0.85, n.cut = 1, complete = F, use.letters = T,
                  show.cases = TRUE, dcc = T, sort.by = "incl, n")

TTP5

#saving the output:
stargazerTT(TTP5,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.1.3 - Positive outcome",
            out = "ttp5.html",
            digits = 3)

# negative truth table for model 5:
lobby$X_TRUST_Z <- 1 - lobby$TRUST_Z

TTN5 <- truthTable(lobby, outcome = "X_TRUST_Z", 
                  conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
                  incl.cut = 0.85, n.cut = 1, complete = FALSE, use.letters = T,
                  show.cases = TRUE, dcc = TRUE, sort.by = "incl, n")
TTN5

#saving the output:
stargazerTT(TTN5,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.1.3 - Negative outcome",
            out = "ttn5.html",
            digits = 3)

### MINIMISATION: ----------------------------------------------------------------

# 1. COMPLEX SOLUTION: --------------------------------------
path5.1 <- minimize(TTP5, 
                  details = TRUE, method = "QMC", row.dom = T)
path5.1
path5.1$PIchart
path5.1$SA

# Plot 1: 
XYplot(path5.1$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Trust in government", jitter = F, 
       enhance = T)

#saving the output:
stargazerSol(path5.1,
             outcome = "TRUST_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.1.3",
             out = "hp1.3_pos.html",
             digits = 3)

# COMPLEX-NEGATIVE:
path5.2 <- minimize(TTN5, 
                     details = TRUE, method = "QMC", row.dom = T)
path5.2
path5.2$PIchart
path5.2$SA

# Plot 2: 
XYplot(path5.2$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Distrust in government", jitter = F, 
       enhance = T)


#saving the output:
stargazerSol(path5.2,
             outcome = "X_TRUST_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.1.3",
             out = "hp1.3_neg.html",
             digits = 3)

