############################################     
##                SCRIPT 3.6              ##
##             MODEL 6: C = CPI           ##
############################################


# Source setup scripts:
source(here::here("Desktop", "TGL", "research", "scr","scr_00.R"))

# Load data:
load("data/lobby.Rda")

### ASSESS THE CLAIM OF INDIVIDUAL NECESSITY --------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "CPI_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
       relation = "nec") 

# with negative outcome
pofind(lobby, outcome = "~CPI_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "nec")

### ASSESS THE CLAIM OF JOINT SUFFICIENCY-------------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "CPI_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")

# with negative outcome
pofind(lobby, outcome = "~CPI_Z", 
       conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")

### The TRUTH TABLE: ----------------------------------------------

# positive truth table for model 6:
TTP6 <- truthTable(lobby, outcome = "CPI_Z",
                  conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
                  incl.cut = 0.85, n.cut = 1, complete = F, use.letters = T,
                  show.cases = TRUE, dcc = T, sort.by = "incl, n")

TTP6

#saving the output:
stargazerTT(TTP6,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.2.3 - Positive outcome",
            out = "ttp6.html",
            digits = 3)

# negative truth table for model 6:
lobby$X_CPI_Z <- 1 - lobby$CPI_Z

TTN6 <- truthTable(lobby, outcome = "X_CPI_Z", 
                  conditions = "JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
                  incl.cut = 0.85, n.cut = 1, complete = FALSE, use.letters = T,
                  show.cases = TRUE, dcc = TRUE, sort.by = "incl, n")
TTN6

#saving the output:
stargazerTT(TTN6,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.2.3 - Negative outcome",
            out = "ttn6.html",
            digits = 3)


### MINIMISATION: ----------------------------------------------------------------

# 1. COMPLEX SOLUTION: --------------------------------------
path6.1 <- minimize(TTP6, 
                  details = TRUE, method = "QMC", row.dom = T)
path6.1
path6.1$PIchart
path6.1$SA

# Plot 1: 
XYplot(path6.1$solution[[1]], CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Lower perceived corruption", jitter = F, 
       enhance = T)

#saving the output:
stargazerSol(path6.1,
             outcome = "CPI_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.2.3",
             out = "hp2.3_pos.html",
             digits = 3)


# COMPLEX-NEGATIVE:
path6.2 <- minimize(TTN6, 
                     details = TRUE, method = "QMC", row.dom = T)
path6.2
path6.2$PIchart
path6.2$SA

# Plot 1: 
XYplot(path6.2$solution[[1]], CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Higher perceived corruption", jitter = F, 
       enhance = T)

#saving the output:
stargazerSol(path6.2,
             outcome = "X_CPI_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.2.3",
             out = "hp2.3_neg.html",
             digits = 3)

