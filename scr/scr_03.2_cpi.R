############################################    
##                SCRIPT 3.2              ##
##         MODEL 2: OUTCOME = CPI         ##
############################################


# Source setup scripts:
source(here::here("Desktop", "TGL", "research", "scr","scr_00.R"))

# Load data:
load("data/lobby.Rda")

### ASSESS THE CLAIM OF INDIVIDUAL NECESSITY --------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
       relation = "nec") 

# with negative outcome
pofind(lobby, outcome = "~CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "nec")

### ASSESS THE CLAIM OF JOINT SUFFICIENCY-------------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")

# with negative outcome
pofind(lobby, outcome = "~CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
       relation = "suff")


### The TRUTH TABLE: ----------------------------------------------

# positive truth table for model 2:
TTP2 <- truthTable(lobby, outcome = "CPI_Z",
                    conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z",
                    incl.cut = 0.85, n.cut = 1, complete = F, use.letters = T,
                    show.cases = TRUE, dcc = T, sort.by = "incl, n")

TTP2

#saving the output:
stargazerTT(TTP2,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.2 - Positive outcome",
            out = "ttp2.html",
            digits = 3)

# negative truth table for model 2:
lobby$X_CPI_Z<- 1 - lobby$CPI_Z

TTN2 <- truthTable(lobby, outcome = "X_CPI_Z", 
                    conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019, 
                      JUD_Z, ADM_Z, TRADE_Z, BUDGET_Z, E_CIT_Z, PRESS_Z", 
                    incl.cut = 0.85, n.cut = 1, complete = FALSE, use.letters = T,
                    show.cases = TRUE, dcc = TRUE, sort.by = "incl, n")
TTN2

#saving the output:
stargazerTT(TTN2,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.2 - Negative outcome",
            out = "ttn2.html",
            digits = 3)

### MINIMISATION: ----------------------------------------------------------------

# 1. COMPLEX SOLUTION: --------------------------------------
path2.1 <- minimize(TTP2, 
                  details = TRUE, method = "QMC", row.dom = T)
path2.1


# Plot 1: 
XYplot(path2.1$solution[[1]], CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Lower perceived corruption", jitter = F,
       enhance = T)

#saving the output:
stargazerSol(path2.1,
             outcome = "CPI_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.2",
             out = "hp2_pos.html",
             digits = 3)

# COMPLEX-NEGATIVE:
path2.2 <- minimize(TTN2, 
                     details = TRUE, method = "QMC", row.dom = T)
path2.2

# Plot 2: 
XYplot(path2.2$solution[[1]], CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Higher perceived corruption", jitter = F, 
       enhance = T)

#saving the output:
stargazerSol(path2.2,
             outcome = "X_CPI_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.2",
             out = "hp2_neg.html",
             digits = 3)


# plotting the two XYplot: 
par(mfrow=c(1,2))
# Plot 1: 
XYplot(path2.1$solution[[1]], CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Lower perceived corruption", jitter = F, amount = 0.04,
       enhance = T)
# Plot 2: 
XYplot(path2.2$solution[[1]], CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Higher perceived corruption", jitter = F, amount = 0.04, 
       enhance = T)

