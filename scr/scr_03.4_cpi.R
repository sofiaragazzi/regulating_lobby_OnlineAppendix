############################################     
##                SCRIPT 3.4              ##
##              MODEL 4: R = CPI          ##
############################################

# Source setup scripts:
source(here::here("Desktop", "TGL", "research", "scr","scr_00.R"))

# Load data:
load("data/lobby.Rda")

### ASSESS THE CLAIM OF INDIVIDUAL NECESSITY --------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019",
       relation = "nec") 

# with negative outcome
pofind(lobby, outcome = "~CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
       relation = "nec")

### ASSESS THE CLAIM OF JOINT SUFFICIENCY-------------------------------------------------------

# with positive outcome
pofind(lobby, outcome = "CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
       relation = "suff")

# with negative outcome
pofind(lobby, outcome = "~CPI_Z", 
       conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
       relation = "suff")

### The TRUTH TABLE: ----------------------------------------------

# positive truth table for model 4:
TTP4 <- truthTable(lobby, outcome = "CPI_Z",
                  conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019",
                  incl.cut = 0.85, n.cut = 1, complete = F, use.letters = T,
                  show.cases = TRUE, dcc = T, sort.by = "incl, n")

TTP4

#saving the output:
stargazerTT(TTP4,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.2.2 - Positive outcome",
            out = "ttp4.html",
            digits = 3)


# negative truth table for model 4:
lobby$X_CPI_Z <- 1 - lobby$CPI_Z

TTN4 <- truthTable(lobby, outcome = "X_CPI_Z", 
                  conditions = "LAW, REGISTER, REG_OVER, CODE, DEF_LOB, DEF_LOB_ACT, TARGET,
                      COOLING, DECL, MEETING, SANCTION_LIM, SANCTION_TRI, DISTANCE2019", 
                  incl.cut = 0.85, n.cut = 1, complete = FALSE, use.letters = T,
                  show.cases = TRUE, dcc = TRUE, sort.by = "incl, n")
TTN4

#saving the output:
stargazerTT(TTN4,
            show.cases = TRUE,
            type = "html", 
            title = "Truth table - Hp.2.2 - Negative outcome",
            out = "ttn4.html",
            digits = 3)

### MINIMISATION: ----------------------------------------------------------------

# 1. COMPLEX SOLUTION: --------------------------------------
path4.1 <- minimize(TTP4, 
                  details = TRUE, method = "QMC", row.dom = T)
path4.1
path4.1$PIchart
path4.1$SA

# Plot 1: 
XYplot(path4.1$solution[[1]], CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Lower perceived corruption", jitter = F,
       enhance = T)


#saving the output:
stargazerSol(path4.1,
             outcome = "CPI_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.2.2",
             out = "hp2.2_pos.html",
             digits = 3)


# COMPLEX-NEGATIVE:
path4.2 <- minimize(TTN4, 
                     details = TRUE, method = "QMC", row.dom = T)
path4.2
path4.2$PIchart
path4.2$SA

# Plot 1: 
XYplot(path4.2$solution[[1]], CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Higher perceived corruption", jitter = F,
       enhance = T)

#saving the output:
stargazerSol(path4.2,
             outcome = "X_CPI_Z",
             sol = 1,
             show.cases = TRUE,
             type = "html", 
             title = "Conservative Solution - Hp.2.2",
             out = "hp2.2_neg.html",
             digits = 3)


