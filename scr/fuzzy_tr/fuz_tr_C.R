#######################################################              
##                CONCENTRATED                       ##
##      ROBUSTNESS TEST: FUZZY TRANSFORMATION        ##
#######################################################


# Source setup scripts:
source(here::here("Desktop", "TGL", "research", "scr","scr_00.R"))

# Load data:
datasetc <- read.csv("data/fuzzy_trans/datasetc.csv", row.names = 1)


### The TRUTH TABLE: 

# positive truth table:
C_TTP1 <- truthTable(datasetc, outcome = "TRUST_Z.C",
                   conditions = "LAW.C, REGISTER.C, REG_OVER.C, CODE.C, DEF_LOB.C, DEF_LOB_ACT.C, TARGET.C, COOLING.C,     
                                 DECL.C, MEETING.C, SANCTION_LIM.C, SANCTION_TRI.C, DISTANCE2019.C, JUD_Z.C, ADM_Z.C, 
                                 TRADE_Z.C, BUDGET_Z.C, E_CIT_Z.C, PRESS_Z.C",
                   incl.cut = 0.85, n.cut = 1, complete = F, use.letters = T,
                   show.cases = TRUE, dcc = T, sort.by = "incl, n")

C_TTP1

# negative truth table: 
datasetc$X_TRUST_Z.C <- 1 - datasetc$TRUST_Z.C

C_TTN1 <- truthTable(datasetc, outcome = "X_TRUST_Z.C", 
                   conditions = "LAW.C, REGISTER.C, REG_OVER.C, CODE.C, DEF_LOB.C, DEF_LOB_ACT.C, TARGET.C, COOLING.C,     
                                DECL.C, MEETING.C, SANCTION_LIM.C, SANCTION_TRI.C, DISTANCE2019.C, JUD_Z.C, ADM_Z.C, 
                                TRADE_Z.C, BUDGET_Z.C, E_CIT_Z.C, PRESS_Z.C", 
                   incl.cut = 0.85, n.cut = 1, complete = FALSE, use.letters = T,
                   show.cases = TRUE, dcc = TRUE, sort.by = "incl, n")
C_TTN1

# COMPLEX SOLUTION: 
C_path1.1 <- minimize(C_TTP1, 
                    details = TRUE, method = "QMC", row.dom = T)
C_path1.1

# Plot 1: 
XYplot(C_path1.1$solution[[1]], TRUST_Z.C, datasetc, 
       clabels = row.names(datasetc), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Trust in government - concentration bias", jitter = F, amount = 0.04, 
       enhance = T)


# COMPLEX-NEGATIVE:
C_path1.2 <- minimize(C_TTN1, 
                    details = TRUE, method = "QMC", row.dom = T)
C_path1.2

# Plot 1: 
XYplot(C_path1.2$solution[[1]], X_TRUST_Z.C, datasetc, 
       clabels = row.names(datasetc), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Distrust in government", jitter = F, amount = 0.04)

