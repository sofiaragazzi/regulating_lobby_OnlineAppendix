#######################################################              
##                   DILATED                         ##
##      ROBUSTNESS TEST: FUZZY TRANSFORMATION        ##
#######################################################


# Source setup scripts:
source(here::here("Desktop", "TGL", "research", "scr","scr_00.R"))

# Load data:
datasetd <- read.csv("data/fuzzy_trans/datasetd.csv", row.names = 1)


### The TRUTH TABLE: 

# positive truth table for model 1:
D_TTP1 <- truthTable(datasetd, outcome = "TRUST_Z.D",
                   conditions = "LAW.D, REGISTER.D, REG_OVER.D, CODE.D, DEF_LOB.D, DEF_LOB_ACT.D, TARGET.D, COOLING.D,     
                                 DECL.D, MEETING.D, SANCTION_LIM.D, SANCTION_TRI.D, DISTANCE2019.D, JUD_Z.D, ADM_Z.D, 
                                 TRADE_Z.D, BUDGET_Z.D, E_CIT_Z.D, PRESS_Z.D",
                   incl.Dut = 0.85, n.Dut = 1, complete = F, use.letters = F,
                   show.Dases = TRUE, dcc = T, sort.by = "incl, n")

D_TTP1

# negative truth table for model 1: 
datasetd$X_TRUST_Z.D <- 1 - datasetd$TRUST_Z.D

D_TTN1 <- truthTable(datasetd, outcome = "X_TRUST_Z.D", 
                   conditions = "LAW.D, REGISTER.D, REG_OVER.D, CODE.D, DEF_LOB.D, DEF_LOB_ACT.D, TARGET.D, COOLING.D,     
                                 DECL.D, MEETING.D, SANCTION_LIM.D, SANCTION_TRI.D, DISTANCE2019.D, JUD_Z.D, ADM_Z.D, 
                                 TRADE_Z.D, BUDGET_Z.D, E_CIT_Z.D, PRESS_Z.D", 
                   incl.Dut = 0.85, n.Dut = 1, complete = FALSE, use.letters = F,
                   show.Dases = TRUE, dcc = TRUE, sort.by = "incl, n")
D_TTN1

# COMPLEX SOLUTION: 
D_path1.1 <- minimize(D_TTP1, 
                    details = TRUE, method = "QMC", row.dom = T)
D_path1.1

# Plot 1: 
XYplot(D_path1.1$solution[[1]], TRUST_Z.D, datasetd, 
       clabels = row.names(datasetd), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Trust in government - dilation bias", jitter = F, amount = 0.04, 
       enhance = T)


# COMPLEX-NEGATIVE:
D_path1.2 <- minimize(D_TTN1, 
                    details = TRUE, method = "QMC", row.dom = T)
D_path1.2

# Plot 1: 
XYplot(D_path1.2$solution[[1]], X_TRUST_Z.D, datasetd, 
       clabels = row.names(datasetd), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Distrust in government", jitter = F, amount = 0.04)

