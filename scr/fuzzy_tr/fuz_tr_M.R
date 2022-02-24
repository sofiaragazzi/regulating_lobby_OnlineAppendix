#######################################################              
##                  MODERATED                        ##
##      ROBUSTNESS TEST: FUZZY TRANSFORMATION        ##
#######################################################


# Source setup scripts:
source(here::here("scr","scr_00.R"))

# Load data:
datasetm <- read.csv("data/fuzzy_trans/datasetm.csv", row.names = 1)

### The TRUTH TABLE: 

# positive truth table for model 1:
M_TTP1 <- truthTable(datasetm, outcome = "TRUST_Z.M",
                   conditions = "LAW.M, REGISTER.M, REG_OVER.M, CODE.M, DEF_LOB.M, DEF_LOB_ACT.M, TARGET.M, COOLING.M,     
                                 DECL.M, MEETING.M, SANCTION_LIM.M, SANCTION_TRI.M, DISTANCE2019.M, JUD_Z.M, ADM_Z.M, 
                                 TRADE_Z.M, BUDGET_Z.M, E_CIT_Z.M, PRESS_Z.M",
                   incl.Mut = 0.85, n.Mut = 1, complete = F, use.letters = F,
                   show.Mases = TRUE, dcc = T, sort.by = "incl, n")

M_TTP1

# negative truth table for model 1: 
datasetm$X_TRUST_Z.M <- 1 - datasetm$TRUST_Z.M

M_TTN1 <- truthTable(datasetm, outcome = "X_TRUST_Z.M", 
                   conditions = "LAW.M, REGISTER.M, REG_OVER.M, CODE.M, DEF_LOB.M, DEF_LOB_ACT.M, TARGET.M, COOLING.M,     
                                 DECL.M, MEETING.M, SANCTION_LIM.M, SANCTION_TRI.M, DISTANCE2019.M, JUD_Z.M, ADM_Z.M, 
                                 TRADE_Z.M, BUDGET_Z.M, E_CIT_Z.M, PRESS_Z.M", 
                   incl.Mut = 0.85, n.Mut = 1, complete = FALSE, use.letters = F,
                   show.Mases = TRUE, dcc = TRUE, sort.by = "incl, n")
M_TTN1

# COMPLEX SOLUTION: 
M_path1.1 <- minimize(M_TTP1, 
                    details = TRUE, method = "QMC", row.Mom = T)
M_path1.1

# Plot 1: 
XYplot(M_path1.1$solution[[1]], TRUST_Z.M, datasetm, 
       clabels = row.names(datasetm), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Trust in government - moderation bias", jitter = F, amount = 0.04,
       enhance = T)

# COMPLEX-NEGATIVE:
M_path1.2 <- minimize(M_TTN1, 
                    details = TRUE, method = "QMC", row.Mom = T)
M_path1.2

# Plot 1: 
XYplot(M_path1.2$solution[[1]], X_TRUST_Z.M, datasetm, 
       clabels = row.names(datasetm), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Distrust in government", jitter = F, amount = 0.04)

