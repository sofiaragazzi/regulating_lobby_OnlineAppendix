#######################################################              
##                 INTESIFIED                        ##
##      ROBUSTNESS TEST: FUZZY TRANSFORMATION        ##
#######################################################


# Source setup scripts:
source(here::here("scr","scr_00.R"))

# Load data:
dataseti <- read.csv("data/fuzzy_trans/dataseti.csv", row.names = 1)


### The TRUTH TABLE: 

# positive truth table:
I_TTP1 <- truthTable(dataseti, outcome = "TRUST_Z.I",
                   conditions = "LAW.I, REGISTER.I, REG_OVER.I, CODE.I, DEF_LOB.I, DEF_LOB_ACT.I, TARGET.I, COOLING.I,     
                                 DECL.I, MEETING.I, SANCTION_LIM.I, SANCTION_TRI.I, DISTANCE2019.I, JUD_Z.I, ADM_Z.I, 
                                 TRADE_Z.I, BUDGET_Z.I, E_CIT_Z.I, PRESS_Z.I",
                   incl.Iut = 0.85, n.Iut = 1, complete = F, use.letters = F,
                   show.Iases = TRUE, dcc = T, sort.by = "incl, n")

I_TTP1

# negative truth table: 
dataseti$X_TRUST_Z.I <- 1 - dataseti$TRUST_Z.I

I_TTN1 <- truthTable(dataseti, outcome = "X_TRUST_Z.I", 
                   conditions = "LAW.I, REGISTER.I, REG_OVER.I, CODE.I, DEF_LOB.I, DEF_LOB_ACT.I, TARGET.I, COOLING.I,     
                                 DECL.I, MEETING.I, SANCTION_LIM.I, SANCTION_TRI.I, DISTANCE2019.I, JUD_Z.I, ADM_Z.I, 
                                 TRADE_Z.I, BUDGET_Z.I, E_CIT_Z.I, PRESS_Z.I", 
                   incl.Iut = 0.85, n.Iut = 1, complete = FALSE, use.letters = F,
                   show.Iases = TRUE, dcc = TRUE, sort.by = "incl, n")
I_TTN1

# COMPLEX SOLUTION: 
I_path1.1 <- minimize(I_TTP1, 
                    details = TRUE, method = "QMC", row.Iom = T)
I_path1.1

# Plot 1: 
XYplot(I_path1.1$solution[[1]], TRUST_Z.I, dataseti, 
       clabels = row.names(dataseti), cex = 0.7, col = "steelblue",
       xlab = "positive conservative", ylab = "Trust in government - intensification bias", jitter = F, amount = 0.04,
       enhance = T)


# COMPLEX-NEGATIVE:
I_path1.2 <- minimize(I_TTN1, 
                    details = TRUE, method = "QMC", row.Iom = T)
I_path1.2

# Plot 1: 
XYplot(I_path1.2$solution[[1]], X_TRUST_Z.I, dataseti, 
       clabels = row.names(dataseti), cex = 0.7, col = "steelblue",
       xlab = "negative conservative", ylab = "Distrust in government", jitter = F, amount = 0.04)


