#ADDITIONAL MINIMIZATION SOLUTIONS

#from 03_1:

# PARSIMONIOUS SOLUTION:------------------------------------------

path2 <- minimize(TTP, include = "?", 
                  details = TRUE, method = "QMC")
path2
path2$solution[[1]]
path2$SA

# Plot 2:
XYplot(path2$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive parsimonious", ylab = "TRUST in government", jitter = T, amount = 0.04)


# INTERMEDIATE SOLUTION:-------------------

#POSITIVE
path3 <- minimize(TTP, include = "?", dir.exp = c(""),
                  details = TRUE, method = "CCubes", row.dom = T)
path3
path3$i.sol$C1P1$solution[[1]]
path3$PIchart

path3$solution

XYplot(path3$i.sol$C1P1$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "darkorange", 
       xlab = "positive intermediate", ylab = "TRUST")
# Incl: 0.956; Cov: 0.347; PRI: 0.935

#NEGATIVE
path3_neg <- minimize(TTN, include = "?", dir.exp = c(""),
                      details = TRUE, method = "CCubes", row.dom = T)

path3_neg$
  
  XYplot(path3_neg$i.sol$C1P1$solution[[1]], TRUST_Z, lobby, 
         clabels = row.names(lobby), cex = 0.7, col = "darkorange", 
         xlab = "negative intermediate", ylab = "TRUST")
# Incl: 1; Cov: 0.048; PRI: 1


# from 03.2
# PARSIMONIOUS SOLUTION:------------------------------------------

path2 <- minimize(TTP_c, include = "?", 
                  details = TRUE, method = "QMC")
path2
path2$solution[[1]]
path2$SA

# Plot 2:
XYplot(path2$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive parsimonious", ylab = "TRUST in government", jitter = T, amount = 0.04)


# INTERMEDIATE SOLUTION:-------------------

#POSITIVE
path3_c <- minimize(TTP_c, include = "?", dir.exp = c(""),
                    details = TRUE, method = "CCubes", row.dom = T)
path3
path3$i.sol$C1P1$solution[[1]]
path3$PIchart

XYplot(path3_c$i.sol$C1P1$solution[[1]], CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "darkorange", 
       xlab = "positive intermediate", ylab = "CPI")
# Incl: 0.969; Cov: 0.192; PRI: 0.963

#NEGATIVE
path3_c_neg <- minimize(TTN_c, include = "?", dir.exp = c(""),
                        details = TRUE, method = "CCubes", row.dom = T)

path3_c_neg

XYplot(path3_c_neg$i.sol$C1P1$solution[[1]], CPI_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "darkorange", 
       xlab = "negative intermediate", ylab = "CPI")
# Incl: 0.967; Cov: 0.068; PRI: 0.953

# from 03.3
# PARSIMONIOUS SOLUTION:------------------------------------------

path2 <- minimize(TTP, include = "?", 
                  details = TRUE, method = "QMC")
path2
path2$solution[[1]]
path2$SA

# Plot 2:
XYplot(path2$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive parsimonious", ylab = "TRUST in government", jitter = T, amount = 0.04)


# INTERMEDIATE SOLUTION:-------------------

path3 <- minimize(TTP, include = "?", dir.exp = "1,1,1,1,1,1,1,1,1,1,1",
                  details = TRUE, method = "CCubes", row.dom = T)
path3
path3$i.sol$C1P1$solution[[1]]

path3$PIchart

path4 <- minimize(TTN, include = "?", dir.exp = "1,1,1,1,1,1,1,1,1,1,1",
                  details = TRUE, method = "CCubes", row.dom = T)

path4

# Plot 3:
XYplot(path3$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive intermediate", ylab = "TRUST in government", jitter = T, amount = 0.04)

# from 3.4
# PARSIMONIOUS SOLUTION:------------------------------------------

path2 <- minimize(TTP, include = "?", 
                  details = TRUE, method = "QMC")
path2
path2$solution[[1]]
path2$SA

# Plot 2:
XYplot(path2$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive parsimonious", ylab = "TRUST in government", jitter = T, amount = 0.04)


# INTERMEDIATE SOLUTION:-------------------

path3 <- minimize(TTP, include = "?", dir.exp = "1,1,1,1,1,1,1,1,1,1,1",
                  details = TRUE, method = "CCubes", row.dom = T)
path3
path3$i.sol$C1P1$solution[[1]]

path3$PIchart

path4 <- minimize(TTN, include = "?", dir.exp = "1,1,1,1,1,1,1,1,1,1,1",
                  details = TRUE, method = "CCubes", row.dom = T)

path4

# Plot 3:
XYplot(path3$solution[[1]], TRUST_Z, lobby, 
       clabels = row.names(lobby), cex = 0.7, col = "steelblue",
       xlab = "positive intermediate", ylab = "TRUST in government", jitter = T, amount = 0.04)
