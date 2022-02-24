#######################################################     
##                 SCRIPT 01                         ##
##      ROBUSTNESS TEST: FUZZY TRANSFORMATION        ##
#######################################################


# Source setup scripts:
source(here::here("scr","scr_00.R"))

# Load data:
load("data/lobby.Rda")


### transform datasets to mimic the different coder's biases

# METHOD 1:
##### .c for concentrated ##### SQUARED --------------------------------------

dataset.c <- lobby^2
colnames(dataset.c)<- c("LAW.C","REGISTER.C","REG_OVER.C","CODE.C","DEF_LOB.C","DEF_LOB_ACT.C","TARGET.C","COOLING.C",     
                         "DECL.C","MEETING.C","SANCTION_LIM.C", "SANCTION_TRI.C", "DISTANCE2019.C", "JUD_Z.C","ADM_Z.C","TRADE_Z.C",     
                         "BUDGET_Z.C","E_CIT_Z.C","PRESS_Z.C", "CPI_Z.C", "MEAN_CPI_Z.C", "TRUST_Z.C", "MEAN_TRUST_Z.C")
dataset.c <- round(dataset.c, digits = 2)

write.csv(dataset.c, file = "datasetc.csv", row.names = T)


# METHOD 2
##### .d for dilated ##### SQUARE ROOT ----------------------------------

dataset.d <- sqrt(lobby)
colnames(dataset.d)<- c("LAW.D","REGISTER.D","REG_OVER.D","CODE.D","DEF_LOB.D","DEF_LOB_ACT.D","TARGET.D","COOLING.D",     
                        "DECL.D","MEETING.D","SANCTION_LIM.D", "SANCTION_TRI.D", "DISTANCE2019.D", "JUD_Z.D","ADM_Z.D","TRADE_Z.D",     
                        "BUDGET_Z.D","E_CIT_Z.D","PRESS_Z.D","CPI_Z.D", "MEAN_CPI_Z.D", "TRUST_Z.D", "MEAN_TRUST_Z.D")
dataset.d <- round(dataset.d, digits = 2)

write.csv(dataset.d, file = "datasetd.csv", row.names = T)


# METHOD 3
##### .i for intensified ##### BOTH SQUARED AND SQUARE ROOT -------------------

fsInt <- function(x) {
  if_else(x<0.5, 
          x^2, 
          sqrt(x))
}

# assuming the dataset has J columns

dataset.i <- data.frame(lapply(lobby[1:23], fsInt))
dataset.i <- round(dataset.i, digits = 2)
rownames(dataset.i) <- rownames(lobby)

# assuming the colnames in dataset end with .Z 

colnames(dataset.i) <- gsub(x = colnames(lobby), 
                            pattern = "$", replacement = ".I")

write.csv(dataset.i, file = "dataseti.csv", row.names = T)


# METHOD 4
##### .m for moderated ##### BOTH SQUARED AND SQUARE ROOT, inverted -------------------------

fsMod <- function(x) {
  if_else(x<0.5, 
          sqrt(x), 
          x^2)
}

# assuming the dataset has J columns

dataset.m <- data.frame(lapply(lobby[1:23], fsMod))
dataset.m <- round(dataset.m, digits = 2)
rownames(dataset.m) <- rownames(lobby)

# assuming the colnames in dataset end with .Z 

colnames(dataset.m) <- gsub(x = colnames(lobby), 
                            pattern = "$", replacement = ".M")

write.csv(dataset.m, file = "datasetm.csv", row.names = T)

