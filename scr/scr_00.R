############################################
##                SCRIPT 00               ##
## INSTALLING PACKAGES AND SETTING W.D.   ##
############################################


# Installing packages:
want <- c("haven", "tidyverse", "here", "readr", "QCA", "QCAtools", "SetMethods")  # list of required packages
have <- want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
rm(have, want)

# Loading packages: 
library(haven)
library(tidyverse)
library(here)
library(readr)
library(QCA)
library(QCAtools)
library(SetMethods)

      