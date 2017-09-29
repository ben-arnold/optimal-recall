

#--------------------------------------
# recall-empirical-tables.R
# Ben Arnold
# 4 Nov 2011
#
# Output results into summary tables
#--------------------------------------

library(xtable)
rm(list=ls())


#--------------------------------------
# Load base functions
#--------------------------------------
source("~/dropbox/articles/wsp-recall/programs/final/2-base-functions/empirical-base-functions-v2.R")


#--------------------------------------
# Output summary statistics to HTML
# Tables (.xls files)
#--------------------------------------

empirical.tables("diar","anemia")
empirical.tables("diar","haz")
empirical.tables("diar","waz")

empirical.tables("cough","anemia")
empirical.tables("cough","haz")
empirical.tables("cough","waz")

empirical.tables("fever","anemia")
empirical.tables("fever","haz")
empirical.tables("fever","waz")

