# clean environment
rm(list=ls(all.names=TRUE))

# set the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)

#------------
# directories 
#-----------
# input
dirinput <- paste0(thisdir, "/Data/")

# intermediate
dirintermediate <- paste0(thisdir, "/intermediate/")

# output
diroutput <- paste0(thisdir, "/output/")

#------------------------
# libraries and functions  
#------------------------
source(paste0(thisdir,"/00_parameters.R"))

#------
# Steps 
#------
### 1. Preprocessing
source(paste0(thisdir,"/01_preprocessing.R"))

### 2. Number of group selection
# 2.1 Stata: run model with 2-7 gruops
# 2.2 R: select best model
source(paste0(thisdir,"/02_2_n_groups_selection.R"))

### 3 Selection of polynomials
# 3.1 Stata: run model with 2-7 polynomials
# 3.2 R: select best model
source(paste0(thisdir,"/03_2_polynomials_selection.R"))

### 5 final modeal 
# 4 Stata: 

### 5 Results description
# 5.1 R: create tables
source(paste0(thisdir,"/05_01_tables.R"))
# 5.2 R: describe group membership
source(paste0(thisdir,"/05_02_group_membership_probability.R"))



### 6 Multinomial regression 
#6.2 R
source(paste0(thisdir,"/06_01_multinomial_reg.R"))

