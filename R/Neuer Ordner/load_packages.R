# ============================================================================================ #
#                                                                                              #
#  The function described below performs the loading of all required R packages  for all       #
#  relevant activities:                                                                        #
#                                                                                              #
#                                                                                              #
#     Input Parameters:                                                                        #
#     ----------------                                                                         #
#                                                                                              #
#        (1) path_meta : directory containing meta information (db_credentials-xlsx)           #
#                                                                                              #
#        (1) lib_location : directory containing installed R-packages (check .liPaths)         #
#                                                                                              #
#                                                                                              #
#                                                                                              #
#     Output Parameters:                                                                       #
#     -----------------                                                                        #
#                                                                                              #
#                                                                                              #
#      ** No outpout required  - all required packages will be loaded                          #
#                                                                                              #
#                                                                                              #
# ============================================================================================ #


# # DEFINING PARAMETER VALUES FOR STEP BY STEP EXECUTION
# prj_root      <- getwd()
# lib_location  <- .libPaths()[1] # repo location within the working environment "c/"
# path_meta <- paste(prj_root, "/meta", sep="") 


# Start function
load_packages <- function(path_meta, lib_location){


    
#======================
# (0) PREPARATION STEPS ####
#======================  


# -------------------------------------------------  
# (02) LOADING PACKAGES LIST FROM A LOCAL DIRECTORY ####
# -------------------------------------------------
  
# Getting the name of the location containing the list of packages to be loaded 
file_location <- paste(path_meta, "/packages_list.txt", sep="")
  
# Loading the list of packages to be loaded
pckgs_required <- read.table(file_location, header=TRUE, stringsAsFactors=FALSE)
  
# Extracting vector containing names of the packages
pckgs_required_names <- pckgs_required$PackageName
  


#==============================
# (1) LOADING REQUIRED PACKAGES ####
#==============================  

# Getting the list of packages already loaded on the system
packages_loaded <- as.data.frame(.packages())
colnames(packages_loaded) <- "Package"

# Getting list of required packages not already loaded
packages_needed <- as.vector(setdiff(pckgs_required_names, packages_loaded$Package) )
length.pckg  <- length(packages_needed) 




#==============================
# (1) LOADING REQUIRED PACKAGES ####
#============================== 


if(length.pckg >  0){
  
  for (k in 1:length.pckg){
    
    # Print package name
    pkg <- packages_needed[k]
    
    # Print loop status
    print(paste(" Loading of package : '",  pkg, "' currently running", sep=""))
    print(paste(round((k/length.pckg)*100, digits = 2), "% of packages loaded", sep=""))
    
    # Loading the selected package
    form <- parse(text=paste("library('",pkg, "',  lib.loc='", lib_location, "')", sep=""))# add lib.loc as parameter for MZ-Environment
    eval(form)       
  }  
}else{
  print(paste("*** Execution stopped: all required packages already loaded in the current session ***", sep = ""))
}


} 
# End function


# # Manual execution
# prj_root <- getwd()
# lib_location  <- .libPaths()[1] # repo location within the working environment "c/"
# 
# 
# packages_load(prj_root, lib.location)
# 
# 
# # Check loaded packages
# packages_loaded_2 <- as.data.frame(.packages())
