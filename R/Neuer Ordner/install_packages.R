# ============================================================================================ #
#                                                                                              #
#  The function described below performs the installation of all required R packages  for      #
#  all relevant Tasks related to the Courseware Data Analaytics Project:                       #
#                                                                                              #
#                                                                                              #
#                                                                                              #
#     Input Parameters:                                                                        #
#     ----------------                                                                         #
#                                                                                              #
#        (1) prj_root : main directory containing objects required for data analytics          #
#                   Tasks (metadata, functions, ...)                                           #
#                                                                                              #
#                                                                                              #
#        (2) repo_location : main repositiory from where the packages to be installed          #
#                   will extracted                                                             #
#                                                                                              #
#                                                                                              #
#        (3) lib_location : directory where the packages will be installed                     #
#                                                                                              #
#                                                                                              #
#                                                                                              #
#                                                                                              #
#                                                                                              #
#     Output Parameters:                                                                       #
#     -----------------                                                                        #
#                                                                                              #
#                                                                                              #
#      NO Outpout -  Required packages will be installed locally on our PC                     #
#                                                                                              #
#                                                                                              #
# ============================================================================================ #





# Start of the function
install_packages <- function(prj_root, lib_location, repo_location){

  
  
#======================
# (0) PREPARATION STEPS ####
#======================  
  
  
# ---------------------------------------  
# (01) PREPARING ENVIROMNMENT AND CONSOLE ####
# ---------------------------------------  
  
# Clearing the console content 
cat("\f")
  
# Getting path of the directory containinf required meta information 
path.meta <- paste(prj_root, "/", "meta", sep="")
  
  

# -------------------------------------------------  
# (02) LOADING PACKAGES LIST FROM A LOCAL DIRECTORY ####
# -------------------------------------------------

# Getting the name of the location containing the list of packages to be installed 
file_location <- paste(path.meta, "/packages_list.txt", sep="")

# Loading the list of packages to be installed
pckgs_required <- read.table(file_location, header=TRUE, stringsAsFactors=FALSE)

# Extracting vector containing names of the packages
pckgs_required_names <- pckgs_required$PackageName


# --------------------------------------------------  
# (03) IDENTIFYING DEPENDENCIES OF REQURIED PACKAGES ####
# --------------------------------------------------

# Getting the list of all dependencies for the required packages
library(packrat)
pckgs_dependencies <- packrat:::recursivePackageDependencies(pckgs_required_names,lib.loc = repo_location)

# Adding dependencies to the list of required packages
pckgs_required_names <- unique(c(pckgs_required_names, pckgs_dependencies))


# ---------------------------------------------------------------------  
# (04) IDENTIFYING PACKAGES ALREADY INSTALLED IN THE TARGET ENVIRONMENT ####
# ---------------------------------------------------------------------

# Getting the lost of packages already installed on the system
installed_packages <- as.data.frame(installed.packages())

# Getting list of required packages not already installed (needed)
packages.needed <- as.vector(setdiff(pckgs_required_names, installed_packages$Package))
length.pckg <- length(packages.needed)



#======================================
# (1) INSTALLING THE REQUIRED PACKAGES ####
#======================================  


if (length.pckg > 0){
  
  # Looping among elements of the package list vector
  for (k in 1:length.pckg){
    
    # Print package name
    pkg <- packages.needed[k]
   
    # Print loop status
    print(paste(" Installation of package : '",  pkg, "' currently running", sep=""))
    print(paste(round((k/length.pckg)*100, digits = 2), "% of packages installed", sep=""))

    
    # Install the package in the required libray
    install.packages(pkg, lib= lib_location, repos=repo_location)
    }    
}else{
  print(paste("*** Execution stopped: all required packages already installed in the current environment ***", sep = ""))
}



}
# End of the function


# # Defining parmater values for step by step execution
# prj_root      <- getwd()
# lib_location  <- .libPaths()[1] # repo location within the working environment "c/"
# repo_location <-  "https://cloud.r-project.org" # Hans baut f?r uns eine cran-replikation
# 
# 
# # Installing required packages
# packages_installation(prj_root,lib_location, repo_location)
# 
# 
# #Check if everything has worked correctly
# 
# # Getting the lost of packages already installed on the system
# installed_packages_2 <- as.data.frame(installed.packages()) # top 437 now
