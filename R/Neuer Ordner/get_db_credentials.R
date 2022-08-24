# ============================================================================================ #
#                                                                                              #
#  The function described below enables the SYSTEM to retrieve the credentials to access       #
#  the required database(s) - The final scenario will probabaly involve a single database      #
#  (Risk_DB) - but currently 2 databases are envisaged (Replica_DB)                            #
#                                                                                              #
#                                                                                              #
#                                                                                              #
#     Input Parameters:                                                                        #
#     ----------------                                                                         #
#                                                                                              #
#      (1) path_meta            : directory where db-credentials are saved                     #
#                                                                                              #
#                                                                                              #
#                                                                                              #
#      Outputs :                                                                               #
#      --------                                                                                #
#                                                                                              #
#                                                                                              #
#      (1) _META_          : data frame containig the credentials for db access                #
#                                                                                              #
#                                                                                              #
#                             for each single order (current customer)                         #
#                                                                                              #
#      (3) _SCORE_         : DB_Schema containing calculated score values for each single      #
#                              order (current customer)                                        #
#                                                                                              #
# ============================================================================================ #



# DEFINING PARAMETER VALUES FOR STEP BY STEP EXECUTION 
# rooty <- getwd()

# START FUNCTION
get_db_credentials <- function(path_meta){
  
  
#======================
# (0) PREPARATION STEPS ####
#======================  
  
  
# --------------------------------------  
# (01) CLEARING ENVIROMNMENT AND CONSOLE ####
# --------------------------------------
  
# rm(list = ls(all.names = TRUE)); # activate the step before execution !!!!!!
cat("\f");   
  
# Getting path of the root directory 
# root <- getwd()
  

  
# ------------------------------------------------------ 
# (02) LOADING FILE CONTAINING CREDENTIALS FOR DB-ACCESS ####
# ------------------------------------------------------



# constructing the path rooting to the location where db credentials are saved
db_cred_loc  <- paste(path_meta, "/db_credentials.xlsx", sep="")
  
# loading required file
db_credentials <- read_excel(db_cred_loc)


# returning the result file containing the db credentials
return(db_credentials)


#============================================
# (1) SENDING EMAIL NOTIFICATION TO JOB OWNER ####
#============================================


# To be implemented later (TM: 30.01.2017)
# The content of the console should  


}
# END FUNCTION


# # DEFINING PARAMETER VALUES FOR MANUAL EXECUTION
# rooty <- getwd()
# 
# # RECORDING START TIME ####
# start.time <- Sys.time()
# #
# credentialsy <- get_db_credentials(root=rooty)
# 
# # RECORDING FINISH TIME ####
# end.time <<- Sys.time()
# 
# # CALCULATING TOTAL EXECUTION TIME ####
# exec.time  <- end.time - start.time
# print(exec.time)
# rm(list=setdiff(ls(), c("credentialsy")))


