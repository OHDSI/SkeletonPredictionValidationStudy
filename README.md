SkeletonPredictionValidationStudy
======================

Introduction
============
This package contains code to externally validate models for the prediction quesiton <add question> developed on the database <add database>.

Features
========
  - Applies models developed using the OHDSI PatientLevelPrediction package
  - Evaluates the performance of the models on new data
  - Packages up the results (after removing sensitive date) to share with study owner

Technology
==========
  SkeletonPredictionValidationStudy is an R package.

System Requirements
===================
  * Requires: OMOP CDM database and connection details
  * Requires: Java runtime enviroment (for the database connection)
  * Requires: R (version 3.3.0 or higher).
  * Sometimes required: Python 

Dependencies
============
  * PatientLevelPrediction
  
Guide
============
A general guide for running a valdiation study package is available here: [Skeleton Validation Study guide](https://github.com/OHDSI/SkeletonPredictionValidationStudy/tree/master/inst/doc/UsingSkeletonValidationPackage.pdf)
  
  
A1. Installing the package from GitHub
===============
```r
# first set up the environment using the lockfile:
# If you don't have renv as an R library you need to install it:
install.packages("renv")

# renv will create an environemnt with all the R libraries and versions that
# were used by the original study developer (this is handy if the study needs to be run 
# in the future when new versions are available and may have different code that 
# causes a study to break)

# You need to specify a project folder for the renv (the study specific environment will be 
# save here) and you need to set you R working direcory to this location before running renv
projectFolder <- "C:/SkeletonPredictionValidationStudy"
if(!dir.exists(projectFolder)){
dir.create(projectFolder,   recursive = T)
}
setwd(projectFolder)
                                                                                              
# Download the lock file:
download.file("https://raw.githubusercontent.com/ohdsi-studies/SkeletonPredictionValidationStudy/master/renv.lock", "renv.lock")

# Build the local library into projectFolder (takes a while):
renv::init()

# (When not in RStudio, you'll need to restart R now)

# To install the package from github:
install.packages("devtools")
devtools::install_github("ohdsi-studies/SkeletonPredictionValidationStudy")
```

A2. Building the package inside RStudio
===============
  1. Open the validation package project file (file ending in .Rproj) 
  2. Build the package in RStudio by selecting the 'Build' option in the top right (the tabs contain  'Environment', 'History', 'Connections', 'Build', 'Git') and then clicking on the 'Install and Restart'

B. Getting Started
===============
  1. Make sure to have either: installed (A1) or built (A2) the package 
  2. In R, run the code in 'extras/codeToRun.R' (see [Skeleton Validation Study guide](https://github.com/OHDSI/SkeletonPredictionValidationStudy/tree/master/inst/doc/UsingSkeletonValidationPackage.pdf) for guideance)


C. Example Code
===============
```r
library(SkeletonPredictionValidationStudy)

outputFolder <- './Validation'

# add the database connection details
dbms = 'your database management system'
server = 'your server'
user = 'your username'
password = 'top secret'
port = 'your port'
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# add cdm database details:
cdmDatabaseSchema <- 'your cdm database schema'

# add a schema you have read/write access to
# this is where the cohorts will be created (or are already created)
cohortDatabaseSchema <- 'your cohort database schema'

# if using oracle specify the temp schema
oracleTempSchema <- NULL

# Add a sharebale name for the database containing the OMOP CDM data
databaseName <- 'your database name'

# table name where the cohorts will be generated
cohortTable <- 'SkeletonPredictionValidationStudyCohort'

#===== execution choices =====

# how much details do you want for in progress report?
verbosity <- "INFO"

# create the cohorts using the sql in the package?
createCohorts = T

# apply the models in the package to your data?
runValidation = F
# if you only want to apply models to a sample of
# patients put the number as the sampleSize
sampleSize = NULL
# do you want to recalibrate results?
# NULL means none (see ?SkeletonPredictionValidationStudy::execute for options)
recalibrate <- NULL

# extract the results to share as a zip file?
packageResults = T
# when extracting results - what is the min cell count?
minCellCount = 5

#=============================
# configure the settings
databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cdmDatabaseName = databaseName,
  tempEmulationSchema = tempEmulationSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTable,
  cdmVersion = 5
)

restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
  sampleSize = sampleSize
)

validationSettings <- PatientLevelPrediction::createValidationSettings(
  recalibrate = recalibrate
)

logSettings <- PatientLevelPrediction::createLogSettings(
  verbosity = verbosity
)

#=============================
# Now run the study
SkeletonPredictionValidationStudy::execute(
  databaseDetails = databaseDetails,
  restrictPlpDataSettings = restrictPlpDataSettings,
  validationSettings = validationSettings,
  logSettings = logSettings,
  outputFolder = outputFolder,
  createCohorts = createCohorts,
  runValidation = runValidation,
  packageResults = packageResults,
  minCellCount = minCellCount
)
                 
```

License
=======
  SkeletonPredictionValidationStudy is licensed under Apache License 2.0

Development
===========
  SkeletonPredictionValidationStudy is being developed in R Studio.
