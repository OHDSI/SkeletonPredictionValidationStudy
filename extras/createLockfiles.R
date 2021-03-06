# code to create lockfiles

##devtools::install_github('ohdsi/OhdsiRTools')

OhdsiRTools::createRenvLockFile(rootPackage = "SkeletonPredictionValidationStudy",
                                includeRootPackage = FALSE,
                                additionalRequiredPackages = c( "keras", "tensorflow", "plyr", "survAUC", "officer",
                                                               "diagram", "xgboost", "DT", "shiny", "shinydashboard", "shinycssloaders",
                                                               "DT", "htmlwidgets", "shinyWidgets", "plotly"))

args <- c('env', 'export','-n','r-reticulate', '--no-builds',
          '|', 'findstr', '-v', '"prefix"' ,'> pyEnvironment.yml')
system2(reticulate::conda_binary(), args, stdout = TRUE)



