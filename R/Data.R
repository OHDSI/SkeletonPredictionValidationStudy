#' Data Class
#' @description
#' Data description goes here.
#' @export
Data <- R6::R6Class(
  "Data",
  public = list(

    #' @field connectionDetails (`[DatabaseConnector][DatabaseConnector::connectionDetails]`)\cr
    #' Connection details.
    connectionDetails = NULL,

    #' @field cdmDatabaseSchema (`character(1)`)\cr
    #' CDM database schema name.
    cdmDatabaseSchema = NULL,

    #' @field cohortDatabaseSchema (`character(1)`)\cr
    #' Database schema with write access to create scratch tables.
    cohortDatabaseSchema = NULL,

    #' @field cohortTable (`character(1)`)\cr
    #' Table name where the target cohorts will be generated.
    cohortTable = NULL,

    #' @field outcomeDatabaseSchema (`character(1)`)\cr
    #' Outcome database schema with write access to create scratch tables.
    outcomeDatabaseSchema = NULL,

    #' @field outcomeTable (`character(1)`)\cr
    #' Table name where the outcome cohorts will be generated.
    outcomeTable = NULL,

    #' @field oracleTempSchema (`character(1)`)\cr
    #' Oracle temp schema.
    oracleTempSchema = NULL,

    #' @field sampleSize (`numeric(1)`)\cr
    #' Sample size for the cohort query.
    sampleSize = NULL,

    #' @field cdmVersion (`numeric(1)`)\cr
    #' CDM version number of the database.
    cdmVersion = NULL,

    # covariateSettings = NULL,
    # cohortId = analysisSettings$cohortId[i],
    # outcomeIds = analysisSettings$outcomeId[i],
    # firstExposureOnly = populationSettings$firstExposureOnly,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
    },

    #' @description
    #' Creates the cohorts in the database
    #' @template param_connectionDetails
    #' @template param_cdmDatabaseSchema
    #' @template param_cohortDatabaseSchema
    #' @template param_cohortTable
    #' @template param_oracleTempSchema
    #' @template param_outputFolder
    createCohorts = function(connectionDetails,
                             cdmDatabaseSchema,
                             cohortDatabaseSchema,
                             cohortTable = "cohort",
                             oracleTempSchema,
                             outputFolder) {
      if (!file.exists(outputFolder))
        dir.create(outputFolder)

      conn <- DatabaseConnector::connect(connectionDetails)

      private$.createCohorts(connection = conn,
                             cdmDatabaseSchema = cdmDatabaseSchema,
                             cohortDatabaseSchema = cohortDatabaseSchema,
                             cohortTable = cohortTable,
                             oracleTempSchema = oracleTempSchema,
                             outputFolder = outputFolder)

      # Check number of subjects per cohort:
      ParallelLogger::logInfo("Counting cohorts")
      sql <- SqlRender::loadRenderTranslateSql("GetCounts.sql",
                                               "SkeletonPredictionValidationStudy",
                                               dbms = connectionDetails$dbms,
                                               oracleTempSchema = oracleTempSchema,
                                               cdm_database_schema = cdmDatabaseSchema,
                                               work_database_schema = cohortDatabaseSchema,
                                               study_cohort_table = cohortTable)
      counts <- DatabaseConnector::querySql(conn, sql)
      colnames(counts) <- SqlRender::snakeCaseToCamelCase(colnames(counts))
      counts <- private$addCohortNames(counts)
      utils::write.csv(counts, file.path(outputFolder, "CohortCounts.csv"), row.names = FALSE)

      DatabaseConnector::disconnect(conn)
    }
  ),
  private = list(

    addCohortNames = function(data, IdColumnName = "cohortDefinitionId", nameColumnName = "cohortName") {
      pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "SkeletonPredictionValidationStudy")
      cohortsToCreate <- utils::read.csv(pathToCsv)

      idToName <- data.frame(cohortId = c(cohortsToCreate$cohortId),
                             cohortName = c(as.character(cohortsToCreate$name)))
      idToName <- idToName[order(idToName$cohortId), ]
      idToName <- idToName[!duplicated(idToName$cohortId), ]
      names(idToName)[1] <- IdColumnName
      names(idToName)[2] <- nameColumnName
      data <- merge(data, idToName, all.x = TRUE)
      # Change order of columns:
      idCol <- which(colnames(data) == IdColumnName)
      if (idCol < ncol(data) - 1) {
        data <- data[, c(1:idCol, ncol(data) , (idCol+1):(ncol(data)-1))]
      }
      return(data)
    },

    .createCohorts = function(connection,
                               cdmDatabaseSchema,
                               vocabularyDatabaseSchema = cdmDatabaseSchema,
                               cohortDatabaseSchema,
                               cohortTable,
                               oracleTempSchema,
                               outputFolder) {

      # Create study cohort table structure:
      sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohortTable.sql",
                                               packageName = "SkeletonPredictionValidationStudy",
                                               dbms = attr(connection, "dbms"),
                                               oracleTempSchema = oracleTempSchema,
                                               cohort_database_schema = cohortDatabaseSchema,
                                               cohort_table = cohortTable)
      DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)

      # Instantiate cohorts:
      pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "SkeletonPredictionValidationStudy")
      cohortsToCreate <- utils::read.csv(pathToCsv)
      for (i in 1:nrow(cohortsToCreate)) {
        writeLines(paste("Creating cohort:", cohortsToCreate$name[i]))
        sql <- SqlRender::loadRenderTranslateSql(sqlFilename = paste0(cohortsToCreate$name[i], ".sql"),
                                                 packageName = "SkeletonPredictionValidationStudy",
                                                 dbms = attr(connection, "dbms"),
                                                 oracleTempSchema = oracleTempSchema,
                                                 cdm_database_schema = cdmDatabaseSchema,
                                                 vocabulary_database_schema = vocabularyDatabaseSchema,

                                                 target_database_schema = cohortDatabaseSchema,
                                                 target_cohort_table = cohortTable,
                                                 target_cohort_id = cohortsToCreate$cohortId[i])
        DatabaseConnector::executeSql(connection, sql)
      }

      pathToCustom <- system.file("settings", 'cohortVariableSetting.csv', package = "SkeletonPredictionValidationStudy")
      if(pathToCustom!=""){
        # if custom cohort covaraites set:
        cohortVarsToCreate <- utils::read.csv(pathToCustom)

        if(sum(colnames(cohortVarsToCreate)%in%c('atlasId', 'cohortName', 'startDay', 'endDay'))!=4){
          stop('Issue with cohortVariableSetting - make sure it is NULL or a setting')
        }

        cohortVarsToCreate <- unique(cohortVarsToCreate[,c('atlasId', 'cohortName')])
        for (i in 1:nrow(cohortVarsToCreate)) {
          writeLines(paste("Creating cohort:", cohortVarsToCreate$cohortName[i]))
          sql <- SqlRender::loadRenderTranslateSql(sqlFilename = paste0(cohortVarsToCreate$cohortName[i], ".sql"),
                                                   packageName = "SkeletonPredictionValidationStudy",
                                                   dbms = attr(connection, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database_schema = cdmDatabaseSchema,
                                                   vocabulary_database_schema = vocabularyDatabaseSchema,

                                                   target_database_schema = cohortDatabaseSchema,
                                                   target_cohort_table = cohortTable,
                                                   target_cohort_id = cohortVarsToCreate$atlasId[i])
          DatabaseConnector::executeSql(connection, sql)
        }
      }
    }
  )
)
