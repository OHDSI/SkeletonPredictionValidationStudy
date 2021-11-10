#' Study Class
#' @description
#' Model description goes here.
#' @export
Study <- R6::R6Class(
  "Study",
  public = list(

    #' @field packageName (`character(1)`)\cr
    #' Package name.
    packageName = NULL,

    #' @field packageDescription (`character(1)`)\cr
    #' Package description.
    packageDescription = NULL,

    #' @field createdBy (`character(1)`)\cr
    #' Created by names.
    createdBy = NULL,

    #' @field organizationName (`character(1)`)\cr
    #' Organization name.
    organizationName = NULL,

    #' @field analysisSettings (`data.frame(1)`)\cr
    #' Analysis settings.
    analysisSettings = NULL,

    #' @field outputFolder (`character(1)`)\cr
    #' Output folder.
    outputFolder = NULL,

    #' @field databaseName (`character(1)`)\cr
    #' Database name.
    databaseName = NULL,

    #' @field model (`[Model]`)\cr
    #' Prediction models.
    model = NULL,

    #' @field data (`[Data]`)\cr
    #' Data handler.
    data = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @template param_studyPath
    initialize = function(databaseName, outputFolder, studyPath = NULL) {
      self$loadStudyFromJson(databaseName, outputFolder, studyPath)
    },

    #' @description
    #' Load study from JSON file.
    #' @template param_studyPath
    #' @template param_outputFolder
    #' @template param_databaseName
    loadStudyFromJson = function(databaseName, outputFolder, studyPath = NULL) {

      if (is.null(studyPath)) {
        studyPath <- system.file("settings/predictionAnalysisList.json",
                            package = "SkeletonPredictionValidationStudy")
      }
      paramTemp <- readChar(studyPath, file.info(studyPath)$size)
      jsonSettings <- RJSONIO::fromJSON(paramTemp)

      # assign Study variables
      self$packageName <- jsonSettings$packageName
      self$packageDescription <- jsonSettings$packageDescription
      self$createdBy <- jsonSettings$createdBy
      self$organizationName <- jsonSettings$organizationName
      self$analysisSettings <- private$getAnalyses(jsonSettings)

      # create output folder
      self$databaseName <- databaseName
      self$outputFolder <- outputFolder
      private$createOutputFolder()

      # instantiate Data handler
      self$data <- Data$new()

      # instantiate Models
      # self$model <- Model$new()
    },

    getAnalysisIter = function() {
      analysisIter <- iterators::iter(self$analysisSettings)
    }

  ),

  private = list(
    createOutputFolder = function() {
      assert_character(self$outputFolder)
      assert_character(self$databaseName)

      if (!file.exists(file.path(self$outputFolder, self$databaseName))){
        dir.create(file.path(self$outputFolder, self$databaseName), recursive = TRUE)
      }
      ParallelLogger::addDefaultFileLogger(file.path(self$outputFolder, self$databaseName, "log.txt"))
    },

    getAnalyses = function(jsonSettings){
      result <- data.frame(modelName = unlist(lapply(jsonSettings$models, function(x) x$name)),
                           modelType = unlist(lapply(jsonSettings$models, function(x) x$attr_type)),
                           cohortId = unlist(lapply(jsonSettings$models, function(x) x$cohortId)),
                           cohortName = private$getCohortName(jsonSettings,unlist(lapply(jsonSettings$models, function(x) x$cohortId))),
                           outcomeId = unlist(lapply(jsonSettings$models, function(x) x$outcomeId)),
                           outcomeName = private$getCohortName(jsonSettings,unlist(lapply(jsonSettings$models, function(x) x$outcomeId)))
      )
      result$analysisId <- paste(result$modelName, result$cohortId, result$outcomeId, sep='_')
      return(result)
    },

    getCohortName = function(jsonSettings, cohortIds){
      cohortIdentifiers <- do.call('rbind', lapply(jsonSettings$cohortDefinition, function(x){c(x$name, x$id)}))
      colnames(cohortIdentifiers) <- c('name','id')
      names <- c()
      for(id in cohortIds){
        ind <- which(cohortIdentifiers[,'id'] ==id)
        if(length(ind)==0){
          ParallelLogger::logInfo('cohort id missing from cohortDefinitions')
          names <- c(names,'Missing')
        } else{
          names <- c(names,cohortIdentifiers[min(ind),'name'])
        }
      }
      return(names)
    },

    getPopulationSettings = function(jsonSettings,
                                      modelName){
      ind <- which(modelName == unlist(lapply(jsonSettings$models, function(x) x$name)))
      populationSettings <- jsonSettings$models[[ind]]$populationSettings
      return(populationSettings)
    }
  )
)
