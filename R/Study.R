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

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @template param_studyPath
    initialize = function(studyPath = NULL) {
      self$loadStudyFromJson(studyPath)
      self$analysisSettings <- self$getAnalyses()
    },

    #' @description
    #' Load study from JSON file
    #' @template param_studyPath
    loadStudyFromJson = function(studyPath = NULL) {

      if (is.null(studyPath)) {
        studyPath <- system.file("settings/predictionAnalysisList.json",
                            package = "SkeletonPredictionValidationStudy")
      }
      paramTemp <- readChar(studyPath, file.info(studyPath)$size)
      jsonSettings <- RJSONIO::fromJSON(paramTemp)

      self$packageName <- jsonSettings$packageName
      self$packageDescription <- jsonSettings$packageDescription
      self$createdBy <- jsonSettings$createdBy
      self$organizationName <- jsonSettings$organizationName
      self$analysisSettings <- self$getAnalysis(jsonSettings)
    }),

  private = list(

    getAnalyses = function(jsonSettings){

      result <- data.frame(modelName = unlist(lapply(jsonSettings$models, function(x) x$name)),
                           modelType = unlist(lapply(jsonSettings$models, function(x) x$attr_type)),
                           cohortId = unlist(lapply(jsonSettings$models, function(x) x$cohortId)),
                           cohortName = self$getCohortName(jsonSettings,unlist(lapply(jsonSettings$models, function(x) x$cohortId))),
                           outcomeId = unlist(lapply(jsonSettings$models, function(x) x$outcomeId)),
                           outcomeName = getCohortName(jsonSettings,unlist(lapply(jsonSettings$models, function(x) x$outcomeId)))
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
    }
  )
)
