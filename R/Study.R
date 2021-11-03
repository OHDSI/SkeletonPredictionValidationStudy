#' @title Study Class
#' @export
Study <- R6Class(
  "Study",
  public = list(

    #' @field package name (`character(1)`)
    packageName = NULL,

    #' @field package description (`character(1)`)
    packageDescription = NULL,

    #' @field created by names (`character(1)`)
    createdBy = NULL,

    #' @field organization name (`character(1)`)
    organizationName = NULL,

    #' @field analysis settings (`data.frame(1)`)
    analysisSettings = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(path = NULL) {
      self$loadStudyFromJson(path)
      self$analysisSettings <- self$getAnalyses()
    },

    loadStudyFromJson = function(path = NULL) {

      if (is.null(path)) {
        path <- system.file("settings/predictionAnalysisList.json",
                            package = "SkeletonPredictionValidationStudy")
      }
      paramTemp <- readChar(path, file.info(path)$size)
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
