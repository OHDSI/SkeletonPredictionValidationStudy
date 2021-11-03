library("R6")
library("checkmate")

#' @title Model Class
#' @export
Model <- R6Class(
  "Model",
  public = list(

    #' @field model name (`character(1)`)
    name = NULL,

    #' @field target cohort identifier (`numeric(1)`)
    cohortId = NULL,

    #' @field list of outcome cohorts (`numeric(N)`)
    outcomeId = NULL,

    #' @field prediction type (`character(1)`)
    attr_predictionType = NULL,

    #' @field model type (`character(1)`)
    attr_type = NULL,

    populationSettings = NULL,
    covariateSettings = NULL,
    model = NULL,
    covariateMap = NULL,
    varImp = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(cohortId = NULL,
                          outcomeId = NULL,
                          coefficients = NULL,
                          finalMapping = 'function(x){return(x)}',
                          offset = 0,
                          baselineHazard = 0) {

      # check input format
      assert_numeric(cohortId, len = 1, null.ok = TRUE)
      assert_numeric(outcomeId, min.len = 1, unique = T, null.ok = TRUE)

      # assign variables
      self$cohortId <- cohortId
      self$outcomeId <- outcomeId

      self$model <- Parameter$new(
        coefficients = coefficients,
        finalMapping = finalMapping,
        offset = offset,
        baselineHazard = baselineHazard
      )
    },

    predict = function() {
    },

    loadModelFromJson = function() {
    },

    setCoefficients = function(covariateSettings) {
      assert_class(self$model, classes = "Parameter")
      self$model$setCoefficients(covariateSettings)
    }

    #' #' @description
    #' #' Printer.
    #' #' @param ... (ignored)
    #' print = function(...) {
    #'   # cat(format(self))
    #'   cat("* Cohort ID:" , self$cohortId, "\n", sep = "")
    #'   cat("* Outcome ID:" , self$outcomeId, "\n", sep = "")
    #'   cat("* Prediction Type:" , self$attr_predictionType, "\n", sep = "")
    #'   cat("* Model Type:" , self$attr_type, "\n", sep = "")
    #'   w = self$warnings
    #'   e = self$errors
    #'   if (length(w)) {
    #'     catn("* Warnings:", w)
    #'   }
    #'   if (length(e)) {
    #'     catn("* Errors:", e)
    #'   }
    #'   return(invisible(self))
    #' }
  )
)
