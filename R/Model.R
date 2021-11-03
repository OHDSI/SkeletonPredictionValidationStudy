#' Model Class
#' @description
#' Model class.
#' @export
Model <- R6::R6Class(
  "Model",
  public = list(

    #' @field name (`character(1)`)\cr
    #' Unique name of the model.
    name = NULL,

    #' @field cohortId (`numeric(1)`)\cr
    #' Target cohort identifier.
    cohortId = NULL,

    #' @field outcomeId (`numeric(N)`)\cr
    #' List of outcome cohort identifiers.
    outcomeId = NULL,

    #' @field attr_predictionType (`character(1)`)\cr
    #' Prediction type.
    attr_predictionType = NULL,

    #' @field attr_type (`character(1)`)\cr
    #' Model type.
    attr_type = NULL,

    #' @field model (`[Parameter]`)\cr
    #' Single set of model parameters
    model = NULL,

    #' @field populationSettings (`TBD`)\cr
    #' Population settings.
    populationSettings = NULL,

    #' @field covariateSettings (`TBD`)\cr
    #' Covariate settings.
    covariateSettings = NULL,

    #' @field covariateMap (`TBD`)\cr
    #' Covariate map.
    covariateMap = NULL,

    #' @field varImp (`TBD`)\cr
    #' Variable importance.
    varImp = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @template param_cohortId
    #' @template param_outcomeId
    #' @template param_coefficients
    #' @template param_finalMapping
    #' @template param_offset
    #' @template param_baselineHazard
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

    #' @description
    #' predict
    predict = function() {
    },

    #' @description
    #' loadModelFromJson
    loadModelFromJson = function() {
    }
  )
)
