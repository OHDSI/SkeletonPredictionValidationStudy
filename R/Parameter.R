#' @title Model Class
Parameter <- R6Class(
  "Parameter",
  public = list(

    #' @field coefficient data (`data.frame(1)`)
    coefficients = NULL,

    #' @field mapping function of the form 'function(x){return(x)}' (`character(1)`)
    finalMapping = NULL,

    #' @field offset (`numeric(1)`)
    offset = NULL,

    #' @field baseline hazard (`numeric(1)`)
    baselineHazard = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(coefficients,
                          finalMapping = 'function(x){return(x)}',
                          offset = numeric(),
                          baselineHazard = numeric()) {

      # check input format
      assert_data_frame(coefficients, ncols = 4, null.ok = TRUE)
      assert_character(finalMapping, fixed = 'function(x)', len = 1)
      assert_numeric(offset, len = 1)
      assert_numeric(baselineHazard, len = 1)

      # assign variables
      self$coefficients <- coefficients
      self$finalMapping <- finalMapping
      self$offset <- offset
      self$baselineHazard <- baselineHazard
    },

    #' @description
    #' Sets the coefficients.
    #'
    #' @param covariateSettings (`list(1)` or `data.frame(1)`).
    setCoefficients = function(covariateSettings) {

      # if parameter is already coefficient format
      if (is.data.frame(coefficients)) {
        self$coefficients <- covariateSettings

      # else, if coefficients are not yet coefficient format
      } else if (is.list(checkcovariateSettings)) {
        coefficients <- data.frame(
          covariateId = unlist(lapply(covariateSettings, function(x)
            x$coeffs$covariateId)),
          points = unlist(lapply(covariateSettings, function(x)
            x$coeffs$points)),
          offset = unlist(lapply(covariateSettings, function(x)
            x$coeffs$offset)),
          power = unlist(lapply(covariateSettings, function(x)
            x$coeffs$power))
        )
        } else {
        # do nothing
      }
    },

    getParameters = function() {

    },

    #' @description
    #' Load the parameters from a JSON file
    #'
    #' @param path (`character(1)`).
    #'
    #' @param modelName (`character(1)`)
    loadParameterFromJson = function(path = NULL, modelName = NULL) {

      if (is.null(path) && !is.null(modelName)) {
        path <- system.file(paste0("models/", modelName, "/model.json"),
                                   package = "SkeletonPredictionValidationStudy")
      }

      paramTemp <- readChar(path, file.info(path)$size)
      param <- RJSONIO::fromJSON(paramTemp)

      self$coefficients <- param$coefficients
      self$finalMapping <- param$finalMapping
      self$offset <- param$offset
      self$baselineHazard <- param$baselineHazard
    }
  )
)
