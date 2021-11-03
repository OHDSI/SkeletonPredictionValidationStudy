#' @title Study Class
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

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(path = NULL) {
      self$loadStudyFromJson(path)
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
    }
  )
)
