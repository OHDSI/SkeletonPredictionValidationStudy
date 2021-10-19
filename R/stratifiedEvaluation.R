
#' stratifiedEvaluation
#'
#' @description
#' Evaluates PLP model in sub-strata of the test/validation set.
#'
#' @details
#' Appends the sub-population results into the main results.
#'
#' @param evaluation The original results object
#' @param analysisId The analysis identifier
#' @param ageStrata A list of age limit pairs. For example, list(c(0, 64),
#' c(65, 150)) evaluates the model in persons aged 0-64 and 65-15 inclusive.
#' @param sexStrata
#' @return
#' A modified evaluation object.
#'
#' @export
stratifiedEvaluation <- function(evaluation, analysisId, strata) {

  # keep a backup of the prediction object as it needs to be modified with the
  # current implementation of evaluatePlp()
  predictionTemp <- evaluation$prediction
  evaluation <- stratifiedEvaluationLoop(evaluation = evaluation,
                                         analysisId = analysisId,
                                         strata = strata)

  # TODO add a more scalable solution for evaluation in recalibration
  # if recalibration has been done, as is done in this specific package
  # CovCoagBaseValidation, locally replace values with it
  if("recalibrationInTheLargeValue" %in% colnames(evaluation$prediction)) {
    ParallelLogger::logInfo('Using mean recalibration prediction values')

    # evaluatePlp() uses the value column, therefore, recalibration values
    # need to be copied over for now. Remember that we kept a backup of the
    # original prediction object above.
    evaluation$prediction$value <- evaluation$prediction$recalibrationInTheLargeValue
    evaluation <- stratifiedEvaluationLoop(evaluation = evaluation,
                                           analysisId = analysisId,
                                           strata = strata)
  }

  if("weakRecalibration" %in% colnames(evaluation$prediction)) {
    ParallelLogger::logInfo('Using weak recalibration prediction values')

    # evaluatePlp() uses the value column, therefore, recalibration values
    # need to be copied over for now. Remember that we kept a backup of the
    # original prediction object above.
    evaluation$prediction$value <- evaluation$prediction$weakRecalibration
    evaluation <- stratifiedEvaluationLoop(evaluation = evaluation,
                                           analysisId = analysisId,
                                           strata = strata)
  }

  # restore value column in prediction object
  evaluation$prediction <- predictionTemp

  # return modified results object
  return(evaluation)
}

#' stratifiedEvaluationLoop
#'
#' @description
#' Internal loop to perform stratified evaluation
#'
#' @details
#' Internal loop to perform stratified evaluation
#'
#' @param evaluation A results object
#' @param analysisId The analysis identifier
#' @param strata A list of lists with age limit pairs and sex strata of the form
#'               list(ageStrata = list(c(0, 64), c(65, 150)),
#'               sexStrata = list(8507, 8532))
#' @return
#' A modified evaluation object.
#'
stratifiedEvaluationLoop(evaluation, analysisId, strata) {

  prediction <- evaluation$prediction

  for (i in 1:length(strata$ageStrata)) {
    # create a unique name of the evaluation
    evalName <- paste0("age", strata$ageStrata[[i]][1],"-", strata$ageStrata[[i]][2])

    # subset population and evaluate
    tempEval <- prediction %>%
      dplyr::filter(ageYear >= strata$ageStrata[[i]][1], ageYear <= strata$ageStrata[[i]][2]) %>%
      PatientLevelPrediction::evaluatePlp() %>%
      formatEvaluation(analysisId = analysisId, eval = evalName)

    evaluation$performanceEvaluation <- addEvaluation(evaluation$performanceEvaluation,
                                                      subpopEvaluation = tempEval)
  }

  for (i in 1:length(strata$sexStrata)) {
    # create a unique name of the evaluation
    evalName <- paste0("sex", strata$sexStrata[[i]])

    # subset population and evaluate
    tempEval <- prediction %>%
      dplyr::filter(gender == strata$sexStrata[[i]]) %>%
      PatientLevelPrediction::evaluatePlp() %>%
      formatEvaluation(analysisId = analysisId, eval = evalName)

    evaluation$performanceEvaluation <- addEvaluation(evaluation$performanceEvaluation,
                                                      subpopEvaluation = tempEval)
  }
  return(evaluation)
}

#' addEvaluation
#'
#' @description
#' Adds the subpopulation results to the main results
#'
#' @details
#' Append the subpopulation results into the main results
#'
#' @param performanceEvaluation           The main result performanceEvaluation
#' @param subpopEvaluation                   The subpopulation result
#' @return
#' An object of class \code{runPlp} that is recalibrated on the new data
#'
addEvaluation <- function(performanceEvaluation, subpopEvaluation){

  if(!is.null(subpopEvaluation$demographicSummary)){
    ParallelLogger::logInfo('Appending subpopulation demographicSummary')
    performanceEvaluation$demographicSummary <- rbind(performanceEvaluation$demographicSummary,
                                                      subpopEvaluation$demographicSummary)
  }

  if(!is.null(subpopEvaluation$calibrationSummary )){
    ParallelLogger::logInfo('Appending subpopulation calibrationSummary ')
    performanceEvaluation$calibrationSummary  <- rbind(performanceEvaluation$calibrationSummary ,
                                                       subpopEvaluation$calibrationSummary )
  }

  if(!is.null(subpopEvaluation$thresholdSummary )){
    ParallelLogger::logInfo('Appending subpopulation thresholdSummary ')
    performanceEvaluation$thresholdSummary  <- rbind(performanceEvaluation$thresholdSummary ,
                                                     subpopEvaluation$thresholdSummary )
  }

  if(!is.null(subpopEvaluation$evaluationStatistics )){
    ParallelLogger::logInfo('Appending subpopulation evaluationStatistics ')

    performanceEvaluation$evaluationStatistics <- as.data.frame(performanceEvaluation$evaluationStatistics)
    performanceEvaluation$evaluationStatistics$Metric <- as.character(performanceEvaluation$evaluationStatistics$Metric)
    performanceEvaluation$evaluationStatistics$Value <- as.character(performanceEvaluation$evaluationStatistics$Value)
    performanceEvaluation$evaluationStatistics <- rbind(performanceEvaluation$evaluationStatistics ,
                                                        subpopEvaluation$evaluationStatistics )
  }

  return(performanceEvaluation)
}



formatEvaluation <- function(evaluation, analysisId, eval){
  if(!is.null(evaluation$demographicSummary)){
    demoNames <- colnames(evaluation$demographicSummary)
    evaluation$demographicSummary$analysisId  <- analysisId
    evaluation$demographicSummary$Eval <- eval
    evaluation$demographicSummary <- evaluation$demographicSummary[,c("analysisId","Eval", demoNames )]
  }

  if(!is.null(evaluation$calibrationSummary)){
    calNames <- colnames(evaluation$calibrationSummary)
    evaluation$calibrationSummary$analysisId  <- analysisId
    evaluation$calibrationSummary$Eval <- eval
    evaluation$calibrationSummary <- evaluation$calibrationSummary[,c("analysisId","Eval", calNames )]
  }

  if(!is.null(evaluation$thresholdSummary)){
    thresNames <- colnames(evaluation$thresholdSummary)
    evaluation$thresholdSummary$analysisId  <- analysisId
    evaluation$thresholdSummary$Eval <- eval
    evaluation$thresholdSummary <- evaluation$thresholdSummary[,c("analysisId","Eval", thresNames )]
  }

  evaluation$evaluationStatistics$analysisId <- NULL
  evaluation$evaluationStatistics <- data.frame(analysisId = analysisId,
                                                Eval = eval,
                                                Metric = names(unlist(evaluation$evaluationStatistics)),
                                                Value = unlist(evaluation$evaluationStatistics))
  return(evaluation)
}
