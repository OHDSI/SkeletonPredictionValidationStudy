# add the functions for the exisitng models here
#======= add custom function here...
#' @export
predict.nonPlpGlm <- function(plpModel, plpData, population){

  coeff <- plpModel$model$coefficients
  finalMapping <- plpModel$model$finalMapping
  type <- attr(plpModel, 'predictionType')
  offset <- plpModel$model$offset
  baselineHazard <- plpModel$model$baselineHazard

  finalMapping <- eval(str2lang(paste0(finalMapping, collapse = ' ')))

  plpData$covariateData$coefficients <- coeff
  on.exit(plpData$covariateData$coefficients <- NULL, add = TRUE)

  if(sum(c('power','offset')%in%colnames(coeff))==2){
    prediction <- plpData$covariateData$covariates %>%
      dplyr::inner_join(plpData$covariateData$coefficients, by= 'covariateId') %>%
      dplyr::mutate(values = (covariateValue-offset)^power*points) %>%
      dplyr::group_by(rowId) %>%
      dplyr::summarise(value = sum(values, na.rm = TRUE)) %>%
      dplyr::select(rowId, value) %>%
      dplyr::collect()
  } else{
    prediction <- plpData$covariateData$covariates %>%
      dplyr::inner_join(plpData$covariateData$coefficients, by= 'covariateId') %>%
      dplyr::mutate(values = covariateValue*points) %>%
      dplyr::group_by(rowId) %>%
      dplyr::summarise(value = sum(values, na.rm = TRUE)) %>%
      dplyr::select(rowId, value) %>%
      dplyr::collect()
  }

  prediction <- merge(population, prediction, by ="rowId", all.x = TRUE)
  prediction$value[is.na(prediction$value)] <- 0

  # add any final mapping here (e.g., add intercept and mapping)
  prediction$value <- finalMapping(prediction$value)

  metaData <- list(predictionType = type,
                   cohortId = attr(population,'metaData')$cohortId,
                   outcomeId = attr(population,'metaData')$outcomeId,
                   timepoint = attr(population,'metaData')$riskWindowEnd)
  attr(prediction, "metaData") <- metaData

  attr(prediction, "baselineHazard") <- baselineHazard
  attr(prediction, "offset") <-  offset
  attr(prediction, "timepoint") <- attr(population,'metaData')$riskWindowEnd

  return(prediction)
}

#' @export
predict.pythonJson <- function(plpModel, plpData, population){

  data <- PatientLevelPrediction::toSparseM(plpData = plpData,
                                            population = population,
                                            map = plpModel$covariateMap)

  #reticulate::conda_install(envname = 'r-reticulate', packages = 'sklearn-json')
  skljson <- reticulate::import('sklearn_json')
  modelTrained <- skljson$from_json(plpModel$model) # if adaBoost/Keras use different load


  dataMat <- data$data[population$rowId,, drop = F]
  if(is.null(dim(dataMat))){
    ParallelLogger::logInfo('Converting dimensions')
    dataMat <- matrix(as.vector(data$data[population$rowId]), ncol = 1)
  }

  pred <- modelTrained$predict_proba(dataMat)

  prediction <- cbind(population, pred[,2])
  colnames(prediction)[ncol(prediction)] <- 'value'

  metaData <- list(predictionType = 'pythonJson',
                   cohortId = attr(population,'metaData')$cohortId,
                   outcomeId = attr(population,'metaData')$outcomeId,
                   timepoint = attr(population,'metaData')$riskWindowEnd)
  attr(prediction, "metaData") <- metaData

  return(prediction)

}
