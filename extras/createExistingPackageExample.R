source('./extras/skeletonHelpers.R')
# example
baseUrl <- 'https://...'
authMethod <- 'windows'
webApiUsername <- '...'
webApiPassword <- '...'

ROhdsiWebApi::authorizeWebApi(baseUrl = baseUrl,
                              authMethod = authMethod,
                              webApiUsername = webApiUsername,
                              webApiPassword = webApiPassword)

popSet <- PatientLevelPrediction::createStudyPopulationSettings(riskWindowStart = 1,
                                                                riskWindowEnd = 365)

covariateHyper <- createCohortCovariateSetting(atlasId = 339,
                                               covariateName = 'Hypertension',
                                               startDay=-365,
                                               endDay=0,
                                               analysisId = 456,
                                               power = 1,
                                               points = 8)

covariateDepression <- createCohortCovariateSetting(atlasId = 380,
                                                    covariateName = 'Depression',
                                                    startDay=-365*5,
                                                    endDay=0,
                                                    analysisId = 456,
                                                    points = 5)

covariateDiabetes <- createCohortCovariateSetting(atlasId = 746,
                                                  covariateName = 'Diabetes',
                                                  startDay=-365*10,
                                                  endDay=0,
                                                  analysisId = 456,
                                                  points = 3)

model1 <- createModelSetting(modelName = 'example 1',
                             targetId = 636,
                             outcomeIds = 2265,
                             populationSettings = popSet,
                             covariateSettings = list(covariateHyper, covariateDepression, covariateDiabetes),
                             predictionType = 'binary',
                             finalMapping = 'function(x){return(1/(1+exp(-x)))}',
                             offset = 0,
                             baseUrl = baseUrl)

covariateDiabetes2 <- createCohortCovariateSetting(atlasId = 746,
                                                   covariateName = 'Diabetes',
                                                   startDay=-365*10,
                                                   endDay=0,
                                                   analysisId = 456,
                                                   points = 30)

model2 <- createModelSetting(modelName = 'example 2',
                             targetId = 636,
                             outcomeId = 2265,
                             populationSettings = popSet,
                             covariateSettings = list(covariateDiabetes2),
                             predictionType = 'binary',
                             finalMapping = 'function(x){return(1/(1+exp(-x)))}',
                             offset = 0,
                             baseUrl = baseUrl)

modelList <- list(model1$modelList,
                  model2$modelList)
cohortDefinitionList <- c(model1$cohortDefinitions,
                          model2$cohortDefinitions)

createExistingPackage(packageName = 'jennaTest3',
                      packageDescription = 'testing',
                      createdBy = 'Jenna',
                      organizationName = 'JNJ',
                      modelList = modelList,
                      cohortDefinitionList = cohortDefinitionList,
                      outputFolder = '/Users/jreps/Documents/testing/newVal2',
                      useHydra = F,
                      baseUrl = baseUrl)
