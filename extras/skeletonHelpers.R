# code to create the package json for existing models
createExistingPackage <- function(packageName,
                               packageDescription = '',
                               createdBy = '',
                               organizationName = '',
                               modelList,
                               cohortDefinitionList,
                               outputFolder,
                               baseUrl = 'http://...',
                               useHydra = F,
                               skeletonVersion = 'v1.0.1'){

  result <- list()

  # analysis settings
  result$packageName <- packageName
  result$packageDescription <- packageDescription
  result$createdBy <- createdBy
  result$organizationName <- organizationName

  result$skeletonType <- "PatientLevelPredictionValidationStudy"

  # cohort definitions
  result$cohortDefinitions <- cohortDefinitionList

  # model list
  result$models <- modelList

  if(useHydra){
    result$skeletonVersion <- skeletonVersion
    json <- RJSONIO::toJSON(result, digits = 23)
    Hydra::hydrate(json, outputFolder = outputFolder)
  } else{

    packageLocation <- downLoadSkeleton(outputFolder = outputFolder,
                     packageName = packageName,
                     skeletonType = 'SkeletonPredictionValidationStudy')

    replaceName(packageLocation = packageLocation,
                packageName = packageName,
                skeletonType = 'SkeletonPredictionValidationStudy')

    saveAnalysisJson(packageLocation = packageLocation,
                     analysisList = result)

    saveCohorts(packageLocation = packageLocation,
                analysisList = result,
                baseUrl = baseUrl)

    # update location for models to be saved...
    outputFolder <- packageLocation

  }

  for(i in 1:length(result$models)){
  #then save the models
    dir.create(file.path(outputFolder,'inst',"models",result$models[[i]]$name), recursive = T)
  write(RJSONIO::toJSON(result$models[[i]]$model, digits = 23),
        file.path(outputFolder,'inst',"models",result$models[[i]]$name,"model.json")
  )
  }

  return(invisible(result))
}

editName <- function(name){
  name <- gsub(' ','_', name)
  name <- gsub("[[:punct:]]", "_", name)
  return(name)
}

getCohorts <- function(cohortIds,
                       baseUrl){
  cohorts <- list()
  length(cohorts) <- length(cohortIds)
  for(i in 1:length(cohortIds)){
    cohorts[[i]] <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortIds[[i]], baseUrl = baseUrl)
  }
  return(cohorts)
}

createCohortCovariateSetting <- function(atlasId = 1,
                                         covariateName = '',
                                         startDay=-30,
                                         endDay=0,
                                         count=F,
                                         ageInteraction = F,
                                         lnAgeInteraction= F,
                                         analysisId = 456,
                                         points = 1,
                                         offset = 0,
                                         power = 1){
  settings <- list(fnct = 'createCohortCovariateSettings',
       settings = list(covariateName = covariateName,
                       covariateId = atlasId*1000+analysisId,
                       cohortId = atlasId,
                       startDay = startDay,
                       endDay = endDay,
                       count = count,
                       ageInteraction = ageInteraction,
                       lnAgeInteraction = lnAgeInteraction,
                       analysisId = analysisId),
       coeffs  = list(covariateId = atlasId*1000+analysisId,
                      points = points,
                      offset = offset,
                      power = power ))

  return(settings)
}


createGenderCovariateSetting <- function(male = T,
                                         points = 1,
                                         offset = 0,
                                         power = 1){
  settings <- list(fnct = 'createCovariateSettings',
                   settings = FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                                         includedCovariateIds = ifelse(male, 8507, 8532)*1000+1),
                   coeffs  = list(covariateId = ifelse(male, 8507, 8532)*1000+1,
                                  points = points,
                                  offset = offset,
                                  power = power ))

  return(settings)
}

createAgeCovariateSetting <- function(covariateName = 'Age at index',
                                      ageMap = function(x){return(x)},
                                      covariateId = 1458,
                                      analysisId = 458,
                                         points = 1,
                                         offset = 0,
                                         power = 1){
  settings <- list(fnct = 'createAgeCovariateSettings',
                   settings = list(covariateName = covariateName,
                                   ageMap = ageMap,
                                   covariateId = covariateId,
                                   analysisId = analysisId),
                   coeffs  = list(covariateId = covariateId,
                                  points = points,
                                  offset = offset,
                                  power = power ))

  return(settings)
}

createMeasurementCovariateSetting <- function(covariateName,
                                              conceptSet,
                                              startDay=-30,
                                              endDay=0,
                                              scaleMap = NULL,
                                              aggregateMethod = 'recent',
                                              imputationValue = 0,
                                              ageInteraction = F,
                                              lnAgeInteraction = F,
                                              lnValue = F,
                                              covariateId = 1466,
                                              analysisId = 466,
                                      points = 1,
                                      offset = 0,
                                      power = 1){
  settings <- list(fnct = 'createMeasurementCovariateSettings',
                   settings = list(covariateName = covariateName,
                                   conceptSet = conceptSet,
                                   startDay=startDay,
                                   endDay=endDay,
                                   scaleMap = scaleMap,
                                   aggregateMethod = aggregateMethod,
                                   imputationValue = imputationValue,
                                   ageInteraction = ageInteraction,
                                   lnAgeInteraction = lnAgeInteraction,
                                   lnValue = lnValue,
                                   covariateId = covariateId,
                                   analysisId = analysisId),
                   coeffs  = list(covariateId = covariateId,
                                  points = points,
                                  offset = offset,
                                  power = power ))

  return(settings)
}

createModelSetting <- function(modelName,
                               targetId ,
                               outcomeIds,
                               populationSettings = PatientLevelPrediction::createStudyPopulationSettings(),
                               covariateSettings = list(list(fnct = '', settings = list(), coeff = list(point = 1)),
                                                        list(fnct = '', settings = list(), coeff = list(point = 1))
                                                        ),
                               predictionType = 'binary',
                               finalMapping = 'function(x){return(1/(1+exp(-x)))}',
                               offset = NULL,
                               baselineHazard = NULL,
                               baseUrl){

  modCovariateSettings <- modifyCovariates(covariateSettings)
  modCovariateSettings$settings <- addAttr(modCovariateSettings$settings)

  settings <- list(name = editName(modelName),
                   cohortId = targetId,
                   outcomeId = outcomeIds,
                   populationSettings = populationSettings,
                   covariateSettings = modCovariateSettings$settings,
                   attr_predictionType = predictionType,
                   attr_type = 'nonPlpGlm',
                   model = list(
                     coefficients = getCoefficients(covariateSettings),
                     finalMapping = finalMapping,
                     offset = offset,
                     baselineHazard = baselineHazard)
  )

  cohortIds = unique(c(targetId, outcomeIds, modCovariateSettings$cohortIds))
  cohortDefinitions <- getCohorts(cohortIds, baseUrl)

  return(list(modelList = settings,
              cohortDefinitions = cohortDefinitions)
         )
}


modifyCovariates <- function(covariateSettings){
  for(i in 1:length(covariateSettings)){
    covariateSettings[[i]]$coeffs <- NULL
  }

  cohortIds <- unique(unlist(lapply(covariateSettings, function(x) x$settings$cohortId)))

  return(list(settings = covariateSettings,
              cohortIds = cohortIds))
}

getCoefficients <- function(covariateSettings){
  #coefficients: covariateId, offset, power, points
  coefficients <- data.frame(
    covariateId = unlist(lapply(covariateSettings, function(x) x$coeffs$covariateId)),
    points = unlist(lapply(covariateSettings, function(x) x$coeffs$points)),
    offset = unlist(lapply(covariateSettings, function(x) x$coeffs$offset)),
    power = unlist(lapply(covariateSettings, function(x) x$coeffs$power))
  )
  return(coefficients)
}



# code to use skeleton master from github rather than hydra
# download a .zip file of the repository
# from the "Clone or download - Download ZIP" button
# on the GitHub repository of interest
downLoadSkeleton <- function(outputFolder,
                             packageName,
                             skeletonType = 'SkeletonPredictionValidationStudy'){
  # check outputFolder exists

  # check file.path(outputFolder,  packageName) does not exist

  # download, unzip and rename:

  download.file(url = paste0("https://github.com/ohdsi/",skeletonType,"/archive/master.zip")
                , destfile = file.path(outputFolder, "package.zip"))
  # unzip the .zip file
  unzip(zipfile = file.path(outputFolder, "package.zip"), exdir = outputFolder)
  file.rename( from = file.path(outputFolder, paste0(skeletonType, '-master')),
               to = file.path(outputFolder,  packageName))
  unlink(file.path(outputFolder, "package.zip"))
  return(file.path(outputFolder, packageName))
}

# change name
replaceName <- function(packageLocation = getwd(),
                        packageName = 'ValidateRCRI',
                        skeletonType = 'SkeletonPredictionValidationStudy'){

  filesToRename <- c(paste0(skeletonType,".Rproj"),paste0("R/",skeletonType,".R"))
  for(f in filesToRename){
    ParallelLogger::logInfo(paste0('Renaming ', f))
    fnew <- gsub(skeletonType, packageName, f)
    file.rename(from = file.path(packageLocation,f), to = file.path(packageLocation,fnew))
  }

  filesToEdit <- c(file.path(packageLocation,"DESCRIPTION"),
                   file.path(packageLocation,"README.md"),
                   file.path(packageLocation,"extras/CodeToRun.R"),
                   file.path(packageLocation, "extras/updateSkeleton.R"),
                   dir(file.path(packageLocation,"R"), full.names = T))
  for( f in filesToEdit ){
    ParallelLogger::logInfo(paste0('Editing ', f))
    x <- readLines(f)
    y <- gsub( skeletonType, packageName, x )
    cat(y, file=f, sep="\n")

  }

  return(packageName)
}

# save json file into isnt/settings/predictionAnalysisList.json
saveAnalysisJson <- function(packageLocation,
                             analysisList){
  write(RJSONIO::toJSON(analysisList, digits =23),
        file=file.path(packageLocation, 'inst', 'settings', 'predictionAnalysisList.json')
  )

  return(packageLocation)
}

# create cohorts to create from cohortDefinitions
# save json and convert+save sql into inst/cohorts and inst/sql/sql_server
saveCohorts <- function(packageLocation,
                        analysisList,
                        baseUrl){

  nameForFile <- function(name){
    writeLines(name)
    name <- gsub(' ','', name)
    name <- gsub("[[:punct:]]", "_", name)
    writeLines(name)
    return(name)
  }

  details <- lapply(1:length(analysisList$cohortDefinitions), function(i){c(name = analysisList$cohortDefinitions[[i]]$name,
                                                                            cohortId = analysisList$cohortDefinitions[[i]]$id,
                                                                            atlasId = analysisList$cohortDefinitions[[i]]$id)})
  details <- do.call('rbind', details)
  details <- as.data.frame(details, stringsAsFactors = F)
  details$name <- nameForFile(details$name) # failing dev

  write.csv(x = details,
            file = file.path(packageLocation, 'inst', 'settings','cohortsToCreate.csv'),
            row.names = F)

  # make sure cohorts and sql/sql_server exist
  if(!dir.exists(file.path(packageLocation, 'inst', 'cohorts'))){
    dir.create(file.path(packageLocation, 'inst', 'cohorts'), recursive = T)
  }
  if(!dir.exists(file.path(packageLocation, 'inst', 'sql', 'sql_server'))){
    dir.create(file.path(packageLocation, 'inst', 'sql', 'sql_server'), recursive = T)
  }

  # save the cohorts as json
  lapply(1:length(analysisList$cohortDefinitions), function(i){
    write(RJSONIO::toJSON(analysisList$cohortDefinitions[[i]], digits = 23),
          file=file.path(packageLocation, 'inst', 'cohorts', paste0(nameForFile(analysisList$cohortDefinitions[[i]]$name),'.json')))
  })

  # save the cohorts as sql
  lapply(1:length(analysisList$cohortDefinitions), function(i){
    write(ROhdsiWebApi::getCohortSql(analysisList$cohortDefinitions[[i]], baseUrl = baseUrl, generateStats = F),
          file=file.path(packageLocation, 'inst', 'sql', 'sql_server', paste0(nameForFile(analysisList$cohortDefinitions[[i]]$name), '.sql')))
  })

  return(packageLocation)
}




addAttr <- function(covariateSettings){
  #find standard
  if(!is.null(covariateSettings$fnct)){
    if(covariateSettings$fnct =='createCovariateSettings'){
      covariateSettings$settings$attr_fun <- attr(covariateSettings$settings,'fun')
      covariateSettings$settings$attr_class <- attr(covariateSettings$settings,'class')
    }
    return(covariateSettings)
  }

  if(!is.null(covariateSettings[[1]]$fnct)){
    for(i in 1:length(covariateSettings)){
      if(covariateSettings[[i]]$fnct =='createCovariateSettings'){
        covariateSettings[[i]]$settings$attr_fun <- attr(covariateSettings[[i]]$settings,'fun')
        covariateSettings[[i]]$settings$attr_class <- attr(covariateSettings[[i]]$settings,'class')
      }
    }
    return(covariateSettings)
  }

  if(class(covariateSettings)=='list' && is.null(covariateSettings$fnct))
    for(i in 1:length(covariateSettings)){
      for(j in 1:length(covariateSettings[[i]]))
        if(covariateSettings[[i]][[j]]$fnct =='createCovariateSettings'){
          covariateSettings[[i]][[j]]$settings$attr_fun <- attr(covariateSettings[[i]][[j]]$settings,'fun')
          covariateSettings[[i]][[j]]$settings$attr_class <- attr(covariateSettings[[i]][[j]]$settings,'class')
        }
    }

  return(covariateSettings)

}

getLatestSkeletonVersion <- function(dir) {
  packageDescription('SkeletonPredictionValidationStudy',
                     lib.loc = dir,
                     fields = "Version")
}
