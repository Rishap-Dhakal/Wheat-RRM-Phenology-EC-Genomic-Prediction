#############################
##VFD

library(asreml)

load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)

dados_temp_na2_listRRM1.cv1 <- list()
predictions_listRRM1.cv1 <- list()
predictions_listRRM2.cv1 <- list()
dados_temp_na2_listRRM2.cv1 <- list()
model1 <- list()
model2 <- list()

## CV1 GbyE
fold <- 5
n_rep <- 50

for (i in 1:n_rep) {
  dados_temp <- data_RRM
  cult <- unique(dados_temp$cultivar)
  
  random_gen <- sample(cult, length(cult), replace = FALSE)
  
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  for (j in 1:fold) {
    dados_temp <- data_RRM
    dados_temp$Yield_BLUP_CTE2 <- dados_temp$Yield_BLUP_CTE
    dados_temp$blup.weight2 <- dados_temp$blup.weight
    
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "Yield_BLUP_CTE"] <- NA
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "blup.weight"] <- NA
    
    model <- asreml(
      fixed = Yield_BLUP_CTE ~ 1 + heladasveg ,
      random = ~ environ:cultivar+environ:trial+
        str(~ vm(cultivar, GINV.sparse$Ginv.sparse) +
              heladasveg: vm(cultivar, GINV.sparse$Ginv.sparse),
            vmodel = ~ us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
      weights = blup.weight,
      data = dados_temp,
      workspace = "32Gb",
      family = asr_gaussian(dispersion = 1.0),
      na.action = na.method(x = "include", y = "include")
    )
    
    model1[[j]] <- model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_BLUP_CTE))
    
    aux_parallel_Init <- as.data.frame(dados_temp_na2[, c(
      'cultivar', 'environ', 'trial', 'cultivar.env.trial', 'heladasveg'
    )])
    
    predictions <- predict.asreml(
      model,
      classify = 'cultivar:heladasveg',
      levels = list(
        'cultivar' = aux_parallel_Init$cultivar,
        'heladasveg' = aux_parallel_Init$heladasveg
      ),
      pworkspace = "16gb",
      parallel = TRUE
    )
    
    predictions <- as.data.frame(predictions$pvals)
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv1[[j]] <- dados_temp_na2
    predictions_listRRM1.cv1[[j]] <- predictions
  }
  
  dados_temp_na2_listRRM2.cv1[[i]] <- dados_temp_na2_listRRM1.cv1
  predictions_listRRM2.cv1[[i]] <- predictions_listRRM1.cv1
  model2[[i]] <- model1
  
  save(
    model1, model2,
    predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
    predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
    file = "RRM_heladasveg_50rep_10122025_cv1.rda"
  )
  
}

save(
  model1, model2,
  predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
  predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
  file = "RRM_heladasveg_50rep_10122025_cv1.rda"
)

###############################################################################################################
##VET
library(asreml)

load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)

dados_temp_na2_listRRM1.cv1 <- list()
predictions_listRRM1.cv1 <- list()
predictions_listRRM2.cv1 <- list()
dados_temp_na2_listRRM2.cv1 <- list()
model1 <- list()
model2 <- list()

## CV1 GbyE
fold <- 5
n_rep <- 50

for (i in 1:n_rep) {
  dados_temp <- data_RRM
  cult <- unique(dados_temp$cultivar)
  
  random_gen <- sample(cult, length(cult), replace = FALSE)
  
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  for (j in 1:fold) {
    dados_temp <- data_RRM
    dados_temp$Yield_BLUP_CTE2 <- dados_temp$Yield_BLUP_CTE
    dados_temp$blup.weight2 <- dados_temp$blup.weight
    
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "Yield_BLUP_CTE"] <- NA
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "blup.weight"] <- NA
    
    model <- asreml(
      fixed = Yield_BLUP_CTE ~ 1 +evapoveg,
      random = ~ environ:cultivar+environ:trial+
        str(~ vm(cultivar, GINV.sparse$Ginv.sparse) +
              evapoveg: vm(cultivar, GINV.sparse$Ginv.sparse),
            vmodel = ~ us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
      weights = blup.weight,
      data = dados_temp,
      workspace = "32Gb",
      family = asr_gaussian(dispersion = 1.0),
      na.action = na.method(x = "include", y = "include")
    )
    
    model1[[j]] <- model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_BLUP_CTE))
    
    aux_parallel_Init <- as.data.frame(dados_temp_na2[, c(
      'cultivar', 'environ', 'trial', 'cultivar.env.trial', 'evapoveg'
    )])
    
    predictions <- predict.asreml(
      model,
      classify = 'cultivar:evapoveg',
      levels = list(
        'cultivar' = aux_parallel_Init$cultivar,
        'evapoveg' = aux_parallel_Init$evapoveg
      ),
      pworkspace = "16gb",
      parallel = TRUE
    )
    
    predictions <- as.data.frame(predictions$pvals)
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv1[[j]] <- dados_temp_na2
    predictions_listRRM1.cv1[[j]] <- predictions
  }
  
  dados_temp_na2_listRRM2.cv1[[i]] <- dados_temp_na2_listRRM1.cv1
  predictions_listRRM2.cv1[[i]] <- predictions_listRRM1.cv1
  model2[[i]] <- model1
  save(
    model1, model2,
    predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
    predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
    file = "RRM_evapoveg_50rep_10122025_cv1.rda"
  )  
  }

save(
  model1, model2,
  predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
  predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
  file = "RRM_evapoveg_50rep_10122025_cv1.rda"
)
###########################################################################################

##VPP

library(asreml)
load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)

dados_temp_na2_listRRM1.cv1 <- list()
predictions_listRRM1.cv1 <- list()
predictions_listRRM2.cv1 <- list()
dados_temp_na2_listRRM2.cv1 <- list()
model1 <- list()
model2 <- list()

## CV1 GbyE
fold <- 5
n_rep <- 50

for (i in 1:n_rep) {
  dados_temp <- data_RRM
  cult <- unique(dados_temp$cultivar)
  
  random_gen <- sample(cult, length(cult), replace = FALSE)
  
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  for (j in 1:fold) {
    dados_temp <- data_RRM
    dados_temp$Yield_BLUP_CTE2 <- dados_temp$Yield_BLUP_CTE
    dados_temp$blup.weight2 <- dados_temp$blup.weight
    
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "Yield_BLUP_CTE"] <- NA
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "blup.weight"] <- NA
    
    model <- asreml(
      fixed = Yield_BLUP_CTE ~ 1 + mmacumuladaveg ,
      random = ~ environ:cultivar+environ:trial+
        str(~ vm(cultivar, GINV.sparse$Ginv.sparse) +
              mmacumuladaveg : vm(cultivar, GINV.sparse$Ginv.sparse),
            vmodel = ~ us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
      weights = blup.weight,
      data = dados_temp,
      workspace = "32Gb",
      family = asr_gaussian(dispersion = 1.0),
      na.action = na.method(x = "include", y = "include")
    )
    
    model1[[j]] <- model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_BLUP_CTE))
    
    aux_parallel_Init <- as.data.frame(dados_temp_na2[, c(
      'cultivar', 'environ', 'trial', 'cultivar.env.trial', 'mmacumuladaveg'
    )])
    
    predictions <- predict.asreml(
      model,
      classify = 'cultivar:mmacumuladaveg',
      levels = list(
        'cultivar' = aux_parallel_Init$cultivar,
        'mmacumuladaveg' = aux_parallel_Init$mmacumuladaveg
      ),
      pworkspace = "16gb",
      parallel = TRUE
    )
    
    predictions <- as.data.frame(predictions$pvals)
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv1[[j]] <- dados_temp_na2
    predictions_listRRM1.cv1[[j]] <- predictions
  }
  
  dados_temp_na2_listRRM2.cv1[[i]] <- dados_temp_na2_listRRM1.cv1
  predictions_listRRM2.cv1[[i]] <- predictions_listRRM1.cv1
  model2[[i]] <- model1
  save(
    model1, model2,
    predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
    predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
    file = "RRM_mmacumuladaveg_50rep_10122025_cv1.rda"
  )
  
}

save(
  model1, model2,
  predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
  predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
  file = "RRM_mmacumuladaveg_50rep_10122025_cv1.rda"
)
###################################################################################


##RPP

library(asreml)
load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)

dados_temp_na2_listRRM1.cv1 <- list()
predictions_listRRM1.cv1 <- list()
predictions_listRRM2.cv1 <- list()
dados_temp_na2_listRRM2.cv1 <- list()
model1 <- list()
model2 <- list()

## CV1 GbyE
fold <- 5
n_rep <- 50

for (i in 1:n_rep) {
  dados_temp <- data_RRM
  cult <- unique(dados_temp$cultivar)
  
  random_gen <- sample(cult, length(cult), replace = FALSE)
  
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  for (j in 1:fold) {
    dados_temp <- data_RRM
    dados_temp$Yield_BLUP_CTE2 <- dados_temp$Yield_BLUP_CTE
    dados_temp$blup.weight2 <- dados_temp$blup.weight
    
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "Yield_BLUP_CTE"] <- NA
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "blup.weight"] <- NA
    
    model <- asreml(
      fixed = Yield_BLUP_CTE ~ 1 + mmacumuladaflor ,
      random = ~ environ:cultivar+environ:trial+
        str(~ vm(cultivar, GINV.sparse$Ginv.sparse) +
              mmacumuladaflor : vm(cultivar, GINV.sparse$Ginv.sparse),
            vmodel = ~ us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
      weights = blup.weight,
      data = dados_temp,
      workspace = "32Gb",
      family = asr_gaussian(dispersion = 1.0),
      na.action = na.method(x = "include", y = "include")
    )
    
    model1[[j]] <- model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_BLUP_CTE))
    
    aux_parallel_Init <- as.data.frame(dados_temp_na2[, c(
      'cultivar', 'environ', 'trial', 'cultivar.env.trial', 'mmacumuladaflor'
    )])
    
    predictions <- predict.asreml(
      model,
      classify = 'cultivar:mmacumuladaflor',
      levels = list(
        'cultivar' = aux_parallel_Init$cultivar,
        'mmacumuladaflor' = aux_parallel_Init$mmacumuladaflor
      ),
      pworkspace = "16gb",
      parallel = TRUE
    )
    
    predictions <- as.data.frame(predictions$pvals)
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv1[[j]] <- dados_temp_na2
    predictions_listRRM1.cv1[[j]] <- predictions
  }
  
  dados_temp_na2_listRRM2.cv1[[i]] <- dados_temp_na2_listRRM1.cv1
  predictions_listRRM2.cv1[[i]] <- predictions_listRRM1.cv1
  model2[[i]] <- model1
  save(
    model1, model2,
    predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
    predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
    file = "RRM_mmacumuladaflor_50rep_10122025_cv1.rda"
  )
}

save(
  model1, model2,
  predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
  predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
  file = "RRM_mmacumuladaflor_50rep_10122025_cv1.rda"
)

###############################################################################
library(asreml)

load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)

dados_temp_na2_listRRM1.cv1 <- list()
predictions_listRRM1.cv1 <- list()
predictions_listRRM2.cv1 <- list()
dados_temp_na2_listRRM2.cv1 <- list()
model1 <- list()
model2 <- list()

## CV1 GbyE
fold <- 5
n_rep <- 50

for (i in 1:n_rep) {
  dados_temp <- data_RRM
  cult <- unique(dados_temp$cultivar)
  
  random_gen <- sample(cult, length(cult), replace = FALSE)
  
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  for (j in 1:fold) {
    dados_temp <- data_RRM
    dados_temp$Yield_BLUP_CTE2 <- dados_temp$Yield_BLUP_CTE
    dados_temp$blup.weight2 <- dados_temp$blup.weight
    
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "Yield_BLUP_CTE"] <- NA
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "blup.weight"] <- NA
    
    model <- asreml(
      fixed = Yield_BLUP_CTE ~ 1 + meanmintempveg,
      random = ~ environ:cultivar+environ:trial+
        str(~ vm(cultivar, GINV.sparse$Ginv.sparse) +
              meanmintempveg: vm(cultivar, GINV.sparse$Ginv.sparse),
            vmodel = ~ us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
      weights = blup.weight,
      data = dados_temp,
      workspace = "32Gb",
      family = asr_gaussian(dispersion = 1.0),
      na.action = na.method(x = "include", y = "include")
    )
    
    model1[[j]] <- model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_BLUP_CTE))
    
    aux_parallel_Init <- as.data.frame(dados_temp_na2[, c(
      'cultivar', 'environ', 'trial', 'cultivar.env.trial', 'meanmintempveg'
    )])
    
    predictions <- predict.asreml(
      model,
      classify = 'cultivar:meanmintempveg',
      levels = list(
        'cultivar' = aux_parallel_Init$cultivar,
        'meanmintempveg' = aux_parallel_Init$meanmintempveg
      ),
      pworkspace = "16gb",
      parallel = TRUE
    )
    
    predictions <- as.data.frame(predictions$pvals)
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv1[[j]] <- dados_temp_na2
    predictions_listRRM1.cv1[[j]] <- predictions
  }
  
  dados_temp_na2_listRRM2.cv1[[i]] <- dados_temp_na2_listRRM1.cv1
  predictions_listRRM2.cv1[[i]] <- predictions_listRRM1.cv1
  model2[[i]] <- model1
  save(
    model1, model2,
    predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
    predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
    file = "RRM_ meanmintempveg_50rep_10122025_cv1.rda"
  )

}

save(
  model1, model2,
  predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
  predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
  file = "RRM_ meanmintempveg_50rep_10122025_cv1.rda"
)

########################################################################################
library(asreml)

load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)

dados_temp_na2_listRRM1.cv1 <- list()
predictions_listRRM1.cv1 <- list()
predictions_listRRM2.cv1 <- list()
dados_temp_na2_listRRM2.cv1 <- list()
model1 <- list()
model2 <- list()

## CV1 GbyE
fold <- 5
n_rep <- 50

for (i in 1:n_rep) {
  dados_temp <- data_RRM
  cult <- unique(dados_temp$cultivar)
  
  random_gen <- sample(cult, length(cult), replace = FALSE)
  
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  for (j in 1:fold) {
    dados_temp <- data_RRM
    dados_temp$Yield_BLUP_CTE2 <- dados_temp$Yield_BLUP_CTE
    dados_temp$blup.weight2 <- dados_temp$blup.weight
    
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "Yield_BLUP_CTE"] <- NA
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "blup.weight"] <- NA
    
    model <- asreml(
      fixed = Yield_BLUP_CTE ~ 1 + meantempveg,
      random = ~ environ:cultivar+environ:trial+
        str(~ vm(cultivar, GINV.sparse$Ginv.sparse) +
              meantempveg: vm(cultivar, GINV.sparse$Ginv.sparse),
            vmodel = ~ us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
      weights = blup.weight,
      data = dados_temp,
      workspace = "32Gb",
      family = asr_gaussian(dispersion = 1.0),
      na.action = na.method(x = "include", y = "include")
    )
    
    model1[[j]] <- model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_BLUP_CTE))
    
    aux_parallel_Init <- as.data.frame(dados_temp_na2[, c(
      'cultivar', 'environ', 'trial', 'cultivar.env.trial', 'meantempveg'
    )])
    
    predictions <- predict.asreml(
      model,
      classify = 'cultivar:meantempveg',
      levels = list(
        'cultivar' = aux_parallel_Init$cultivar,
        'meantempveg' = aux_parallel_Init$meantempveg
      ),
      pworkspace = "16gb",
      parallel = TRUE
    )
    
    predictions <- as.data.frame(predictions$pvals)
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv1[[j]] <- dados_temp_na2
    predictions_listRRM1.cv1[[j]] <- predictions
  }
  
  dados_temp_na2_listRRM2.cv1[[i]] <- dados_temp_na2_listRRM1.cv1
  predictions_listRRM2.cv1[[i]] <- predictions_listRRM1.cv1
  model2[[i]] <- model1
  save(
    model1, model2,
    predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
    predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
    file = "RRM_ meantempveg_50rep_10122025_cv1.rda"
  )
}

save(
  model1, model2,
  predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
  predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
  file = "RRM_ meantempveg_35rep_10122025_cv1.rda"
)
##########################################################################################

library(asreml)

load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)

dados_temp_na2_listRRM1.cv1 <- list()
predictions_listRRM1.cv1 <- list()
predictions_listRRM2.cv1 <- list()
dados_temp_na2_listRRM2.cv1 <- list()
model1 <- list()
model2 <- list()

## CV1 GbyE
fold <- 5
n_rep <- 50

for (i in 1:n_rep) {
  dados_temp <- data_RRM
  cult <- unique(dados_temp$cultivar)
  
  random_gen <- sample(cult, length(cult), replace = FALSE)
  
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  for (j in 1:fold) {
    dados_temp <- data_RRM
    dados_temp$Yield_BLUP_CTE2 <- dados_temp$Yield_BLUP_CTE
    dados_temp$blup.weight2 <- dados_temp$blup.weight
    
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "Yield_BLUP_CTE"] <- NA
    dados_temp[dados_temp$cultivar %in% split_geno[[j]], "blup.weight"] <- NA
    
    model <- asreml(
      fixed = Yield_BLUP_CTE ~ 1 + meanmaxtempveg ,
      random = ~ environ:cultivar+environ:trial+
        str(~ vm(cultivar, GINV.sparse$Ginv.sparse) +
              meanmaxtempveg:vm(cultivar, GINV.sparse$Ginv.sparse),
            vmodel = ~ us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
      weights = blup.weight,
      data = dados_temp,
      workspace = "32Gb",
      family = asr_gaussian(dispersion = 1.0),
      na.action = na.method(x = "include", y = "include")
    )
    
    model1[[j]] <- model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_BLUP_CTE))
    
    aux_parallel_Init <- as.data.frame(dados_temp_na2[, c(
      'cultivar', 'environ', 'trial', 'cultivar.env.trial', 'meanmaxtempveg'
    )])
    
    predictions <- predict.asreml(
      model,
      classify = 'cultivar:meanmaxtempveg',
      levels = list(
        'cultivar' = aux_parallel_Init$cultivar,
        'meanmaxtempveg' = aux_parallel_Init$meanmaxtempveg
      ),
      pworkspace = "16gb",
      parallel = TRUE
    )
    
    predictions <- as.data.frame(predictions$pvals)
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv1[[j]] <- dados_temp_na2
    predictions_listRRM1.cv1[[j]] <- predictions
  }
  
  dados_temp_na2_listRRM2.cv1[[i]] <- dados_temp_na2_listRRM1.cv1
  predictions_listRRM2.cv1[[i]] <- predictions_listRRM1.cv1
  model2[[i]] <- model1
  save(
    model1, model2,
    predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
    predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
    file = "RRM_meanmaxtempveg_50rep_10122025_cv1.rda"
  )
}

save(
  model1, model2,
  predictions_listRRM1.cv1, dados_temp_na2_listRRM1.cv1,
  predictions_listRRM2.cv1, dados_temp_na2_listRRM2.cv1,
  file = "RRM_meanmaxtempveg_50rep_10122025_cv1.rda"
)

###############################################################

