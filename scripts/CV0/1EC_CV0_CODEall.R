##########################################
setwd("C:/Users/Ines/Downloads/data/new analysis_GxE paper with EC/cross validation/RRM_new analysis_20may2025/CV0/1 EC")

load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$environ.trial <- paste(data_RRM$environ, data_RRM$trial, sep = "_")
data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)




library(asreml)

dados_temp_na2_listRRM1.cv0 <- list()
predictions_listRRM1.cv0 <- list()
dados_temp_na2_listRRM2.cv0 <- list()
predictions_listRRM2.cv0 <- list()
model1 <- list()
model2 <- list()
failed_trials <- list()  # To store trials that failed

environ_trials_to_leaveout <- unique(data_RRM$environ)


train_data <- data_RRM
train_data$Yield_BLUP_CTE2 <- train_data$Yield_BLUP_CTE
n_reps <- 1  # Number of repetitions

for (rep in 1:n_reps) {
  for (trial in environ_trials_to_leaveout) {
    tryCatch({
      cat("Running trial:", trial, "\n")  # Track progress
      
      train_data <- data_RRM
      train_data$Yield_BLUP_CTE2 <- train_data$Yield_BLUP_CTE  # Backup
      train_data$Yield_BLUP_CTE[train_data$environ == trial] <- NA
      train_data$blup.weight[train_data$environ == trial] <- NA
    
      model <- asreml(
        fixed = Yield_BLUP_CTE ~ 1 + meanmaxtempveg, 
        random = ~environ:cultivar + environ:trial +
          str(~vm(cultivar, GINV.sparse$Ginv.sparse) +
                meanmaxtempveg:vm(cultivar, GINV.sparse$Ginv.sparse),
              vmodel = ~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
        weights = blup.weight,
        data = train_data,
        workspace = "32Gb",
        family = asr_gaussian(dispersion = 1.0),
        na.action = na.method(x = "include", y = "include")
      )
      
      model1[[trial]] <- model
      dados_temp_na2 <- subset(train_data, is.na(Yield_BLUP_CTE))
      
      # FIXED: Missing closing parenthesis
      aux_parallel_Init <- as.data.frame(dados_temp_na2[, c('cultivar', 'environ', 'trial', 'cultivar.env.trial', 'meanmaxtempveg')])
      
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
      
      dados_temp_na2_listRRM1.cv0[[trial]] <- dados_temp_na2
      predictions_listRRM1.cv0[[trial]] <- predictions
    }, error = function(e) {
      cat("Error in trial", trial, ":", conditionMessage(e), "\n")
      failed_trials[[trial]] <<- conditionMessage(e)  # Store the error
    })
  }
  
  dados_temp_na2_listRRM2.cv0[[rep]] <- dados_temp_na2_listRRM1.cv0
  predictions_listRRM2.cv0[[rep]] <- predictions_listRRM1.cv0
  model2[[rep]] <- model1
}

# Save all objects including failed trial info
save(
  model1, model2,
  predictions_listRRM1.cv0, dados_temp_na2_listRRM1.cv0,
  predictions_listRRM2.cv0, dados_temp_na2_listRRM2.cv0,
  failed_trials,
  file = "RRM_meanmaxtempveg_CVO_new.rda"
)

###########################################################################################




#############################################################################################


##########################################
setwd("C:/Users/Ines/Downloads/data/new analysis_GxE paper with EC/cross validation/RRM_new analysis_20may2025/CV0/1 EC")

load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$environ.trial <- paste(data_RRM$environ, data_RRM$trial, sep = "_")
data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)

library(asreml)

dados_temp_na2_listRRM1.cv0 <- list()
predictions_listRRM1.cv0 <- list()
dados_temp_na2_listRRM2.cv0 <- list()
predictions_listRRM2.cv0 <- list()
model1 <- list()
model2 <- list()
failed_trials <- list()  # To store trials that failed

environ_trials_to_leaveout <- unique(data_RRM$environ)


train_data <- data_RRM
train_data$Yield_BLUP_CTE2 <- train_data$Yield_BLUP_CTE
n_reps <- 1  # Number of repetitions

for (rep in 1:n_reps) {
  for (trial in environ_trials_to_leaveout) {
    tryCatch({
      cat("Running trial:", trial, "\n")  # Track progress
      
      train_data <- data_RRM
      train_data$Yield_BLUP_CTE2 <- train_data$Yield_BLUP_CTE  # Backup
      train_data$Yield_BLUP_CTE[train_data$environ == trial] <- NA
      train_data$blup.weight[train_data$environ == trial] <- NA
      
      model <- asreml(
        fixed = Yield_BLUP_CTE ~ 1 + meanmintempveg, 
        random = ~environ:cultivar + environ:trial +
          str(~vm(cultivar, GINV.sparse$Ginv.sparse) +
                meanmintempveg:vm(cultivar, GINV.sparse$Ginv.sparse),
              vmodel = ~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
        weights = blup.weight,
        data = train_data,
        workspace = "32Gb",
        family = asr_gaussian(dispersion = 1.0),
        na.action = na.method(x = "include", y = "include")
      )
      
      model1[[trial]] <- model
      dados_temp_na2 <- subset(train_data, is.na(Yield_BLUP_CTE))
      
      # FIXED: Missing closing parenthesis
      aux_parallel_Init <- as.data.frame(dados_temp_na2[, c('cultivar', 'environ', 'trial', 'cultivar.env.trial', 'meanmintempveg')])
      
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
      
      dados_temp_na2_listRRM1.cv0[[trial]] <- dados_temp_na2
      predictions_listRRM1.cv0[[trial]] <- predictions
    }, error = function(e) {
      cat("Error in trial", trial, ":", conditionMessage(e), "\n")
      failed_trials[[trial]] <<- conditionMessage(e)  # Store the error
    })
  }
  
  dados_temp_na2_listRRM2.cv0[[rep]] <- dados_temp_na2_listRRM1.cv0
  predictions_listRRM2.cv0[[rep]] <- predictions_listRRM1.cv0
  model2[[rep]] <- model1
}

# Save all objects including failed trial info
save(
  model1, model2,
  predictions_listRRM1.cv0, dados_temp_na2_listRRM1.cv0,
  predictions_listRRM2.cv0, dados_temp_na2_listRRM2.cv0,
  failed_trials,
  file = "RRM_meanmintempveg_CVO_new.rda"
)

##############################################################################

##########################################
setwd("C:/Users/Ines/Downloads/data/new analysis_GxE paper with EC/cross validation/RRM_new analysis_20may2025/CV0/1 EC")

load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$environ.trial <- paste(data_RRM$environ, data_RRM$trial, sep = "_")
data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)

library(asreml)

dados_temp_na2_listRRM1.cv0 <- list()
predictions_listRRM1.cv0 <- list()
dados_temp_na2_listRRM2.cv0 <- list()
predictions_listRRM2.cv0 <- list()
model1 <- list()
model2 <- list()
failed_trials <- list()  # To store trials that failed

environ_trials_to_leaveout <- unique(data_RRM$environ)


train_data <- data_RRM
train_data$Yield_BLUP_CTE2 <- train_data$Yield_BLUP_CTE
n_reps <- 1  # Number of repetitions

for (rep in 1:n_reps) {
  for (trial in environ_trials_to_leaveout) {
    tryCatch({
      cat("Running trial:", trial, "\n")  # Track progress
      
      train_data <- data_RRM
      train_data$Yield_BLUP_CTE2 <- train_data$Yield_BLUP_CTE  # Backup
      train_data$Yield_BLUP_CTE[train_data$environ == trial] <- NA
      train_data$blup.weight[train_data$environ == trial] <- NA
      
      model <- asreml(
        fixed = Yield_BLUP_CTE ~ 1 + meantempveg, 
        random = ~environ:cultivar + environ:trial +
          str(~vm(cultivar, GINV.sparse$Ginv.sparse) +
                meantempveg:vm(cultivar, GINV.sparse$Ginv.sparse),
              vmodel = ~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
        weights = blup.weight,
        data = train_data,
        workspace = "32Gb",
        family = asr_gaussian(dispersion = 1.0),
        na.action = na.method(x = "include", y = "include")
      )
      
      model1[[trial]] <- model
      dados_temp_na2 <- subset(train_data, is.na(Yield_BLUP_CTE))
      
      # FIXED: Missing closing parenthesis
      aux_parallel_Init <- as.data.frame(dados_temp_na2[, c('cultivar', 'environ', 'trial', 'cultivar.env.trial', 'meantempveg')])
      
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
      
      dados_temp_na2_listRRM1.cv0[[trial]] <- dados_temp_na2
      predictions_listRRM1.cv0[[trial]] <- predictions
    }, error = function(e) {
      cat("Error in trial", trial, ":", conditionMessage(e), "\n")
      failed_trials[[trial]] <<- conditionMessage(e)  # Store the error
    })
  }
  
  dados_temp_na2_listRRM2.cv0[[rep]] <- dados_temp_na2_listRRM1.cv0
  predictions_listRRM2.cv0[[rep]] <- predictions_listRRM1.cv0
  model2[[rep]] <- model1
}

# Save all objects including failed trial info
save(
  model1, model2,
  predictions_listRRM1.cv0, dados_temp_na2_listRRM1.cv0,
  predictions_listRRM2.cv0, dados_temp_na2_listRRM2.cv0,
  failed_trials,
  file = "RRM_meantempveg_CVO_new.rda"
)




#########################################################

##########################################
setwd("C:/Users/Ines/Downloads/data/new analysis_GxE paper with EC/cross validation/RRM_new analysis_20may2025/CV0/1 EC")

load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$environ.trial <- paste(data_RRM$environ, data_RRM$trial, sep = "_")
data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)

library(asreml)

dados_temp_na2_listRRM1.cv0 <- list()
predictions_listRRM1.cv0 <- list()
dados_temp_na2_listRRM2.cv0 <- list()
predictions_listRRM2.cv0 <- list()
model1 <- list()
model2 <- list()
failed_trials <- list()  # To store trials that failed

environ_trials_to_leaveout <- unique(data_RRM$environ)


train_data <- data_RRM
train_data$Yield_BLUP_CTE2 <- train_data$Yield_BLUP_CTE
n_reps <- 1  # Number of repetitions

for (rep in 1:n_reps) {
  for (trial in environ_trials_to_leaveout) {
    tryCatch({
      cat("Running trial:", trial, "\n")  # Track progress
      
      train_data <- data_RRM
      train_data$Yield_BLUP_CTE2 <- train_data$Yield_BLUP_CTE  # Backup
      train_data$Yield_BLUP_CTE[train_data$environ == trial] <- NA
      train_data$blup.weight[train_data$environ == trial] <- NA
      
      model <- asreml(
        fixed = Yield_BLUP_CTE ~ 1 +mmacumuladaflor, 
        random = ~environ:cultivar + environ:trial +
          str(~vm(cultivar, GINV.sparse$Ginv.sparse) +
                mmacumuladaflor:vm(cultivar, GINV.sparse$Ginv.sparse),
              vmodel = ~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
        weights = blup.weight,
        data = train_data,
        workspace = "32Gb",
        family = asr_gaussian(dispersion = 1.0),
        na.action = na.method(x = "include", y = "include")
      )
      
      model1[[trial]] <- model
      dados_temp_na2 <- subset(train_data, is.na(Yield_BLUP_CTE))
      
      # FIXED: Missing closing parenthesis
      aux_parallel_Init <- as.data.frame(dados_temp_na2[, c('cultivar', 'environ', 'trial', 'cultivar.env.trial', 'mmacumuladaflor')])
      
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
      
      dados_temp_na2_listRRM1.cv0[[trial]] <- dados_temp_na2
      predictions_listRRM1.cv0[[trial]] <- predictions
    }, error = function(e) {
      cat("Error in trial", trial, ":", conditionMessage(e), "\n")
      failed_trials[[trial]] <<- conditionMessage(e)  # Store the error
    })
  }
  
  dados_temp_na2_listRRM2.cv0[[rep]] <- dados_temp_na2_listRRM1.cv0
  predictions_listRRM2.cv0[[rep]] <- predictions_listRRM1.cv0
  model2[[rep]] <- model1
}

# Save all objects including failed trial info
save(
  model1, model2,
  predictions_listRRM1.cv0, dados_temp_na2_listRRM1.cv0,
  predictions_listRRM2.cv0, dados_temp_na2_listRRM2.cv0,
  failed_trials,
  file = "RRM_mmacumuladaflor_CVO_new.rda"
)


######################################################################

##########################################
setwd("C:/Users/Ines/Downloads/data/new analysis_GxE paper with EC/cross validation/RRM_new analysis_20may2025/CV0/1 EC")

load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

data_RRM$environ.trial <- paste(data_RRM$environ, data_RRM$trial, sep = "_")
data_RRM$cultivar <- as.factor(data_RRM$cultivar)
data_RRM$environ <- as.factor(data_RRM$environ)

library(asreml)

dados_temp_na2_listRRM1.cv0 <- list()
predictions_listRRM1.cv0 <- list()
dados_temp_na2_listRRM2.cv0 <- list()
predictions_listRRM2.cv0 <- list()
model1 <- list()
model2 <- list()
failed_trials <- list()  # To store trials that failed

environ_trials_to_leaveout <- unique(data_RRM$environ)


train_data <- data_RRM
train_data$Yield_BLUP_CTE2 <- train_data$Yield_BLUP_CTE
n_reps <- 1  # Number of repetitions

for (rep in 1:n_reps) {
  for (trial in environ_trials_to_leaveout) {
    tryCatch({
      cat("Running trial:", trial, "\n")  # Track progress
      
      train_data <- data_RRM
      train_data$Yield_BLUP_CTE2 <- train_data$Yield_BLUP_CTE  # Backup
      train_data$Yield_BLUP_CTE[train_data$environ == trial] <- NA
      train_data$blup.weight[train_data$environ == trial] <- NA
      
      model <- asreml(
        fixed = Yield_BLUP_CTE ~ 1 + mmacumuladaveg, 
        random = ~environ:cultivar + environ:trial +
          str(~vm(cultivar, GINV.sparse$Ginv.sparse) +
                mmacumuladaveg:vm(cultivar, GINV.sparse$Ginv.sparse),
              vmodel = ~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
        weights = blup.weight,
        data = train_data,
        workspace = "32Gb",
        family = asr_gaussian(dispersion = 1.0),
        na.action = na.method(x = "include", y = "include")
      )
      
      model1[[trial]] <- model
      dados_temp_na2 <- subset(train_data, is.na(Yield_BLUP_CTE))
      
      # FIXED: Missing closing parenthesis
      aux_parallel_Init <- as.data.frame(dados_temp_na2[, c('cultivar', 'environ', 'trial', 'cultivar.env.trial', 'mmacumuladaveg')])
      
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
      
      dados_temp_na2_listRRM1.cv0[[trial]] <- dados_temp_na2
      predictions_listRRM1.cv0[[trial]] <- predictions
    }, error = function(e) {
      cat("Error in trial", trial, ":", conditionMessage(e), "\n")
      failed_trials[[trial]] <<- conditionMessage(e)  # Store the error
    })
  }
  
  dados_temp_na2_listRRM2.cv0[[rep]] <- dados_temp_na2_listRRM1.cv0
  predictions_listRRM2.cv0[[rep]] <- predictions_listRRM1.cv0
  model2[[rep]] <- model1
}

# Save all objects including failed trial info
save(
  model1, model2,
  predictions_listRRM1.cv0, dados_temp_na2_listRRM1.cv0,
  predictions_listRRM2.cv0, dados_temp_na2_listRRM2.cv0,
  failed_trials,
  file = "RRM_mmacumuladaveg_CVO_new.rda"
)







