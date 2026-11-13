
########

##VFD
load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

library(asreml)

data_RRM$cultivar<-as.factor(data_RRM$cultivar)
data_RRM$environ<-as.factor(data_RRM$environ)

fold <- 5 # K-fold
n_rep <- 50

list_results <- list()
dados_temp_na2_listRRM1.cv2<-list()
predictions_listRRM1.cv2<-list()
predictions_listRRM2.cv2<-list()
dados_temp_na2_listRRM2.cv2<-list()
model1<-list()
model2<-list()

for(i in 1:n_rep){
  dados_temp <- data_RRM
  dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
  dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
  cult <- unique(dados_temp$cultivarenviron)
  
  random_gen <- sample(cult, length(cult), replace = F)
  
  # Crear tamaño de los folders 
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  # Dataframe para acumular predicciones y observados
  all_predictions <- data.frame()
  
  # Proceso de validación cruzada
  for(j in 1:fold){
    dados_temp <- data_RRM
    dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
    dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
   

     dados_temp$Yield_LSM2 <- dados_temp$Yield_LSM
    dados_temp$weight3 <- dados_temp$weight2
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "Yield_LSM"] <- NA
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "weight2"] <- NA
    
 
    model <- asreml(fixed = Yield_LSM  ~ 1+heladasveg, 
                    random = ~environ:cultivar +environ:trial+ str(~vm(cultivar, GINV.sparse$Ginv.sparse)+
                                                                     heladasveg:vm(cultivar, GINV.sparse$Ginv.sparse),
                                              vmodel=~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
                    weights =weight2,
                    data =dados_temp,
                    workspace = "16Gb",
                    family = asr_gaussian(dispersion = 1.0),
                    na.action = na.method(x = "include",y="include"))
    
    model1[[j]]<-model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_LSM))
    
    aux_parallel_Init <-as.data.frame((dados_temp_na2[,c( 'cultivar','environ','trial','cultivar.env.trial', 'heladasveg')]))
    
    predictions <- predict.asreml(model,classify="cultivar:heladasveg", levels = list("cultivar"=aux_parallel_Init$cultivar,
                                                                                      "heladasveg"=aux_parallel_Init$heladasveg
    ),pworkspace="8Gb",parallel = T)
    
    predictions<-as.data.frame(predictions$pvals)
    
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv2[[j]] <-  dados_temp_na2
    predictions_listRRM1.cv2[[j]] <-predictions
    
  }
  dados_temp_na2_listRRM2.cv2[[i]] <-   dados_temp_na2_listRRM1.cv2
  predictions_listRRM2.cv2[[i]] <-predictions_listRRM1.cv2
  model2[[i]]<-model1
  
  save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_heladasveg_50_rep_cv2_20122025.rda")
  
  
}
save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_heladasveg_50_rep_cv2_20122025.rda")

###############################################################################################################################################
###VET
load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

library(asreml)

data_RRM$cultivar<-as.factor(data_RRM$cultivar)
data_RRM$environ<-as.factor(data_RRM$environ)

fold <- 5 # K-fold
n_rep <- 50

list_results <- list()
dados_temp_na2_listRRM1.cv2<-list()
predictions_listRRM1.cv2<-list()
predictions_listRRM2.cv2<-list()
dados_temp_na2_listRRM2.cv2<-list()
model1<-list()
model2<-list()

for(i in 1:n_rep){
  dados_temp <- data_RRM
  dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
  dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
  cult <- unique(dados_temp$cultivarenviron)
  
  random_gen <- sample(cult, length(cult), replace = F)
  
  # Crear tamaño de los folders 
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  # Dataframe para acumular predicciones y observados
  all_predictions <- data.frame()
  
  # Proceso de validación cruzada
  for(j in 1:fold){
    dados_temp <- data_RRM
    dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
    dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
    
    dados_temp$Yield_LSM2 <- dados_temp$Yield_LSM
    dados_temp$weight3 <- dados_temp$weight2
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "Yield_LSM"] <- NA
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "weight2"] <- NA
    
  
    
    model <- asreml(fixed = Yield_LSM ~ 1+evapoveg, 
                    random = ~environ:cultivar +environ:trial+ str(~vm(cultivar, GINV.sparse$Ginv.sparse)+
                                                                     evapoveg:vm(cultivar, GINV.sparse$Ginv.sparse),
                                                                   vmodel=~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
                    weights = weight2,
                    data =dados_temp,
                    workspace = "16Gb",
                    family = asr_gaussian(dispersion = 1.0),
                    na.action = na.method(x = "include",y="include"))
    
    model1[[j]]<-model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_LSM))
    
    aux_parallel_Init <-as.data.frame((dados_temp_na2[,c( 'cultivar','environ','trial','cultivar.env.trial', 'evapoveg')]))
    
    predictions <- predict.asreml(model,classify="cultivar:evapoveg", levels = list("cultivar"=aux_parallel_Init$cultivar,
                                                                                    "evapoveg"=aux_parallel_Init$evapoveg
    ),pworkspace="8Gb",parallel = T)
    
    predictions<-as.data.frame(predictions$pvals)
    
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv2[[j]] <-  dados_temp_na2
    predictions_listRRM1.cv2[[j]] <-predictions
    
  }
  dados_temp_na2_listRRM2.cv2[[i]] <-   dados_temp_na2_listRRM1.cv2
  predictions_listRRM2.cv2[[i]] <-predictions_listRRM1.cv2
  model2[[i]]<-model1
  save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_evapoveg_50_rep_cv2_20122025.rda")  
  
  
}
save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_evapoveg_50_rep_cv2_20122025.rda")
############################################################################################################################
##VPP
load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

library(asreml)

data_RRM$cultivar<-as.factor(data_RRM$cultivar)
data_RRM$environ<-as.factor(data_RRM$environ)

fold <- 5 # K-fold
n_rep <- 50

list_results <- list()
dados_temp_na2_listRRM1.cv2<-list()
predictions_listRRM1.cv2<-list()
predictions_listRRM2.cv2<-list()
dados_temp_na2_listRRM2.cv2<-list()
model1<-list()
model2<-list()

for(i in 1:n_rep){
  dados_temp <- data_RRM
  dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
  dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
  cult <- unique(dados_temp$cultivarenviron)
  
  random_gen <- sample(cult, length(cult), replace = F)
  
  # Crear tamaño de los folders 
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  # Dataframe para acumular predicciones y observados
  all_predictions <- data.frame()
  
  # Proceso de validación cruzada
  for(j in 1:fold){
    dados_temp <- data_RRM
    dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
    dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
    
    dados_temp$Yield_LSM2 <- dados_temp$Yield_LSM
    dados_temp$weight3 <- dados_temp$weight2
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "Yield_LSM"] <- NA
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "weight2"] <- NA
  
    
    
    model <- asreml(fixed = Yield_LSM ~ 1+mmacumuladaveg, 
                    random = ~environ:cultivar +environ:trial+ str(~vm(cultivar, GINV.sparse$Ginv.sparse)+
                                                                     mmacumuladaveg:vm(cultivar, GINV.sparse$Ginv.sparse),
                                                                   vmodel=~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
                    weights = weight2,
                    data =dados_temp,
                    workspace = "16Gb",
                    family = asr_gaussian(dispersion = 1.0),
                    na.action = na.method(x = "include",y="include"))
    
    model1[[j]]<-model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_LSM))
    
    aux_parallel_Init <-as.data.frame((dados_temp_na2[,c( 'cultivar','environ','trial','cultivar.env.trial', 'mmacumuladaveg')]))
    
    predictions <- predict.asreml(model,classify="cultivar:mmacumuladaveg", levels = list("cultivar"=aux_parallel_Init$cultivar,
                                                                                          "mmacumuladaveg"=aux_parallel_Init$mmacumuladaveg
    ),pworkspace="8Gb",parallel = T)
    
    predictions<-as.data.frame(predictions$pvals)
    
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv2[[j]] <-  dados_temp_na2
    predictions_listRRM1.cv2[[j]] <-predictions
    
  }
  dados_temp_na2_listRRM2.cv2[[i]] <-   dados_temp_na2_listRRM1.cv2
  predictions_listRRM2.cv2[[i]] <-predictions_listRRM1.cv2
  model2[[i]]<-model1
  
  save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_mmacumuladaveg_50_rep_20122025_cv2.rda")
  
}
save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_mmacumuladaveg_50_rep_20122025_cv2.rda")


#################################################################################################################################################################################################
##RPP
load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

library(asreml)

data_RRM$cultivar<-as.factor(data_RRM$cultivar)
data_RRM$environ<-as.factor(data_RRM$environ)

fold <- 5 # K-fold
n_rep <- 50

list_results <- list()
dados_temp_na2_listRRM1.cv2<-list()
predictions_listRRM1.cv2<-list()
predictions_listRRM2.cv2<-list()
dados_temp_na2_listRRM2.cv2<-list()
model1<-list()
model2<-list()

for(i in 1:n_rep){
  dados_temp <- data_RRM
  dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
  dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
  cult <- unique(dados_temp$cultivarenviron)
  
  random_gen <- sample(cult, length(cult), replace = F)
  
  # Crear tamaño de los folders 
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  # Dataframe para acumular predicciones y observados
  all_predictions <- data.frame()
  
  # Proceso de validación cruzada
  for(j in 1:fold){
    dados_temp <- data_RRM
    dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
    dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
    dados_temp$Yield_LSM2 <- dados_temp$Yield_LSM
    dados_temp$weight3 <- dados_temp$weight2
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "Yield_LSM"] <- NA
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "weight2"] <- NA
    
    
    model <- asreml(fixed = Yield_LSM~ 1+mmacumuladaflor, 
                    random = ~environ:cultivar +environ:trial+ str(~vm(cultivar, GINV.sparse$Ginv.sparse)+
                                                                     mmacumuladaflor:vm(cultivar, GINV.sparse$Ginv.sparse),
                                                                   vmodel=~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
                    weights =weight2,
                    data =dados_temp,
                    workspace = "16Gb",
                    family = asr_gaussian(dispersion = 1.0),
                    na.action = na.method(x = "include",y="include"))
    
    model1[[j]]<-model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_LSM))
    
    aux_parallel_Init <-as.data.frame((dados_temp_na2[,c( 'cultivar','environ','trial','cultivar.env.trial', 'mmacumuladaflor')]))
    
    predictions <- predict.asreml(model,classify="cultivar:mmacumuladaflor", levels = list("cultivar"=aux_parallel_Init$cultivar,
                                                                                           "mmacumuladaflor"=aux_parallel_Init$mmacumuladaflor
    ),pworkspace="8Gb",parallel = T)
    
    predictions<-as.data.frame(predictions$pvals)
    
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv2[[j]] <-  dados_temp_na2
    predictions_listRRM1.cv2[[j]] <-predictions
    
  }
  dados_temp_na2_listRRM2.cv2[[i]] <-   dados_temp_na2_listRRM1.cv2
  predictions_listRRM2.cv2[[i]] <-predictions_listRRM1.cv2
  model2[[i]]<-model1
  
  save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_mmacumuladaflor_50_rep_20122025_cv2.rda")
  
  
}

save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_mmacumuladaflor_50_rep_20122025_cv2.rda")

###################################################################################################################################

##VTmin
load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

library(asreml)

data_RRM$cultivar<-as.factor(data_RRM$cultivar)
data_RRM$environ<-as.factor(data_RRM$environ)

fold <- 5 # K-fold
n_rep <- 50

list_results <- list()
dados_temp_na2_listRRM1.cv2<-list()
predictions_listRRM1.cv2<-list()
predictions_listRRM2.cv2<-list()
dados_temp_na2_listRRM2.cv2<-list()
model1<-list()
model2<-list()

for(i in 1:n_rep){
  dados_temp <- data_RRM
  dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
  dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
  cult <- unique(dados_temp$cultivarenviron)
  
  random_gen <- sample(cult, length(cult), replace = F)
  
  # Crear tamaño de los folders 
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  # Dataframe para acumular predicciones y observados
  all_predictions <- data.frame()
  
  # Proceso de validación cruzada
  for(j in 1:fold){
    dados_temp <- data_RRM
    dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
    dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
    dados_temp$Yield_LSM2 <- dados_temp$Yield_LSM
    dados_temp$weight3 <- dados_temp$weight2
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "Yield_LSM"] <- NA
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "weight2"] <- NA
    
    
    model <- asreml(fixed = Yield_LSM~ 1+meanmintempveg, 
                    random = ~environ:cultivar +environ:trial+ str(~vm(cultivar, GINV.sparse$Ginv.sparse)+
                                                                     meanmintempveg:vm(cultivar, GINV.sparse$Ginv.sparse),
                                                                   vmodel=~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
                    weights =weight2,
                    data =dados_temp,
                    workspace = "16Gb",
                    family = asr_gaussian(dispersion = 1.0),
                    na.action = na.method(x = "include",y="include"))
    
    model1[[j]]<-model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_LSM))
    
    aux_parallel_Init <-as.data.frame((dados_temp_na2[,c( 'cultivar','environ','trial','cultivar.env.trial', 'meanmintempveg')]))
    
    predictions <- predict.asreml(model,classify="cultivar:meanmintempveg", levels = list("cultivar"=aux_parallel_Init$cultivar,
                                                                                           "meanmintempveg"=aux_parallel_Init$meanmintempveg
    ),pworkspace="8Gb",parallel = T)
    
    predictions<-as.data.frame(predictions$pvals)
    
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv2[[j]] <-  dados_temp_na2
    predictions_listRRM1.cv2[[j]] <-predictions
    
  }
  dados_temp_na2_listRRM2.cv2[[i]] <-   dados_temp_na2_listRRM1.cv2
  predictions_listRRM2.cv2[[i]] <-predictions_listRRM1.cv2
  model2[[i]]<-model1
  
  save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_meanmintempveg_50_rep_20122025_cv2.rda")
  
  
}

save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_meanmintempveg_50_rep_20122025_cv2.rda")
#####################################################################################################################################################################################
##VTmean
load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

library(asreml)

data_RRM$cultivar<-as.factor(data_RRM$cultivar)
data_RRM$environ<-as.factor(data_RRM$environ)

fold <- 5 # K-fold
n_rep <- 50

list_results <- list()
dados_temp_na2_listRRM1.cv2<-list()
predictions_listRRM1.cv2<-list()
predictions_listRRM2.cv2<-list()
dados_temp_na2_listRRM2.cv2<-list()
model1<-list()
model2<-list()

for(i in 1:n_rep){
  dados_temp <- data_RRM
  dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
  dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
  cult <- unique(dados_temp$cultivarenviron)
  
  random_gen <- sample(cult, length(cult), replace = F)
  
  # Crear tamaño de los folders 
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  # Dataframe para acumular predicciones y observados
  all_predictions <- data.frame()
  
  # Proceso de validación cruzada
  for(j in 1:fold){
    dados_temp <- data_RRM
    dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
    dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
    
    dados_temp$Yield_LSM2 <- dados_temp$Yield_LSM
    dados_temp$weight3 <- dados_temp$weight2
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "Yield_LSM"] <- NA
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "weight2"] <- NA
    
    
    model <- asreml(fixed = Yield_LSM~ 1+meantempveg, 
                    random = ~environ:cultivar +environ:trial+ str(~vm(cultivar, GINV.sparse$Ginv.sparse)+
                                                                     meantempveg:vm(cultivar, GINV.sparse$Ginv.sparse),
                                                                   vmodel=~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
                    weights = weight2,
                    data =dados_temp,
                    workspace = "16Gb",
                    family = asr_gaussian(dispersion = 1.0),
                    na.action = na.method(x = "include",y="include"))
    
    model1[[j]]<-model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_BLUP_CTE))
    
    aux_parallel_Init <-as.data.frame((dados_temp_na2[,c( 'cultivar','environ','trial','cultivar.env.trial', 'meantempveg')]))
    
    predictions <- predict.asreml(model,classify="cultivar:meantempveg", levels = list("cultivar"=aux_parallel_Init$cultivar,
                                                                                          "meantempveg"=aux_parallel_Init$meantempveg
    ),pworkspace="8Gb",parallel = T)
    
    predictions<-as.data.frame(predictions$pvals)
    
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv2[[j]] <-  dados_temp_na2
    predictions_listRRM1.cv2[[j]] <-predictions
    
  }
  dados_temp_na2_listRRM2.cv2[[i]] <-   dados_temp_na2_listRRM1.cv2
  predictions_listRRM2.cv2[[i]] <-predictions_listRRM1.cv2
  model2[[i]]<-model1
  
  save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_meantempveg_50_rep_20122025_cv2.rda")
  
  
}

save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_meantempveg_50_rep_20122025_cv2.rda")

#####################################################################################################################
##VTmax
load("data_RRM_filtered_FA2_0508.rda")
load("GINV.sparse.rda")

library(asreml)

data_RRM$cultivar<-as.factor(data_RRM$cultivar)
data_RRM$environ<-as.factor(data_RRM$environ)

fold <- 5 # K-fold
n_rep <- 50

list_results <- list()
dados_temp_na2_listRRM1.cv2<-list()
predictions_listRRM1.cv2<-list()
predictions_listRRM2.cv2<-list()
dados_temp_na2_listRRM2.cv2<-list()
model1<-list()
model2<-list()

for(i in 1:n_rep){
  dados_temp <- data_RRM
  dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
  dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
  cult <- unique(dados_temp$cultivarenviron)
  
  random_gen <- sample(cult, length(cult), replace = F)
  
  # Crear tamaño de los folders 
  remain <- length(cult) %% fold
  per_folder <- floor(length(cult) / fold)
  folder_size <- rep(per_folder, fold)
  folder_size <- folder_size + rep(c(1, 0), c(remain, fold - remain))
  folder <- rep(1:fold, times = folder_size)
  
  split_geno <- split(random_gen, folder)
  split_geno <- lapply(split_geno, droplevels)
  
  # Dataframe para acumular predicciones y observados
  all_predictions <- data.frame()
  
  # Proceso de validación cruzada
  for(j in 1:fold){
    dados_temp <- data_RRM
    dados_temp$cultivarenviron <- paste(dados_temp$cultivar, dados_temp$environ, sep = "")
    dados_temp$cultivarenviron <- as.factor(dados_temp$cultivarenviron)
    dados_temp$Yield_LSM2 <- dados_temp$Yield_LSM
    dados_temp$weight3 <- dados_temp$weight2
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "Yield_LSM"] <- NA
    dados_temp[dados_temp$cultivarenviron %in% split_geno[[j]], "weight2"] <- NA
    
    
    model <- asreml(fixed = Yield_LSM ~ 1+meanmaxtempveg, 
                    random = ~environ:cultivar +environ:trial+ str(~vm(cultivar, GINV.sparse$Ginv.sparse)+
                                                                     meanmaxtempveg:vm(cultivar, GINV.sparse$Ginv.sparse),
                                                                   vmodel=~us(2):vm(cultivar, GINV.sparse$Ginv.sparse)),
                    weights = weight2,
                    data =dados_temp,
                    workspace = "16Gb",
                    family = asr_gaussian(dispersion = 1.0),
                    na.action = na.method(x = "include",y="include"))
    
    model1[[j]]<-model
    dados_temp_na2 <- subset(dados_temp, is.na(Yield_LSM))
    
    aux_parallel_Init <-as.data.frame((dados_temp_na2[,c( 'cultivar','environ','trial','cultivar.env.trial', 'meanmaxtempveg')]))
    
    predictions <- predict.asreml(model,classify="cultivar:meanmaxtempveg", levels = list("cultivar"=aux_parallel_Init$cultivar,
                                                                                       "meanmaxtempveg"=aux_parallel_Init$meanmaxtempveg
    ),pworkspace="8Gb",parallel = T)
    
    predictions<-as.data.frame(predictions$pvals)
    
    predictions$cultivar.environ.trial <- aux_parallel_Init$cultivar.env.trial
    
    dados_temp_na2_listRRM1.cv2[[j]] <-  dados_temp_na2
    predictions_listRRM1.cv2[[j]] <-predictions
    
  }
  dados_temp_na2_listRRM2.cv2[[i]] <-   dados_temp_na2_listRRM1.cv2
  predictions_listRRM2.cv2[[i]] <-predictions_listRRM1.cv2
  model2[[i]]<-model1
  
  save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_meanmaxtempveg_50_rep_20122025_cv2.rda")
  
  
}

save(model1, model2, predictions_listRRM1.cv2,dados_temp_na2_listRRM1.cv2,predictions_listRRM2.cv2,dados_temp_na2_listRRM2.cv2,file="RRM_meanmaxtempveg_50_rep_20122025_cv2.rda")


