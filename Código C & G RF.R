#-------------------------------------------------------------------------------------------
#Instalando pacotes
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")
library(pbapply)
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")
library(EBImage)
library(dplyr)
library(e1071)
cran <- getOption("repos")
cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
options(repos = cran)
install.packages("mxnet")
library(mxnet)
library(caret)
library(ggplot2)
#-------------------------------------------------------------------------------------------
#Vou redimencionar as imagens para um tamanho de 28 x 28 e deixa-las em escala de cinza para serem facilmente lidas pelo R, vou usar uma função para tal.

width <- 28
height <- 28
## pbapply is a library to add progress bar *apply functions
## pblapply will replace lapply
library(pbapply)
extract_feature <- function(dir_path, width, height, is_cat = TRUE, add_label = TRUE) {
  img_size <- width*height
  ## List images in path
  images_names <- list.files(dir_path)
  if (add_label) {
    ## Select only cats or dogs images
    images_names <- images_names[grepl(ifelse(is_cat, "cat", "dog"), images_names)]
    ## Set label, cat = 0, dog = 1
    label <- ifelse(is_cat, 0, 1)
  }
  print(paste("Start processing", length(images_names), "images"))
  ## This function will resize an image, turn it into greyscale
  feature_list <- pblapply(images_names, function(imgname) {
    ## Read image
    img <- readImage(file.path(dir_path, imgname))
    ## Resize image
    img_resized <- resize(img, w = width, h = height)
    ## Set to grayscale
    grayimg <- channel(img_resized, "gray")
    ## Get the image as a matrix
    img_matrix <- grayimg@.Data
    ## Coerce to a vector
    img_vector <- as.vector(t(img_matrix))
    return(img_vector)
  })
  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, feature_list)
  feature_matrix <- as.data.frame(feature_matrix)
  ## Set names
  names(feature_matrix) <- paste0("pixel", c(1:img_size))
  if (add_label) {
    ## Add label
    feature_matrix <- cbind(label = label, feature_matrix)
  }
  return(feature_matrix)
}
#-------------------------------------------------------------------------------------------

#Aplicando a função no banco de dados e dividindo entre cães e gatos. 

cats_data <- extract_feature(dir_path = "train", width = width, height = height)
dogs_data <- extract_feature(dir_path = "train", width = width, height = height, is_cat = FALSE)

#-------------------------------------------------------------------------------------------

complete_set <- rbind(cats_data, dogs_data)

#-------------------------------------------------------------------------------------------
#Salvando Td
saveRDS(cats_data, "cat.rds")
saveRDS(dogs_data, "dog.rds")

#Lendo td
dogs_data=readRDS("dog.rds", refhook = NULL)
cats_data=readRDS("cat.rds", refhook = NULL)
#------------------------------------------------------------------------------------------

#ANALISANDO
full_set <- rbind(cats_data, dogs_data)
complete_set <- rbind(cats_data[1:500,], dogs_data[1:500,])

#Treinamento

complete_set[,1]=ifelse(complete_set$label < 1, "Gato", "Cao")

fitControl <- trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

svmRadialGrid <- expand.grid(C = round(runif(10,2,2024)), sigma = c(0.1, 0.25, 0.50, 0.75))

imgSvmLinear <- train(label ~ ., data = complete_set, method = "svmLinear", trControl = fitControl,tuneGrid = svmRadialGrid)

imgSvmLinear$bestTune

confusionMatrix(imgSvmLinear)
#modelo <- svm(label ~ ., complete_set, type="C", kernel="linear")

#plot(modelo,complete_set)







library(mxnet)



Imagem1<- readImage("2.jpg")

class(dogs_data)

display(cats_data)
print(I)

I=matrix()

for (i in 1:9) #(i in 1:12500)
{
  I=readImage(paste(i,".jpg",sep = ""))
}

I


Imagem[1]=readImage(paste(i,".jpg",sep = ""))
dim(readImage(paste(i,".jpg",sep = "")))
typeof(Imagem1)
class(Imagem1)

?array


?EBImage

#--------------
#Utilizando random forest

dogs_data=readRDS("dog.rds", refhook = NULL)
cats_data=readRDS("cat.rds", refhook = NULL)
complete_set <- rbind(cats_data[1:500,], dogs_data[1:500,])


complete_set[,1]=ifelse(complete_set$label < 1, "Gato", "Cao")


fitControl <- trainControl(method = "cv", number = 10, savePred = TRUE, classProb = TRUE)

CG_RF <- train(label ~ ., data = complete_set, method = "rf", trControl = fitControl)
beep(8)
#--------------------------------------------------------------------------------------------------------
#Vou aumentar a quantidade de pixels e ver se consigo um aumento da acuracia

width <- 45
height <- 45
## pbapply is a library to add progress bar *apply functions
## pblapply will replace lapply
library(pbapply)
extract_feature <- function(dir_path, width, height, is_cat = TRUE, add_label = TRUE) {
  img_size <- width*height
  ## List images in path
  images_names <- list.files(dir_path)
  if (add_label) {
    ## Select only cats or dogs images
    images_names <- images_names[grepl(ifelse(is_cat, "cat", "dog"), images_names)]
    ## Set label, cat = 0, dog = 1
    label <- ifelse(is_cat, 0, 1)
  }
  print(paste("Start processing", length(images_names), "images"))
  ## This function will resize an image, turn it into greyscale
  feature_list <- pblapply(images_names, function(imgname) {
    ## Read image
    img <- readImage(file.path(dir_path, imgname))
    ## Resize image
    img_resized <- resize(img, w = width, h = height)
    ## Set to grayscale
    grayimg <- channel(img_resized, "gray")
    ## Get the image as a matrix
    img_matrix <- grayimg@.Data
    ## Coerce to a vector
    img_vector <- as.vector(t(img_matrix))
    return(img_vector)
  })
  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, feature_list)
  feature_matrix <- as.data.frame(feature_matrix)
  ## Set names
  names(feature_matrix) <- paste0("pixel", c(1:img_size))
  if (add_label) {
    ## Add label
    feature_matrix <- cbind(label = label, feature_matrix)
  }
  return(feature_matrix)
}

#cats_data <- extract_feature(dir_path = "train", width = width, height = height)
#dogs_data <- extract_feature(dir_path = "train", width = width, height = height, is_cat = FALSE)
#saveRDS(cats_data, "cat45.rds")
#saveRDS(dogs_data, "dog45.rds")

dogs_data=readRDS("dog45.rds", refhook = NULL)
cats_data=readRDS("cat45.rds", refhook = NULL)

full_set <- rbind(cats_data, dogs_data)
partial_set <- rbind(cats_data, dogs_data)
partial_set[,1]=ifelse(complete_set$label < 1, "Gato", "Cao")

ajuste <- trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)
mtry <- round(runif(10,2,2024))
tunegrid<- expand.grid(.mtry=mtry)
CG_RF <- train(label ~ ., data = partial_set, method = "rf", trControl = fitControl,tuneGrid=tunegrid)
beep(8)
#-------------------------------------------------------------------------------------------
