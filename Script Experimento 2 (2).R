#Função que redimensiona e muda cores.
library(EBImage)
library(pbapply)
## pbapply is a library to add progress bar *apply functions
## pblapply will replace lapply
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
    ## Resize imagecats_data <- extract_feature(dir_path = image_dir, width = width, height = height)
    dogs_data <- extract_feature(dir_path = image_dir, width = width, height = height, is_cat = FALSE)
    dim(cats_data)
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
#-----------------------------------------------------------------------------
#10X10
width <- 10
height <- 10
cats_data <- extract_feature(dir_path = "train", width = width, height = height)
dogs_data <- extract_feature(dir_path = "train", width = width, height = height, is_cat = FALSE)
saveRDS(cats_data, "cat10.rds")
saveRDS(dogs_data, "dog10.rds")
#-----------------------------------------------------------------------------
#64X64
width <- 64
height <- 64
cats_data <- extract_feature(dir_path = "train", width = width, height = height)
dogs_data <- extract_feature(dir_path = "train", width = width, height = height, is_cat = FALSE)
saveRDS(cats_data, "cat64.rds")
saveRDS(dogs_data, "dog64.rds")
#-----------------------------------------------------------------------------
#128X128
width <- 128	
height <- 128
cats_data <- extract_feature(dir_path = "train", width = width, height = height)
dogs_data <- extract_feature(dir_path = "train", width = width, height = height, is_cat = FALSE)
saveRDS(cats_data, "cat128.rds")
saveRDS(dogs_data, "dog128.rds")
#-----------------------------------------------------------------------------
#n=500 45X45
a=proc.time()
dogs_data_45=readRDS("dog45.rds", refhook = NULL)
cats_data_45=readRDS("cat45.rds", refhook = NULL)
n500_set	=rbind(cats_data_45[1:500,], dogs_data_45[1:500,])
n500_set[,1]=ifelse(n500_set$label < 1, "Gato", "Cao")

fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n500_p45	=train(label ~ ., data = n500_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n500_p45=(b-a)[3]

save(T_RF_n500_p45,file="T_RF_n500_p45")
save(RF_n500_p45,file="RF_n500_p45")
#-----------------------------------------------------------------------------
#n=1000 45X45
a=proc.time()
dogs_data_45=readRDS("dog45.rds", refhook = NULL)
cats_data_45=readRDS("cat45.rds", refhook = NULL)
n1000_set	=rbind(cats_data_45[501:1501,], dogs_data_45[501:1501,])
n1000_set[,1]=ifelse(n1000_set$label < 1, "Gato", "Cao")
fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n1000_p45	=train(label ~ ., data = n1000_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n1000_p45=(b-a)[3]

save(T_RF_n1000_p45,file="T_RF_n1000_p45")
save(RF_n1000_p45,file="RF_n1000_p45")
beep(8)
#-----------------------------------------------------------------------------
# n=500 64X64
a=proc.time()
dogs_data_64=readRDS("dog64.rds", refhook = NULL)
cats_data_64=readRDS("cat64.rds", refhook = NULL)
n500_set	=rbind(cats_data_64[1:500,], dogs_data_64[1:500,])
n500_set[,1]=ifelse(n500_set$label < 1, "Gato", "Cao")

fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n500_p64	=train(label ~ ., data = n500_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n500_p64=(b-a)[3]

save(T_RF_n500_p64,file="T_RF_n500_p64")
save(RF_n500_p64,file="RF_n500_p64")


#-----------------------------------------------------------------------------
# n=1000 64X64
a=proc.time()
dogs_data_64=readRDS("dog64.rds", refhook = NULL)
cats_data_64=readRDS("cat64.rds", refhook = NULL)
n1000_set	=rbind(cats_data_64[501:1501,], dogs_data_64[501:1501,])
n1000_set[,1]=ifelse(n1000_set$label < 1, "Gato", "Cao")

fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n1000_p64=train(label ~ ., data = n1000_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n1000_p64=(b-a)[3]

save(T_RF_n1000_p64,file="T_RF_n1000_p64")
save(RF_n1000_p64,file="RF_n1000_p64")
#-----------------------------------------------------------------------------
# n=500 128X128
a=proc.time()
dogs_data_128=readRDS("dog128.rds", refhook = NULL)
cats_data_128=readRDS("cat128.rds", refhook = NULL)
n500_set   	=rbind(cats_data_128[1:500,], dogs_data_128[1:500,])
n500_set[,1]=ifelse(n500_set$label < 1, "Gato", "Cao")

fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n500_p128=train(label ~ ., data = n500_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n500_p128=(b-a)[3]

save(T_RF_n500_p128,file="T_RF_n500_p128")
save(RF_n500_p128,file="RF_n500_p128")
#-----------------------------------------------------------------------------
# n=1000 128X128
dogs_data_128=readRDS("dog128.rds", refhook = NULL)
cats_data_128=readRDS("cat128.rds", refhook = NULL)
n1000_set	=rbind(cats_data_64[501:1501,], dogs_data_64[501:1501,])
n1000_set[,1]=ifelse(n1000_set$label < 1, "Gato", "Cao")

fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n1000_p128=train(label ~ ., data = n1000_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n1000_p128=(b-a)[3]

save(T_RF_n1000_p128,file="T_RF_n1000_p128")
save(RF_n1000_p128,file="RF_n1000_p128")



#-------------------------------------------------------------------------------
#Encontrando a melhor quantidade de pixels

width <- 40
height <- 40
cats_data <- extract_feature(dir_path = "train", width = width, height = height)
dogs_data <- extract_feature(dir_path = "train", width = width, height = height, is_cat = FALSE)
saveRDS(cats_data, "cat40.rds")
saveRDS(dogs_data, "dog40.rds")

width <- 50
height <- 50  
cats_data <- extract_feature(dir_path = "train", width = width, height = height)
dogs_data <- extract_feature(dir_path = "train", width = width, height = height, is_cat = FALSE)
saveRDS(cats_data, "cat50.rds")
saveRDS(dogs_data, "dog50.rds")

width <- 55
height <- 55
cats_data <- extract_feature(dir_path = "train", width = width, height = height)
dogs_data <- extract_feature(dir_path = "train", width = width, height = height, is_cat = FALSE)
saveRDS(cats_data, "cat55.rds")
saveRDS(dogs_data, "dog55.rds")
 
width <- 60
height <- 60
cats_data <- extract_feature(dir_path = "train", width = width, height = height)
dogs_data <- extract_feature(dir_path = "train", width = width, height = height, is_cat = FALSE)
saveRDS(cats_data, "cat60.rds")
saveRDS(dogs_data, "dog60.rds")
 
library(doParallel)

cl <- makeCluster(detectCores())
registerDoParallel(cl,cores=8)
 
#n=1000 40X40
a=proc.time()
dogs_data_40=readRDS("dog40.rds", refhook = NULL)
cats_data_40=readRDS("cat40.rds", refhook = NULL)
n1000_set	=rbind(cats_data_40[501:1501,], dogs_data_40[501:1501,])
n1000_set[,1]=ifelse(n1000_set$label < 1, "Gato", "Cao")
fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n1000_p40	=train(label ~ ., data = n1000_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n1000_p40=(b-a)[3]

save(T_RF_n1000_p40,file="T_RF_n1000_p40")
save(RF_n1000_p40,file="RF_n1000_p40")

#n=1000 50X50
a=proc.time()
dogs_data_50=readRDS("dog50.rds", refhook = NULL)
cats_data_50=readRDS("cat50.rds", refhook = NULL)
n1000_set	=rbind(cats_data_50[501:1501,], dogs_data_50[501:1501,])
n1000_set[,1]=ifelse(n1000_set$label < 1, "Gato", "Cao")
fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n1000_p50	=train(label ~ ., data = n1000_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n1000_p50=(b-a)[3]

save(T_RF_n1000_p50,file="T_RF_n1000_p50")
save(RF_n1000_p50,file="RF_n1000_p50")

#n=1000 55X55
a=proc.time()
dogs_data_55=readRDS("dog55.rds", refhook = NULL)
cats_data_55=readRDS("cat55.rds", refhook = NULL)
n1000_set	=rbind(cats_data_55[501:1501,], dogs_data_55[501:1501,])
n1000_set[,1]=ifelse(n1000_set$label < 1, "Gato", "Cao")
fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n1000_p55	=train(label ~ ., data = n1000_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n1000_p55=(b-a)[3]

save(T_RF_n1000_p55,file="T_RF_n1000_p55")
save(RF_n1000_p55,file="RF_n1000_p55")

#n=1000 60X60
a=proc.time()
dogs_data_60=readRDS("dog60.rds", refhook = NULL)
cats_data_60=readRDS("cat60.rds", refhook = NULL)
n1000_set	=rbind(cats_data_60[501:1501,], dogs_data_60[501:1501,])
n1000_set[,1]=ifelse(n1000_set$label < 1, "Gato", "Cao")
fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n1000_p60	=train(label ~ ., data = n1000_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n1000_p60=(b-a)[3]

save(T_RF_n1000_p60,file="T_RF_n1000_p60")
save(RF_n1000_p60,file="RF_n1000_p60")
beep(8)
#-----------------------------------------------------------------------------
#n=5000 45X45 10 grupos
a=proc.time()
dogs_data_45=readRDS("dog45.rds", refhook = NULL)
cats_data_45=readRDS("cat45.rds", refhook = NULL)
n5000_set	=rbind(cats_data_45[1:5000,], dogs_data_45[1:5000,])
n5000_set[,1]=ifelse(n5000_set$label < 1, "Gato", "Cao")
fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n5000_p45_g10	=train(label ~ ., data = n5000_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n5000_p45=(b-a)[3]

save(T_RF_n5000_p45,file="T_RF_n5000_p45")
save(RF_n5000_p45,file="RF_n5000_p45")
beep(8)

library(caret)
library(doParallel)

cl <- makeCluster(detectCores())
registerDoParallel(cl,cores=7)

#n=12499 45X45 
a=proc.time()
dogs_data_45=readRDS("dog45.rds", refhook = NULL)
cats_data_45=readRDS("cat45.rds", refhook = NULL)
n12499_set	=rbind(cats_data_45[1:12499,], dogs_data_45[1:12499,])
n12499_set[,1]=ifelse(n12499_set$label < 1, "Gato", "Cao")
fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n12499_p45	=train(label ~ ., data = n12499_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n12499_p45=(b-a)[3]

save(T_RF_n12499_p45,file="T_RF_n12499_p45")
save(RF_n12499_p45,file="RF_n12499_p45")
beep(8)

#-----------------------------------------------------------------------------
library(caret)
library(doParallel)

cl <- makeCluster(detectCores())
registerDoParallel(cl,cores=7)

#n=1000 10X10 
a=proc.time()
dogs_data_10=readRDS("dog10.rds", refhook = NULL)
cats_data_10=readRDS("cat10.rds", refhook = NULL)
n1000_set	=rbind(cats_data_10[1:1000,], dogs_data_10[1:1000,])
n1000_set[,1]=ifelse(n1000_set$label < 1, "Gato", "Cao")
fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n1000_p10	=train(label ~ ., data = n1000_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n1000_p10=(b-a)[3]

save(T_RF_n1000_p10,file="T_RF_n1000_p10")
save(RF_n1000_p10,file="RF_n1000_p10")
beep(8)

load("T_RF_n12499_p45")
T_RF_n12499_p45/3600

#-----------------------------------------------------------------------------
library(caret)
library(doParallel)

cl <- makeCluster(detectCores())
registerDoParallel(cl,cores=7)

#n=5000 10X10 
a=proc.time()
dogs_data_10=readRDS("dog10.rds", refhook = NULL)
cats_data_10=readRDS("cat10.rds", refhook = NULL)
n12000_set	=rbind(cats_data_10[1:12000,], dogs_data_10[1:12000,])
n12000_set[,1]=ifelse(n12000_set$label < 1, "Gato", "Cao")
fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n12000_p10	=train(label ~ ., data = n12000_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n12000_p10=(b-a)[3]

save(T_RF_n12000_p10,file="T_RF_n12000_p10")
save(RF_n12000_p10,file="RF_n12000_p10")
beep(8)


library(caret)
library(doParallel)

cl <- makeCluster(detectCores())
registerDoParallel(cl,cores=7)

# n=12499 64X64
a=proc.time()
dogs_data_64=readRDS("dog64.rds", refhook = NULL)
cats_data_64=readRDS("cat64.rds", refhook = NULL)
n12499_set	=rbind(cats_data_64[1:12499,], dogs_data_64[1:12499,])
n12499_set[,1]=ifelse(n12499_set$label < 1, "Gato", "Cao")

fitControl 	=trainControl(method = "cv", number = 5, savePred = TRUE, classProb = TRUE)

RF_n12999_p64=train(label ~ ., data = n12999_set, method = "rf", trControl = fitControl)
b=proc.time()
T_RF_n12999_p64=(b-a)[3]

save(T_RF_n12999_p64,file="T_RF_n12999_p64")
save(RF_n12999_p64,file="RF_n12999_p64")

#--------------------------------------------------------------------------------------------

load("RF_n12000_p10")
load("RF_n2000_p10")
load("RF_n5000_p10")
fisher_10=extract_feature(dir_path="fisher",width=10,height=10,is_cat=F,add_label = T)
predict(RF_n12000_p10,fisher_10)
predict(RF_n2000_p10,fisher_10)
predict(RF_n5000_p10,fisher_10)

load("RF_n1000_p40")
fisher_40=extract_feature(dir_path="fisher",width=40,height=40,is_cat=F,add_label = T)
predict(RF_n1000_p40,fisher_40)

load("RF_n12499_p45")
load("RF_n1000_p45")
fisher_45=extract_feature(dir_path="fisher",width=45,height=45,is_cat=F,add_label = T)
predict(RF_n12499_p45,fisher_45)

load("RF_n1000_p50")
fisher_50=extract_feature(dir_path="fisher",width=50,height=50,is_cat=F,add_label = T)
predict(RF_n1000_p50,fisher_50)

load("RF_n1000_p55")
fisher_55=extract_feature(dir_path="fisher",width=55,height=55,is_cat=F,add_label = T)
predict(RF_n1000_p55,fisher_55)

load("RF_n1000_p60")
fisher_60=extract_feature(dir_path="fisher",width=60,height=60,is_cat=F,add_label = T)
predict(RF_n1000_p60,fisher_60)

load("RF_n12999_p64")
load("RF_n1000_p64")
fisher_64=extract_feature(dir_path="fisher",width=65,height=65,is_cat=F,add_label = T)
predict(RF_n12999_p64,fisher_64)
predict(RF_n1000_p64,fisher_64)

#--------------------------------------------------------------------------------------------

load("RF_n12000_p10")
load("RF_n2000_p10")
load("RF_n5000_p10")
fisher_10=extract_feature(dir_path="gato",width=10,height=10,is_cat=F,add_label = F)
predict(RF_n12000_p10,fisher_10)
predict(RF_n2000_p10,fisher_10)
predict(RF_n5000_p10,fisher_10)

load("RF_n1000_p40")
fisher_40=extract_feature(dir_path="gato",width=40,height=40,is_cat=F,add_label = F)
predict(RF_n1000_p40,fisher_40)

load("RF_n12499_p45")
load("RF_n1000_p45")
fisher_45=extract_feature(dir_path="gato",width=45,height=45,is_cat=F,add_label = F)
predict(RF_n12499_p45,fisher_45)
predict(RF_n1000_p45,fisher_45)

load("RF_n1000_p50")
fisher_50=extract_feature(dir_path="gato",width=50,height=50,is_cat=F,add_label = F)
predict(RF_n1000_p50,fisher_50)

load("RF_n1000_p55")
fisher_55=extract_feature(dir_path="gato",width=55,height=55,is_cat=F,add_label = F)
predict(RF_n1000_p55,fisher_55)

load("RF_n1000_p60")
fisher_60=extract_feature(dir_path="gato",width=60,height=60,is_cat=F,add_label = F)
predict(RF_n1000_p60,fisher_60)

load("RF_n12999_p64")
load("RF_n1000_p64")
fisher_64=extract_feature(dir_path="gato",width=65,height=65,is_cat=F,add_label = F)
predict(RF_n12999_p64,fisher_64)
predict(RF_n1000_p64,fisher_64)

#----------------------------------------------------------------------------------------------
predict(RF_n12000_p10,fisher_10)
predict(RF_n2000_p10,fisher_10)
predict(RF_n5000_p10,fisher_10)
predict(RF_n1000_p40,fisher_40)
predict(RF_n12499_p45,fisher_45)
predict(RF_n1000_p45,fisher_45)
predict(RF_n1000_p50,fisher_50)
predict(RF_n1000_p55,fisher_55)
predict(RF_n1000_p60,fisher_60)
predict(RF_n12999_p64,fisher_64)
confusionmatrix(predict(RF_n1000_p64,fisher_64))

rf=predict(RF_n500_p64,n500_set)
tab=table(n500_set[,"label"], rf)
confusionMatrix(tab)
names(n500_set)
