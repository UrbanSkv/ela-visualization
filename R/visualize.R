library(flacco)
library(Rcpp)
library(mlr)
library(tsne)
library(R.matlab)
library(caret)
library(purrr)
library(RColorBrewer)
library(VennDiagram)
library(tidyverse)
library(stringr)


setwd("DATA_DIR") #Set to directory containing samples


#' generate_data_2
#' take in two sets of problem samples from files name1 and name2, and return a combined dataframe
#' containing combined samples, with samples in name2 shifted and scaled
#'
#' @param root_folder the path of the root folder containing all of the sample files
#' @param name1 filename of the first problem sample file, without the .mat extension
#' @param name2 filename of the second problem sample file, without the .mat extension
#' @param dim the dimension of the data
#' @param step number of samples for each individual problem
#' @param nfun total number of problems
#' @param scale_x_factor how much to scale the x1,...,xn part of the problem
#' @param scale_y_factor how much to scale the y part of the problem
#' @param shift_x_factor how much to shift the x1,...,xn part of the problem
#' @param shift_y_factor how much to shift the y part of the problem
#' @param normalize_Y should y valus be normalized
#' @param gecco do the files contain gecco problems (FALSE for cec problems)
#' @param gecco_cec does one of the files contain gecco problems, and the other CEC problems (FALSE if both files contain the same benchmark)
#' @param rotate should the problems also be rotated (not used in the paper, as rotation wasn't considered)
#'
#' @return a combined dataframe containing samples and results from name1 and name2, with the samples and results in name2 shifted and scaled
#' @export
#'
#' @examples
generate_data_2 <- function(root_folder = "DATA_DIR", name1, name2, dim, step, nfun, scale_x_factor=10000, scale_y_factor=10000, shift_x_factor=10000,  shift_y_factor=10000, normalize_Y=FALSE, gecco=FALSE, gecco_cec=FALSE, rotate=FALSE) {
  if(gecco){
    cec_x_2 <- readMat(paste(root_folder,"\\all_samples_mat_",name1, ".mat", sep=""))
    cec_x_2 <- cec_x_2$all.samples.mat.1000
    cec_x_3 <- readMat(paste(root_folder,"\\all_samples_mat_",  name2, ".mat",sep=""))
    cec_x_3 <- cec_x_3$all.samples.mat.1000

    cec_y_2 <- readMat(paste(root_folder, "\\all_results_mat_", name1, ".mat",sep=""))
    cec_y_2 <- cec_y_2$all.results.mat.1000
    cec_y_3 <- readMat(paste(root_folder, "\\all_results_mat", name2, ".mat",sep=""))
    cec_y_3 <- cec_y_3$all.results.mat.1000
  } else if(gecco_cec){
    cec_x_2 <- readMat(paste(root_folder, "\\all_samples_mat_", name1, ".mat", sep=""))
    cec_x_2 <- cec_x_2$all.samples.mat.200
    cec_x_3 <- readMat(paste(root_folder, "\\all_samples_mat_", name2, ".mat",sep=""))
    cec_x_3 <- cec_x_3$all.samples.mat.1000


    cec_y_2 <- readMat(paste(root_folder, "\\all_results_mat_", name1, ".mat",sep=""))
    cec_y_2 <- cec_y_2$all.results.mat.200
    cec_y_3 <- readMat(paste(root_folder, "\\all_results_mat_", name2, ".mat",sep=""))
    cec_y_3 <- cec_y_3$all.results.mat.1000
  } else {
    cec_x_2 <- readMat(paste(root_folder, "\\all_samples_mat_", name1, ".mat", sep=""))
    cec_x_2 <- cec_x_2$all.samples.mat.200
    cec_x_3 <- readMat(paste(root_folder, "\\all_samples_mat_", name2, ".mat",sep=""))
    cec_x_3 <- cec_x_3$all.samples.mat.200

    cec_y_2 <- readMat(paste(root_folder, "\\all_results_mat_", name1, ".mat",sep=""))
    cec_y_2 <- cec_y_2$all.results.mat.200
    cec_y_3 <- readMat(paste(root_folder, "\\all_results_mat_", name2, ".mat",sep=""))
    cec_y_3 <- cec_y_3$all.results.mat.200
  }

  print("Scaling")
  scale_x<-runif(dim) * scale_x_factor
  scale_y<-runif(1) * scale_y_factor

  if (scale_x_factor == 1) {
    scale_x=rep(1, dim)
  }

  if (scale_y_factor == 1) {
    scale_y=rep(1, dim)
  }
  move_x<-runif(dim) * shift_x_factor
  print(move_x)
  move_y<-runif(1) * shift_y_factor
  print(move_y)
  print(scale_x)
  print(scale_y)
  #if we don't want to scale, we set scale_factor to 1
  #if we don't want to shift, we set shift factor to 0
  for(i in 1:dim){
    cec_x_3 [,i]<- (cec_x_3 [,i] * scale_x[i]) + move_x[i]
  }
  cec_y_3 <- (cec_y_3 * scale_y) + move_y


  if (rotate) {
    rotation<-runif(dim*dim)*2
    cec_x_3<-rotate_X_mat(cec_x_3, dim, rotation)
  }

  cec_x <- rbind(cec_x_2, cec_x_3)
  cec_y <- c(cec_y_2, cec_y_3)
  if (normalize_Y==TRUE) {
    cec_y <- normalize_all_Y(cec_y, step, nfun)
  }
  print("done scaling")
  print(paste("total data rows", nrow(cec_x), sep=" "))




  ret <- c()
  ret$x<-cec_x
  ret$y<-cec_y
  return(ret)
}


#' filter_features
#' remove coulmns containing the specified features from df
#' @param df the dataframe to remove columns from
#' @param features the vector of column names to remove
#'
#' @return
#' @export
#'
#' @examples
filter_features<-function(df, features) {
  return(df[ , -which(names(df) %in% features)])
}



#' Normalize the features in a dataframe
#'
#' Not used in the paper, but provided as an option
#'
#' @param df the dataframe to normalize
#'
#' @return a normalized dataframe
#' @export
#'
#' @examples
normalize_data <- function(df) {
  df<- normalizeFeatures(df)
  return(as.data.frame(df))
}



#' clean_data
#'
#' remove unwanted features from the dataframe
#' mostly used to femove basic features and fun_evals and costs_runtime features,
#' but also features that produce missing or invalid values
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
clean_data <- function(df) {
  df$basic.dim <- NULL #remove this, since we don't want it here
  df$ela_meta.costs_runtime <- NULL #remove this, since we don't want it here
  df$ela_local.fun_evals.median  <- NULL #remove this, since we don't want it here
  df$eela_local.fun_evals.lq  <- NULL #remove this, since we don't want it here
  df$ela_local.fun_evals.mean <- NULL #remove this, since we don't want it here
  ela_local.fun_evals.uq <- NULL #remove this, since we don't want it here
  df$ela_local.costs_fun_evals  <- NULL #remove this, since we don't want it here
  df$ela_local.fun_evals.max <- NULL #remove this, since we don't want it here
  df$limo.costs_runtime <- NULL #remove this, since we don't want it here
  df$ela_local.fun_evals.sd <- NULL #remove this, since we don't want it here
  df$ela_local.fun_evals.min <- NULL #remove this, since we don't want it here
  df$ic.costs_runtime<- NULL #remove this, since we don't want it here
  df$disp.costs_runtime<- NULL #remove this, since we don't want it here
  df$ela_local.costs_runtime<- NULL #remove this, since we don't want it here
  df$basic.costs_runtime<- NULL #remove this, since we don't want it here
  df$basic.observations<- NULL #remove this, since we don't want it here
  df$basic.ela_level.costs_runtime<- NULL #remove this, since we don't want it here
  df$basic.nbc.costs_runtimecosts_runtime<- NULL #remove this, since we don't want it here
  df$basic.cm_grad.costs_runtime<- NULL #remove this, since we don't want it here
  df$basic.pca.costs_runtime<- NULL #remove this, since we don't want it here
  df$ela_local.fun_evals.uq<- NULL #remove this, since we don't want it here
  df$ela_local.fun_evals.lq<- NULL #remove this, since we don't want it here
  df$ela_level.costs_runtime<- NULL #remove this, since we don't want it here
  df$cm_grad.costs_runtime<- NULL #remove this, since we don't want it here
  df$pca.costs_runtime<- NULL #remove this, since we don't want it here
  df$basic.upper_min <- NULL #remove this, since we don't want it here
  df$basic.upper_max <- NULL #remove this, since we don't want it here
  df$basic.lower_min<- NULL #remove this, since we don't want it here
  df$basic.lower_max<- NULL #remove this, since we don't want it here
  df$basic.objective_min<- NULL #remove this, since we don't want it here
  df$basic.objective_max<- NULL #remove this, since we don't want it here
  df$ela_distr.costs_runtime<- NULL #remove this, since we don't want it here
  df$pca.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$cm_angle.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$cm_angle.costs_runtime<- NULL #remove this, since we don't want it here
  df$cm_grad.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$ela_distr.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$ela_level.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$ela_meta.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$ic.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$basic.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$disp.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$limo.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$nbc.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$pca.costs_fun_evals<- NULL #remove this, since we don't want it here
  df$basic.minimize_fun<- NULL #remove this, since we don't want it here
  df$nbc.costs_runtime<- NULL #remove this, since we don't want it here


  df$ic.eps.s<- NULL #someimes has missing values
  df$ela_local.best2mean_contr.ratio<- NULL #sometimes has missing values
  df$ic.eps.ratio<- NULL #sometimes has infinite values
  return(df)


}


#' clean_data_full
#' remove unwanted features, and also remove all constant features
#'
#' @param features_cec_orig the dataframe to clean
#'
#' @return a dataframe with no constant and unwanted features
#' @export
#'
#' @examples
clean_data_full <- function(features_cec_orig){
  features_cec_orig<-as.data.frame(features_cec_orig)
  features_cec_orig <- sapply(features_cec_orig, as.numeric)
  features_cec_orig <- as.data.frame(features_cec_orig)
  features_cec_orig <- clean_data(features_cec_orig)
  features_cec_orig<-removeConstantFeatures(features_cec_orig)
  return(features_cec_orig)
}


#' process_features
#'
#' do the final feature processing. This includes
#' 1. Removing all problems where the landscape feature calculation encountered an error
#' 2. Removing the features affected by shifting and scaling
#' 3. Removing correlated features
#' 4. Normalizing the data
#' 5. Performing PCA
#'
#' The function allows two modes of operation. In the first, we calculate the features affected by shifting and scaling from the provided data.
#' In the second, we use an existing file which lists which features are affected.
#'
#' In order to not overfit the data, we first calculated the affected features using only the data from the 2014 CEC problems, saved this data,
#' and then used these affected features when plotting comparisons between CEC and GECCO problems
#' @param data The dataframe of landscape features to preprocess
#' @param save_prefix prefix of the filename that will contained the saved data about which features are affected by shifting and scaling
#' @param nfun The total number of problems in the data
#' @param errors Indices of rows which contained errors during feature calculation
#' @param saved_wilcoxon Should we use existing knowledge of which features are affected by shifting and scaling, or calculate it from the provided data. If FALSE, the calculated features will be saved to a file.
#' @param wilcoxon_filename The filename containing the names of the features affected by shifting and scaling. Used when saved_wilcoxon = TRUE
#'
#' @return A Structure with the following fields
#' $basic - the original features, without errors
#' $basic_normalized - same as basic, but normalized
#' $normalized <- normalized data with removed errors and removed correlated features
#' $pca <-  pca of the normalized data
#' $wilcoxon - Only the features not affected by shifting and scaling
#' $wilcoxon_normalized - normalized and unaffected by shifting and scaling
#' $pr_wilcoxon <- unaffected by shitfing and scaling, and after performing PCA

#' @export
#'
#' @examples
process_features<-function(data, save_prefix, nfun, errors, saved_wilcoxon=FALSE, wilcoxon_filename = "C:\\R_Code\\saved_wilcoxon.rds") {
  error_indices <- errors
  errors<-c()
  #take out functions with errors
  for (i in errors){
    #The error indexes are missing rows in feature data
    #since we want the data to be symmetric, we also have to remove the corresponding feature in the other half
    if (i>nfun/2) {
      error_index <- i - (nfun/2)

    } else {
      error_index <- i + (nfun/2)
    }
    error_indices <- c(error_indices, error_index)
  }

  if(is.null(error_indices)){
    error_indices<-c()
  }
  if (length(error_indices > 0)){
    data <- data[-error_indices,]
  }
  #adjust nfun to new number of functions
  nfun<-nrow(data)


  data<-clean_data_full(data)


  if(saved_wilcoxon){
    anderson_excluded<-readRDS(wilcoxon_filename)
    all_features<-names(data)
  } else {
    half_nfun=nfun/2
    result<-anderson_darling(data[1:half_nfun,],data[(half_nfun+1):nfun,])
    result<-sort(result)
    p_val <- 0.05
    p_val_bonferonni <- p.adjust(result, method = "BH")

    #Uncomment if you want to plot the p values (Figure 2)
    #filename <- "R_images_new/2014_BH_correctio25.png"
    #png(file=filename, pointsize=10, width=3000, height=2000,res=300)
    #par(mar=c(18,4,1,4), cex=1.1) #put par after png if you want it to effect the exported image
    #a<-barplot(p_val_bonferonni, names=names(p_val_bonferonni), las=3, space=rep(0.2, length(p_val_bonferonni)), ylim = c(0,1), ylab = "p-value")
    #dev.off()

    anderson_excluded <- names(which(result<p_val))
    anderson_excluded_2 <- names(which(result<p_val_bonferonni))
    all_features<-names(data)
    saveRDS(anderson_excluded_2, paste(save_prefix, "wilcoxon.rds", sep="_"))
  }
  if (length(anderson_excluded > 0)) {
    data_filtered<-filter_features(data, anderson_excluded)
  } else {
    data_filtered <- data
  }



  data_filtered <- na.omit(data_filtered)
  df2 = cor(data_filtered)



  #corrplot(df2, method="circle", type="full", tl.col="black", tl.cex=1.2) #uncomment if you want to plot the correlations
  hc = findCorrelation(df2, cutoff=0.95) # putt any value as a "cutoff"
  hc = sort(hc)
  if (length(hc) > 0) {
    reduced_Data = data_filtered[,-c(hc)]
  } else {
    reduced_Data <- data_filtered
  }



  reduced_Data <- normalize_data(reduced_Data)

  pr <- prcomp(reduced_Data)

  df_pca<-pr$x
  ret <- c()
  ret$basic <- data
  ret$basic_normalized <- na.omit(normalize_data(data))
  ret$wilcoxon <- data_filtered
  ret$wilcoxon_normalized <- normalize_data(data_filtered)
  ret$pr_wilcoxon <- prcomp(data_filtered)$x
  ret$normalized <- reduced_Data
  ret$pca <-  df_pca
  return(ret)

}


#' do_visualization
#'
#' preforms all steps needed to visualize the provided data
#' @param name1 the filename containing the features from the first set of problems
#' @param name2 the filename containing the features from the second set of problems
#' @param save_prefix
#' @param step
#' @param nfun
#' @param dim
#' @param scale_x_factor
#' @param scale_y_factor
#' @param shift_x_factor
#' @param shift_y_factor
#' @param perplexity
#' @param normalize_Y
#' @param exclude
#' @param error_on_fnf
#' @param gecco
#' @param gecco_cec
#' @param saved_wilcoxon
#' @param save_prefix_gecco
#' @param rotate
#' @param nfun1
#' @param nfun2
#' @param cec_all
#'
#' @return
#' @export
#'
#' @examples
do_visualization<-function(name1, name2,  save_prefix, step, nfun,dim, scale_x_factor=10000, scale_y_factor=10000, shift_x_factor=10000,  shift_y_factor=10000, perplexity=5, normalize_Y=FALSE, exclude=c(), error_on_fnf=FALSE, gecco=FALSE, gecco_cec=FALSE, saved_wilcoxon=FALSE, save_prefix_gecco="", rotate=FALSE, nfun1=0, nfun2=0, cec_all=FALSE){
  if (!cec_all)
  {
    cec_data <- generate_data_2(name1, name2, dim, step, nfun,scale_x_factor, scale_y_factor, shift_x_factor, shift_y_factor, normalize_Y, gecco=gecco, gecco_cec=gecco_cec, rotate=rotate)
    cec_x<-cec_data$x
    cec_y<-cec_data$y
  }


  if(cec_all){
    cec_files<-list.files(".", pattern = paste("\\d+", save_prefix, ".*features", sep=""))
    cec_errors<-list.files(".", pattern = paste("\\d+", save_prefix, ".*errors", sep=""))
    features_cec_fixed_x=NULL
    feature_errors <- NULL
    for (f in cec_files){
      if (is.null(features_cec_fixed_x)){
        features_cec_fixed_x <-readRDS(f)
      } else{
        features_cec_fixed_x <- rbind(features_cec_fixed_x, readRDS(f))
      }
    }

    for (f in cec_errors){
      if (is.null( feature_errors)){
        feature_errors <-readRDS(f)
      } else{
        feature_errors <- c( feature_errors, readRDS(f))
      }
    }
    nrow_cec<-nrow(features_cec_fixed_x)/2
    features_cec<-features_cec_fixed_x[1:nrow_cec,]
    nerror <- length(feature_errors)
    nfun1<-nfun1-nerror

    feature_errors <- feature_errors[1:nerror]
    print("ERROR IN FUNCTIONS")
    print(feature_errors)
    features_cec<-features_cec_fixed_x[1:nrow_cec,]


    features_gecco<-readRDS(paste(save_prefix_gecco, "features.rds", sep="_"))


    feature_errors_gecco <- readRDS(paste(save_prefix_gecco, "errors.rds", sep="_"))
    nerror_gecco <- length(feature_errors_gecco)/2
    feature_errors_gecco <- feature_errors_gecco[nerror_gecco:length(feature_errors_gecco)]


    feature_errors <- c(feature_errors, feature_errors_gecco)

    nrow_cec<-nrow(features_cec_fixed_x)/2
    features_cec<-features_cec_fixed_x[1:nrow_cec,]

    nrow_2<-nrow(features_gecco)/2
    features_gecco<-features_gecco[1:nrow_2,]
    features_cec_fixed_x<-rbind(features_cec, features_gecco)



  }
  else if(gecco_cec){
    features_cec_fixed_x<-readRDS(paste(save_prefix, "features.rds", sep="_"))
    feature_errors <- readRDS(paste(save_prefix, "errors.rds", sep="_"))
    nerror <- length(feature_errors)/2
    feature_errors <- feature_errors[1:nerror]
    #feature_errors <- c()
    features_gecco<-readRDS(paste(save_prefix_gecco, "features.rds", sep="_"))
    feature_errors_gecco <- readRDS(paste(save_prefix_gecco, "errors.rds", sep="_"))
    nerror_gecco <- length(feature_errors_gecco)/2
    feature_errors_gecco <- feature_errors_gecco[nerror_gecco:length(feature_errors_gecco)]
    nfun1<-nfun1-length(feature_errors)
    nfun2<-nfun2-nerror_gecco
    feature_errors <- c(feature_errors, feature_errors_gecco)

    nrow_cec<-nrow(features_cec_fixed_x)/2
    features_cec<-features_cec_fixed_x[1:nrow_cec,]

    nrow_2<-nrow(features_gecco)/2
    features_gecco<-features_gecco[1:nrow_2,]
    features_cec_fixed_x<-rbind(features_cec, features_gecco)
  } else {
    if (file.exists(paste(save_prefix, "features.rds", sep="_"))) {
      features_cec_fixed_x<-readRDS(paste(save_prefix, "features.rds", sep="_"))
      feature_errors <- readRDS(paste(save_prefix, "errors.rds", sep="_"))
    } else {
      if(error_on_fnf){
        stop("File not found")
      }
      features<- calculate_features_multiple(cec_x, cec_y, step=step, nfun=nfun, all_instances=1, exclude=exclude)
      feature_errors <- features$errors
      features_cec_fixed_x <- features$features
      saveRDS(features_cec_fixed_x, paste(save_prefix, "features.rds", sep="_"))
      saveRDS(feature_errors, paste(save_prefix, "errors.rds", sep="_"))
      print("Features saved as ")
      print(paste(save_prefix, "features.rds", sep="_"))
    }
  }




  processed_features <- process_features(features_cec_fixed_x, save_prefix, nfun, feature_errors, saved_wilcoxon=saved_wilcoxon)





  data<-processed_features$basic

  wilcoxon_data<-processed_features$wilcoxon
  normalized_data<-processed_features$normalized

  normalized_data_allf<-processed_features$normalized_allf
  pca_data_allf2<-processed_features$pca_allf[,1:2]
  pca_data_allf3<-processed_features$pca_allf[,1:3]
  pca_data_allf4<-processed_features$pca_allf[,1:4]
  pca_data_allf5<-processed_features$pca_allf[,1:5]
  pca_data2<-processed_features$pca[,1:2]
  pca_data3<-processed_features$pca[,1:3]
  pca_data4<-processed_features$pca[,1:4]
  pca_data5<-processed_features$pca[,1:5]
  pca_data6<-processed_features$pca[,1:6]
  pca_data7<-processed_features$pca[,1:7]
  pca_data8<-processed_features$pca[,1:8]
  pca_data9<-processed_features$pca[,1:9]
  pca_data10<-processed_features$pca[,1:10]
  pca_data5<-processed_features$pca[,1:5]




  nfun <- nrow(data)
  #Uncomment the data you want to visualize:
  #Currently, we visualize pca data using the first five principal components
  #draw_data(data, nfun, perplexity=perplexity, times=1, title="", save_prefix = paste("unprocessed", sep="_"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2, cec_all=cec_all)
  #draw_data(wilcoxon_data, nfun, perplexity=perplexity,times=1, title= "Wilcoxon duplicates removed", save_prefix = paste(save_prefix, "wilcoxon", sep="_"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2, cec_all=cec_all)
  #draw_data(normalized_data, nfun, perplexity=perplexity,times=1, title= "Wilcoxon normalized", save_prefix = paste(save_prefix, "wilcoxon", sep="_"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2, cec_all=cec_all)
  draw_data(pca_data5, nfun, perplexity=perplexity,times=1, title= "PCA, wilcoxon", save_prefix = paste(save_prefix, "wilcoxon_pca", sep="_"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2, cec_all=cec_all)
  #draw_data(normalized_data_allf, nfun, perplexity=perplexity,times=1, title= "Normalized, all features", save_prefix = paste(save_prefix, "normalized", sep="_"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2, cec_all=cec_all)
  #draw_data(pca_data_allf3, nfun, perplexity=5,times=1, title= "PCA, all features", save_prefix = paste(save_prefix, "pca", sep="_"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2)
  #draw_data(pca_data_allf2, nfun, perplexity=5,times=1, title= "PCA, all features 2", save_prefix = paste(save_prefix, "pca", sep="_"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2)
  #draw_data(pca_data_allf3, nfun, perplexity=5,times=1, title= "PCA, with non-invariant features", save_prefix = paste(save_prefix, "pca", sep="_"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2)
  #draw_data(pca_data_allf4, nfun, perplexity=5,times=1, title= "PCA, all features 4", save_prefix = paste(save_prefix, "pca", sep="_"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2)
  #draw_data(pca_data_allf5, nfun, perplexity=5,times=1, title= "", save_prefix = paste("pcaall", sep="_"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2)


  #draw_data(pca_data4, nfun, perplexity=5,times=1, title= "", save_prefix = paste("pca"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2, cec_all=cec_all, id=name1)
  #draw_data(pca_data5, nfun, perplexity=5,times=1, title= "", save_prefix = paste("pca"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2, cec_all=cec_all, id=name1)
  #draw_data(pca_data6, nfun, perplexity=5,times=1, title= "", save_prefix = paste("pca"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2, cec_all=cec_all, id=name1)


  #draw_data(pca_data6, nfun, perplexity=5,times=1, title= "", save_prefix = paste("pca"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2, cec_all=cec_all, id=name1)
  #draw_data(pca_data8, nfun, perplexity=5,times=1, title= "", save_prefix = paste("pca"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2, cec_all=cec_all, id=name1)


  #draw_data(pca_data3, nfun, perplexity=5,times=1, title= "", save_prefix = paste("pca"), gecco_cec=gecco_cec, nfun1=nfun1, nfun2=nfun2, cec_all=cec_all, id=name1)

}



#' draw_data
#'
#' Handles the tsne visualization
#'
#' @param data The data to visualize
#' @param nfunThe total number of functions
#' @param perplexity The perplexity parameter to use
#' @param times How many times to perform the visualization
#' @param title The title to be  included in the plot
#' @param save_prefix A prefix that will be added to the filenames if saving the plots to disk
#' @param gecco_cec Is the comparison between CEC and GECCO, or just between different CEC problems
#' @param nfun1 The number of function in the first dataset (colored black)
#' @param nfun2 The number of functions in the second dataset (colored red)
#' @param cec_all Are we using all CEC years, or just a single year
#' @param id An id number that will be added to the filename if saving the plot to disk
#'
#' @return
#' @export
#'
#' @examples
draw_data <- function(data, nfun, perplexity, times=2, title="", save_prefix="", gecco_cec=FALSE, nfun1=0, nfun2=0, cec_all=FALSE, id=""){
  set.seed(613245) #for reproducability
  ecb = function(x, y){ plot(x, t='n', xlab="t-sne dimension 1", ylab="t-sne dimension 2"); text(x, labels=function_names, col=cols[classes], cex=1.5, cex.lab=5, cex.axis=5, cex.main=5, cex.sub=5)} #labels as function_ids
  if (gecco_cec || cec_all) {
    function_names<-c(1:nfun1, 1:nfun2)
    classes<-c(rep(1,nfun1), rep(2,nfun2))
    cols<-c("#000000", "#E52525")
  } else {
    function_names<-c(1:(nfun/2), 1:(nfun/2))
    classes<-c(rep(1,nfun/2), rep(2,nfun/2))
    cols<-c("#000000", "#E52525")
  }

  all_distances_mat <- matrix(ncol=10)
  for (i2 in 1:1){
    print("STARTING ITERATION")
    print(i2)
    all_distances <- c()
    for (perp in c(5)){
      distances<-c()
      times<-1
      for (i in 1:times){
        filename <- paste("R_images_new/",save_prefix, i, id, ".png", sep="_")
        #Uncomment if you want to save the plot to a file
        #png(file=filename, pointsize=10, width=2000, height=2000, res=300)
        tsne_res = tsne(data, epoch_callback = ecb, perplexity=perplexity, epoch=10000, max_iter=10000)
        #title(title)
        #dev.off()
        #saveRDS(tsne_res, filename)
        #saveRDS(distances, paste("all_distances", perp, i2, sep="_"))
      }
      all_distances[[perp]] <- median(distances)

    }
    print(all_distances)
    all_distances_mat <- rbind(all_distances_mat, all_distances)
  }
  print(all_distances_mat)
  saveRDS(all_distances_mat, "all_distances_200")
}


#' plot_comparisson
#'
#'
#' The main entry point of the code. Visualizes a tsne comparison between two data sets after removing features affected by shifting and scaling
#'
#' The idea of this function is that it can be used in two ways: either to visualize functions, or to calculate which landscape features are affected by shifting and scaling.
#'
#' If saved_wilcoxon is TRUE, it will load an existing file which contains names of the features that are affected by shifting and scaling
#' and delete these features before performing visualization
#' If saved_wilcoxon is FALSe, it will calculate which features are affected by shifting and scaling. In this case, we need to
#' provide the parameters shift_x, shift_y, scale_x, scale_y to select how much to shift and scale the samples by.
#' @param year then name of the first dataset. can be "2013", "2014", "2015", "2017" for the CEC problems, or "gecco" for gecco problems
#' @param year2 then name of the second dataset. can be "2013", "2014", "2015", "2017" for the CEC problems, or "gecco" for gecco problems
#' @param sample_size the sample size used when sampling
#' @param dim the dimensionality of the problems
#' @param scale_x how much to scale the samples by. Only usaed for calculating which features are unaffected by shifting and scaling
#' @param scale_y how much to scale the samples by
#' @param shift_x how much to shift the samples by
#' @param shift_y how much to shift the samples by
#' @param normalize_Y should y values be normalized
#' @param same_samples are the two sample sets from the same year
#' @param type the type part of the filename used for the analysis
#' @param perplexity the perplexity parameter used by the tsne
#' @param gecco_cec is the comparisson between gecco and cec, or just different cec years
#' @param saved_wilcoxon are we using a pre-save list of unaffected features, or should it be calculated based on the input data
#'
#' @return
#' @export
#'
#' @examples
plot_comparisson <- function(year=2014, year2="gecco", sample_size=200,dim=2, scale_x=FALSE, scale_y=FALSE, shift_x=FALSE, shift_y=FALSE,  normalize_Y=FALSE,same_samples = FALSE, type="sameX", perplexity=5, gecco_cec=TRUE, saved_wilcoxon=TRUE) {
  step<-as.numeric(sample_size) * as.numeric(dim)
  if(dim=="10"){
    if (year=="2013") {
      nfun=28
    }
    if (year=="2014") {
      nfun=30
    }
    if (year=="2015") {
      nfun=15
    }
    if (year=="2017") {
      nfun=29
    }
    if (year=="gecco") {
      nfun= 24
    }
  } else {
    if (year=="2013") {
      nfun=28
    }
    if (year=="2014") {
      nfun=22
    }
    if (year=="2015") {
      nfun=10
    }
    if (year=="2017") {
      nfun=14
    }
    if (year=="gecco") {
      nfun= 24
    }
  }

  #------ SCALING PARAMTERS ------
  scale_x_factor<- if(scale_x) 100 else 1
  scale_y_factor<- if(scale_y) 100 else 1
  shift_x_factor<- if(shift_x) 1000 else 0
  shift_y_factor<- if(shift_y) 1000 else 0
  exclude=c()


  #add the 2014  functions
  nfun1=nfun

  if(dim=="10"){
    if (year2=="2013") {
      nfun=nfun + 28
      nfun2=28
    }
    if (year2=="2014") {
      nfun=nfun + 30
      nfun2=30
    }
    if (year2=="2015") {
      nfun=nfun + 15
      nfun2=15
    }
    if (year2=="2017") {
      nfun=nfun + 29
      nfun2=29
    }
    if (year2=="gecco") {
      nfun=nfun + 24
      nfun2=24
    }
  } else {
    if (year2=="2013") {
      nfun=nfun + 28
      nfun2=28
    }
    if (year2=="2014") {
      nfun=nfun + 22
      nfun2=22
    }
    if (year2=="2015") {
      nfun=nfun + 10
      nfun2=10
    }
    if (year2=="2017") {
      nfun=nfun + 14
      nfun2=14
    }
    if (year2=="gecco") {
      nfun=nfun + 24
      nfun2=24
    }
  }
  print("NFUN")
  print(nfun)
  name1<-paste(year, "_d", dim,"_", sample_size, "_a", sep="")
  save_prefix<-paste(name1,scale_x_factor, scale_y_factor, shift_x_factor, shift_y_factor,normalize_Y, type, sep="_")
  name2<-paste(year2, "_d", dim,"_", sample_size, "_a", sep="")
  save_prefix_gecco <- paste(name2,scale_x_factor, scale_y_factor, shift_x_factor, shift_y_factor,normalize_Y, type, sep="_")
  do_visualization(name1, name2, save_prefix, step, nfun, as.numeric(dim), scale_x_factor, scale_y_factor, shift_x_factor, shift_y_factor, normalize_Y, gecco_cec=gecco_cec, saved_wilcoxon=saved_wilcoxon, save_prefix_gecco = save_prefix_gecco, nfun1=nfun1, nfun2=nfun2, perplexity = perplexity)
}
