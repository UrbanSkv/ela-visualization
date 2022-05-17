library(mlr)
library(tsne)
library(caret)


#A Simpler example of visuzalization
#example() visualizes two sets of landscape functions, saved in rds files



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


calculate_invariance <- function(data){
  nfun <- nrow(data)
  half_nfun=nfun/2
  result<-anderson_darling(data[1:half_nfun,],data[(half_nfun+1):nfun,])
  result<-sort(result)
  p_val <- 0.05
  p_val_bonferonni <- p.adjust(result, method = "BH")

  #Uncomment if you want to plot the p values (Figure 2 in the paper)
  #filename <- "R_images_new/2014_BH_correctio25.png"
  #png(file=filename, pointsize=10, width=3000, height=2000,res=300)
  #par(mar=c(18,4,1,4), cex=1.1) #put par after png if you want it to effect the exported image
  #a<-barplot(p_val_bonferonni, names=names(p_val_bonferonni), las=3, space=rep(0.2, length(p_val_bonferonni)), ylim = c(0,1), ylab = "p-value")
  #dev.off()

  anderson_excluded <- names(which(result<p_val))
  anderson_excluded_2 <- names(which(result<p_val_bonferonni))
  all_features<-names(data)
  saveRDS(anderson_excluded_2, paste("wilcoxon.rds", sep="_"))
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
process_features<-function(data, saved_wilcoxon=FALSE, wilcoxon_filename = "C:\\R_Code\\saved_wilcoxon.rds") {
  nfun<-nrow(data)


  data<-clean_data_full(data)
  anderson_excluded<-readRDS(wilcoxon_filename)
  all_features<-names(data)

  if (length(anderson_excluded > 0)) {
    data_filtered<-filter_features(data, anderson_excluded)
  } else {
    data_filtered <- data
  }



  data_filtered <- na.omit(data_filtered) #drop nas
  data_filtered <- data_filtered[,apply(data_filtered, 2, var, na.rm=TRUE) != 0] #remove constant columns, if there are any
  df2 = cor(data_filtered)

  hc = findCorrelation(df2, cutoff=0.95)
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
draw_data <- function(data, perplexity=5, title="", gecco_cec=FALSE, nfun1=0, nfun2=0, cec_all=FALSE, id=""){
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

  #uncomment these 3 lines if you want to save the figure rather than plot it on screen
  #png(file="filename", pointsize=10, width=2000, height=2000, res=300)
  tsne_res = tsne(data, epoch_callback = ecb, perplexity=perplexity, epoch=1000, max_iter=1000)
  #title("Plot Title")
  #dev.off()
}



plot_visualization <- function(features1, features2){
  nfun1 <- nrow(features1)
  nfun2 <- nrow(features2)

  combined_features <- rbind(features1, features2)
  processed_features <- process_features(combined_features, saved_wilcoxon=TRUE)

  data<-processed_features$basic

  wilcoxon_data<-processed_features$wilcoxon
  normalized_data<-processed_features$normalized


  #different types of data that can be visualized
  #All_F are normal features, pca are features processed with pca
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
  #first parameter determines which data to draw. Currently, it draws the pca data using the first five principal components
  draw_data(pca_data5, nfun, perplexity=5, gecco_cec=TRUE, nfun1=nfun1, nfun2=nfun2, cec_all=FALSE)

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

example <- function(){
  setwd("%DATA_DIRECTORY%")
  data_cec <- readRDS("%FIRST_FEATURES")
  data_gecco <-  readRDS("SECOND_FEATURES")
  plot_visualization(data_gecco, data_cec)
}
