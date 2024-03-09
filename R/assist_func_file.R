library(tidyverse)
library(Matrix)
library(umap)
library(MOVICS)
library(class)


#' Convert the file to the datalist
#'
#' @param idx.sel the indices to indicate the training data
#' @param ex.col the indices to exclude the columns
#' @param flist a vector containing the names of the feature files
#'
#' @return a data list
#' @export
#'
#' @examples
getDatalist <- function(flist, idx.sel, ex.col = NULL)
{
  dt.list <- list()
  for(fname in flist)
  {
    dt.i <- read_csv(fname)
    if(!is.null(ex.col))
    {
      dt.i <- dt.i %>% select(-ex.col)
    }
    baseName <- str_replace(fname, '.csv', '')

    dt.i.train <- dt.i[idx.sel, ]

    dt.list[[baseName]] <- dt.i.train
  }

  dt.list
}

#' This function is used for dimension reduction for both training and test datasets, if dt.list.test is null
#' only output training dataset.
#'
#' @param dt.list.train a list containing the multi-modality training features.
#' @param dt.list.test a list contraining the muli-modality test features or NULL.
#'
#' @return a list containing the training (and test) data matrix after dimension.
#' @export
DR_umap <- function(dt.list.train, dt.list.test = NULL)
{
  if(!is.null(dt.list.test)){stopifnot(length(dt.list.train) == length(dt.list.test))}

  clust.data <- list()
  if(!is.null(dt.list.test)){clust.data.test <- list()}

  num_dataset <- length(dt.list.train)
  for(i.a in seq(1, num_dataset, by = 1))
  {
    dt <- dt.list.train[[i.a]]
    dt.umap <- umap(dt, random_state = 123, n_neighbors = 10, n_components = 10)
    d.mat <- dt.umap$layout %>% t()

    colnames(d.mat) <- paste0('subj', 1:ncol(d.mat))
    rownames(d.mat) <- paste0('feat', 1:nrow(d.mat))
    baseName <- names(dt.list.train)[i.a]
    clust.data[[baseName]] <- d.mat

    if(!is.null(dt.list.test))
    {
      d.mat.test <- predict(dt.umap, dt.list.test[[i.a]]) %>% t()

      colnames(d.mat.test) <- paste0('subj', 1:ncol(d.mat.test))
      rownames(d.mat.test) <- paste0('feat', 1:nrow(d.mat.test))
      clust.data.test[[baseName]] <- d.mat.test
    }
  }

  if(is.null(dt.list.test))
  {
    return(list(clust.data))
  } else {
    return(list(clust.data, clust.data.test))
  }
}

#' The function to get the consensus clustering results.
#'
#' @param clust.data list of matrices
#' @param is.binary A logicial vector to indicate if the subdata is binary matrix of 0 and 1 such as mutation.
#' @param type Data type corresponding to the list of matrics, which can be gaussian, binomial or possion
#'
#' @return
#' @export
#'
#' @examples
clust_identify <- function(clust.data, is.binary = c(F, F), type = c('gaussian', 'gaussian'))
{
  optk.cluster <- getClustNum(data = clust.data,
                              is.binary = is.binary,
                              try.N.clust = 2:8,
                              fig.name = 'Number of Cluster')

  N.clust.CPI <- which.max(rowSums(optk.cluster$CPI)) + 1

  moic.res.list <- getMOIC(data        = clust.data,
                           methodslist = list("iClusterBayes", "SNF", "PINSPlus", "NEMO", "COCA",
                                              "LRAcluster", "ConsensusClustering", "IntNMF", "CIMLR", "MoCluster"),
                           N.clust     = N.clust.CPI,
                           type        = type)


  cmoic.clust <- getConsensusMOIC(moic.res.list = moic.res.list,
                                  fig.name      = "CONSENSUS HEATMAP",
                                  distance      = "euclidean",
                                  linkage       = "average")

  return(cmoic.clust)
}

#' Title
#'
#' @param cmoic.clust The consensus clustering result variable
#' @param clust.data.train The training list of matrices
#' @param clust.data.test The test list of matrices
#'
#' @return
#' @export
#'
#' @examples
knn_train_test <- function(cmoic.clust, clust.data.train, clust.data.test)
{
  cl <- cmoic.clust$clust.res$clust %>% as_factor()
  dt.train <- NULL
  for(dt.mat in clust.data.train)
  {
    dt.train <- cbind(dt.train, t(dt.mat))
  }

  dt.test <- NULL
  for(dt.mat.test in clust.data.test)
  {
    dt.test <- cbind(dt.test, t(dt.mat.test))
  }

  cl.test <- knn(dt.train, dt.test, cl, k = 3, prob = F)

  return(cl.test)
}

#' Draw the Heatmap to display the feature distribution.
#'
#' @param clust.data The datalist containing data.list.train, dt.list.test
#' @param out.clust The prediction cluster results
#' @param fig.name The name of heatmap
#'
#' @return
#' @export
#'
#' @examples
draw_Heatmap <- function(clust.data, out.clust, fig.name = 'Heatmap')
{
  plotdata <- getStdiz(data       = clust.data,
                       halfwidth  = rep(2, times = length(clust.data)), # no truncation for mutation
                       centerFlag = rep(T, times = length(clust.data)), # no center for mutation
                       scaleFlag  = rep(T, times = length(clust.data))) # no scale for mutation

  a.col   <- c("#00FF00", "#008000", "#000000", "#800000", "#FF0000")
  b.col <- c("#0074FE", "#96EBF9", "#FEE900", "#F00003")

  col.list <- list()

  for(i.a in 1:length(clust.data))
  {
    if(i.a %% 2 == 0)
    {
      col.list[[i.a]] <- a.col
    }else {
      col.list[[i.a]] <- b.col
    }
  }

  # feat   <- cmoic.clust$feat.res
  # feat1  <- feat[which(feat$dataset == "static"),][1:10,"feature"]
  # feat2  <- feat[which(feat$dataset == "dynamic"),][1:10,"feature"]
  #
  # annRow <- list(feat1, feat2)

  # comprehensive heatmap (may take a while)
  getMoHeatmap(data          = plotdata,
               row.title     = names(clust.data),
               is.binary     = rep(F, length(clust.data)), # the 4th data is mutation which is binary
               legend.name   = names(clust.data),
               clust.res     = out.clust, # cluster results
               clust.dend    = NULL, # no dendrogram
               show.rownames = rep(F, length(clust.data)), # specify for each omics data
               show.colnames = FALSE, # show no sample names
               # annRow        = annRow, # mark selected features
               color         = col.list,
               annCol        = NULL, # no annotation for samples
               annColors     = NULL, # no annotation color
               width         = 10, # width of each subheatmap
               height        = 5, # height of each subheatmap
               fig.name      = fig.name)

}
