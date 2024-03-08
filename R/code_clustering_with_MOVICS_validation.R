library(tidyverse)
library(umap)
library(MOVICS)
library(class)


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
    return(clust.data)
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

flist <- c('feature_static_all.csv',
           'feature_dynamic_all.csv')
src.info <- read_csv('data_from_source.csv')
dt.list.train <- list()
dt.list.test <- list()
for(fname in flist)
{
  dt.i <- read_csv(fname) %>% select(-1)
  baseName <- str_replace(fname, '.csv', '')

  dt.i.train <- dt.i[which(src.info$Source == 0), ]
  dt.i.test <- dt.i[which(src.info$Source == 1), ]

  dt.list.train[[baseName]] <- dt.i.train
  dt.list.test[[baseName]] <- dt.i
}

# dimension reduction.
clust.data <- DR_umap(dt.list.train = dt.list.train, dt.list.test = dt.list.test)

# clustering analysis
cmoic.clust <- clust_identify(clust.data[[1]])

# predict the cluster
test.pred <- knn_train_test(cmoic.clust,
               clust.data.train = clust.data[[1]],
               clust.data.test = clust.data[[2]])

src.info$Group <- test.pred

out.clust <- data.frame(samID = paste0('subj', 1:length(test.pred)),
                    clust = test.pred)

write_csv(src.info, 'grp_info.csv')



# Filter the data to draw heatmap -----------------------------------------

out.clust.validation <- out.clust %>% slice(which(src.info$Source == 1)) %>%
  as.data.frame
rownames(out.clust.validation) <- out.clust.validation$samID


clust.data.2 <- clust.data[[2]]
clust.data.2$feature_static_all <- clust.data.2$feature_static_all[, which(src.info$Source == 1)]
clust.data.2$feature_dynamic_all <- clust.data.2$feature_dynamic_all[, which(src.info$Source == 1)]




# Draw heatmap ------------------------------------------------------------

plotdata <- getStdiz(data       = clust.data.2,
                     halfwidth  = c(2,2), # no truncation for mutation
                     centerFlag = c(T,T), # no center for mutation
                     scaleFlag  = c(T,T)) # no scale for mutation

static.col   <- c("#00FF00", "#008000", "#000000", "#800000", "#FF0000")
dynamic.col <- c("#0074FE", "#96EBF9", "#FEE900", "#F00003")
# t1ce.col   <- c("#00FF00", "#008000", "#000000", "#800000", "#FF0000")
# t2wi.col    <- c("#6699CC", "white"  , "#FF3C38")
col.list   <- list(static.col, dynamic.col)


# feat   <- cmoic.clust$feat.res
# feat1  <- feat[which(feat$dataset == "static"),][1:10,"feature"]
# feat2  <- feat[which(feat$dataset == "dynamic"),][1:10,"feature"]
#
# annRow <- list(feat1, feat2)

# comprehensive heatmap (may take a while)
getMoHeatmap(data          = plotdata,
             row.title     = c('static', 'dynamic'),
             is.binary     = c(F,F), # the 4th data is mutation which is binary
             legend.name   = c('static', 'dynamic'),
             clust.res     = out.clust.validation, # cluster results
             clust.dend    = NULL, # no dendrogram
             show.rownames = c(F,F), # specify for each omics data
             show.colnames = FALSE, # show no sample names
             # annRow        = annRow, # mark selected features
             color         = col.list,
             annCol        = NULL, # no annotation for samples
             annColors     = NULL, # no annotation color
             width         = 10, # width of each subheatmap
             height        = 5, # height of each subheatmap
             fig.name      = "COMPREHENSIVE HEATMAP OF ICLUSTERBAYES_validation")
