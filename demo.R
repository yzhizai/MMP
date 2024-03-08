library(tidyverse)
library(Matrix)
library(umap)
library(MOVICS)
library(class)

flist <- c('feature_static_all.csv',
           'feature_dynamic_all.csv')
src.info <- read_csv('data_from_source.csv')

dt.list.train <- getDatalist(flist, which(src.info$Source == 0))
dt.list.test <- getDatalist(flist, which(src.info$Source == 1))
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

out.clust.validation <- out.clust %>% slice(which(src.info$Source == 1)) %>%
  as.data.frame
rownames(out.clust.validation) <- out.clust.validation$samID

clust.data.2 <- clust.data[[2]]
draw_Heatmap(clust.data.2, out.clust.validation, fig.name = 'Heatmap')


