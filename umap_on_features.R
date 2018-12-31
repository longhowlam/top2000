library(dplyr)
library(h2o)
library(ggplot2)
library(plotly)
library(reticulate)
use_condaenv("my_py36")

top2000TrainSet = top2000 %>% 
  left_join(top2000Audio, by = c("trackid" = "id")) %>% 
  left_join(top2000trackinfo) 
top2000TrainSet = top2000TrainSet %>% mutate(ouderdom = 2018 - as.numeric(stringr::str_sub(releasedate,1,4)))

plot_ly(
  top2000TrainSet, 
  text = ~paste(
    artist, "<br>", song, " ", "<br> top 2000 positie", positie, "<br> spotify popularity", popularity),
  x = ~popularity,
  y = ~positie) 

ggplot(top2000TrainSet, aes(popularity,positie)) + geom_point() + geom_smooth(method = lm) + labs(title = "Top2000 positie versus Spotify popularity")


featurematrix = top2000TrainSet[, 8:18] %>%  as.matrix
featurematrix = top2000TrainSet[, c(5,8:18,26,28)] %>%  as.matrix

umap = import("umap")

embedding = umap$UMAP(
  n_neighbors = 5L,
  n_components = 3L,
  min_dist = 0.15,
  metric='euclidean'
)

## compute UMAP with 3 components
embedding_out = embedding$fit_transform(featurematrix)

top2000TrainSet2 = data.frame(embedding_out)
top2000TrainSet2$trackid = top2000TrainSet$trackid
top2000TrainSet2 = top2000TrainSet2 %>% 
  left_join(top2000TrainSet) %>% 
  mutate(duur_min = as.numeric(duration)/1000/60)

text = paste(top2000TrainSet2$artist, "<br>", top2000TrainSet2$song, " ", top2000TrainSet2$positie)

plot_ly(
  top2000TrainSet2, 
  x = ~X1,
  y = ~X2, 
  z = ~X3,
  color=~positie,
  text = ~paste(
    artist, "<br>", song, " ", "<br>", positie),
  size =.1 , sizes = c(4,5)
) %>% 
  layout(title = '3D umap of Radio2 top 2000 songs 2018')

######## predictie ########################################################################


library(h2o)

# initialiseer h2o via R
#h2o.init(nthreads=-1, port=54323, startH2O = FALSE)
h2o.init()


ttrain.h2o = as.h2o(top2000TrainSet2) 
traintest = h2o.splitFrame(ttrain.h2o)

linreg = h2o.glm(
  x = c(1:3, 29,31,32),
  y = "positie",
  training_frame  = traintest[[1]],
  validation_frame = traintest[[2]]
)
linreg


RFmodel = h2o.randomForest(
  x = c(11:21, 29,31,32),
  y = "positie",
  training_frame  = traintest[[1]],
  validation_frame = traintest[[2]]
  )
RFmodel

h2o.varimp_plot(linreg)
predlm = h2o.predict(linreg, ttrain.h2o) %>% as.data.frame()
top2000TrainSet2$predictedlinreg = predlm$predict

h2o.varimp_plot(RFmodel)
predRF = h2o.predict(RFmodel, ttrain.h2o) %>% as.data.frame()
top2000TrainSet2$predictedRF = predRF$predict

rsq <- function (x, y) cor(x, y) ^ 2
rsq(top2000TrainSet2$positie, top2000TrainSet2$predictedlinreg)
rsq(top2000TrainSet2$positie, top2000TrainSet2$predictedRF)
rsq(top2000TrainSet2$positie, top2000TrainSet2$popularity)


ggplot(top2000TrainSet2, aes(popularity,positie)) + geom_point() + geom_smooth(method = lm) + labs(title = "Top2000 positie versus Spotify popularity")


ggplot(top2000TrainSet2, aes(predictedlinreg,positie)) + geom_point() + geom_smooth() + labs(title = "Top2000 positie versus Random Forest predicted positie")


top2000TrainSet2 = top2000TrainSet2 %>% 
  mutate(
    RFrank = rank(predictedRF)
  )


p = ggplot(top2000TrainSet2, aes(RFrank,positie)) +
  geom_point(
    alpha = 0.75, 
    aes(
      colour = popularity,
      text = paste(
      artist, "<br>", song, " ", "<br> top 2000 positie", positie, "<br> spotify popularity", popularity)
    )
  ) + 
  scale_color_gradient(low="green", high="red") +
  geom_smooth(se = FALSE, size = 1.4) +
  labs(title = "Top2000 positie versus Random Forest voorspelde positie", y="2018 top2000 positie", x = "random forest voorspelde positie")
p
ggplotly(p)




################  

saveRDS(top2000TrainSet, "top2000dataset.RDs")
