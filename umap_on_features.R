library(dplyr)
library(h2o)
library(ggplot2)
library(plotly)
library(reticulate)
use_condaenv("my_py36")

top2000Audio2 = top2000 %>% 
  left_join(top2000Audio, by = c("trackid" = "id")) %>% 
  left_join(top2000trackinfo) 

plot_ly(
  top2000Audio2, 
  x = ~popularity,
  y = ~positie) 

ggplot(top2000Audio2, aes(popularity,positie)) + geom_point() + geom_smooth(method = lm) + labs(title = "Top2000 positie versus Spotify popularity")


featurematrix = top2000Audio2[, 8:18] %>%  as.matrix

umap = import("umap")

embedding = umap$UMAP(
  n_neighbors = 5L,
  n_components = 3L,
  min_dist = 0.15,
  metric='euclidean'
)

## compute UMAP with 3 components
embedding_out = embedding$fit_transform(featurematrix)

plotdata = data.frame(embedding_out)
plotdata$trackid = top2000Audio2$trackid
plotdata = plotdata %>% 
  left_join(top2000Audio2) %>% 
  mutate(duur_min = as.numeric(duration)/1000/60)

text = paste(plotdata$artist, "<br>", plotdata$song, " ", plotdata$positie)

plot_ly(
  plotdata, 
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
ttrain.h2o = as.h2o(top2000Audio2) 


RFmodel = h2o.randomForest(
  x = c(8:18, 23, 26),
  y = "positie",
  training_frame  = ttrain.h2o
  )
RFmodel


h2o.varimp_plot(RFmodel)
pred = h2o.predict(RFmodel, ttrain.h2o) %>% as.data.frame()
top2000Audio2$predicted = pred$predict

rsq <- function (x, y) cor(x, y) ^ 2
rsq(top2000Audio2$positie, top2000Audio2$predicted)


ggplot(top2000Audio2, aes(popularity,positie)) + geom_point() + geom_smooth(method = lm) + labs(title = "Top2000 positie versus Spotify popularity")


ggplot(top2000Audio2, aes(predicted,positie)) + geom_point() + geom_smooth() + labs(title = "Top2000 positie versus Spotify popularity")
