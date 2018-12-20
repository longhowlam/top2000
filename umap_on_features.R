library(dplyr)
library(plotly)
library(reticulate)
use_condaenv("my_py36")

top2000 = top2000 %>% mutate(positie = 1:1987)
top2000Audio2 = top2000 %>% 
  left_join(top2000Audio, by = c("trackid" = "id")) %>% 
  left_join(top2000trackinfo) %>% 
  left_join(top2000artistinfo)

plot_ly(
  top2000Audio2, 
  x = ~popularity,
  y = ~positie) 

featurematrix = top2000Audio2[, 8:18] %>%  as.matrix

umap = import("umap")

embedding = umap$UMAP(
  n_neighbors = 4L,
  n_components = 3L,
  min_dist = 0.05,
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
  color=~genre,
  text = ~paste(
    artist, "<br>", song, " ", "<br>", positie),
  size =.1 , sizes = c(4,5)
) %>% 
  layout(title = '3D umap of top 2000 songs')

