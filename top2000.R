###############################################################################
##
##  visualise songs of the top 2000

#### libraries needed #########################################################
library(httr)
library(purrr)
library(furrr)
library(dplyr)
library(reticulate)
library(plotly)

plan(multiprocess)

#### spotify credientials #####################################################
#clientID = "123456789"
#secret = "ABCDEFGHIJ"
clientID = readRDS("clientID.RDs")
secret = readRDS("secret.RDs")

###### Spotify api helper functions ###########################################

## given a clientID and a secret we can retrieve a token that is 
## needed for further Spotify API calls

GetSpotifyToken = function(clientID, secret){
  
  response <- POST(
    'https://accounts.spotify.com/api/token',
    accept_json(),
    authenticate(clientID, secret),
    body=list(grant_type='client_credentials'),
    encode='form',
    verbose()
  )
  
  if (status_code(response) == 200){
    return(content(response)$access_token)
  }
  else{
    return("")
  }
}

#### given an userid and a playlistID we extract the tracks from this specific playlist
#### and put some of the track info in a tibble

ExtractTracksFromPlaylist = function(offset = 0, ownerID, playlistID, clientID, secret, mylabel = ""){ 
  ### get the playlist itself
  URI = paste0(
    "https://api.spotify.com/v1/users/", 
    ownerID,
    "/playlists/", 
    playlistID,
    "/tracks",
    "?offset=",
    offset
  )
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  r2 = GET(url = URI, add_headers(Authorization = HeaderValue))
  tracks = content(r2)
  
  ## put track info in a data set, we need to extract it from nested lists
  tibble::tibble(
    label       = mylabel,
    artist      = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["artists"]][[1]][["name"]]),
    song        = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["name"]]),
    preview_url = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["preview_url"]] %||% ""),
  #  image       = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["album"]][["images"]][[1]][["url"]]),
    duration    = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["duration_ms"]]),
    trackid     = purrr::map_chr(tracks$items, .f=function(x)x[["track"]][["id"]])
  )
}

######## get the songs from the top 2000 playlists ############################
# fortunately radio2 has put a spotify playlist together

ownerID = "radio2nl"
playlistID = "1DTzz7Nh2rJBnyFbjsH1Mh"
#spotify:user:radio2nl:playlist:1DTzz7Nh2rJBnyFbjsH1Mh
## we use purrr to get all songs, spotify allows you to get max 100 songs in a call

ofsets = c(0,(1:19)*100 +1)
top2000 = purrr::map_df(
  ofsets,
  ExtractTracksFromPlaylist,
  ownerID = ownerID,  
  playlistID = playlistID,
  clientID, 
  secret, 
  mylabel = "top2000")

top2000 = top2000 %>% mutate(positie = 1:1984)

## ignore the songs without preview URL
top2000mp3 = top2000 %>% filter(preview_url != "")

##### download the mp3's ######################################################
## into a directory called mp3songs 

for(i in seq_along(top2000mp3$preview_url))
{
  download.file(
    top2000mp3$preview_url[i], 
    destfile = paste0("mp3songs/", top2000mp3$trackid[i]),
    mode="wb" 
  )
}

########  Calculate mel spectogram ############################################
## using python librosa pacakge (via the reticulate package)
## all downloaded mp3's are put trhough librosa
use_condaenv("my_py36")
librosa = import("librosa")
ff = librosa$feature

#### helper function around librosa call 
mfcc = function(file, dir, .pb = NULL)
{
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  pathfile = paste0(dir, "/", file)
  mp3 = librosa$load(pathfile)
  
  # calc  mel to file
  ff$melspectrogram(
      mp3[[1]], 
      sr = mp3[[2]],
      n_mels=96)
}

mp3s = list.files("mp3songs/")
pb = progress_estimated(length(mp3s))

### now using furrr::map we can calculate mfcc for each mp3 in the folder mp3songs

t0 = proc.time()
AllSongsMFCC = furrr::future_map(mp3s, mfcc, dir = "mp3songs", .pb = pb)
t1 = proc.time()
t1-t0

## create a plot of the mfcc matrix
rotate <- function(x) t(apply(x, 2, rev))
image(rotate(rotate(rotate(AllSongsMFCC[[17]]))))

## create a feature matrix. Simply flatten the matrix
## each song is now a row of 13*1292 values

nsongs = dim(AllSongs)[1]
AllSongsMFCCMatrix = matrix(NA , nrow = nsongs, ncol=96*1292)
for(i in 1:nsongs){
  AllSongsMFCCMatrix[i,] = as.numeric(AllSongsMFCC[[i]])
}


#### UMAP and plotly ####################################################

umap = import("umap")

embedding = umap$UMAP(
  n_neighbors = 5L,
  n_components = 3L,
  min_dist = 0.1,
  metric='euclidean'
)

## compute UMAP with 3 components
embedding_out = embedding$fit_transform(AllSongsMFCCMatrix)



##### Plot the embeddings with plotly #########################################

plotdata = data.frame(embedding_out)
plotdata$trackid = mp3s
plotdata = plotdata %>% 
  left_join(AllSongs) 

plot_ly(
  plotdata, 
  x = ~X1,
  y = ~X2, 
  z = ~X3,
  color=~label,
  text = ~paste(artist, "<br>", song),
  size = 1, sizes = c(1,4) 
) %>% 
  layout(title = '3D umap of artists')



#######  audio features #######################################################

getaudiofeatures  = function(tid){
  Sys.sleep(0.4) # avoid spotify rate limit
  print(tid)
  URI = paste0(
    "https://api.spotify.com/v1/audio-features/",
    tid
  )
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  GET(
    url = URI, add_headers(Authorization = HeaderValue)
  ) %>% 
    content() %>% 
    as.data.frame(stringsAsFactors=FALSE)
}  

out1 = getaudiofeatures(top2000$trackid[155])

#top200audiofeat = furrr::future_map_dfr(top2000$trackid, getaudiofeatures)
top2000Audio = purrr::map_df(top2000$trackid, getaudiofeatures)

saveRDS(top2000Audio, "top2000Audio.RDs")
saveRDS(top2000, "top2000.RDs")

########### additional track info
## met tracks api kan nog extra info van een track ophalen

gettrackinfo  = function(tid){
  Sys.sleep(0.4) # avoid spotify rate limit
  print(tid)
  URI = paste0( "https://api.spotify.com/v1/tracks/",  tid)
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  pp = GET(
    url = URI, add_headers(Authorization = HeaderValue)
  ) %>% 
  content()
  data.frame(
    trackid = tid,
    artistid =  pp$artists[[1]]$id,
    popularity = pp[["popularity"]],
    releasedate = pp[["album"]][["release_date"]],
    stringsAsFactors = FALSE
  )
}

top2000trackinfo = purrr::map_df(top2000$trackid, gettrackinfo)
saveRDS(top2000trackinfo, "top2000trackinfo.RDs")






getartistnfo  = function(aid){
  Sys.sleep(0.4) # avoid spotify rate limit
  print(tid)
  URI = paste0( "https://api.spotify.com/v1/artists/",  aid)
  token = GetSpotifyToken(clientID = clientID, secret = secret)
  HeaderValue = paste("Bearer ", token, sep="")
  pp = GET(
    url = URI, add_headers(Authorization = HeaderValue)
  ) %>% 
    content()
  
   data.frame(
     artistid = aid,
     genre = ifelse(length(pp$genres) < 1, "", pp$genres[[1]]),
     stringsAsFactors = FALSE
   )
}

artiesten = unique(top2000Audio2$artistid)
top2000artistinfo = purrr::map_df(artiesten, getartistnfo)

saveRDS(top2000artistinfo, "top2000artistinfo.RDs")

genres = top2000artistinfo %>% group_by(genre) %>%  summarise(n=n())

###### apply UMAP on the audio features #############################################



