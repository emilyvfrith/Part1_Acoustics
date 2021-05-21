## Bio8068 assessment part 1
## Bird Acoustics
## Emily Frith 05/05/2021
## lets begin!

## install the packages required for analysis

install.packages("devtools")
devtools::install_github("https://github.com/DenaJGibbon/behaviouR")
install.packages("warbleR")
source("nes8010.R")

library(behaviouR)
library(warbleR)
library(stringr)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)
library(vegan)

## all required packages should now be installed

## now must download songbird data for analysis
## going to use the Eurasian Blue Tit (Cyanistes caeruleus)
## Eurasiam Wren (Troglodytes troglodytes)
## and song thrush (Turdus philomelos)
## comapring song calls
## restricted records to the UK and betweem 5 and 30 seconds in length

## check records in xeno-canto as download is set to false

##check number of recordings for blue tit
bluetit <- query_xc(qword = 'Cyanistes caeruleus cnt:"united kingdom"
                            type:song len:5-30', download = FALSE)

##check for number of recordings for wren
wren <- query_xc(qword = 'Troglodytes troglodytes cnt:"united kingdom"
                            type:song len:5-30', download = FALSE)

## check for number of recordings for song hrush
thrush <- query_xc(qword = 'Turdus philomelos cnt:"united kingdom"
                            type:song len:5-30', download = FALSE)


## now the species for analysis have been identified can download
## must tag the audio with the species and vocalisation type
## also convert from .MP3 to .WAV

## create subfolders in project for the audio to be sorted in
dir.create(file.path("bluetit_song"))
dir.create(file.path("wren_song"))
dir.create(file.path("thrush_song"))

## the MP3 files can now be downloaded into respective folders
query_xc(X = bluetit, path="bluetit_song")
query_xc(X = wren, path="wren_song")
query_xc(X = thrush, path="thrush_song")

## these are in.MP3 and must be converted to .WAV
## next section of code is renaming the bluetit files
oldbluetit <- list.files("bluetit_song", full.names=TRUE)
newbluetit <- NULL
for(file in 1:length(oldbluetit)){
  curr_file <- str_split(oldbluetit[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), 
                    collapse="")
  newbluetit <- c(newbluetit, new_name)
}
file.rename(oldbluetit, newbluetit)

## next section of code is renaming wren files
oldwren <- list.files("wren_song", full.names=TRUE)
newwren <- NULL
for(file in 1:length(oldwren)){
  curr_file <- str_split(oldwren[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), 
                    collapse="")
  newwren <- c(newwren, new_name)
}
file.rename(oldwren, newwren)

## next section of code renames song thrush files
oldthrush <- list.files("thrush_song", full.names=TRUE)
newthrush <- NULL
for(file in 1:length(oldthrush)){
  curr_file <- str_split(oldthrush[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), 
                    collapse="")
  newthrush <- c(newthrush, new_name)
}
file.rename(oldthrush, newthrush)

## all .MP3 files have now been renamed
## can now copy all these files to a new subfolder
## this new folder will be called songbird_audio

dir.create(file.path("songbird_audio"))
file.copy(from=paste0("bluetit_song/",list.files("bluetit_song")),
          to="songbird_audio")
file.copy(from=paste0("wren_song/",list.files("wren_song")),
          to="songbird_audio")
file.copy(from=paste0("thrush_song/",list.files("thrush_song")),
          to="songbird_audio")

## now we are converting the .MP3 files in the songbird_audio folder
## to .WAV files
## bulk convert using the mp32wav function
## also remove the unwanted mp3s to dave precious diskspace

mp32wav(path="songbird_audio", dest.path="songbird_audio")
## before this step check to make sure conversion was successful in file explorer
unwanted_mp3 <- dir(path="songbird_audio", pattern="*.mp3")
file.remove(paste0("songbird_audio/", unwanted_mp3))

## all prep has been done and the files are ready for analysis!
## :)

## visualising and analysing the songs of 3 different birds

## bluetit
## produce an oscillogram and a spectrogram for a single bluetit
bluetit_wav <- readWave("songbird_audio/Cyanistescaeruleus-song_627335.wav")
bluetit_wav
## plot an oscillogram
oscillo(bluetit_wav)
## plot a spectrogram of the single bird
SpectrogramSingle(sound.file = "songbird_audio/Cyanistescaeruleus-song_627335.wav",
                  Colors = "Colors")

## wren
wren_wav <- readWave("songbird_audio/Troglodytestroglodytes-song_643107.wav")
wren_wav
## plot oscillogram
oscillo(wren_wav)
## plot spectrogram
SpectrogramSingle(sound.file = "songbird_audio/Troglodytestroglodytes-song_643107.wav",
                  Colors = "Colors")

## song thrush
thrush_wav <- readWave("songbird_audio/Turdusphilomelos-song_609455.wav")
thrush_wav
## plot oscillogram
oscillo(thrush_wav)
## plot spectrogram
SpectrogramSingle(sound.file = "songbird_audio/Turdusphilomelos-song_609455.wav",
                  Colors = "Colors")

## now graphs for all individual birds are done
## move on to an MFCC of all these different songbird calls
## calls can be quite high so max frequency increased to 10000

songbird_mfcc <- MFCCFunction(input.dir = "songbird_audio",
                              max.freq = 10000)

## now create a PCA of the data for visualisation
songbird_pca <- ordi_pca(songbird_mfcc[, -1], scale=TRUE)
summary(songbird_pca)

## plot PCA values
songbird_sco <- ordi_scores(songbird_pca, display="sites")
songbird_sco <- mutate(songbird_sco, group_code = songbird_mfcc$Class)

ggplot(songbird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point() 

#####################################################################
## please ignore - below code is not being used
## spectrogram displayed together
#dir.create(file.path("spectrogram"))
#file.copy(from=paste0("songbird_audio/Cyanistescaeruleus-song_627335.wav",list.files("songbird_audio")),
 #         to="spectrogram")
#file.copy(from=paste0("songbird_audio/Troglodytestroglodytes-song_643107.wav",list.files("songbird_audio")),
#          to="spectrogram")
#file.copy(from=paste0("songbird_audio/Turdusphilomelos-song_609455.wav",list.files("songbird_audio")),
 #         to="spectrogram")

#SpectrogramFunction(input.dir = "spectrogram", min.freq = 500, max.freq = 10000,
#                    colors = "colors")
