library(warbleR)
#library(textcat)
##mp3 to wav 
r <- readMP3("alok.mp3")  ## MP3 file path
writeWave(r,"tmp.wav",extensible=FALSE)
audio_dur=duration(r) # duration of audio file in secs

## reading wave file
wave_file <- readWave("tmp.wav", from = 1, to = Inf, units = c("seconds"), header = FALSE, toWaveMC = NULL)
head(wave_file)
sound.files="tmp.wav"
selec=1
start=0.0001
end=audio_dur-0.001
inputaudio=data.frame(sound.files,selec,start,end)  ##making selection table
write.csv(inputaudio,"input_audio.csv")
data=read.csv("input_audio.csv")  ### input audio function stored in this csv, data is a dataframe includiin the start and stop time of audio
sp= specan(X=data, bp = c(0, 22),fast = TRUE)
#mode and centroid of the given wave file
wavespec=spec(wave_file,f=44100,plot = FALSE)
waveanalysis=specprop(wavespec,f=44100,flim = c(0,22))
options(digits=7)
mode=as.double(waveanalysis[5])/1000.0     # in kHz
centroid=as.double(waveanalysis[9])/1000   # in kHz

#fundamental frequecies of the acoustic signal
fundfreq=fund(wave_file, f=44100, fmax = 22000)
meanfun=mean(fundfreq, na.rm = T)
minfun=min(fundfreq, na.rm = T)
maxfun=max(fundfreq, na.rm = T)

#creating the dataframe for output 
output=data.frame(sp[4:9],sp[14:16],sp[19], mode, centroid, meanfun,minfun,maxfun, sp[20:24])
write.csv(output, "output_audio_processing.csv")
