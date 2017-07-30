#### DATA PREPARATION

setwd("~/Documents/Experiments/SocialAttitude_all/DATA/Scripts/Script1")

files <- list.files()

for(f in 1:length(files)){
    setwd("~/Documents/Experiments/SocialAttitude_all/DATA/Scripts/Script1")
    bla <- read.csv(files[f], header=T)
    name <- files[f]
    name2 <- sub("*.csv", "", name)
    head(bla)
    bla <- bla[, c('Sender', 'Text')]
    bla$Turn <- 1:nrow(bla)
    bla$TurnSender <- with(bla, interaction(Turn, Sender))
    #setwd("~/Documents/Experiments/SocialAttitude_all/DATA/Scripts/Script2/Rda")
    #fname <- paste('a', name2, sep="", collapse="")
    #fname <- bla
    #save(fname, file=fname)
    setwd("~/Documents/Experiments/SocialAttitude_all/DATA/Scripts/Script2/Csv")
    write.table(bla, file=name, row.names=TRUE)
}

