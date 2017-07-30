#### DATA PREPARATION
#### for CROSS-RECURRENCE analysis

setwd("~/Documents/Experiments/SocialAttitude_all/DATA/Scripts/Script2/Csv")

files <- list.files()


lstBLA <- list()


### note to coding:
### we only keep in meaningufl symbols that substitutes words: / (=or); = (=means); + (=and); & (=and); %; $; £
### we got rid of the others: ""; !; ?; *; ( ); -; ...; , ; .



    
for(f in 1:length(files)){
    #setwd("~/Documents/Experiments/SocialAttitude_all/DATA/Scripts/Script2/Csv")
    
    TEMP1 <- NULL
    blabla <- as.character()
    #blablabla <- NULL
    
    bla <- read.csv(files[f], header=T, sep='')
    name <- files[f]
    name1 <- sub("*.csv", "", name)
    name2 <- paste('a', sub("*.csv", "", name), sep='', collapse='')
    head(bla)
    bla$Text <- as.character(bla$Text)
    
    #
    
    n_row <- 1:nrow(bla)
    for(r in 1:length(n_row)){
        
        dialogue <- as.character(bla[r,]$Text)
        dialogue <- tolower(dialogue)      #transform everything to small caps
        dialogue = unlist(strsplit(dialogue,' '))
        
        #contains non-alphabetic characters ([^[:alnum:]]) (anywhere in the string)
        #dialogue[grep("[^[:alnum:]]", dialogue)]     
        
        dialogue <- gsub("\\?", " ?", dialogue)     #to put a space between word and "?" so to treat "?" as a separate word
        dialogue <- gsub("\\!", " !", dialogue)
        dialogue <- gsub("\\...", " ... ", dialogue)
        dialogue <- gsub("\\=", " = ", dialogue)      #put a space before and after '='
        dialogue[grep("[[:alpha:]]+/", dialogue)] <- gsub("/", " / ", dialogue[grep("[[:alpha:]]+/", dialogue)])      #put a space before and after '/' if before it there is a character letter, so not ':' because that's an emoticon
        dialogue[grep("[[:alpha:]]+\\:", dialogue)] <- gsub("\\:", "", dialogue[grep("[[:alpha:]]+\\:", dialogue)])     #get rid of ':' but only if not part of emoticon
        dialogue[grep("[[:alpha:]]+\\;", dialogue)] <- gsub("\\;", "", dialogue[grep("[[:alpha:]]+\\;", dialogue)]) 
        #
        dialogue = unlist(strsplit(dialogue,' '))
        
        dialogue <- gsub("\\*", "", dialogue)       #get rid of '*' symbol
        dialogue <- gsub("\\-", "", dialogue)      #get rid of '-'
        dialogue <- gsub("\\?", "", dialogue)      #get rid of '?'
        dialogue <- gsub("\\!", "", dialogue)      #get rid of '!'
        
        dialogue[grep(",", dialogue)] <- gsub("\\,", " ,", dialogue[grep(",", dialogue)])   #put space between ',' and preceding word
        dialogue[grep(",", dialogue)] <- gsub("\\,", "", dialogue[grep(",", dialogue)])     #get rid of ','
        
        
        #word followed by a space
        dialogue[grep("\\ ", dialogue)] <- gsub("\\ ", "", dialogue[grep("\\ ", dialogue)])
        
        dialogue <- gsub("\\.", "", dialogue)       #get rid of '.'
        #
        dialogue <- gsub("\\  ", " ", dialogue)     #if two spaces, make one
        
        dialogue[grep("([[:alpha:]]+|^)\\)", dialogue)]    #)
        dialogue[grep("([[:alpha:]]+|^)\\)", dialogue)] <- unlist(strsplit(dialogue[grep("([[:alpha:]]+|^)\\)", dialogue)], ')'))
        
        dialogue[grep("([[:alpha:]]+|^)\\(", dialogue)]     #(
        dia <- unlist(strsplit(dialogue[grep("([[:alpha:]]+|^)\\(", dialogue)], '\\('))
        dia <- dia[grep(".+", dia)]
        dialogue[grep("([[:alpha:]]+|^)\\(", dialogue)] <- dia
        
        dialogue <- gsub("\\]", "", dialogue)
        
        dialogue <- gsub("\\|", "", dialogue)
        
        dialogue = unlist(strsplit(dialogue,' '))
        bla[r,]$Text <- paste(dialogue, collapse=" ")
    }
    
    bla <- bla[1:(length(bla$Text)-2),]          #get rid of final $$
    
    
    for(i in 1:length(bla$Text)){
        turn <- as.character(bla$Text[i])
        turn <- strsplit(turn, " ")
        blabla <- c(blabla, turn)
    }
    
    # get dimension of list
    lis <- lapply(blabla, lapply, length)
    length(lis)
    
    #create new dataframe with one unique time serie of words and corresponding turn and speaker
    for(j in 1:length(lis)){
        turn2 <- unlist(blabla[j])
        turn3 <- cbind(as.vector(turn2), rep(j, length(turn2)), as.character(rep(bla$Sender[j], length(turn2))))
        TEMP1 <- rbind(TEMP1, turn3)
    }
    
    # rename column names
    blablabla <- as.data.frame(TEMP1)
    colnames(blablabla)[1] <- 'Text'
    colnames(blablabla)[2] <- 'Turn'
    colnames(blablabla)[3] <- 'Speaker'
    
    
    # name of subjects in this script
    subj <- levels(blablabla$Speaker)
    
    #create the two time series of words
    blablabla$Text <- as.character(blablabla$Text)
    blablabla$TextS1 <- with(blablabla, ifelse(Speaker == subj[1], Text, '99999'))
    blablabla$TextS2 <- with(blablabla, ifelse(Speaker == subj[2], Text, '88888'))
    
    
    lstBLA[[name2]] <- blablabla
    
  
}    
    

str(lstBLA)


##### SAVE LIST #####
    
#setwd("~/Documents/Experiments/SocialAttitude_all/DATA/Scripts/Script3_twoseries")
#save(lstBLA, file='lstBLA_wSilence_SeriesWords.Rda')
   
#####################




