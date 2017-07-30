library(crqa)



setwd("~/Documents/Experiments/SocialAttitude_all/DATA/Scripts/Script3_twoseries")

load('lstBLA_wSilence_SeriesWords.Rda')
##lstBLA

str(lstBLA)

names(lstBLA)



listRES.2tsS <- list()
data2tsS <- NULL

nms <- names(lstBLA)

for(n in 1:length(nms)){
    
    bla <- lstBLA[[nms[n]]]
    name <- nms[n]
    Dyad <- substring(gsub("*_.+_.+", '', name), 2)
    head(bla)
    
    length_conversation_w <- length(bla$Text)
    
    speaker1 <- as.character(unique(bla[bla$TextS2 == '88888',]$Speaker))
    speaker2 <- as.character(unique(bla[bla$TextS1 == '99999',]$Speaker))
    
    wordsS1 = unlist(strsplit(bla$TextS1,' '))        # discrete sequence of words speaker 1  (silent = 99999)
    wordsS2 = unlist(strsplit(bla$TextS2,' '))        # discrete sequence of words speaker 2  (silent = 88888)
    
    length(wordsS1); length(wordsS2)     #
    
    ### unique words
    n_unique_S1 <- length(unique(wordsS1)[!(unique(wordsS1) == '99999')]); n_unique_S2 <- length(unique(wordsS2)[!(unique(wordsS2) == '88888')])    
    
    wordsS1 = as.vector(as.matrix(wordsS1))
    wordsS2 = as.vector(as.matrix(wordsS2))
    
    types = unique(c(wordsS1,wordsS2))[!(unique(c(wordsS1,wordsS2)) %in% c('88888', '99999'))]
    n_types_union_joint_as_dyad <- length(types)         # unique words as a dyad
    
    #shared and different words
    states = intersect(wordsS1, wordsS2)
    n_types_shared_intersected <- length(states)     #shared (in common between the two speakers)
    #
    #difstates = c(setdiff(wordsS1, wordsS2), setdiff(wordsS2, wordsS1))
    #length(difstates)  #195 different
    
    
    ### type-by-tokens ratio (TTR)
    ### number of types/number of tokens
    tokens <- c(wordsS1,wordsS2)[!(c(wordsS1,wordsS2) %in% c('88888', '99999'))]
    TTR <- length(types) / length(tokens)
    
    # same as:
    #freqtab <- as.data.frame(as.matrix(xtabs(~tokens)))
    #nrow(freqtab) / colSums(freqtab)

    
    
    recode = checkts(wordsS1, wordsS2, datatype = "categorical", thrshd = 10, pad = FALSE)[[1]]      #code series numerically (unique code for speaker's silence)
    
    wordSeriesS1 = recode[,1]
    wordSeriesS2 = recode[,2]
    
    #crqa parameters
    delay = 1; embed =  1 ; rescale =  1; radius = 0.00001;
    normalize = 0; minvertline = 2; mindiagline = 2; whiteline = FALSE;
    recpt = FALSE; tw = 0
    
    #crqa run   (side = 'both' / default)
    res = crqa(wordSeriesS1, wordSeriesS2, delay, embed, rescale, radius,
                  normalize, minvertline, mindiagline, tw,  whiteline, recpt)
    
    str(res)
    
    # collect res in a list
    listRES.2tsS[[name]] <- res
    
    #collect outputs in a dataset
    dataT = NULL
    dataTT = NULL
        sublst <- listRES.2tsS[[n]]         # n is index of unique file name/ dyad 
        resPar <- unlist( sublst[1:9] )  
        dataT <- rbind(dataT, resPar)  
        dataTT <- cbind(dataT, Dyad, speaker1, speaker2, TTR, n_types_union_joint_as_dyad, n_types_shared_intersected, length_conversation_w, n_unique_S1, n_unique_S2)
        data2tsS <- rbind(data2tsS, dataTT)
        rownames(data2tsS)[n] <- name       # name is unique name for that filename/dyad
    

}

str(listRES.2tsS)
names(listRES.2tsS)

###### SAVE LIST ########

#setwd("~/Documents/Experiments/SocialAttitude_all/DATA/Crqa/Datasets")
#save(listRES.2tsS, file="listRES.2series_2diag_wSilence.Rda")

#########################



str(data2tsS)
data2tsS

######### SAVE DATASET w/outcomes ########

#setwd("~/Documents/Experiments/SocialAttitude_all/DATA/Crqa/Datasets")
#save(data2tsS, file='data_2series_wSilence.Rda')


dataframe2tsS <- as.data.frame(data2tsS)
#dataframe2tsS == data2tsS


### add conditions

dataframe2tsS$condition <- NA

dim(dataframe2tsS)

for(i in 1:nrow(dataframe2tsS)){
    name <- as.character(rownames(dataframe2tsS)[i])
    x <- strsplit(name, "\\_")
    code <- sapply(x, function(y) y[2])
    condi <- ifelse(code %in% c("UCP", "DEF", "ZAZ", "SAZ", "MAP", "GMJ", "FUR", "BEE", "BRO","BUM","DEA","DEF","EEL","KIT","MUM","ROO","STA","WEE"), 'Individualistic', 'Collectivist')
    dataframe2tsS[i,]$condition <- condi
}

dataframe2tsS$condition <- as.factor(dataframe2tsS$condition)

str(dataframe2tsS)


setwd("~/Documents/Experiments/SocialAttitude_all/DATA/Crqa/Datasets")
write.table(dataframe2tsS, file='dataframe_2series_2diag_wSilence.csv', row.names=F)



##### !!!!
# a16sept10_DEA_F_bethany_maria has ENTR = 0, and rENTR = NaN
## this is because L = max L :-)


#for(c in 1:dim(data2tsS)[2]){
#    data2tsS[,c] <- as.numeric(as.matrix(data2tsS[,c]))   
#}
