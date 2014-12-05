## building the model
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(Hmisc))

triGramPred <- function(BiTab = is.data.table(), TriTab = is.data.table(), 
                        words = is.character(), bad.words.tab = badWdsTab) {
    ## This function is used to model a 3-gram text prediction algorithm
    ## UniTab: 1-gram term frequency table
    ## BiTab: 2-gram term frequency table
    ## TriTab: 3-gram term frequency table
    ## will return the predicted word given the prefix.
    
    # words must be pre-processed, like to lower case and punctuation or symbol removal.
    words <- getProcessed(words)
    
    predWord <- c(NA, NA); predInd <<- 1   # initialize the predicted words vector.
    
    
    ## Below function is used to get the prefix of any 3-gram term
    getPref <- function(words = is.character()) {
        words <- paste("<s> <s>", words)
        words <- unlist(strsplit(words, " ", fixed = T))
        prefix <- paste(words[length(words) - 1], words[length(words)])
        return(prefix)        
    }
    
    backoff <- function(words = is.character(), pred.words, bad.words.tab = badWdsTab) {
        ## This backoff method is used to reduce 2-gram preffix to 1-gram preffix by eliminate the first word.
        
        words <- unlist(strsplit(words, " ", fixed = T))
        pref <- words[2]
        subBiTab <- BiTab[prefix == pref]
        subRows <- nrow(subBiTab)
        if (subRows > 0) {
            # find out the highest frequency word given the preffix
            TopFreqSet <- subBiTab[subBiTab[, Freq.bi == max(Freq.bi), by = prefix]$V1]
            pred.words <- getPredWord(TopFreqSet, pred.words)
            if (anyNA(pred.words)) {
                setkey(subBiTab, Freq.uni)
                setkey(TopFreqSet, Freq.uni)
                SecFreqSet <- subBiTab[!TopFreqSet]
                if (nrow(SecFreqSet) > 0) {
                    pred.words <- getPredWord(SecFreqSet, pred.words)
                    if (anyNA(pred.words)) {
                        pred.words[predInd] <- "the"
                        predInd <<- predInd + 1
                        if (predInd == 2) {
                            pred.words[predInd] <- "to"
                            predInd <<- predInd + 1
                        }
                    }
                }
                else {
                    pred.words[predInd] <- "the"
                    predInd <<- predInd + 1
                    if (predInd == 2) {
                        pred.words[predInd] <- "to"
                        predInd <<- predInd + 1
                    }
                }
            }
        } else {
            if (substr(pref, nchar(pref), nchar(pref)) == ".") {
                # if the last character is dot, treat it as end-of-sentence.
                pred.words[predInd] <- "I"
                predInd <<- predInd + 1
                if (predInd == 2) {
                    pred.words[predInd] <- "The"
                    predInd <<- predInd + 1
                }
            } else {
                pred.words[predInd] <- "the"
                predInd <<- predInd + 1
                if (predInd == 2) {
                    pred.words[predInd] <- "to"
                    predInd <<- predInd + 1
                }
            }
        }
        return(pred.words)
    }
    
    getPredWord <- function(token.tab = data.table(), pred.words, bad.words.tab = badWdsTab) {
        tabRows <- nrow(token.tab)
        setkey(token.tab, Freq.uni)
        for (i in tabRows:1) { 
            pWord <- token.tab[i]$word
            badW <- bad.words.tab[bad.wds == pWord]
            if (nrow(badW) == 0) {
                if (is.na(pred.words[predInd]) && predInd <= 2) {
                    #if (pref == "<s> <s>") pWord <- capitalize(pWord)
                    pred.words[predInd] <- pWord
                    predInd <<- predInd + 1
                }
            }
        }
        return(pred.words)
    }
    
    pref <- getPref(words)
    subTriTab <- TriTab[prefix == pref]
    subRows <- nrow(subTriTab)
    if (subRows > 0) {
        # find out the highest frequency word given the preffix
        TopFreqSet <- subTriTab[subTriTab[, Freq.tri == max(Freq.tri), by = prefix]$V1]
        predWord <- getPredWord(TopFreqSet, predWord)
        if (anyNA(predWord)) {
            setkey(subTriTab, Freq.uni)
            setkey(TopFreqSet, Freq.uni)
            SecFreqSet <- subTriTab[!TopFreqSet]
            if (nrow(SecFreqSet) > 0) {
                predWord <- getPredWord(SecFreqSet, predWord)
                if (anyNA(predWord)) predWord <- backoff(pref, predWord)
            }
            else predWord <- backoff(pref, predWord)            
        }
    } else predWord <- backoff(pref, predWord)
    
    for (i in 1:length(predWord)) {
        if (is.na(predWord[i])) predWord[i] <- " "
        else if (predWord[i] == "</S>") predWord[i] <- "<.?!>"
    }
    
    return(predWord)
}

getProcessed <- function(words = is.character()) {
    # preprocess the words string, conver to lower case, take care punctuations, symbols, numbers or 
    # even email address and so on.
    words <- tolower(words)
    
    # keep only english characters, numbers and some useful regular marks
    words <- gsub("[^a-zA-Z0-9@'’\\$\\.\\?\\!_ \\-]", "", words)
    
    # replace email with <email>, replace website with <website>
    emailExp <- "[-a-z0-9_.%]+@[-a-z0-9_.%]+\\.[a-z]+"
    words <- gsub(emailExp, "<email>", words)
    websiteExp <- "http:[-a-z0-9/_.%\\?]+"
    words <- gsub(websiteExp, "<website>", words)
    
    # replace numbers with <num>
    numExp <- "-*[0-9]+((\\.|/|\\-|\\:)[0-9]+)*"   # represent all numbers                    
    words <- gsub(numExp, "<num>", words)
    words <- gsub("( | -)\\.<num>", " <num>", words)
    
    words <- gsub("\\.+", "\\.", words)
    
    # remove all _ - signs except dash in words.
    words <- gsub("_+", " ", words)
    words <- gsub("(([^a-z0-9>]-+[^a-z0-9<])|(^-+)|([^a-z0-9>]-+)|(-+[^a-z0-9<])|(-+$))+", " ", words)
    
    # remove single quote, but preserve apostrophe in the format like "John's"
    apossExp <- "('|’)s "   # replace all apostrophe like 's  with <aposs>
    apostExp <- "n('|’)t "  # replace all apostrophe like n't with <apost>
    apost2Exp <- "n('|’)t\\." # replace all apostrophe like n't. with <apost2>
    aposmExp <- "i('|’)m "  # replace all apostrophe like i'm with <aposm>
    aposlExp <- "('|’)ll "  # replace all apostrophe like 'll with <aposl>
    aposrExp <- "('|’)re "   # replace all apostrophe like 're with <aposr>
    aposdExp <- "('|’)d "    # replace all apostrophe like 'd with <aposd>
    aposvExp <- "('|’)ve "   # replace all apostrophe like 've with <aposv>
    
    words <- gsub(apossExp, "<aposs>", words)
    words <- gsub(apostExp, "<apost>", words)
    words <- gsub(apost2Exp, "<apost2>", words)
    words <- gsub(aposmExp, "<aposm>", words)
    words <- gsub(aposlExp, "<aposl>", words)
    words <- gsub(aposrExp, "<aposr>", words)
    words <- gsub(aposdExp, "<aposd>", words)
    words <- gsub(aposvExp, "<aposv>", words)
    
    punctMarks <- "'+|’+"
    words <- gsub(punctMarks, "", words)
    
    # since most of the punctuations cleared, now it's time to restore back the apostrophes.
    words <- gsub("<aposs>", "'s ", words)
    words <- gsub("<apost>", "n't ", words)
    words <- gsub("<apost2>", "n't.", words)
    words <- gsub("<aposm>", "i'm ", words)
    words <- gsub("<aposl>", "'ll ", words)
    words <- gsub("<aposr>", "'re ", words)
    words <- gsub("<aposd>", "'d ", words)
    words <- gsub("<aposv>", "'ve ", words)
    
    # remove extra while space.
    words <- gsub(" +", " ", words)
    words <- gsub("^ ", "", words)
    
    words <- gsub("\\!+|\\?+", " <s> <s> ", words)
    
    words <- gsub("([Jj]an\\.)|([Jj]an )", "January ", words)
    words <- gsub("([Ff]eb\\.)|([Ff]eb )", "February ", words)
    words <- gsub("([Mm]ar\\.)|([Mm]ar )", "March ", words)
    words <- gsub("([Aa]pr\\.)|([Aa]pr )", "Apirl ", words)
    words <- gsub("([Mm]ay\\.)|([Mm]ay )", "May ", words)
    words <- gsub("([Jj]un\\.)|([Jj]un )", "June ", words)
    words <- gsub("([Jj]ul\\.)|([Jj]ul )", "July ", words)
    words <- gsub("([Aa]ug\\.)|([Aa]ug )", "August ", words)
    words <- gsub("([Ss]ep\\.)|([Ss]ep )", "September ", words)
    words <- gsub("([Oo]ct\\.)|([Oo]ct )", "October ", words)
    words <- gsub("([Nn]ov\\.)|([Nn]ov )", "November ", words)
    words <- gsub("([Dd]ec\\.)|([Dd]ec )", "December ", words)
    
    words <- gsub(" +", " ", words)
}