### keep capitials
dataDir <- "./final/en_US/sample/"
training <- training[nchar(training) > 8]

# remove all symbols except endmarks, and other necessary marks
training <- gsub("[^a-zA-Z0-9@'’\\$\\.\\?\\!_ \\-]", "", training)

# replace email with <email>, replace website with <website>
emailExp <- "[-a-zA-Z0-9_.%]+@[-a-zA-Z0-9_.%]+\\.[a-zA-Z]+"
training <- gsub(emailExp, "<email>", training)
websiteExp <- "http:[-a-zA-Z0-9/_.%\\?]+"
training <- gsub(websiteExp, "<website>", training)

# replace numbers with <num>
numExp <- "-*[0-9]+((\\.|/|\\-|\\:)[0-9]+)*"   # represent all numbers                    
training <- gsub(numExp, "<num>", training)
training <- gsub("( | -)\\.<num>", " <num>", training)

training <- gsub("\\.+", "\\.", training)

# remove all _ - signs except dash in words.
training <- gsub("_+", " ", training)
training <- gsub("(([^a-zA-Z0-9>]-+[^a-zA-Z0-9<])|(^-+)|([^a-zA-Z0-9>]-+)|(-+[^a-zA-Z0-9<])|(-+$))+", " ", training) 

# remove single quote, but preserve apostrophe in the format like "John's"
apossExp <- "('|’)s "   # replace all apostrophe like 's  with <aposs>
apostExp <- "n('|’)t "  # replace all apostrophe like n't with <apost>
apost2Exp <- "n('|’)t\\." # replace all apostrophe like n't. with <apost2>
aposmExp <- "[Ii]('|’)m "  # replace all apostrophe like i'm with <aposm>
aposlExp <- "('|’)ll "  # replace all apostrophe like 'll with <aposl>
aposrExp <- "('|’)re "   # replace all apostrophe like 're with <aposr>
aposdExp <- "('|’)d "    # replace all apostrophe like 'd with <aposd>
aposvExp <- "('|’)ve "   # replace all apostrophe like 've with <aposv>

training <- gsub(apossExp, "<aposs>", training)
training <- gsub(apostExp, "<apost>", training)
training <- gsub(apost2Exp, "<apost2>", training)
training <- gsub(aposmExp, "<aposm>", training)
training <- gsub(aposlExp, "<aposl>", training)
training <- gsub(aposrExp, "<aposr>", training)
training <- gsub(aposdExp, "<aposd>", training)
training <- gsub(aposvExp, "<aposv>", training)

punctMarks <- "'+|’+"
training <- gsub(punctMarks, "", training)

# since most of the punctuations cleared, now it's time to restore back the apostrophes.
training <- gsub("<aposs>", "'s ", training)
training <- gsub("<apost>", "n't ", training)
training <- gsub("<apost2>", "n't.", training)
training <- gsub("<aposm>", "I'm ", training)
training <- gsub("<aposl>", "'ll ", training)
training <- gsub("<aposr>", "'re ", training)
training <- gsub("<aposd>", "'d ", training)
training <- gsub("<aposv>", "'ve ", training)

training <- gsub(" +", " ", training)
training <- gsub("^ | $", "", training)

## sentence segmentation
# using apache-openNLP package, executed through command line. Executed from file train-2c-w-cap.txt, 
# wrote the result to train-3c-w-cap.txt
con <- file(paste0(dataDir, "train-3c-w-cap.txt"), "r")
training <- readLines(con)
close(con)

# eliminate the endmarks
training <- gsub("\\!|\\?", "", training)

training <- gsub("([Jj]an\\.)|([Jj]an )", "January ", training)
training <- gsub("([Ff]eb\\.)|([Ff]eb )", "February ", training)
training <- gsub("([Mm]ar\\.)|([Mm]ar )", "March ", training)
training <- gsub("([Aa]pr\\.)|([Aa]pr )", "Apirl ", training)
training <- gsub("([Mm]ay\\.)|([Mm]ay )", "May ", training)
training <- gsub("([Jj]un\\.)|([Jj]un )", "June ", training)
training <- gsub("([Jj]ul\\.)|([Jj]ul )", "July ", training)
training <- gsub("([Aa]ug\\.)|([Aa]ug )", "August ", training)
training <- gsub("([Ss]ep\\.)|([Ss]ep )", "September ", training)
training <- gsub("([Oo]ct\\.)|([Oo]ct )", "October ", training)
training <- gsub("([Nn]ov\\.)|([Nn]ov )", "November ", training)
training <- gsub("([Dd]ec\\.)|([Dd]ec )", "December ", training)

training <- gsub("\\.+$", "", training)
training <- gsub(" +", " ", training)
training <- training[nchar(training) > 8]

# separate out training and validation sets.
set.seed(141122)
sel <- rbinom(length(training), 1, 0.6)
sel <- (sel == 1)
training2 <- training[sel]
validation <- training[!sel]
training <- training2
rm(training2)

file.create(paste0(dataDir, "train-w-cap1.txt"))
file.create(paste0(dataDir, "validation-w-cap1.txt"))
con <- file(paste0(dataDir, "train-w-cap1.txt"), "w")
writeLines(training, con, sep = "\n")
close(con)
con <- file(paste0(dataDir, "validation-w-cap1.txt"), "w")
writeLines(validation, con, sep = "\n")
close(con)
rm(sel)
rm(validation)


# N-gram tokenization
library(RWeka)
library(data.table)

# unigram tokens
len <- length(training)
UniTokenTab1 <- as.data.frame(table(NGramTokenizer(training[1:round(len/2)], Weka_control(min = 1, max = 1, delimiters = " \n"))))
UniTokenTab2 <- as.data.frame(table(NGramTokenizer(training[round(len/2)+1:len], Weka_control(min = 1, max = 1, delimiters = " \n"))))
UniTokenTab <- rbind(UniTokenTab1, UniTokenTab2)
rm(UniTokenTab1, UniTokenTab2)
UniTokenTab$Var1 <- tolower(UniTokenTab$Var1)

UniTokenTab <- aggregate(Freq ~ Var1, UniTokenTab, sum)
UniTokenTab <- data.table(UniTokenTab, key = "Var1")
UniTokenTab <- UniTokenTab[UniTokenTab$Freq > 5]
temDt <- data.table(Var1 = c("</s>", "<s>"), Freq = c(1,1), key = "Var1")
UniTokenTab <- rbind(UniTokenTab, temDt)
UniTokenTab <- transform(UniTokenTab, Var1 = as.character(Var1))

## generate 2-Gram tokens
training <- unlist(lapply(training, function(x) paste("<S>", x, "</S>")))
len <- length(training)
BiTokenTab1 <- as.data.frame(table(NGramTokenizer(training[1:round(len/7)], Weka_control(min = 2, max = 2, delimiters = " \n"))))
BiTokenTab2 <- as.data.frame(table(NGramTokenizer(training[round(len/7)+1:round(len*2/7)], Weka_control(min = 2, max = 2, delimiters = " \n"))))
BiTokenTab3 <- as.data.frame(table(NGramTokenizer(training[round(len*2/7)+1:round(len*3/7)], Weka_control(min = 2, max = 2, delimiters = " \n"))))
BiTokenTab4 <- as.data.frame(table(NGramTokenizer(training[round(len*3/7)+1:round(len*4/7)], Weka_control(min = 2, max = 2, delimiters = " \n"))))
BiTokenTab5 <- as.data.frame(table(NGramTokenizer(training[round(len*4/7)+1:round(len*5/7)], Weka_control(min = 2, max = 2, delimiters = " \n"))))
BiTokenTab6 <- as.data.frame(table(NGramTokenizer(training[round(len*5/7)+1:round(len*6/7)], Weka_control(min = 2, max = 2, delimiters = " \n"))))
BiTokenTab7 <- as.data.frame(table(NGramTokenizer(training[round(len*6/7)+1:len], Weka_control(min = 2, max = 2, delimiters = " \n"))))
BiTokenTab <- rbind(BiTokenTab1, BiTokenTab2, BiTokenTab3, BiTokenTab4, BiTokenTab5, BiTokenTab6, BiTokenTab7)
rm(BiTokenTab1, BiTokenTab2, BiTokenTab3, BiTokenTab4, BiTokenTab5, BiTokenTab6, BiTokenTab7)   
BiTokenTab <- aggregate(Freq ~ Var1, BiTokenTab, sum)
var <- as.character(BiTokenTab$Var1)
var <- unlist(strsplit(var, " ", fixed = T))
var1 <- tolower(var[seq(1, length(var), 2)])
var2 <- tolower(var[seq(2, length(var), 2)])
goodRows1 <- (var1 %in% UniTokenTab$Var1)
goodRows2 <- (var2 %in% UniTokenTab$Var1)
goodRows <- goodRows1 * goodRows2
BiTokenTab <- BiTokenTab[goodRows == 1, ]
BiTokenTab <- data.table(BiTokenTab, key = "Var1")
rm(goodRows1, goodRows2, goodRows, var, var1, var2)

# split out the preffix and word
var <- as.character(BiTokenTab$Var1)
var <- unlist(strsplit(var, " ", fixed = T))
var1 <- tolower(var[seq(1, length(var), 2)])   # turn prefixes to lower case
var2 <- var[seq(2, length(var), 2)]
BiTokenTab$prefix <- var1
BiTokenTab$word <- var2
rm(var, var1, var2)
BiTokenTab$Var1 <- tolower(BiTokenTab$Var1)
BiTokenTab <- BiTokenTab[, sum(Freq), by = c("prefix", "word", "Var1")]
setnames(BiTokenTab, "V1", "Freq")

# computing frequency probability
BiTokenTabLo <- transform(BiTokenTab, prefix = prefix, word = tolower(word), Var1 = Var1, Freq = Freq)
BiTokenTabLo <- BiTokenTabLo[, sum(Freq), by = c("prefix", "word", "Var1")]
setnames(BiTokenTabLo, "V1", "Freq")
grpTot <- BiTokenTabLo[, sum(Freq), by = prefix]
BiTokenTabLo <- merge(BiTokenTabLo, grpTot, by = "prefix", all = T)
rm(grpTot)
BiTokenTabLo$prop <- BiTokenTabLo$Freq/BiTokenTabLo$V1
BiTokenTabLo$V1 <- NULL
BiTokenTabLo$Var1 <- as.character(BiTokenTabLo$Var1)

## perplexity evaluation
con <- file(paste0(dataDir, "validation-w-cap1.txt"), "r")
testing <- readLines(con)
close(con)
# selecting 1/50 of the validation data as testing, approximate 10k sentences
set.seed(141124)
sel <- rbinom(length(testing), 1, 0.02)
sel <- (sel == 1)
testing <- testing[sel]
rm(sel)

# 2-gram for testing
testing <- unlist(lapply(testing, function(x) paste("<S>", x, "</S>")))
testing <- tolower(testing)
BiTokenTabT <- as.data.frame(table(NGramTokenizer(testing, Weka_control(min = 2, max = 2, delimiters = " \n"))))
BiTokenTabT <- data.table(BiTokenTabT, key = "Var1")

BiTabMer <- merge(BiTokenTabT, BiTokenTabLo, by = "Var1", all.x = T, all.y = F, suffixes = c(".te", ".tr"))
# unseen words total frequency:
unseenFreTot <- sum(BiTabMer[is.na(BiTabMer$prefix)]$Freq.te)
# compute probabilities of unseen words.
BiTabMer[is.na(BiTabMer$prefix)]$prop <- BiTabMer[is.na(BiTabMer$prefix)]$Freq.te/unseenFreTot

invP <- 1/BiTabMer$prop
perplexity.bi2 <- prod(invP ^ (1/length(invP)))
rm(BiTokenTabT, invP)

## generate 3-gram tokens
training <- unlist(lapply(training, function(x) paste("<S>", x)))
len <- length(training)
TriTokenTab1 <- as.data.frame(table(NGramTokenizer(training[1:round(len/10)], Weka_control(min = 3, max = 3, delimiters = " \n"))))
TriTokenTab2 <- as.data.frame(table(NGramTokenizer(training[round(len/10)+1:round(len*2/10)], Weka_control(min = 3, max = 3, delimiters = " \n"))))
TriTokenTab3 <- as.data.frame(table(NGramTokenizer(training[round(len*2/10)+1:round(len*3/10)], Weka_control(min = 3, max = 3, delimiters = " \n"))))
TriTokenTab4 <- as.data.frame(table(NGramTokenizer(training[round(len*3/10)+1:round(len*4/10)], Weka_control(min = 3, max = 3, delimiters = " \n"))))
TriTokenTab5 <- as.data.frame(table(NGramTokenizer(training[round(len*4/10)+1:round(len*5/10)], Weka_control(min = 3, max = 3, delimiters = " \n")))) 
TriTokenTab6 <- as.data.frame(table(NGramTokenizer(training[round(len*5/10)+1:round(len*6/10)], Weka_control(min = 3, max = 3, delimiters = " \n"))))
TriTokenTab7 <- as.data.frame(table(NGramTokenizer(training[round(len*6/10)+1:round(len*7/10)], Weka_control(min = 3, max = 3, delimiters = " \n"))))
TriTokenTab8 <- as.data.frame(table(NGramTokenizer(training[round(len*7/10)+1:round(len*8/10)], Weka_control(min = 3, max = 3, delimiters = " \n"))))
TriTokenTab9 <- as.data.frame(table(NGramTokenizer(training[round(len*8/10)+1:round(len*9/10)], Weka_control(min = 3, max = 3, delimiters = " \n"))))
TriTokenTab10 <- as.data.frame(table(NGramTokenizer(training[round(len*9/10)+1:len], Weka_control(min = 3, max = 3, delimiters = " \n"))))
TriTokenTab <- rbind(TriTokenTab1, TriTokenTab2, TriTokenTab3, TriTokenTab4, TriTokenTab5, TriTokenTab6, 
                     TriTokenTab7, TriTokenTab8, TriTokenTab9, TriTokenTab10)
rm(TriTokenTab1, TriTokenTab2, TriTokenTab3, TriTokenTab4, TriTokenTab5, TriTokenTab6, TriTokenTab7, TriTokenTab8,
   TriTokenTab9, TriTokenTab10)
TriTokenTab <- aggregate(Freq ~ Var1, TriTokenTab, sum)

var <- as.character(TriTokenTab$Var1)
var <- strsplit(var, " ", fixed = T)
var <- unlist(lapply(var, function(x) x <- c(x[2], x[3])))
var1 <- tolower(var[seq(1, length(var), 2)])
var2 <- tolower(var[seq(2, length(var), 2)])
goodRows1 <- (var1 %in% UniTokenTab$Var1)
goodRows2 <- (var2 %in% UniTokenTab$Var1)
goodRows <- goodRows1 * goodRows2
TriTokenTab <- TriTokenTab[goodRows == 1, ]
rm(goodRows1, goodRows2, goodRows, var, var1, var2)
TriTokenTab <- data.table(TriTokenTab, key = "Var1")

# split out the preffix and word
var <- as.character(TriTokenTab$Var1)
var <- strsplit(var, " ", fixed = T)
var <- unlist(lapply(var, function(x) c(paste(x[1], x[2]), x[3])))
var1 <- tolower(var[seq(1, length(var), 2)])  # turn prefixies to lower case
var2 <- var[seq(2, length(var), 2)]
TriTokenTab$prefix <- var1
TriTokenTab$word <- var2
rm(var, var1, var2)
TriTokenTab$Var1 <- tolower(TriTokenTab$Var1)
TriTokenTab <- TriTokenTab[, sum(Freq), by = c("prefix", "word", "Var1")]
setnames(TriTokenTab, "V1", "Freq")

# computing frequency probability
TriTokenTabLo <- transform(TriTokenTab, prefix = prefix, word = tolower(word), Var1 = Var1, Freq = Freq)
TriTokenTabLo <- TriTokenTabLo[, sum(Freq), by = c("prefix", "word", "Var1")]
setnames(TriTokenTabLo, "V1", "Freq")
grpTot <- TriTokenTabLo[, sum(Freq), by = prefix]
TriTokenTabLo <- merge(TriTokenTabLo, grpTot, by = "prefix", all = T)
rm(grpTot)
TriTokenTabLo$prop <- TriTokenTabLo$Freq/TriTokenTabLo$V1
TriTokenTabLo$V1 <- NULL
TriTokenTabLo$Var1 <- as.character(TriTokenTabLo$Var1)

## 3-gram perplexity
# 3-gram for testing
testing <- unlist(lapply(testing, function(x) paste("<s>", x)))
len <- length(testing)
TriTokenTabT <- as.data.frame(table(NGramTokenizer(testing, Weka_control(min = 3, max = 3, delimiters = " \n"))))
TriTokenTabT <- data.table(TriTokenTabT, key = "Var1")
TriTokenTabT$Var1 <- as.character(TriTokenTabT$Var1)

TriTabMer <- merge(TriTokenTabT, TriTokenTabLo, by = "Var1", all.x = T, all.y = F, suffixes = c(".te", ".tr"))

# back-off comparison
var <- as.character(TriTabMer[is.na(prefix)]$Var1)
var <- strsplit(var, " ", fixed = T)
var <- unlist(lapply(var, function(x) c(x[1], paste(x[2], x[3]))))
backoff.l <- var[seq(2, length(var), 2)]
backoff.ll <- unique(backoff.l)
setkey(BiTokenTabLo, Var1)
fromBiG <- BiTokenTabLo[Var1 %in% backoff.ll]
fromBiG <- transform(fromBiG, prefix = NULL, Freq = NULL, word = NULL, Var1 = as.character(Var1))
TriTabMer2 <- TriTabMer[is.na(prefix)]
setnames(TriTabMer2, "Var1", "Var1.1")
TriTabMer2$Var1 <- backoff.l
TriTabMer2 <- merge(TriTabMer2, fromBiG, by = "Var1", all = T)
TriTabMer2 <- transform(TriTabMer2, Var1 = Var1.1, Var1.1 = NULL, prop = prop.y, prop.x = NULL, prop.y = NULL)
TriTabMer <- TriTabMer[!is.na(prefix)]
TriTabMer <- rbind(TriTabMer, TriTabMer2)
rm(fromBiG, TriTabMer2, backoff.l, backoff.ll, var)

# unseen words total frequency:
unseenFreTot <- sum(TriTabMer[is.na(TriTabMer$prop)]$Freq.te)
# compute probabilities of unseen words.
TriTabMer[is.na(prop)]$prop <- TriTabMer[is.na(prop)]$Freq.te/unseenFreTot

invP <- 1/TriTabMer$prop
perplexity.tri2 <- prod(invP ^ (1/length(invP)))
rm(TriTokenTabT, invP)

rm(BiTokenTabLo, TriTokenTabLo)

## simplify the BiGram and TriGram models
# simplify 2-gram model
BiTokenTabS <- transform(BiTokenTab, Var1 = NULL)
setkey(BiTokenTabS, prefix)
# by each prefix, get the highest frequency words
BiTokenTabS1 <- BiTokenTabS[BiTokenTabS[, Freq == max(Freq), by = prefix][, V1]]
# get the second highest frequency words
setkey(BiTokenTabS, prefix, word)
setkey(BiTokenTabS1, prefix, word)
BiTokenTabS <- BiTokenTabS[!BiTokenTabS1]
setkey(BiTokenTabS, prefix)
BiTokenTabS2 <- BiTokenTabS[BiTokenTabS[, Freq == max(Freq), by = prefix][, V1]]
# put them together 
BiTokenTabS <- rbind(BiTokenTabS1, BiTokenTabS2)
setkey(BiTokenTabS, prefix)
rm(BiTokenTabS1, BiTokenTabS2)

# simplify 3-gram model
TriTokenTabS <- transform(TriTokenTab, Var1 = NULL)
setkey(TriTokenTabS, prefix)
# by each prefix, get the highest frequency words
TriTokenTabS1 <- TriTokenTabS[TriTokenTabS[, Freq == max(Freq), by = prefix][, V1]]
# get the second highest frequency words
setkey(TriTokenTabS, prefix, word)
setkey(TriTokenTabS1, prefix, word)
TriTokenTabS <- TriTokenTabS[!TriTokenTabS1]
setkey(TriTokenTabS, prefix)
TriTokenTabS2 <- TriTokenTabS[TriTokenTabS[, Freq == max(Freq), by = prefix][, V1]]
# put them together 
TriTokenTabS <- rbind(TriTokenTabS1, TriTokenTabS2)
setkey(TriTokenTabS, prefix)
rm(TriTokenTabS1, TriTokenTabS2)

rm(testing, training)
badWdsTab <- data.table(bad.wds = badWds, key = "bad.wds")

setnames(UniTokenTab, "Var1", "word2")
setkey(UniTokenTab, key = word2)
TriTokenTabS$word2 <- tolower(TriTokenTabS$word)
BiTokenTabS$word2 <- tolower(BiTokenTabS$word)
TriTokenTabS <- merge(TriTokenTabS, UniTokenTab, by = "word2", all.x = T, all.y = F, suffixes = c(".tri", ".uni"))
BiTokenTabS <- merge(BiTokenTabS, UniTokenTab, by = "word2", all.x = T, all.y = F, suffixes = c(".bi", ".uni"))
TriTokenTabS$word2 <- NULL
BiTokenTabS$word2 <- NULL

setkey(TriTokenTabS, prefix)
setkey(BiTokenTabS, prefix)