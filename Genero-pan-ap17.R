# install.packages("rJava", type='source')

# install.packages("qdap")
# install.packages("splitstackshape")
# install.packages("caret")
library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(kernlab)
library(e1071)


GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
	setwd(path)
	
	files = list.files(pattern="*.xml")
	
	corpus.raw <- NULL
	i <- 0
	for (file in files) {
		xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE, encoding = "UTF-8")
		corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
		i <- i + 1
		if (verbose) print(paste(i, " ", file))	
	}
	# quitar palabras de longitud 1 
	corpus.preprocessed <- corpus.raw
	corpus.preprocessed <- gsub('\\b\\w{1}\\s','',corpus.preprocessed) #longitud 1
	corpus.preprocessed <- gsub("[^[:graph:]]", " ", corpus.preprocessed)#palabra que empiezan
	corpus.preprocessed <- gsub("[^[A-z]*[s]$", " ", corpus.preprocessed)#palabra que terminan por s
	

	
	if (lowcase) {
		if (verbose) print("Tolower...")
		corpus.preprocessed <- tolower(corpus.preprocessed)
	}	
	
	if (punctuations) {
		if (verbose) print("Removing punctuations...")		
		corpus.preprocessed <- removePunctuation(corpus.preprocessed)
	}

	if (numbers) {
		if (verbose) print("Removing numbers...")
		corpus.preprocessed <- removeNumbers(corpus.preprocessed)
	}

	if (whitespaces) {
		if (verbose) print("Stripping whitestpaces...")
		corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
	}

	if (swlang!="")	{
		if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
		corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
	}
	
	
	if (swlist!="") {
		if (verbose) print("Removing provided stopwords...")
		corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
	}
	
	corpus.preprocessed <- removeWords(corpus.preprocessed, c("youtube", "video"))
	
	if (whitespaces) {
	  if (verbose) print("Stripping whitestpaces...")
	  corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
	}


	if (verbose) print("Generating frequency terms")
	
	corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
	if (verbose) plot(corpus.frequentterms)
	

	
	
	
	return (corpus.frequentterms)
}


GenerateBoW <- function(path, vocabulary, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE) {
	setwd(path)

	truth <- read.csv("truth.txt", sep=":", header=FALSE)
	truth <- truth[,c(1,4,7)]
	colnames(truth) <- c("author", "gender", "variety")

	i <- 0
	bow <- NULL
	files = list.files(pattern="*.xml")
	for (file in files) {
		author <- gsub(".xml", "", file)
		variety <- truth[truth$author==author,"variety"]
		gender <- truth[truth$author==author,"gender"]

		xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
		txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
		

		if (lowcase) {
			txtdata <- tolower(txtdata)
		}

		if (punctuations) {
			txtdata <- removePunctuation(txtdata)
		}

		if (numbers) {
			txtdata <- removeNumbers(txtdata)
		}

		if (whitespaces) {
			txtdata <- stripWhitespace(txtdata)
		}
	
		line <- author
		freq <- freq_terms(txtdata, n)
		for (word in vocabulary$WORD) {
			thefreq <- 0
			if (length(freq[freq$WORD==word,"FREQ"])>0) {
				thefreq <- freq[freq$WORD==word,"FREQ"]
			} 
			line <- paste(line, ",", thefreq, sep="")
		}
		
		if (class=="variety") {
			line <- paste(line, ",", variety, sep="")
		} else {
			line <- paste(line, ",", gender, sep="")
		}

		bow <- rbind(bow, line)

		i <- i + 1

		if (verbose) {
			if (class=="variety") {
				print(paste(i, author, variety))
			} else {
				print(paste(i, author, gender))
			}
		}
	}

	return (bow)
}


n <- 1000
path_training <- "C:/Users/Javier/Desktop/TextMining/pan-ap17-bigdata/training"	# Your training path
path_test <- "C:/Users/Javier/Desktop/TextMining/pan-ap17-bigdata//test"			# Your test path

vocabulary <- GenerateVocabulary(path_training, n, swlang="es")

bow_training <- GenerateBoW(path_training, vocabulary, n, class="gender")
bow_test <- GenerateBoW(path_test, vocabulary, n, class="gender")

training <- cSplit(bow_training, "V1", ",")
test <- cSplit(bow_test, "V1", ",")


# OJO CON ESTO!!!! COMPROBAR QUE LA ultima columna CONTIENE LA CLASE
training[,1006] #Aqui estan los paises
names(training)[1006] <- "class"
training <- training[,3:1006]
# comprobar el valor de la ultima columna
training[1, 1004]
# comprobar el valor de la ultima columna
test[1, 1006]
# una lista solo con la clase a predecir
truth  <- unlist(test[,(1006):(1006)])
# una lista sin la clase a predecir para test
test <- test[,3:(1006)]




model_rf <- train(class ~ ., data = training, method = "rf", ntree=100)



pred <- predict(model_rf, test)
confusionMatrix(pred, truth)






