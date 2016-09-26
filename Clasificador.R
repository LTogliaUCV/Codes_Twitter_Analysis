#Clasificador  Naive Bayes  Identificador de Tweets
#Cargamos el set de datos 
clinton<-clintontweets
trump1<-tweetst.df
trump2<-tweetst.df[1:566,]
clin2<-clinton[1:566,]
trump3<-tweetst.df[567:755,]
clint3<-clinton[567:755,]

#Generamos un conjunto de entrenamiento y otro de prueba  para el modelo
training<-rbind(trump2,clin2)
test<-rbind(trump3,clint3)


#Le agregamos a cada uno de los conjuntos , una nueva clase , al conjunto de datos de los #tweets de la cuenta de trump le agregamos la  variable "trump", respectivamente a los #tweets de clinton le agregamos "clinton"


trump2["class"]<-rep("trump",nrow(trump2))
clin2["class"]<-rep("clinton",nrow(clin2))

#Limpieza del Texto Pre-Procesamiento
#Construyamos el Corpus indicando que la fuente es un vector de caracteres
myCorpus<-Corpus(VectorSource(clin2$text))

#El texto del corpus  se convierte en texto plano
mycorpus <- tm_map(myCorpus, PlainTextDocument)

#El texto del corpus  se convierte en texto plano
mycorpus <- tm_map(myCorpus, PlainTextDocument)
#Se remueven los signos de puntuacion
myCorpus <- tm_map(myCorpus, removePunctuation)
# Se remueven los numeros 
myCorpus <- tm_map(myCorpus, removeNumbers)
# Se remueven los  URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
#El texto del corpus  se convierte en  minusculas
myCorpus <- tm_map(myCorpus, tolower)
#El texto del corpus  se convierte en texto plano
mycorpus <- tm_map(myCorpus, PlainTextDocument)
#Se remueven las  stop words
myStopwords <- c(stopwords("english"))
myStopwords <- c(stopwords("english"), "the", "to","of","on","that","in","in","for","a")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
#Guardamos una copia del  corpus
myCorpusCopy <- myCorpus
# Aplicamos el proceso de steaming 
myCorpus <- tm_map(myCorpus, stemDocument)




myCorpus2<-Corpus(VectorSource(trump2$text))

#El texto del corpus  se convierte en texto plano
mycorpus2<- tm_map(myCorpus2, PlainTextDocument)

#El texto del corpus  se convierte en texto plano
mycorpus2 <- tm_map(myCorpus2, PlainTextDocument)
#Se remueven los signos de puntuacion
myCorpus2 <- tm_map(myCorpus2, removePunctuation)
# Se remueven los numeros 
myCorpus2 <- tm_map(myCorpus2, removeNumbers)
# Se remueven los  URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus2 <- tm_map(myCorpus2, removeURL)
#El texto del corpus  se convierte en  minusculas
myCorpus2<- tm_map(myCorpus2, tolower)
#El texto del corpus  se convierte en texto plano
mycorpus2 <- tm_map(myCorpus2, PlainTextDocument)
#Se remueven las  stop words
myStopwords <- c(stopwords("english"))
myStopwords <- c(stopwords("english"), "the", "to","of","on","that","in","in","for","a")
myCorpus2 <- tm_map(myCorpus2, removeWords, myStopwords)
#Guardamos una copia del  corpus
myCorpusCopy2 <- myCorpus2
# Aplicamos el proceso de steaming 
myCorpus2<- tm_map(myCorpus2, stemDocument)




myCorpus3<-Corpus(VectorSource(test$text))

#El texto del corpus  se convierte en texto plano
mycorpus3<- tm_map(myCorpus3, PlainTextDocument)

#El texto del corpus  se convierte en texto plano
mycorpus3 <- tm_map(myCorpus3, PlainTextDocument)
#Se remueven los signos de puntuacion
myCorpus3 <- tm_map(myCorpus3, removePunctuation)
# Se remueven los numeros 
myCorpus3 <- tm_map(myCorpus3, removeNumbers)
# Se remueven los  URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus3 <- tm_map(myCorpus3, removeURL)
#El texto del corpus  se convierte en  minusculas
myCorpus3<- tm_map(myCorpus3, tolower)
#El texto del corpus  se convierte en texto plano
mycorpus3 <- tm_map(myCorpus3, PlainTextDocument)
#Se remueven las  stop words
myStopwords <- c(stopwords("english"))
myStopwords <- c(stopwords("english"), "the", "to","of","on","that","in","in","for","a")
myCorpus3 <- tm_map(myCorpus3, removeWords, myStopwords)
#Guardamos una copia del  corpus
myCorpusCopy3 <- myCorpus3
# Aplicamos el proceso de steaming 
myCorpus3<- tm_map(myCorpus3, stemDocument)







tmatrix <- t(TermDocumentMatrix(myCorpus,control = list(wordLengths=c(4,Inf))));
cmatrix <- t(TermDocumentMatrix(yCorpus2,control = list(wordLengths=c(4,Inf))));
testmatrix <- t(TermDocumentMatrix(yCorpus3,control = list(wordLengths=c(4,Inf))));

#ahora se tiene que construir el modelo, para ello  se tiene que calcular las #probabilidades  del modelo,contar el numero de apariciones de cada palabra,añadir  la #estimacion de laplace, posterior calcular el log de las probabilidades y guardar en un #archivo csv.

probabilityMatrix <-function(docMatrix)
{
  # Sumar las frecuencias
  termSums<-cbind(colnames(as.matrix(docMatrix)),as.numeric(colSums(as.matrix(docMatrix))))
  # sumar 1 "laplace"
  termSums<-cbind(termSums,as.numeric(termSums[,2])+1)
  # calcular las priobabilidades
  termSums<-cbind(termSums,(as.numeric(termSums[,3])/sum(as.numeric(termSums[,3]))))
  # Calcular el log natural de las probabilidades
  termSums<-cbind(termSums,log(as.numeric(termSums[,4])))
  # Adicionar nombres a las columnas 
  colnames(termSums)<-c("term","count","additive","probability","lnProbability")
  termSums
}



#hacemos el llamado
tp<-probabilityMatrix(tmatrix)
cp<-probabilityMatrix(cmatrix)

#guardamos en un archivo csv 


write.csv(file="trumpprobmatrix.csv",tp)
write.csv(file="clintonprobmatrix.csv",cp)

#ahora se usa Bayes y se prueba el modelo con el conjunto de test 
#se quiere comparar los twwets del conjunto de entrenmiento  con las  dos matrices de #probabilidades , cada tweets se compara con las matrices , queremos saber cuantas palabras #no aparecen , si esto pasa se le aggrega  la estimacion
 #de lapalce  , luego obtenemos las suma  de las probailidades de las palabras que si #aparecen .

 getProbability <- function(testChars,probabilityMatrix)
{
  charactersFound<-probabilityMatrix[probabilityMatrix[,1] %in% testChars,"term"]
  # cuenta cuantas palabras aprecen el los matrix de trump
  charactersNotFound<-length(testChars)-length(charactersFound)
  # agregamos las  probabilidades normalizdas de las palabras que si fueron encontradas
  charactersFoundSum<-sum(as.numeric(probabilityMatrix[probabilityMatrix[,1] %in% testChars,"lnProbability"]))
  # usamosln(1/total de las palabras con estimacion de laplace)  para palbras no encontradas
  charactersNotFoundSum<-charactersNotFound*log(1/sum(as.numeric(probabilityMatrix[,"additive"])))
  #esta es la probailidad
  prob<-charactersFoundSum+charactersNotFoundSum 
  prob
}

#La funcion anterior se usa para cada tweet , para ello se genera un loop 

# obtenemos la matriz
testmatrix<-as.matrix(testmatrix)
 
classified<-NULL
 
for(documentNumber in 1:nrow(testmatrix))
{
  # Extract the test words
  tweets.test.chars<-names(testmatrix[documentNumber,testmatrix[documentNumber,] %in% 1])
  # Get the probabilities
  trumpprob <- getProbability(tweets.test.chars,tp)
  clintonprob <- getProbability(tweets.test.chars,cp)
  # Add it to the classification list
  classified<-c(classified,ifelse(trumpprob>clintonprob,"trump","clinton"))
}

#visualizamos los resultados del clasificador
View(cbind(classified,test$test))

resultados<-cbind(classified,test$test)
summary(resultados)
