#Clasificador  Naive Bayes  Identificador de Tweets
#Cargamos el set de datos 
clinton<-tweetsc.df
trump1<-tweetst.df
trump2<-tweetst.df[1:125,]
clin2<-clinton[1:125,]
trump3<-tweetst.df[126:188,]
clint3<-clinton[126:189,]

#Generamos un conjunto de entrenamiento y otro de prueba  para el modelo
training<-rbind(trump2,clin2)
test<-rbind(trump3,clint3)


#Le agregamos a cada uno de los conjuntos , una nueva clase , al conjunto de datos de los #tweets de la cuenta de trump le agregamos la  variable "trump", respectivamente a los #tweets de clinton le agregamos "clinton"


trump2["class"]<-rep("trump",nrow(trump2))
clin2["class"]<-rep("clinton",nrow(clin2))

# Ahora se crea una funcion que  pre.procese los tweets 
replacePunctuation <- function(x)
{
  x <- tolower(x)
  x <- gsub("[.]+[ ]"," ",x)
  x <- gsub("[:]+[ ]"," ",x)
  x <- gsub("[?]"," ",x)
  x <- gsub("[!]"," ",x)
  x <- gsub("[;]"," ",x)
  x <- gsub("[,]"," ",x)
  x
}

trump2$Tweet <- replacePunctuation(trump2$text)
clin2$Tweet <- replacePunctuation(clin2$text)
test$Tweet <- replacePunctuation(test$text)

#Ahora hacemos uso de la libreia tm para generar el corpus y posterior  la matriz de #terminos-documentos

tcorpus <- Corpus(VectorSource(as.vector(trump2$text)))
ccorpus <- Corpus(VectorSource(as.vector(clin2$text)))
corpustest <- Corpus(VectorSource(as.vector(test$text)))


tmatrix <- t(TermDocumentMatrix(tcorpus,control = list(wordLengths=c(4,Inf))));
cmatrix <- t(TermDocumentMatrix(ccorpus,control = list(wordLengths=c(4,Inf))));
testmatrix <- t(TermDocumentMatrix(corpustest,control = list(wordLengths=c(4,Inf))));

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