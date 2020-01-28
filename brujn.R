brujn <- function(secuencias, traslape)
{
  #Primero crearemos un vector con todos los nodos que contendra el bgrafo de bruijn
  nodos <- c()
  print('Generando nodos del grafo...')
  for (stri in secuencias) {
    l <- nchar(stri)
    #El vector de nodos tendra los prefijos y sufijos de los kmeros
    s1<- substr(stri,1,traslape)
    s2<- substr(stri,l-traslape+1,l)
    nodos <- union(nodos,s1)
    nodos <- union(nodos,s2)
    s1 <- ""
    s2 <- ""
  }
  #Las aristas del grafo estaran representadas mediante una matriz de adyacencia
  adyacencia <- matrix(0,nrow=length(nodos),ncol = length(nodos))
  #Ademas Los cameros en las aristas estaran representados mediante una lista de listas
  kmers<-crearlistas(nodos)
  colnames(adyacencia)<-nodos
  rownames(adyacencia)<-nodos
  print('Generando aristas del grafo...')
  for (stri in secuencias) {
    l <- nchar(stri)
    s1 <- substr(stri,1,traslape)
    s2<- substr(stri,l-traslape+1,l)
    adyacencia[s1,s2]<-adyacencia[s1,s2]+1
    kmers[[s1]][[s2]]<-c(kmers[[s1]][[s2]],stri)
  }
  #print(adyacencia)
  print('Buscando camino euleriano...')
  for (inicio in nodos) {
    res<-euleriano(adyacencia,inicio,nodos,inicio)
    if(res[1]=="TRUE")
    {
      print(res[-1])
      sres<-kmers[[res[2]]][[res[3]]][2]
      kmers[[res[2]]][[res[3]]]<-kmers[[res[2]]][[res[3]]][-2]
      primer<-res[3]
      for (sig in res[c(-1,-2,-3)]) {
        kmer<-kmers[[primer]][[sig]][2]
        kmers[[primer]][[sig]]<-kmers[[primer]][[sig]][-2]
        sres<-paste(sres,substr(kmer,nchar(primer)+1,nchar(kmer)),sep="")
        primer<-sig
      }
      print('Exito')
      return(sres)
    }
      
  }
  return("No se encontro")
}

haycamino<-function(m)
{
  for (variable in m) {
    if(variable>0)
      return(TRUE)
  }
  return(FALSE)
}

siguientepaso<-function(m,st,nodos)
{
  for (n in nodos) {
    if(m[st,n]>0)
      return(n)
  }
  return(NA)
}

euleriano<-function(m,inicio,nodos,camino)
{
  if(!haycamino(m))
  {
    return(c("TRUE",camino))
  }
  else
  {
     for (sp in nodos) 
     {
       if(m[inicio,sp]>0)
       {
         m[inicio,sp]<-m[inicio,sp]-1
         res <- buscarcamino(m,sp,nodos,c(camino,sp))
         if(res[1]=="TRUE")
           return(res)
         else
           m[inicio,sp]<-m[inicio,sp]+1
       }
     }
     return(c(FALSE,""))
  }
}

desdecsv<-function(archivo)
{
  trozos<-c()
  for (linea in readLines(file(archivo))) {
    trozos<-c(trozos,linea)
  }
  return(trozos)
}

crearlistas<-function(nodos)
{
  lista<-list()
  lista<-c(lista,nodos)
  names(lista)<-nodos
  for (nod in nodos) {
    lista[[nod]]<-list()
    lista[[nod]]<-c(lista[[nod]],nodos)
    names(lista[[nod]])<-nodos
    lista[[nod]][nodos]<-""
  }
  return(lista)
}

dividir<-function(fuente,destino,traslape,tamano)
{
  completo<-""
  for (linea in readLines(con<-file(fuente))) {
    completo<-paste(completo,linea,sep = "")
  }
  inicio<-1
  fin<-inicio + tamano-1
  lineas<-c()
  while (fin<=nchar(completo)) {
    token<-substr(completo,inicio,fin)
    lineas<-c(lineas,token)
    inicio<-(inicio+tamano)-traslape
    fin<-inicio+tamano-1
  }
  if(inicio<nchar(completo) && fin>nchar(completo))
  {
    token<-substr(completo,inicio,nchar(completo))
    lineas<-c(lineas,token)
  }
  writeLines(lineas,con = file(destino),sep = "\n")
}

obtenergenoma<-function(archivo)
{
  completo<-""
  for (linea in readLines(con<-file(archivo))) {
    completo<-paste(completo,linea,sep = "")
  }
  return(completo)
}

escorrecto<-function(archivo,gen)
{
  completo<-""
  for (linea in readLines(con<-file(archivo))) {
    completo<-paste(completo,linea,sep = "")
  }
  return(gen == substr(completo,1,nchar(gen)))
}