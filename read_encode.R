read_encode<-function (filepath,separator=";"){
  
  EncodingList<-c("UTF-8","UTF-16","Windows-1252")
  
  for(i in EncodingList){
    df<-try(read.table(filepath, separator,header=T, stringsAsFactors=F,encoding=i),silent=F)
    df<-toEncoding(df,encoding=i)
  }
  return(df)
  
  
}

toEncoding <-
  function(df, encoding="utf-8")
  {
    for(i in ncol(df)){
      if(class(df[[i]])=="character"){
        Encoding(df[[i]])<-encoding
      }
    }
    return(df)
  }
