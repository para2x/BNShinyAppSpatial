# TODO update the following path
jarPath <- 'bayesserver-7.6.jar'
.jinit(classpath=c(jarPath))

Double <- J("java.lang.Double")
Stringg <- J("java.lang.String")
Network <- J("com.bayesserver.Network")
State <- J("com.bayesserver.State")
Node <- J("com.bayesserver.Node")
Link <- J("com.bayesserver.Link")
Table <- J("com.bayesserver.Table")
TableIterator <- J("com.bayesserver.TableIterator")
Variable<-J("com.bayesserver.Variable")
VariableType<-J("com.bayesserver.VariableValueType")
RelevanceTreeInferenceFactory <- J("com.bayesserver.inference.RelevanceTreeInferenceFactory")
RelevanceTreeInference <- J("com.bayesserver.inference.RelevanceTreeInference")
ParameterLearning <- J("com.bayesserver.learning.parameters.ParameterLearning")
ParameterLearningOptions<-J("com.bayesserver.learning.parameters.ParameterLearningOptions")
DatabaseDataReaderCommand <- J("com.bayesserver.data.DatabaseDataReaderCommand")
VariableReference <- J("com.bayesserver.data.VariableReference")
ColumnValueType<-J("com.bayesserver.data.ColumnValueType")
DefaultEvidenceReaderCommand<-J("com.bayesserver.data.DefaultEvidenceReaderCommand")
ReaderOptions<-J("com.bayesserver.data.ReaderOptions")
ArrayasList<-J("java.util.Arrays")
DataTable<-J("com.bayesserver.data.DataTable")
DataRow<-J("com.bayesserver.data.DataRow")

Evidence<-J("com.bayesserver.inference.Evidence")
CLGaussian <- J("com.bayesserver.CLGaussian")

LikelihoodSamplingInferenceFactory <- J("com.bayesserver.inference.LikelihoodSamplingInferenceFactory")
#DataReaderCommand<-J("com.bayesserver.data.DataReaderCommand")
DataTableDataReaderCommand<- J("com.bayesserver.data.DataTableDataReaderCommand")
Integer <- J("java.lang.Integer")
Boolean <- J("java.lang.Boolean")
String <- J("java.lang.String")
Variable <- J("com.bayesserver.Variable")
VariableValueType <- J("com.bayesserver.VariableValueType")
TableIterator <- J("com.bayesserver.TableIterator")
DataTableDataReaderCommand <- J("com.bayesserver.data.DataTableDataReaderCommand")
ValidationOptions <- J("com.bayesserver.ValidationOptions")
ColumnValueType <- J("com.bayesserver.data.ColumnValueType")

LikelihoodSamplingQueryOptions <- J("com.bayesserver.inference.LikelihoodSamplingQueryOptions")
LikelihoodSamplingInference <- J("com.bayesserver.inference.LikelihoodSamplingInference")
LikelihoodSamplingQueryOutput<- J("com.bayesserver.inference.LikelihoodSamplingQueryOutput")
ParameterCounter <- J("com.bayesserver.ParameterCounter")
#########
toStateArray <- function(...){
  return(.jarray(c(...), contents.class = "com.bayesserver.State"))
}

toNodeArray <- function(...){
  return(.jarray(c(...), contents.class = "com.bayesserver.Node"))
}

toVariableReferenceArray <- function(...){
  return(.jarray(c(...), contents.class = "com.bayesserver.data.VariableReference"))
}

toDoubleArray <- function(...){
  return(.jarray(c(...), contents.class = "java.lang.Double[]"))
}

licenseBayesServer <- function(licenseKey){
  J("com.bayesserver.License")$validate(licenseKey)
}





toVariableReferenceList <- function(...){
  
  xs <- new( J("java.util.ArrayList") )
  
  lapply(..., function(x) { xs$add(x)})
  
  
  return(xs)
}


toDataTable <- function(df) {
  
  dt <- new(DataTable)
  
  dfTypes <- sapply(df, typeof)
  dfClasses <- sapply(df, class)
  dfNames <- names(df)
  
  columnCount <- length(dfNames)
  
  for (i in 1:columnCount)
  {
    dt$getColumns()$add(dfNames[i], toJavaClass(dfTypes[i], dfClasses[i]))
  }
  
  for (r in 1:nrow(df))
  {
    values <- lapply(df[r,], function(x) {
      return(toJavaObject(x))
    })
    
    values <- .jarray(values, contents.class = "java.lang.Object")
    dt$getRows()$add(values)
  }
  
  return(dt)
  
}

toJavaClass <- function(rType, rClass) {
  
  if(rType == "double" && rClass == "numeric")
  {
    return(Double$class)
  }
  else if(rType == "logical" && rClass == "logical")
  {
    return(Boolean$class)
  }
  else if(rType == "integer" && rClass == "factor")
  {
    return(String$class)   # store factor integer values as factor level names
  }
  else if(rType == "character")
  {
    return(String$class)
  }
  else
  {
    stop(sprintf("type [%s] or class [%s] not supported", rType, rClass))
  }
  
}

toJavaObject <- function(x) {
  
  rType <- typeof(x)
  rClass <- class(x) 
  
  if(rType == "double" && rClass == "numeric")
  {
    return(new (Double, x))
  }
  else if(rType == "logical" && rClass == "logical")
  {
    return(new (Boolean, x))
  }
  else if(rType == "integer" && rClass == "factor")
  {
    s <- as.character(x)
    
    return(new (String, s))
  }
  else if(rType == "character")
  {
    return(new (String, as.character(x)))
  }
  else
  {
    stop(sprintf("type [%s] or class [%s] not supported", rType, rClass))
  }
  
}
