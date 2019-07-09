buildnet<-function(){
  tryCatch({
  network <- new(Network, "Demo")
  #### importing the network
  network$loadFromString(readLines("../BNcoefs/networkdata.txt", n = -1))
  return(network)
  },
  error = function(e) {
    showModal(modalDialog(
      title = "Error",
      conditionMessage(e)
    ))
    
  })
}

quary.BN<-function(network,sandv=NA,siltv=NA,clayv=NA,socv=NA,soilpHv=NA,cecv=NA,latv=NA,
                   BARv=NA,NARv=NA,BpHv=NA,HTv=NA, BCv=NA, BNv=NA, BAv=NA, BCNv=NA,
                   prv="1",feedstockv="1",textv="1"){
  allvars<-network$getVariables()
  # use the factory design pattern to create the necessary inference related objects
  #factory <- new(LikelihoodSamplingInferenceFactory)
  inference <- new(LikelihoodSamplingInference,network)
  #queryOptions <- factory$createQueryOptions()
  #queryOutput <- factory$createQueryOutput()
  queryOptions<-new(LikelihoodSamplingQueryOptions)
  queryOptions$setSeed(new(Integer, "123"))
  queryOptions$setSampleCount(new(Integer, "123"))
  queryOutput<-new(LikelihoodSamplingQueryOutput)
  # Add some queries
  queryRR <- new(CLGaussian,  allvars$get("RR", TRUE))
  queryDistributions <- inference$getQueryDistributions()
  queryDistributions$add(queryRR)
  
  # Set some evidence
  evidence <- inference$getEvidence()
  ####################### SAND
  if(!is.na(sandv)) {
    evidence$set(allvars$get("sand", TRUE), new(Double, sandv))
  }
  ####################### SILT
  if(!is.na(siltv)){ 
    evidence$set(allvars$get("silt", TRUE), new(Double, siltv))
  }
  ####################### clay
  if(!is.na(clayv)){ 
    evidence$set(allvars$get("clay", TRUE), new(Double, clayv))
  }
  ####################### SOC
  if(!is.na(socv)) {
    evidence$set(allvars$get("Soil.Organic.Carbon", TRUE), new(Double, socv))
  }
  ####################### SoilpH
  if(!is.na(soilpHv)){
    evidence$set(allvars$get("Soil_PH", TRUE), new(Double, soilpHv))
  }
  ####################### CEC
  if(!is.na(cecv)) {
    evidence$set(allvars$get("CEC", TRUE), new(Double, cecv))
  }
  ############################LAT
  if(!is.na(latv)) 
    evidence$set(allvars$get("latitude", TRUE), new(Double, latv))
  ##########################
  if(!is.na(BARv)) 
    evidence$set(allvars$get("BApplication", TRUE), new(Double, BARv))
  ##########################
  if(!is.na(NARv)) 
    evidence$set(allvars$get("NAP", TRUE), new(Double, NARv))
  ##########################
  if(!is.na(BpHv)) 
    evidence$set(allvars$get("biochar_pH", TRUE), new(Double, BpHv))
  ##########################
  if(!is.na(HTv)) 
    evidence$set(allvars$get("highestT", TRUE), new(Double, HTv))
  ##########################
  if(!is.na(BCv)) 
    evidence$set(allvars$get("Biochar.carbon", TRUE), new(Double, BCv))
  ##########################
  if(!is.na(BNv)) 
    evidence$set(allvars$get("Biochar.Nitrogen", TRUE), new(Double, BNv))
  ##########################
  if(!is.na(BAv)) 
    evidence$set(allvars$get("Biochar.Ash", TRUE), new(Double, BAv))
  ########################## biochar C/N
  if(!is.na(BCNv)) 
    evidence$set(allvars$get("Biochar.C.N", TRUE), new(Double, BCNv))
  ########################## croptype
  #if(croptv!="1") 
  # evidence$setState(allvars$get("croptype", TRUE)$getStates()$get(croptv, TRUE))
  ########################## Pyrolysis rate
  if(prv!="1") 
    evidence$setState(allvars$get("PyrolysisType", TRUE)$getStates()$get(prv, TRUE))
  ########################## Feedstock
  if(feedstockv!="1") 
    evidence$setState(allvars$get("feedstock", TRUE)$getStates()$get(feedstockv, TRUE))
  ########################## Feedstock
  if(textv!="1") 
    evidence$setState(allvars$get("texture", TRUE)$getStates()$get(textv, TRUE))
  ######################
  # Execute the query
  inference$query(queryOptions, queryOutput) # note that this can raise an exception (see help for details)
  
  # Read the results of the query
  #print(queryRR$getMean(RRType))
  #print(queryRR$getVariance(RRType))
  return(list(queryRR$getMean( allvars$get("RR", TRUE)), queryRR$getVariance( allvars$get("RR", TRUE))))
}


soildataret<-function(mukeys=2747727){
  require(XML)
  ######### Reteiv soil
  headerFields =
    c(Accept = "text/xml",
      Accept = "multipart/*",
      'Content-Type' = "text/xml; charset=utf-8",
      SOAPAction = "http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx/RunQuery")
  
  body = paste('<?xml version="1.0" encoding="utf-8"?>
               <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
               <soap:Body>
               <RunQuery xmlns="http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx">
               <Query>
               SELECT mapunit.mukey, mapunit.muname, mapunit.muwathelcl, mapunit.iacornsr, component.cokey, component.mukey,chorizon.cec7_r,
               chorizon.sandtotal_r,chorizon.silttotal_r,chorizon.claytotal_r,chorizon.om_r,chorizon.hzdept_r,
               chorizon.ph1to1h2o_r,chorizon.wthirdbar_r,chorizon.wfifteenbar_l,chorizon.cokey,chorizon.chkey,
               muaggatt.aws050wta from mapunit
               join muaggatt on mapunit.mukey=muaggatt.mukey
               join component on mapunit.mukey=component.mukey
               join chorizon on component.cokey=chorizon.cokey
               where mapunit.mukey in (', paste(mukeys,collapse = ", "),');
               </Query>
               </RunQuery>
               </soap:Body>
               </soap:Envelope>')
  reader = RCurl::basicTextGatherer()
  out<-RCurl::curlPerform(url = "https://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx",
                          httpheader = headerFields,  postfields = body,
                          writefunction = reader$update
  )
  
  # xml_data <-read_xml(reader$value())
  xml_doc<-xmlTreeParse(reader$value())
  xmltop = xmlRoot(xml_doc)
  tablesxml<-(xmltop[[1]]["RunQueryResponse"][[1]]["RunQueryResult"][[1]]["diffgram"][[1]]["NewDataSet"][[1]])
  
  tryCatch({
    tables<-getNodeSet(tablesxml,"//Table")
    
    # xmlValue(xmlChildren(tables[[1]])$chkey)
    ##### All datatables below newdataset
    dfs<-data.frame(
      mukey=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$mukey)  })),
      cokey=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$cokey)  })),
      chkey=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$chkey)  })),
      muname=(sapply(tables,function(x){ xmlValue(xmlChildren(x)$muname)  })),
      cec7_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$cec7_r)  })),
      sandtotal_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$sandtotal_r)  })),
      silttotal_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$silttotal_r)  })),
      claytotal_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$claytotal_r)  })),
      om_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$om_r)  })),
      ph1to1h2o_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$ph1to1h2o_r)  })),
      depth=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$hzdept_r)  })),
      Fc2=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$wthirdbar_r)  })),
      PWP1=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$wfifteenbar_l)  })),
      Erodibility=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$muwathelcl)  })),
      CSR=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$iacornsr)  })),
      AWC=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$aws050wta)  }))
    )
    return(dfs)
  },
  error=function(cond) {
    return(NULL)
  })
  
}

utmzonefinder<-function(long){
  long<-abs(long)
  if(long>126){zone<-9}else if(long>120 & long<126){zone<-10}else if(long>114 & long<120){zone<-11}else if (long>108 & long<114)
  {zone<-12} else if (long>108 & long<102){zone<-13}else if (long>96 & long<102){zone<-14}else if (long>90 & long<96){zone<-15}else if (long>84 & long<90)
  {zone<-16}else if (long>78 & long<84){zone<-17}else if (long>72 & long<78){zone<-18}else if (long<72){zone<-19}
  return(zone)
}