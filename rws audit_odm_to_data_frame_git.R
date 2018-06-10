## Date: 2018-03-16
## Author: Sam Tomioka
## sam.tomioka@sunovion.com
## Rave Operational Data (CDISC ODM) to Data Frame
## Note some attributes are not parsed as I extracted only necessary data
##


library(RCurl);library(httr);require(XML);library(dplyr);library(RCurl);library(rvest);library(data.table)
loginURL<-"https://xxxx"
userid<-"stomioka"
pass<-"airukun32"
studyOID<-"xxx"
###########FUNCTIONS########
#get attr data
getAttrData <-
  function( node, attr ){
    sapply( node, function(x) xmlGetAttr(x, attr, default = ""))
  }
#get val
getValData <-
  function( node, xpath ){
    sapply( node, function(x) xmlValue(x, xpath))
  }
#get attr data with missing node
getAttrDataM <-
  function(OutVector, ParentNode, xpath, attr){
    
    OutVector <-c()
    for(i in 1:length(ParentNode)){
      event<-getNodeSet(ParentNode[[i]],paste0("./ns:",xpath), namespaces)
      if (length(event)>0) {OutVector<-append(OutVector,getAttrData(event,attr ))} 
      else {OutVector<-append(OutVector,NA)}
    }
    return(OutVector)
  }

#get xval data with missing node
getValDataM <-
  function(OutVector, ParentNode, xpath){
    
    OutVector <-c()
    for(i in 1:length(ParentNode)){
      event<-getNodeSet(ParentNode[[i]],paste0("./ns:",xpath), namespaces)
      if (length(event)>0) {OutVector<-append(OutVector,getValData(event,paste0(".//",xpath) ))} 
      else {OutVector<-append(OutVector,NA)}
    }
    return(OutVector)
  }


####empty data frame
auditrecord<-data.frame(
  AuditID  =character(),      
  StudyID  = character(),       
  SubjectName    = character(),
  SubjectTType   = character(),
  SubjectStatus  = character(),
  FolderID       = character(),
  StudyEventTType = character(),
  FormID         = character(),
  FormTType      = character(),
  ItemGroupData  = character(),
  ItemGroupTType = character(),
  FieldID        = character(),
  Data           = character(),
  FiledTType     = character(),
  Freeze         = character(),
  Verify         = character(),
  Lock           = character(),
  AuditUser      = character(),
  DateTimeStamp  = character(),
  ReasonForChange = character(),
  SiteID    = character())
###############################




# initial URL construction parts for first URL
studyoid=studyOID
rws_start=as.character("1")
rws_urllimit = as.character("10000")
next0 <- paste0(loginURL,"/RaveWebServices/datasets/ClinicalAuditRecords.odm?studyoid=",
                studyoid,
                "&startid=",
                rws_start,
                "&per_page=",
                rws_urllimit)
auditurl<-list()
nexturl<-list()

#the first URL

nexturl<-GET(next0,authenticate(userid,pass)) #header

auditurl[1]<-substr(nexturl[["headers"]][["link"]],2,nchar(nexturl[["headers"]][["link"]])-13) #URL
auditurl<-unlist(auditurldata)

#get rest of URL
i<-2
while (grepl("next",nexturl[["headers"]][["link"]])==TRUE){
  nexturl<-GET(unlist(auditurl[[i-1]]),authenticate("stomioka", "airukun32"))
  auditurl[i]<-substr(nexturl[["headers"]][["link"]],2,nchar(nexturl[["headers"]][["link"]])-13) 
  i<-i+1
}

#load("audit.RData")
#auditurl<-as.vector(auditurldata)
#auditurl<-as.vector(unlist(auditurl))
auditurl<-unlist(auditurl)

namespaces <- c(ns='http://www.cdisc.org/ns/odm/v1.3', 
                data='http://www.mdsol.com/ns/odm/metadata')
for (i in 1:length(auditurl)){

next0<-auditurl[i]

doc<-xmlTreeParse(GET(next0,authenticate(userid,pass)),useInternalNodes=T,
                      fullNamespaceInfo = T,parentFirst = T)
#setwd("~/My Documents/r/rws")
#saveXML(doc,file="audit.xml")



#Get Node Set


DataNode <- getNodeSet(doc, "//ns:ClinicalData", namespaces)
  SubjectDataNode<- getNodeSet(doc, "//ns:SubjectData", namespaces)
    #StudyEventDataNode<- getNodeSet(doc, "//ns:StudyEventData", namespaces)
    #  FormDataNode<- getNodeSet(doc, "//FormData", namespaces)

#AuditRecord
SourceIDNode<- getNodeSet(doc, "//ns:SourceID", namespaces)
AuditNode<- getNodeSet(doc, "//ns:AuditRecord", namespaces)




AuditID          <-getValData(SourceIDNode, ".//SourceID")
StudyID          <-getAttrData(DataNode,attr="StudyOID" )


audit<-cbind(AuditID,StudyID)
rm(AuditID,StudyID)

#SubjectData
#SubjectKey   <-getAttrDataM(SubjectKey,   DataNode,xpath="SubjectData",attr="SubjectKey")
#SubjectKType <-getAttrDataM(SubjectKType, DataNode,xpath="SubjectData",attr="mdsol:SubjectKeyType")
SubjectName  <-getAttrDataM(SubjectName,  DataNode,xpath="SubjectData",attr="mdsol:SubjectName")
SubjectTType <-getAttrDataM(SubjectName,  DataNode,xpath="SubjectData",attr="TransactionType")
SubjectStatus<-getAttrDataM(SubjectStatus,DataNode,xpath="SubjectData",attr="mdsol:Status")
audit<-cbind(audit,SubjectName,SubjectTType,SubjectStatus)
rm(SubjectName,SubjectTType,SubjectStatus)
#StudyDeventData
FolderID        <-getAttrDataM(FolderID,       SubjectDataNode,xpath="StudyEventData",attr="StudyEventOID")
StudyEventTType <-getAttrDataM(StudyEventTType,SubjectDataNode,xpath="StudyEventData",attr="TransactionType")
audit<-cbind(audit,FolderID,StudyEventTType)
rm(FolderID,StudyEventTType)
#FormData
FormID    <-getAttrDataM(FormID,   SubjectDataNode,xpath="StudyEventData/ns:FormData",attr="FormOID")
FormTType <-getAttrDataM(FormTType,SubjectDataNode,xpath="StudyEventData/ns:FormData",attr="TransactionType")
audit<-cbind(audit,FormID,FormTType)
rm(FormID,FormTType)

#ItemGroupData
ItemGroupData  <-getAttrDataM(ItemGroupData, SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData",attr="ItemGroupOID")
ItemGroupTType <-getAttrDataM(ItemGroupTType,SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData",attr="TransactionType")
audit<-cbind(audit,ItemGroupData,ItemGroupTType)
rm(ItemGroupData,ItemGroupTType)


#ItemData
FieldID    <-getAttrDataM(FieldID,   SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData/ns:ItemData",attr="ItemOID")
Data       <-getAttrDataM(Data,      SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData/ns:ItemData",attr="Value")
FiledTType <-getAttrDataM(FiledTType,SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData/ns:ItemData",attr="TransactionType")
Freeze     <-getAttrDataM(Freeze,    SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData/ns:ItemData",attr="mdsol:Freeze")
Verify     <-getAttrDataM(Verify,    SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData/ns:ItemData",attr="mdsol:Verify")
Lock       <-getAttrDataM(Lock,      SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData/ns:ItemData",attr="mdsol:Lock")
audit<-cbind(audit,FieldID,Data,FiledTType,Freeze,Verify,Lock)
rm(FieldID,Data,FiledTType,Freeze,Verify,Lock)


#Audit Record
AuditUser        <-getAttrDataM(AuditUser,     AuditNode,xpath="UserRef", attr="UserOID" )
DateTimeStamp    <-getValDataM(DateTimeStamp,  AuditNode,"DateTimeStamp")
ReasonForChange  <-getValDataM(ReasonForChange,AuditNode,"ReasonForChange")

SiteID  <-getAttrDataM(SiteID,AuditNode,xpath="LocationRef", attr="LocationOID" )

audit<-cbind(audit,AuditUser,DateTimeStamp,ReasonForChange,SiteID)
rm(DateTimeStamp,ReasonForChange,SiteID,AuditUser)

#Query
QueryID        <-getAttrDataM(QueryID,       SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData/ns:ItemData/data:*[name()='mdsol:Query']",attr="QueryRepeatKey")
QueryResponse  <-getAttrDataM(QueryResponse, SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData/ns:ItemData/data:*[name()='mdsol:Query']",attr="Response")
QueryRecipient <-getAttrDataM(QueryRecipient,SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData/ns:ItemData/data:*[name()='mdsol:Query']",attr="Recipient")
QueryValue     <-getAttrDataM(QueryValue,    SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData/ns:ItemData/data:*[name()='mdsol:Query']",attr="Value")
QueryStatus    <-getAttrDataM(QueryStatus,   SubjectDataNode,xpath="StudyEventData/ns:FormData/ns:ItemGroupData/ns:ItemData/data:*[name()='mdsol:Query']",attr="Status")

audit<-cbind(audit,QueryID,QueryResponse,QueryRecipient,QueryValue,QueryStatus)
rm(QueryID,QueryResponse,QueryRecipient,QueryValue,QueryStatus) 

audit<-as.data.frame(audit)

auditrecord<-rbind(auditrecord,audit)
rm(audit) 
}
save(auditrecord,file="auditrecord.Rda")
