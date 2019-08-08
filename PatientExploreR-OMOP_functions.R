suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(purrr))

# sets standard concepts for use with mapped vs. direct search options
standard_concepts <- data.table("domain_type"= c("Measurement","Condition","Drug","Observation","Device","Procedure"),"concepts"= c("LOINC,SNOMED,CPT4","SNOMED","RxNorm,CPT4,NDC","SNOMED,CPT4,LOINC,HCPCS","SNOMED,HCPCS","SNOMED,CPT4,HCPCS"))

##################
### CONNECTION ###
##################

setConnectFunction <- function(username, password, host, dbname, port) {
  connectString <- paste0("dbname='",dbname,"'")
  if (username != ""){
    connectString <- paste0(connectString, ", user='", username,"'")
  }
  if (password != ""){
    connectString <- paste0(connectString, ", password='", password,"'")
  }
  if (host != ""){
    connectString <- paste0(connectString, ", host='", host,"'")
  }
  if (port != ""){
    connectString <- paste0(connectString, ", port= ", as.integer(port))
  }
  
  fullConnectString <- paste0('DBI::dbConnect(drv, ', connectString , ')')

  return(fullConnectString)
}



checkOMOPconnection <- function(driver, username, password, host, dbname, port) {
  
  status<- tryCatch(
    {
      if (driver=="mysql") {
        drv <- dbDriver("MySQL")
        fullConnectString <- setConnectFunction(username, password, host, dbname, port)
        con <- eval(parse(text = fullConnectString))
      } else {
        con <- DatabaseConnector::connect(dbms = driver,
                                          server = host,
                                          user = username,
                                          password = password,
                                          schema = dbname,
                        		  port = port)
        
      }
    },
    warning = function(w) {
      # ignore
    },
    error = function(e) {
      # ignore
    }
  )
  
  if(!is.null(status)){
    out <- TRUE
    if (driver=="mysql") {
      on.exit(dbDisconnect(con))
    } else {
      on.exit(DatabaseConnector::disconnect(con))
    }
  }else{
    out <- FALSE
  }
  

  
  return(out)
  
}

checkOMOPtables <- function(driver, username, password, host, dbname, port) {
  
  necessaryTables = c("concept","concept_ancestor","concept_relationship","condition_occurrence","death","device_exposure","drug_exposure","measurement","observation","person","procedure_occurrence","visit_occurrence")
  
  if (driver=="mysql") {
    drv <- dbDriver("MySQL")
     fullConnectString <- setConnectFunction(username, password, host, dbname, port)
    con <- eval(parse(text = fullConnectString))

  } else {
    # creating connection object using DatabaseConnector
    con <- DatabaseConnector::connect(dbms = driver,
                                      server = host,
                                      user = username,
                                      password = password,
                                      schema = dbname,
                        	      port = port)
}
  
  foundTablesData <- tolower(dbListTables(con))
  
  if (driver=="mysql") {
    on.exit(dbDisconnect(con))
  } else {
    on.exit(DatabaseConnector::disconnect(con))
  }
  
  
  
  missingTables <- list()
  emptyTables <-list()
  
  for (tbls in necessaryTables) {

    if (!tbls %in% foundTablesData) { # check if table exists
      missingTables <- c(missingTables, tbls)
    } else { # check if any data in found table
      
      if (driver=="mysql") {
        dataCheckQuery <- paste0("SELECT * FROM " , tbls , " LIMIT 1;")
      } else {
        dataCheckQuery <- paste0("SELECT TOP 1 * FROM " , tbls, ";")
      }
      
      dataCheck <- sqlQuery(dataCheckQuery)
      if (nrow(dataCheck)==0) {
        emptyTables <- c(emptyTables, tbls)
      }
    }
  }
  
return(list("missingTables" = missingTables, "emptyTables" = emptyTables))
  
}


#### check if patient exists for search function
check_if_pt_exists<- function(ptid){
  found=FALSE
  if(ptid%in% pts_demographics$person_id){
    found = TRUE
  }
  return(found)
}

##################
### PRE - LOAD ###
##################

# Data Ontology

make_data_ontology <- function(){
  if (file.exists(paste0(getOption("currentPath"), "dataOntology.rds")) ) {
    # if Data Ontology exists, load it
    dataOntology = readRDS(paste0(getOption("currentPath"), "dataOntology.rds"))
  }else{
    # if not, create it, then save
    conceptQuery <- "SELECT concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, concept_code FROM concept WHERE (invalid_reason = '' OR invalid_reason IS NULL);"
    dataOntology <- sqlQuery(conceptQuery)
    dataOntology <- data.table(dataOntology)
    dataOntology$concept_name <- enc2utf8(dataOntology$concept_name)
  
    saveRDS(dataOntology, paste0(getOption("currentPath"), "dataOntology.rds")) # save Data Ontology
  }
  
  return(dataOntology)
  
}


# Demographic data
## pre-load demographic data for all patients to save in memory to map to cohort from searches

getDemographics <-function() { # patient list will restrict search
    
    
    queryStatement <- "SELECT person_id, year_of_birth, gender_concept_id, ethnicity_concept_id, race_concept_id FROM person" 
    deathqueryStatement <-"SELECT person_id, death_date FROM death"
    
    # first get main patient data
    ptDemo <- sqlQuery(queryStatement)
      
      ptDemo <- data.table(ptDemo) # convert to data.table
      current_year <- as.numeric(format(Sys.Date(),"%Y")) # get current year to calculate age
      ptDemo$age <- current_year - ptDemo$year_of_birth # calculate age
      
      # map concepts to reference table
      ptDemo <- merge(ptDemo, dataOntology[domain_id=="Gender",c("concept_id","concept_name")], by.x ="gender_concept_id", by.y = "concept_id" ,all.x=T) # Gender
      names(ptDemo)[names(ptDemo) == 'concept_name'] <- 'Gender' # rename column
      ptDemo=markNAasUnknown(ptDemo,"Gender")
      
      ptDemo <- merge(ptDemo, dataOntology[domain_id=="Race",c("concept_id","concept_name")], by.x ="race_concept_id", by.y = "concept_id" ,all.x=T) # Race
      names(ptDemo)[names(ptDemo) == 'concept_name'] <- 'Race' # rename column
      ptDemo=markNAasUnknown(ptDemo,"Race")
      
      ptDemo <- merge(ptDemo, dataOntology[domain_id=="Ethnicity",c("concept_id","concept_name")], by.x ="ethnicity_concept_id", by.y = "concept_id" ,all.x=T) # Ethnicity
      names(ptDemo)[names(ptDemo) == 'concept_name'] <- 'Ethnicity' # rename column
      ptDemo <- markNAasUnknown(ptDemo,"Ethnicity")
      
      ### clean up extra columns
      ptDemo <- ptDemo[,-c("ethnicity_concept_id","race_concept_id","gender_concept_id")]
      
      # add in death date
      ptDeath <- sqlQuery(deathqueryStatement)
      ptDeath <- data.table(ptDeath) # convert to data.table
      
      # merge with patient data
      ptDemo <- merge(ptDemo, ptDeath,by="person_id",all.x=T)
      # mark Alive/Deceased
      ptDemo$Status <- ifelse(is.na(ptDemo$death_date),"Alive","Deceased")
      
      return(ptDemo)
  
}


####################
#### FORMATTING ####
####################


## unpack vocabularies and codes for search function
unpackAndMap <- function(vocab_term_list) {
  vocabularies <- str_split(vocab_term_list, ":") %>% map_chr(`[`, 1)
  codes <- str_split(vocab_term_list, ":") %>% map_chr(`[`, 2)

  # # match to one another
   dataCriteria <- data.table::data.table(vocabularies = vocabularies, codes = codes)
  
  # # map inclusion criteria to dataOntology
   dataCriteriaMapped <- merge(dataCriteria, dataOntology, by.x= "codes", by.y = "concept_code")
   dataCriteriaMapped <- dataCriteriaMapped[vocabularies==vocabulary_id]

   return(dataCriteriaMapped)
  
}

# for 'Mapped' straegy; map input concept codes to common ontology
identifySynonyms <- function(codesFormatted) {
  synonymQuery <- paste0('SELECT concept_id_1, concept_id_2, relationship_id, invalid_reason FROM concept_relationship WHERE concept_id_1 IN (',codesFormatted,');')
  synonymData <- sqlQuery(synonymQuery)
  synonymData <- data.table::data.table(synonymData)
  synonymData <- synonymData[invalid_reason == ""]
  synonymData <- synonymData[,-"invalid_reason"]
  
  # check for "Maps to" or "%- RxNorm%" or "%- SNOMED%" | standard concepts
  synonymDataFiltered <- synonymData[(relationship_id == "Maps to") | (grepl("- RxNorm",relationship_id)) | (grepl("- SNOMED",relationship_id)) ]
  
  return(synonymDataFiltered)
  
}

# for 'Mapped' straegy; map input concept codes (from common ontology) to common ontology descendants
identifyMappings <- function(synonymCodes) {
  
  mappingQuery <- paste0('SELECT ancestor_concept_id, descendant_concept_id FROM concept_ancestor A WHERE A.ancestor_concept_id IN (', synonymCodes,' );')
  mappingData <- sqlQuery(mappingQuery)
  mappingData <- data.table::data.table(mappingData)
  
  mappingDataInfo <- merge(mappingData,dataOntology, by.x = "descendant_concept_id", by.y = "concept_id")
  
  return(mappingDataInfo)
  
}


# identify tables to seach for concepts of interest (direct strategy)
identifyTablesDirect <- function(criteriaTable) {
  
  searchTable = list()
  
  for(d in unique(standard_concepts$domain_type)){ # scan through all domain types
    mappingData = criteriaTable[domain_id == d]
    mappingCodes = mappingData[domain_id == d]$concept_id
    searchTable[[d]] <- mappingCodes # compile codes per domain type into one table
  }
  
  return(searchTable)
}

# identify tables to seach for concepts of interest (mapped strategy)
identifyTablesMapped <- function(mappingDataInfo) {
  
  searchTable = list()
  
  for(d in unique(standard_concepts$domain_type)) { # scan through all domain types
    
    mappingDataInfoFiltered <- mappingDataInfo[domain_id==d]
    mappingDataInfoFiltered <-  mappingDataInfoFiltered[(grep(gsub(",","|",standard_concepts[domain_type==d,concepts]),vocabulary_id))] # map to common concepts specifically used to the domain
    mappingCodes <- mappingDataInfoFiltered$concept_id
    searchTable[[d]] <- mappingCodes
  }
  
  return(searchTable)
  
}


### identifyPatients based on function
# function = OR (union)
identifyPatientsOR <- function(pts_condition, pts_observation, pts_measurement, pts_device, pts_drug, pts_procedure) {
  
  patient_list=c()
  
  if (!is.null(pts_condition)) {
    patient_list = union(patient_list, unique(pts_condition$person_id))
  }
  
  if (!is.null(pts_observation)) {
    patient_list = union(patient_list, unique(pts_observation$person_id))
  }
  
  if (!is.null(pts_measurement)) {
    patient_list = union(patient_list, unique(pts_measurement$person_id))
  }
  
  if (!is.null(pts_device)) {
    patient_list = union(patient_list, unique(pts_device$person_id))
  }
  
  if (!is.null(pts_drug)) {
    patient_list = union(patient_list, unique(pts_drug$person_id))
  }
  
  if (!is.null(pts_procedure)) {
    patient_list = union(patient_list, unique(pts_procedure$person_id))
  }
  
  return(patient_list)
  
}


# function = AND (intersect)
# To identify overlapping patients, we have to backmap the descendant terms to the original concepts
identifyPatientsAND <- function(criteriaMapped, synonymDataFiltered, mappingDataInfo, pts_condition, pts_observation, pts_measurement, pts_device, pts_drug, pts_procedure) {
  
  names(mappingDataInfo)[names(mappingDataInfo) == 'vocabulary_id'] <- 'mapped_vocabulary_id'
  names(mappingDataInfo)[names(mappingDataInfo) == 'concept_name'] <- 'mapped_concept_name'
  
  synonymMapped <- merge(mappingDataInfo[,c("descendant_concept_id","ancestor_concept_id","mapped_vocabulary_id","mapped_concept_name")], synonymDataFiltered[,c("concept_id_1","concept_id_2")], by.x = "ancestor_concept_id", by.y = "concept_id_2", allow.cartesian=TRUE)
  synonymMapped <- synonymMapped[!duplicated(synonymMapped)]
  
  combinedMapped <- merge(synonymMapped, criteriaMapped, by.x = "concept_id_1", by.y = "concept_id", allow.cartesian=TRUE)
  combinedMapped <- combinedMapped[!duplicated(combinedMapped)]
  
  combinedDirect <- merge(mappingDataInfo, criteriaMapped, by.x = "ancestor_concept_id", by.y = "concept_id", allow.cartesian=TRUE)
  combinedDirect <- combinedDirect[!duplicated(combinedDirect)]
  
  
  ### derive patient list by concept_codes
  # create code dictionary per original concept input
  # initializepatient_list
  
  unique_codes <- unique(criteriaMapped$codes)
  
  code_map = list()
  patient_list = list()
  
  for(c in unique_codes) {
    seed_codes = paste(criteriaMapped[codes == c]$concept_id,collapse=",")
    code_map[[c]] <- c(seed_codes) # initialize list with original concept code (i.e. in case of ATC category)
    code_map[[c]] <- c(code_map[[c]], combinedDirect[ancestor_concept_id %in% seed_codes]$descendant_concept_id) # add in direct mapped descendants
    code_map[[c]] <- c(code_map[[c]], combinedMapped[concept_id_1 %in% seed_codes]$descendant_concept_id)  # add in synonym codes and descendants
    
    patient_list[[c]] <- c()
  }
  
  if (!is.null(pts_condition)) { #Condition
    
    condition_codes <- unique(criteriaMapped[domain_id=="Condition"]$codes)
    
    for(c in condition_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_condition[condition_concept_id %in% code_map[[c]]]$person_id)
    }
  }
  
  if (!is.null(pts_observation)) { #Observation
    observation_codes <- unique(criteriaMapped[domain_id=="Observation"]$codes)
    
    for(c in observation_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_observation[observation_concept_id %in% code_map[[c]]]$person_id)
    }
  }
  
  if (!is.null(pts_measurement)) { #Measurement
    measurement_codes <- unique(criteriaMapped[domain_id=="Measurement"]$codes)
    
    for(c in measurement_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_measurement[measurement_concept_id %in% code_map[[c]]]$person_id)
    }
  }
  
  if (!is.null(pts_device)) {#Device
    device_codes <- unique(criteriaMapped[domain_id=="Device"]$codes)
    
    for(c in device_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_device[device_concept_id %in% code_map[[c]]]$person_id)
    }
  }
  
  if (!is.null(pts_drug)) { #Drug
    drug_codes = unique(criteriaMapped[domain_id=="Drug"]$codes)
    
    for(c in drug_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_drug[drug_concept_id %in% code_map[[c]]]$person_id)
    }
  }
  
  if (!is.null(pts_procedure)) {#Procedure
    procedure_codes <- unique(criteriaMapped[domain_id=="Procedure"]$codes)
    
    for(c in procedure_codes) {
      patient_list[[c]]  <- union(patient_list[[c]], pts_procedure[procedure_concept_id %in% code_map[[c]]]$person_id)
    }
  }
  
  # get intersected list
  patient_list_intersected = Reduce(intersect,patient_list)
  
  return(patient_list_intersected)
  
}


### mark any empty fields as Unknown
markNAasUnknown <- function(tbl, ColToUse) {
  
  if (ColToUse %in% colnames(tbl)) {
    if (any(is.na(tbl[is.na(get(ColToUse))]))) {
      missing_rows=tbl[is.na(get(ColToUse))]
      tbl[is.na(get(ColToUse)),eval(ColToUse):="Unknown"]
    }
  }
  
  return(tbl)
  
}


#### generate patient background and summary for report header
generate_pt_background<- function(pt_background){

  if(!is.na(pt_background$death_date)){
    age_of_death = as.numeric(year(as.Date(pt_background$death_date))) - as.numeric(pt_background$year_of_birth)
  }else{
    age_of_death = NA
  }
  
  
  str1=paste0("<strong>Status:</strong> ",pt_background$Status)
  str2=paste0("<strong>Age:</strong> ",pt_background$age)
  str3=paste0("<strong>Age of Death:</strong> ",age_of_death)
  str4=paste0("<strong>Ethnicity:</strong> ",pt_background$Ethnicity)
  str5=paste0("<strong>Race:</strong> ",pt_background$Race)
  #str4=paste0("<strong>Multi-racial?:</strong> " ,pt_background$MultiRacial)
  str6=paste0("<strong>Gender:</strong> ",pt_background$Gender)
  
  bstrs=list(str1,str2,str3,str4,str5,str6)
  return(bstrs)
}

generate_pt_summary<- function(pt_data){
  encounters = pt_data$Encounters
  observations = pt_data$Observations
  conditions = pt_data$Conditions
  procedures = pt_data$Procedures
  medications = pt_data$Medications
  measurements = pt_data$Measurements
  devices = pt_data$Devices

  deduped_encounters=encounters[,c("visit_occurrence_id","visit_concept")]
  deduped_encounters=deduped_encounters[!duplicated(deduped_encounters),]

  deduped_observations=observations[,c("visit_occurrence_id","observation_concept_name")]
  deduped_observations=deduped_observations[!duplicated(deduped_observations),]

  deduped_conditions=conditions[,c("visit_occurrence_id","condition_concept_name")]
  deduped_conditions=deduped_conditions[!duplicated(deduped_conditions),]
  
  deduped_procedures=procedures[,c("visit_occurrence_id","procedure_concept_name")]
  deduped_procedures=deduped_procedures[!duplicated(deduped_procedures),]
  
  deduped_medications=medications[,c("visit_occurrence_id","medication_concept_name")]
  deduped_medications=deduped_medications[!duplicated(deduped_medications),]

  deduped_measurements=measurements[,c("visit_occurrence_id","measurement_concept_name")]
  deduped_measurements=deduped_measurements[!duplicated(deduped_measurements),]

  deduped_devices=devices[,c("visit_occurrence_id","device_concept_name")]
  deduped_devices=deduped_devices[!duplicated(deduped_devices),]

  earliest_date = as.Date(encounters$visit_start_date[order(encounters$visit_start_date,decreasing=F)[1]])
  recent_date = as.Date(encounters$visit_start_date[order(encounters$visit_start_date,decreasing=T)[1]])

  str1a=paste0("<strong>Earliest encounter:</strong> ",earliest_date)
  str2a=paste0("<strong>Most recent encounter:</strong> ",recent_date)
  str3a=paste0("<strong># unique encounter types:</strong> ",length(unique(deduped_encounters$visit_concept)))
  str4a=paste0("<strong># Encounters:</strong> " ,nrow(deduped_encounters))
  str5a=paste0("<strong># Outpatient encounters:</strong> ",nrow(deduped_encounters[which(deduped_encounters$visit_concept=="Outpatient Visit"),]))
  str6a=paste0("<strong># Inpatient encounters:</strong> ",nrow(deduped_encounters[which(deduped_encounters$Encounter_Is_Inpatient=="Inpatient Visit"),]))

  strsa=c(str1a,str2a,str3a,str4a,str5a,str6a)


  str1b=paste0("<strong># observations:</strong> ",nrow(deduped_observations))
  str2b=paste0("<strong># unique observation concepts:</strong> ",length(unique(deduped_observations[!is.na(observation_concept_name)]$observation_concept_name)))
  str3b=paste0("<strong># conditions:</strong> ",nrow(deduped_conditions))
  str4b=paste0("<strong># unique condition concepts:</strong> " ,length(unique(deduped_conditions[!is.na(condition_concept_name)]$condition_concept_name)))
  str5b=paste0("<strong># procedures:</strong> ",nrow(deduped_procedures))
  str6b=paste0("<strong># unique procedure concepts:</strong> ",length(unique(deduped_procedures[!is.na(procedure_concept_name)]$procedure_concept_name)))
  str7b=paste0("<strong># medication prescriptions:</strong> ",nrow(deduped_medications))
  str8b=paste0("<strong># unique medication concepts:</strong> ",length(unique(deduped_medications[!is.na(medication_concept_name)]$medication_concept_name)))
  str9b=paste0("<strong># measurements:</strong> ",nrow(deduped_measurements))
  str10b=paste0("<strong># unique measurement concepts:</strong> ",length(unique(deduped_measurements[!is.na(measurement_concept_name)]$measurement_concept_name)))
  str11b=paste0("<strong># devices:</strong> ",nrow(deduped_devices))
  str12b=paste0("<strong># unique device concepts:</strong> ", length(unique(deduped_devices[!is.na(device_concept_name)]$device_concept_name)))
  strsb=c(str1b,str2b,str3b,str4b,str5b,str6b,str7b,str8b,str9b,str10b,str11b,str12b)

  return(list(strsa,strsb))
}


#### format data for patient report

generate_pt_report<-function(pt_data){
  # initialize master report table
  master_report=data.table(
    Date = as.Date(character()),
    Date_end = character(),
    Type = character(),
    Event = character(),
    Value = character()
  )

  # extract table-specific data
  observations_original = pt_data$Observations
  conditions_original = pt_data$Conditions
  procedures_original = pt_data$Procedures
  medications_original = pt_data$Medications
  measurements_original = pt_data$Measurements
  devices_original = pt_data$Devices

  ## format observations
  observations=observations_original[,c("observation_date","observation_concept_name", "value_as_number")]
  observations$Type = "Observation"
  observations$Date_end <- NA
  observations=observations[!duplicated(observations),]
  observations=observations[!is.na(observations$observation_date),]
  observations=observations[!is.na(observations$observation_concept_name),]
  observations[value_as_number==0]$value_as_number <- NA
  observations=observations[,c("observation_date","Date_end","Type","observation_concept_name","value_as_number")]
  colnames(observations)=c("Date","Date_end","Type","Event","Value")
  
  ## format conditions
  conditions=conditions_original[,c("condition_start_date","condition_end_date","condition_concept_name","condition_source_value")]
  conditions$Type = "Condition"
  conditions=conditions[!duplicated(conditions),]
  conditions=conditions[!is.na(conditions$condition_start_date),]
  conditions=conditions[!is.na(conditions$condition_concept_name),]
  conditions=conditions[,c("condition_start_date","condition_end_date","Type","condition_concept_name","condition_source_value")]
  colnames(conditions)=c("Date","Date_end","Type","Event","Value")

  ## format procedures
  procedures=procedures_original[,c("procedure_date","procedure_concept_name","procedure_source_value")]
  procedures$Type = "Procedure"
  procedures$Date_end <- NA
  procedures=procedures[!duplicated(procedures),]
  procedures=procedures[!is.na(procedures$procedure_date),]
  procedures=procedures[!is.na(procedures$procedure_concept_name),]
  procedures=procedures[,c("procedure_date","Date_end","Type","procedure_concept_name","procedure_source_value")]
  colnames(procedures)=c("Date","Date_end","Type","Event","Value")

  ## format Medications
  medications=medications_original[,c("drug_exposure_start_date","drug_exposure_end_date","medication_concept_name","dose_unit_source_value")]
  medications$Type = "Medication"
  medications=medications[!duplicated(medications),]
  medications=medications[!is.na(medications$drug_exposure_start_date),]
  medications=medications[!is.na(medications$medication_concept_name),]
  medications=medications[,c("drug_exposure_start_date","drug_exposure_end_date","Type","medication_concept_name","dose_unit_source_value")]
  colnames(medications)=c("Date","Date_end","Type","Event","Value")
  
  ## format Measurements
  measurements=measurements_original[,c("measurement_date","measurement_concept_name","value_as_number")]
  measurements$Type = "Measurement"
  measurements$Date_end <- NA
  measurements=measurements[!duplicated(measurements),]
  measurements=measurements[!is.na(measurements$measurement_date),]
  measurements=measurements[!is.na(measurements$measurement_concept_name),]
  measurements=measurements[,c("measurement_date","Date_end","Type","measurement_concept_name","value_as_number")]
  colnames(measurements)=c("Date","Date_end","Type","Event","Value")

  ## format Devices
  devices=devices_original[,c("device_exposure_start_date","device_exposure_end_date", "device_concept_name","device_source_value")]
  devices$Type = "Device"
  devices=devices[!duplicated(devices),]
  devices=devices[!is.na(devices$device_exposure_start_date),]
  devices=devices[!is.na(devices$device_concept_name),]
  devices=devices[,c("device_exposure_start_date","device_exposure_end_date","Type","device_concept_name","device_source_value")]
  colnames(devices)=c("Date","Date_end","Type","Event","Value")


  ## rbind all data modalities together
  master_report=rbind(master_report,observations,conditions,procedures,medications,measurements,devices)

  # verify Events are characters
  master_report$Event = as.character(master_report$Event)
  
  # verify Dates are dates
  master_report$Date = as.Date(as.character(master_report$Date))
  master_report$Date_end = as.Date(as.character(master_report$Date_end),format="%Y-%m-%d")
  
  
  return(master_report)
}

### format data for multiplex timeline
format_multiplex_timeline <- function(pt_data_report){

multiplex_timeline <- pt_data_report  
  
multiplex_timeline$id = row.names(multiplex_timeline)
multiplex_timeline$type <- as.character(NA)
multiplex_timeline = multiplex_timeline[,c("id","Event","Date","Date_end","Type","type", "Value")] # keep Value to display when clicked
colnames(multiplex_timeline) = c("id","content","start","end","group","type","Value")

# if end date same as start, set end to NA
multiplex_timeline[start==end]$end <- NA

# if end date is not NA, set type to range
multiplex_timeline[!is.na(end)]$type <- "range"

# otherwise set type to point
multiplex_timeline[is.na(end)]$type <- "point"

return(multiplex_timeline)

}
  
####################
### LOADING DATA ###
####################


#  Wrapper for domain-specific getData functions (e.g., getObservations). Produces a list of tables for all relevant domains.
get_all_pt_data <- function(pt_id){

  ptEncs <- getEncounters(pt_id)
  ptObsData <- getObservations(pt_id)
  ptCondData <- getConditions(pt_id)
  ptProcData <- getProcedures(pt_id)
  ptsMedsData <- getMedications(pt_id)
  ptMeasData <- getMeasurements(pt_id)
  ptDeviceData <- getDevices(pt_id)

  return(list(Encounters = ptEncs,
              Observations = ptObsData,
              Conditions = ptCondData,
              Procedures = ptProcData,
              Medications = ptsMedsData,
              Measurements = ptMeasData,
              Devices = ptDeviceData
  ))

} # END get_data function

# modality specific get functions (utilized in get_all_pt_data)

getEncounters <- function(pt_id) {
  
    queryStatement <- paste0('SELECT person_id, visit_occurrence_id, visit_concept_id, visit_start_date, visit_end_date, visit_source_concept_id, visit_source_value, admitting_source_concept_id, discharge_to_concept_id FROM visit_occurrence WHERE person_id = ', pt_id)
    
    # get visit data
    ptEncs <- sqlQuery(queryStatement)
      
      ptEncs <- data.table(ptEncs) # convert to data.table
      
      # convert NA source_concept_ids to 0
      ptEncs[is.na(admitting_source_concept_id)]$admitting_source_concept_id <- 0
      ptEncs[is.na(discharge_to_concept_id)]$discharge_to_concept_id <- 0
      
      # merge in relevant information concept ids
      ptEncs <- merge(ptEncs,dataOntology[,c("concept_id","concept_name")], by.x="visit_concept_id", by.y="concept_id", all.x=TRUE)
      names(ptEncs)[names(ptEncs) == 'concept_name'] <- 'visit_concept' # rename column
      ptEncs <- ptEncs[,-"visit_concept_id"]
      ptEncs <- merge(ptEncs,dataOntology[,c("concept_id","concept_name")], by.x="visit_source_concept_id", by.y="concept_id", all.x=TRUE)
      names(ptEncs)[names(ptEncs) == 'concept_name'] <- 'visit_source_concept' # rename column
      ptEncs <- ptEncs[,-"visit_source_concept_id"]
      ptEncs <- merge(ptEncs,dataOntology[,c("concept_id","concept_name")], by.x="admitting_source_concept_id", by.y="concept_id", all.x=TRUE)
      names(ptEncs)[names(ptEncs) == 'concept_name'] <- 'admitting_concept' # rename column
      ptEncs <- ptEncs[,-"admitting_source_concept_id"]
      ptEncs <- merge(ptEncs,dataOntology[,c("concept_id","concept_name")], by.x="discharge_to_concept_id", by.y="concept_id", all.x=TRUE)
      names(ptEncs)[names(ptEncs) == 'concept_name'] <- 'discharge_concept' # rename column
      ptEncs <- ptEncs[,-"discharge_to_concept_id"]

      ptEncs$visit_start_date <- as.Date(ptEncs$visit_start_date)
    
    return(ptEncs)
    
}


getObservations <- function(pt_id) {
  
    queryStatement <- paste0('SELECT person_id, observation_concept_id, observation_source_concept_id, observation_date, observation_type_concept_id, value_as_number, value_as_string, value_as_concept_id, visit_occurrence_id, observation_source_value, unit_source_value FROM observation WHERE person_id = ', pt_id)
    
    ptObsData <- sqlQuery(queryStatement)
    ptObsData <- data.table(ptObsData) # convert to data.table
      
      # obtain table specific ontology
      observationTableOntology <- dataOntology[domain_id=="Observation"]
      
      # format clinical data
      ptObsData <- merge(ptObsData, observationTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="observation_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptObsData)[names(ptObsData) == 'concept_code'] <- 'observation_concept_code' # rename column
      names(ptObsData)[names(ptObsData) == 'concept_name'] <- 'observation_concept_name' # rename column
      names(ptObsData)[names(ptObsData) == 'vocabulary_id'] <- 'observation_concept_vocabulary' # rename column
      ptObsData <- ptObsData[,-"observation_concept_id"]
      
      ptObsData <- merge(ptObsData, observationTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="observation_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptObsData)[names(ptObsData) == 'concept_code'] <- 'observation_source_code' # rename column
      names(ptObsData)[names(ptObsData) == 'concept_name'] <- 'observation_source_name' # rename column
      names(ptObsData)[names(ptObsData) == 'vocabulary_id'] <- 'observation_source_vocabulary' # rename column
      ptObsData <- ptObsData[,-"observation_source_concept_id"]
      
      # format metadata
      ptObsData <- merge(ptObsData,dataOntology[,c("concept_id","concept_name")],by.x="observation_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptObsData)[names(ptObsData) == 'concept_name'] <- 'observation_type' # rename column
      ptObsData <- ptObsData[,-"observation_type_concept_id"]
      
      ptObsData=merge(ptObsData,dataOntology[,c("concept_id","concept_name")],by.x="value_as_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptObsData)[names(ptObsData) == 'concept_name'] <- 'value_concept' # rename column
      ptObsData <- ptObsData[,-"value_as_concept_id"]

      ptObsData$observation_date <- as.Date(ptObsData$observation_date)


    return(ptObsData)

}


getConditions <- function(pt_id) {
    
    queryStatement <- paste0('SELECT person_id, condition_concept_id, condition_start_date, condition_end_date, visit_occurrence_id, condition_type_concept_id, condition_source_value, condition_source_concept_id, condition_status_concept_id FROM condition_occurrence WHERE person_id = ', pt_id)

    ptCondData <- sqlQuery(queryStatement)
    ptCondData <- data.table(ptCondData) # convert to data.table
      
      # obtain table specific ontology
      conditionTableOntology <- dataOntology[grep("Condition",domain_id)]
      
      # format clinical data
      ptCondData <- merge(ptCondData, conditionTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="condition_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptCondData)[names(ptCondData) == 'concept_code'] <- 'condition_concept_code' # rename column
      names(ptCondData)[names(ptCondData) == 'concept_name'] <- 'condition_concept_name' # rename column
      names(ptCondData)[names(ptCondData) == 'vocabulary_id'] <- 'condition_concept_vocabulary' # rename column
      ptCondData <- ptCondData[,-"condition_concept_id"]
      
      ptCondData <- merge(ptCondData, conditionTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="condition_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptCondData)[names(ptCondData) == 'concept_code'] <- 'condition_source_code' # rename column
      names(ptCondData)[names(ptCondData) == 'concept_name'] <- 'condition_source_name' # rename column
      names(ptCondData)[names(ptCondData) == 'vocabulary_id'] <- 'condition_source_vocabulary' # rename column
      ptCondData <- ptCondData[,-"condition_source_concept_id"]
      
      # format metadatadata
      ptCondData <- merge(ptCondData,dataOntology[,c("concept_id","concept_name")],by.x="condition_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptCondData)[names(ptCondData) == 'concept_name'] <- 'condition_type' # rename column
      ptCondData <- ptCondData[,-"condition_type_concept_id"]
      ptCondData <- merge(ptCondData,dataOntology[,c("concept_id","concept_name")],by.x="condition_status_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptCondData)[names(ptCondData) == 'concept_name'] <- 'condition_status_type' # rename column
      ptCondData <- ptCondData[,-"condition_status_concept_id"]

    
      ptCondData$condition_start_date <- as.Date(ptCondData$condition_start_date)

    return(ptCondData)

}

getProcedures <- function(pt_id){

    
    queryStatement <- paste0('SELECT person_id, procedure_concept_id, procedure_date, quantity, visit_occurrence_id, procedure_type_concept_id, procedure_source_value, procedure_source_concept_id  FROM procedure_occurrence WHERE person_id = ', pt_id)

    ptProcData <- sqlQuery(queryStatement)
    ptProcData <- data.table(ptProcData) # convert to data.table

      # obtain table specific ontology
      procedureTableOntology <- dataOntology[domain_id=="Procedure"]
      
      # format clinical data
      ptProcData <- merge(ptProcData, procedureTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="procedure_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptProcData)[names(ptProcData) == 'concept_code'] <- 'procedure_concept_code' # rename column
      names(ptProcData)[names(ptProcData) == 'concept_name'] <- 'procedure_concept_name' # rename column
      names(ptProcData)[names(ptProcData) == 'vocabulary_id'] <- 'procedure_concept_vocabulary' # rename column
      ptProcData <- ptProcData[,-"procedure_concept_id"]
      
      ptProcData <- merge(ptProcData, procedureTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="procedure_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptProcData)[names(ptProcData) == 'concept_code'] <- 'procedure_source_code' # rename column
      names(ptProcData)[names(ptProcData) == 'concept_name'] <- 'procedure_source_name' # rename column
      names(ptProcData)[names(ptProcData) == 'vocabulary_id'] <- 'procedure_source_vocabulary' # rename column
      ptProcData <- ptProcData[,-"procedure_source_concept_id"]
      
      # format metadata
      ptProcData <- merge(ptProcData,dataOntology[,c("concept_id","concept_name")],by.x="procedure_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptProcData)[names(ptProcData) == 'concept_name'] <- 'procedure_type' # rename column
      ptProcData <- ptProcData[,-"procedure_type_concept_id"]

      ptProcData$procedure_date <- as.Date(ptProcData$procedure_date)

    
    return(ptProcData)

}


getMedications <- function(pt_id) {

    
    queryStatement <- paste0('SELECT person_id, drug_concept_id, drug_exposure_start_date, drug_exposure_end_date, drug_type_concept_id, stop_reason, refills, quantity, days_supply, sig, route_concept_id, dose_unit_source_value, visit_occurrence_id, drug_source_value, drug_source_concept_id, route_source_value FROM drug_exposure WHERE person_id = ', pt_id)
    
    ptsMedsData <- sqlQuery(queryStatement)
    ptsMedsData <- data.table(ptsMedsData) # convert to data.table
    

      # obtain table specific ontology
      medicationTableOntology <- dataOntology[domain_id=="Drug"]
      
      # format clinical data
      ptsMedsData <- merge(ptsMedsData, medicationTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="drug_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptsMedsData)[names(ptsMedsData) == 'concept_code'] <- 'medication_concept_code' # rename column
      names(ptsMedsData)[names(ptsMedsData) == 'concept_name'] <- 'medication_concept_name' # rename column
      names(ptsMedsData)[names(ptsMedsData) == 'vocabulary_id'] <- 'medication_concept_vocabulary' # rename column
      ptsMedsData <- ptsMedsData[,-"drug_concept_id"]
      
      ptsMedsData <- merge(ptsMedsData, medicationTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="drug_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptsMedsData)[names(ptsMedsData) == 'concept_code'] <- 'medication_source_code' # rename column
      names(ptsMedsData)[names(ptsMedsData) == 'concept_name'] <- 'medication_source_name' # rename column
      names(ptsMedsData)[names(ptsMedsData) == 'vocabulary_id'] <- 'medication_source_vocabulary' # rename column
      ptsMedsData <- ptsMedsData[,-"drug_source_concept_id"]
      
      # format metadata
      ptsMedsData <- merge(ptsMedsData,dataOntology[,c("concept_id","concept_name")],by.x="drug_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptsMedsData)[names(ptsMedsData) == 'concept_name'] <- 'drug_type' # rename column
      ptsMedsData <- ptsMedsData[,-"drug_type_concept_id"]
      ptsMedsData <- merge(ptsMedsData,dataOntology[,c("concept_id","concept_name")],by.x="route_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptsMedsData)[names(ptsMedsData) == 'concept_name'] <- 'route_concept' # rename column
      ptsMedsData <- ptsMedsData[,-"route_concept_id"]
    
ptsMedsData$drug_exposure_start_date <- as.Date(ptsMedsData$drug_exposure_start_date)

    return(ptsMedsData)
}



getMeasurements <- function(pt_id) {

    queryStatement <- paste0('SELECT person_id, measurement_concept_id, measurement_date, measurement_type_concept_id, value_as_number, value_as_concept_id, unit_concept_id, visit_occurrence_id, measurement_source_value, measurement_source_concept_id FROM measurement WHERE person_id = ', pt_id);
    
    ptMeasData <- sqlQuery(queryStatement)
    ptMeasData <- data.table(ptMeasData) # convert to data.table
    

      # obtain table specific ontology
      measurementTableOntology <- dataOntology[domain_id=="Measurement"]
      
      # format clinical data
      ptMeasData <- merge(ptMeasData, measurementTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="measurement_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptMeasData)[names(ptMeasData) == 'concept_code'] <- 'measurement_concept_code' # rename column
      names(ptMeasData)[names(ptMeasData) == 'concept_name'] <- 'measurement_concept_name' # rename column
      names(ptMeasData)[names(ptMeasData) == 'vocabulary_id'] <- 'measurement_concept_vocabulary' # rename column
      ptMeasData <- ptMeasData[,-"measurement_concept_id"]
      
      ptMeasData <- merge(ptMeasData, measurementTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="measurement_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptMeasData)[names(ptMeasData) == 'concept_code'] <- 'measurement_source_code' # rename column
      names(ptMeasData)[names(ptMeasData) == 'concept_name'] <- 'measurement_source_name' # rename column
      names(ptMeasData)[names(ptMeasData) == 'vocabulary_id'] <- 'measurement_source_vocabulary' # rename column
      ptMeasData <- ptMeasData[,-"measurement_source_concept_id"]
      
      # format metadata
      ptMeasData <- merge(ptMeasData,dataOntology[,c("concept_id","concept_name")],by.x="measurement_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptMeasData)[names(ptMeasData) == 'concept_name'] <- 'measurement_type' # rename column
      ptMeasData <- ptMeasData[,-"measurement_type_concept_id"]
      ptMeasData <- merge(ptMeasData,dataOntology[,c("concept_id","concept_name")],by.x="value_as_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptMeasData)[names(ptMeasData) == 'concept_name'] <- 'value_concept' # rename column
      ptMeasData <- ptMeasData[,-"value_as_concept_id"]
      ptMeasData <- merge(ptMeasData,dataOntology[,c("concept_id","concept_name")],by.x="unit_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptMeasData)[names(ptMeasData) == 'concept_name'] <- 'unit_concept' # rename column
      ptMeasData <- ptMeasData[,-"unit_concept_id"]

	ptMeasData$measurement_date <- as.Date(ptMeasData$measurement_date)    

    return(ptMeasData)
  
}


getDevices <- function(pt_id) {
  
    queryStatement <- paste0('SELECT person_id, device_concept_id, device_exposure_start_date, device_exposure_end_date, device_type_concept_id, device_source_value, visit_occurrence_id, device_source_concept_id FROM device_exposure WHERE person_id = ', pt_id)

    ptDeviceData <- sqlQuery(queryStatement)
    ptDeviceData <- data.table(ptDeviceData) # convert to data.table

      # obtain table specific ontology
      deviceTableOntology = dataOntology[grep("Device",domain_id)]
      
      # format clinical data
      ptDeviceData <- merge(ptDeviceData, deviceTableOntology[,c("concept_id","vocabulary_id","concept_code","concept_name")], by.x="device_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptDeviceData)[names(ptDeviceData) == 'concept_code'] <- 'device_concept_code' # rename column
      names(ptDeviceData)[names(ptDeviceData) == 'concept_name'] <- 'device_concept_name' # rename column
      names(ptDeviceData)[names(ptDeviceData) == 'vocabulary_id'] <- 'device_concept_vocabulary' # rename column
      ptDeviceData <- ptDeviceData[,-"device_concept_id"]
      
      ptDeviceData <- merge(ptDeviceData, deviceTableOntology[,c("concept_id","vocabulary_id", "concept_code","concept_name")], by.x="device_source_concept_id",by.y="concept_id",all.x=TRUE)
      names(ptDeviceData)[names(ptDeviceData) == 'concept_code'] <- 'device_source_code' # rename column
      names(ptDeviceData)[names(ptDeviceData) == 'concept_name'] <- 'device_source_name' # rename column
      names(ptDeviceData)[names(ptDeviceData) == 'vocabulary_id'] <- 'device_source_vocabulary' # rename column
      ptDeviceData <- ptDeviceData[,-"device_source_concept_id"]
      
      # format metadata
      ptDeviceData <- merge(ptDeviceData,dataOntology[,c("concept_id","concept_name")],by.x="device_type_concept_id",by.y="concept_id", all.x=TRUE)
      names(ptDeviceData)[names(ptDeviceData) == 'concept_name'] <- 'device_type' # rename column
      ptDeviceData <- ptDeviceData[,-"device_type_concept_id"]
      
    
ptDeviceData$device_exposure_start_date <- as.Date(ptDeviceData$device_exposure_start_date)

    return(ptDeviceData)

}

#####################
### FIND PATIENTS ###
#####################


findPatients <- function(selected_terms, func_type, search_strat) {


  dataCriteriaMapped <- unpackAndMap(selected_terms)
  
  
  if (search_strat == "direct") {
    
    useSource <- "_source" # search _source_concept_id
    searchTable <- identifyTablesDirect(dataCriteriaMapped)
    
  } else if (search_strat == "mapped") {
    
  useSource <- "" # search _concept_id

  dataCodesFormatted <- paste0(dataCriteriaMapped$concept_id,collapse=",")

  # get common ontology synonyms
  synonymDataFiltered <- identifySynonyms(dataCodesFormatted)
  synonymData <- merge(synonymDataFiltered[,"concept_id_2"], dataOntology[,c("concept_id","domain_id","vocabulary_id")], by.x="concept_id_2",by.y = "concept_id")
  colnames(synonymData) <- c("concept_id","domain_id","vocabulary_id")
  synonymCodes <- paste(union(dataCriteriaMapped$concept_id, synonymDataFiltered$concept_id_2),collapse = ",") ## adds original codes into ancestor query (b/c of scenarios with ATC))

  # get descendents
  mappingDataInfo <- identifyMappings(synonymCodes)
  mappingData <- mappingDataInfo[,c("descendant_concept_id","domain_id","vocabulary_id")]
  colnames(mappingData) <- c("concept_id","domain_id","vocabulary_id")

  conceptsCombined <- rbind(dataCriteriaMapped[,c("concept_id","domain_id","vocabulary_id")],synonymData)
  conceptsCombined <- rbind(conceptsCombined, mappingData)
  conceptsCombined <- conceptsCombined[!duplicated(conceptsCombined),]

  # get tables to search for mapped concepts
  searchTable <- identifyTablesMapped(conceptsCombined)

  }
  

  # if any condition table codes
  if (length(searchTable$Condition)>0) {
    condition_codes <- paste(searchTable$Condition,collapse=",")
    pts_condition <- searchCondition(useSource, condition_codes)
  } else {
    pts_condition <- NULL
  }

  # if any observation table codes
  if (length(searchTable$Observation)>0) {
    observation_codes <- paste(searchTable$Observation,collapse=",")
    pts_observation <- searchObservation(useSource, observation_codes)
  } else {
    pts_observation <- NULL
  }

  # if any measurement table codes
  if (length(searchTable$Measurement)>0) {
    measurement_codes <- paste(searchTable$Measurement,collapse=",")
    pts_measurement <- searchMeasurement(useSource, measurement_codes)
  } else {
    pts_measurement <- NULL
  }

  # if any drug table codes
  if (length(searchTable$Drug)>0) {
    drug_codes <- paste(searchTable$Drug,collapse=",")
    pts_drug <- searchDrug(useSource, drug_codes)
  } else {
    pts_drug <- NULL
  }

  # if any device table codes
  if (length(searchTable$Device)>0) {
    device_codes <- paste(searchTable$Drug,collapse=",")
    pts_device <- searchDevice(useSource, device_codes)
  } else {
    pts_device <- NULL
  }

  # if any procedure table codes
  if (length(searchTable$Procedure)>0) {
    procedure_codes <- paste(searchTable$Procedure,collapse=",")
    pts_procedure <- searchProcedure(useSource, procedure_codes)
  }else{
    pts_procedure <- NULL
  }

  # search

  if (func_type=="or") {
    patient_list <- identifyPatientsOR(pts_condition, pts_observation, pts_measurement, pts_device, pts_drug, pts_procedure)
  } else if (func_type=="and") {
    patient_list <- identifyPatientsAND(dataCriteriaMapped, synonymDataFiltered, mappingDataInfo, pts_condition, pts_observation, pts_measurement, pts_device, pts_drug, pts_procedure)
  }

return(patient_list)
  
}  


### specific table search functions (used in Find Patients function)

searchCondition <- function(useSource, codes) {
  conditionQuery <- paste0('SELECT person_id, condition_concept_id FROM condition_occurrence WHERE condition',useSource,'_concept_id IN (',codes,') ')
  dataCondition <- sqlQuery(conditionQuery)
  dataCondition <- data.table(dataCondition)
  dataCondition <- dataCondition[!duplicated(dataCondition)]
  return(dataCondition)
}

searchObservation <- function(useSource, codes) {
  observationQuery <- paste0('SELECT person_id, observation_concept_id FROM observation WHERE observation',useSource,'_concept_id IN (',codes,') ')
  dataObservation <- sqlQuery(observationQuery)
  dataObservation <- data.table(dataObservation)
  dataObservation <- dataObservation[!duplicated(dataObservation)]
  return(dataObservation)
}


searchMeasurement <- function(useSource, codes) {
  measurementQuery <- paste0('SELECT person_id, measurement_concept_id FROM measurement WHERE measurement',useSource,'_concept_id IN (',codes,') ')
  dataMeasurement <- sqlQuery(measurementQuery)
  dataMeasurement <- data.table(dataMeasurement)
  dataMeasurement <- dataMeasurement[!duplicated(dataMeasurement)]
  return(dataMeasurement)
}


searchDrug <- function(useSource, codes) {
  drugQuery <- paste0('SELECT person_id, drug_concept_id FROM drug_exposure WHERE drug',useSource,'_concept_id IN (',codes,') ')
  dataDrug <- sqlQuery(drugQuery)
  dataDrug <- data.table(dataDrug)
  dataDrug <- dataDrug[!duplicated(dataDrug)]
  return(dataDrug)
}


searchDevice <- function(useSource, codes) {
  deviceQuery <- paste0('SELECT person_id, device_concept_id FROM device_exposure WHERE device',useSource,'_concept_id IN (',codes,') ')
  dataDevice <- sqlQuery(deviceQuery)
  dataDevice <- data.table(dataDevice)
  dataDevice <- dataDevice[!duplicated(dataDevice)]
  return(dataDevice)
}

searchProcedure<- function(useSource, codes) {
  procedureQuery <- paste0('SELECT person_id, procedure_concept_id FROM procedure_occurrence WHERE procedure',useSource,'_concept_id IN (',codes,') ')
  dataProcedure <- sqlQuery(procedureQuery)
  dataProcedure <- data.table(dataProcedure)
  dataProcedure <- dataProcedure[!duplicated(dataProcedure)]
  return(dataProcedure)
}
