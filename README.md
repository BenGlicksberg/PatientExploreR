PatientExploreR Readme
================
Benjamin S. Glicksberg
2018-19

## PatientExploreR

PatientExploreR is an extensible application built on the R/Shiny framework  to interface with the
[Observational Health Data Sciences and Informatics
(OHDSI)](https://www.ohdsi.org/) [OMOP Common Data
Model](https://www.ohdsi.org/data-standardization/). Briefly, OMOP is a
standardized relational database schema for Electronic Health Record
(EHR) or Electronic Medical Record (EMR) data (i.e., patient data
collected during clinical visits to a health system). The main benefit
of a standardized schema is that it allows for interoperability between
institutions, even if the underlying EHR vendors are disparate.

For a detailed description of the OMOP common data model, please visit
this [helpful wiki](https://github.com/OHDSI/CommonDataModel/wiki).

In its backend, OMOP relies on standardized data ontologies and
metathesaureses, such as the [Unified Medical Language System
(UMLS)](https://www.nlm.nih.gov/research/umls/). Much of the underlying logic of the app for interfacing with OMOP data is adapted from our [ROMOP](https://github.com/BenGlicksberg/ROMOP) package (further description and details can be found in ROMOP's GitHub page). 

### Manuscript information:
Glicksberg BS, Oskotsky B, Thangaraj PM, Giangreco N, Badgeley MA, Johnson KW, Datta D, Rudrapatna VA, Rappoport N, Shervey MM, Miotto R. PatientExploreR: an extensible application for dynamic visualization of patient clinical history from electronic health records in the OMOP common data model. Bioinformatics. 2019 Nov 1;35(21):4515-8.

### Sandbox Server

The Centers for Medicare and Medicaid Services (CMS) have released a
synthetic clinical dataset
[DE-SynPUF](https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF.html))
in the public domain with the aim of being reflective of the patient
population but containing no protected health information. The OHDSI
group has underwent the task of converting these data into the [OMOP CDM
format](https://github.com/OHDSI/ETL-CMS). Users are certainly able to
set up this configuration on their own system following the instructions
on the GitHub page. We obtained all data files from the [OHDSI FTP
server](ftp://ftp.ohdsi.org/synpuf) (accessed June 17th, 2018) and
created the CDM (DDL and indexes) according to their [official
instructions](https://github.com/OHDSI/CommonDataModel/tree/master/PostgreSQL),
but modified for MySQL. For space considerations, we only uploaded one
million rows of each of the data files. The sandbox server is a Rshiny
server running as an Elastic Compute Cloud (EC2) instance on Amazon Web
Services (AWS) querying a MySQL database server (AWS Aurora MySQL).

### Example patient

As the DE-SynPUF data does not contain patient measurement results, we
generated a profile for a patient with Chron'€™s Disease with
representative clinical data (e.g., disease codes and lab test results)
for illustrative purposes. Users can recreate this example patient using
the script contained in the "data/new_pt_insert_commands.txt" file. The script is formatted for
a MySQL database.

### Requirements

 - Personal Computer or Server with connection to internet
 - R
 - All required packages (see Install.R)
 - Database software (either: MySQL, PostgreSQL, Amazon Redshift, Microsoft SQL Server, Microsoft Parallel Data Warehouse, Google BigQuery
 - Access to Electronic Health Record data (recommended for use with a de-identified version) that is properly formatted to OMOP Common Data Model v5

### Installation

 - Download app from GitHub 
 - Navigate to directory and run Install.R (Rscript Install.R) to install all required packages
 - (Optional) Create .Renviron file in directory with database credentials (Note: this can be done in the app itself). See section below for formatting this file.
 - Open app using either Rstudio (Run App) or from command line: R -e \"shiny::runApp('PatientExploreR.R')\", then navigate to the IP address after \"Listening on?\" using a web browser.
 
 #### Storing Credentials
 
 For quick connection, users can quickly load and save their credentials to connect to EHR database within an R environment file (.Renviron). Either this file can be created after the credentials are entered in the input fields (Save Credentials button) which will automatically create this file in the directory of interest. Alternatively, users can create an .Renviron file the project directory in the following format:
>   driver = ""  
>   host = ""  
>   username = ""  
>   password = ""  
>   dbname = ""  
>   port = ""  
                                        
Full instructions on these connection parameters can be found from the OHDSI consortium's [Database Connector](https://github.com/OHDSI/DatabaseConnector) GitHub page.
