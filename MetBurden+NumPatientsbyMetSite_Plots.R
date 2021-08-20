#load libraries 
library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)

#load files 
fname <- "C:/Users/Sam/Desktop/Samantha/HOPP_SummerProgram_MSK/ShahLabSpecific/metatrop_met_site_annotations_clinical_and_impact - metatrop_met_site_annotations_clinical_and_impact.csv.csv"
df_meta_trop <- read.csv(fname)
radiology_predictions_file <- "C:/Users/Sam/Downloads/metsites_by_id_0712.csv"
radiology_predictions <- read.csv(radiology_predictions_file)
file <- "C:/Users/Sam/Downloads/mskimpact_clinical_data (6).tsv"
df_clinical_sum <- read.csv(file, sep="\t")
dataset <- "C:/Users/Sam/Downloads/tableS2_data.xlsx"
tropisms_dataset <- read_excel(dataset, skip=2)

#separating the meta tropisms data into pathology and billing
df_pathology_met_sites <- df_meta_trop %>% select (DMP_ID, SOURCE, METASTATIC_SITE_BILLING_RDN) %>% filter(SOURCE == "Pathology") %>% distinct()
df_billing_met_sites <- df_meta_trop %>% select (DMP_ID, SOURCE, METASTATIC_SITE_BILLING_RDN) %>% filter(SOURCE == "ICD Billing") %>% distinct()

#change the column names in the radiology predictions
  #column names: DMP ID and a column for each of the metastatic organ sites 
df_met_sites_radiology <- radiology_predictions %>% select(DMP_ID, ADRENAL_GLAND = adrenal,
  BONE = bone, CNS_BRAIN = cnsbrain,LIVER = liver, LUNG = lung, LYMPH = lymphoid,
  MEDIASTINUM = mediastinum, PERITONEUM = peritoneum, OTHER = softtissue)

#make sure changing the column names worked correctly 
head(df_met_sites_radiology)

#change the radiology predictions data frame so that there is one column for met site 
  #in the column are the metastatic sites and the next column is a 1 or 0 for yes or no
df_met_sites_radio_melt <- melt(data = df_met_sites_radiology, id.vars = c("DMP_ID"))

#make sure the column names changed 
head(df_met_sites_radio_melt)

#change the column names to metastatic site and yes/no metastasis
colnames(df_met_sites_radio_melt)[which(names(df_met_sites_radio_melt) == 
    "variable")] <- "METASTATIC_SITE_BILLING_RDN"
colnames(df_met_sites_radio_melt)[which(names(df_met_sites_radio_melt) == 
    "value")] <- "YES/NO_METASTASIS"

#filter the data frame for the instances when there was metastasis to that organ site
df_met_radiology <- df_met_sites_radio_melt %>% filter(`YES/NO_METASTASIS`== 1)

#make sure only ones are present in the "YES/NO_METASTASIS" column
head(df_met_radiology)

#add a source column to the radiology predictions - the source is Radiology_Predictions
df_met_radiology$SOURCE <- "Radiology_Predictions"

#make sure the source column was added 
head(df_met_radiology)

#create a data frame with only the dmp_id, met site, and source
  #remember that existing data frame is filtered for only the 
  #instances when metastasis was actually present 
df_met_radio <- df_met_radiology %>% select(DMP_ID, METASTATIC_SITE_BILLING_RDN, SOURCE) %>% distinct()

#make sure the data frame was created correctly 
head(df_met_radio)

#bring the radiology to meet the pathology and billing 
df_met_sites_ALL <- bind_rows(df_met_radio, df_pathology_met_sites, df_billing_met_sites)

#make sure a data frame with all three sources was created 
head(df_met_sites_ALL)

#create a data frame with all of the data from the radiology, pathology, and billing
df_met_sites_all <- df_met_sites_ALL %>% select (DMP_ID, METASTATIC_SITE_BILLING_RDN) %>% distinct()

#make the source for this data frame "all"
df_met_sites_all$SOURCE <- "all"

#combine the two "all" data frame - four sources: radiology, pathology, billing, and all
df_met_sites_comparisons <- bind_rows(df_met_sites_ALL, df_met_sites_all)

#create a metastatic burden column
  #do this by grouping by dmp_id and source and then summarizing the 
  #met site by the amount per patient (this is met burden)
df_met_burden<- as.data.frame(unique(df_met_sites_comparisons) %>% group_by(DMP_ID,SOURCE) %>% summarise(METASTATIC_BURDEN=n()))

#make sure met burden is added to the data frame 
head(df_met_burden)

#create a boxplot to showcase the metastatic burden of patients from the different sources 
ggplot(df_met_burden,aes(x=, y=METASTATIC_BURDEN, fill=SOURCE))+geom_boxplot()+coord_flip()




#inner join between data frame and the clinical data summary in order to add the cancer type column
df_met_sites_by_ID <- inner_join(df_met_sites_comparisons, df_clinical_sum, by = c("DMP_ID" = "Patient.ID"))

#filter the above data set for instances when metastatic site is not blank and 
  #summarize by the amount of patients with meastacis to these sites 
df_met_sites_counts <- df_met_sites_by_ID %>% filter(METASTATIC_SITE_BILLING_RDN != "") %>%
  group_by(METASTATIC_SITE_BILLING_RDN, SOURCE, Cancer.Type) %>%
  summarise(Num_Patients = n())

#make sure "Num_Patients" is a column and that the blanks are gone 
head(df_met_sites_counts)
dim(df_met_sites_counts)

#another way to get the counts 
df_met_sites_comb <- df_met_sites_counts %>% group_by(METASTATIC_SITE_BILLING_RDN, 
  SOURCE, Cancer.Type) %>% summarise(Num_Patients = n())

#make sure the data frame changed accordingly 
head (df_met_sites_comb)

#validate that the data frame has what is needed - how many patients had mets to the adrenal glandb 
adrenal_validation = df_met_sites_comb %>% filter(METASTATIC_SITE_BILLING_RDN == "ADRENAL_GLAND", SOURCE == "all")
nrow(adrenal_validation)

#using the counts data frame - create a bar graph of the amount of patients with 
  #mets to each of the organ sites - color based on source 
df_met_sites_num_patients <-ggplot(df_met_sites_counts, aes(x=METASTATIC_SITE_BILLING_RDN, y=Num_Patients, fill=SOURCE)) + 
  geom_bar(stat='identity',position = position_dodge()) +
  #scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  #facet_wrap(CANCER_TYPE ~ .)+
  coord_flip()






#run this to view the bar graph 
df_met_sites_num_patients 

#create a data frame that counts only for colorectal cancer 
df_crc_met_counts <- df_met_sites_counts %>% filter(Cancer.Type == "Colorectal Cancer")

#create a bar graph just like the one created before - but only for colorectal cancer
df_crc <-ggplot(df_crc_met_counts, aes(x=METASTATIC_SITE_BILLING_RDN, y=Num_Patients, fill=SOURCE)) + 
  geom_bar(stat='identity',position = position_dodge())+ 
  coord_flip()
  #facet_wrap(Cancer.Type)

#run this to view the bar graph 
df_crc







#using the tropisms data to create a boxplot to show the met burden of patients 
  #in tropisms vs radio predictions - facet on cancer type 

#add a source column to the tropisms data set 
tropisms_dataset$SOURCE <- "MSK_Metastatic_Tropisms_Paper"

#create a data set for the tropisms only with necessary info
tropisms_for_comparisons <- tropisms_dataset %>% select(patient_id, metastatic_site_curated, 
  SOURCE, curated_subtype_display, met_site_count) %>% distinct()

#make sure the data set has only the necessary columns 
head(tropisms_for_comparisons)

#make sure the tropisms data set does not have cases were the met site is unknown 
new_tropisms_for_comparisons <- tropisms_for_comparisons %>% filter(metastatic_site_curated != "NA")

#view changes above
head(new_tropisms_for_comparisons)

#change the names of the columns to match that of the radiology predictions 
colnames(new_tropisms_for_comparisons)[which(names(tropisms_for_comparisons) == 
  "patient_id")] <- "DMP_ID"

colnames(new_tropisms_for_comparisons)[which(names(tropisms_for_comparisons) == 
  "metastatic_site_curated")] <- "METASTATIC_SITE_BILLING_RDN"

colnames(new_tropisms_for_comparisons)[which(names(tropisms_for_comparisons) == 
  "curated_subtype_display")] <- "Cancer.Type"

#change met site count to met burden
colnames(new_tropisms_for_comparisons) [which(names(tropisms_for_comparisons)== 
  "met_site_count")] <- "METASTATIC_BURDEN"

#make sure the column names changed 
head(new_tropisms_for_comparisons)

#change the patient ID to DMP ID in the clinical sum data
colnames(df_clinical_sum)[which(names(df_clinical_sum) == 
  "Patient.ID")] <- "DMP_ID"

#make sure the column names were changed 
colnames(df_clinical_sum)

#add cancer type to the radiology predictions 
new_radio_predictions <- inner_join(df_met_radio, df_clinical_sum, by= c("DMP_ID"))

#select only dmp id cancer type, met site, and source 
radio_for_joining <- new_radio_predictions %>% select(DMP_ID, SOURCE, Cancer.Type, METASTATIC_SITE_BILLING_RDN)

#create a metastatic burden column
#do this by grouping by dmp_id and source and then summarizing the 
#met site by the amount per patient (this is met burden)

radio_with_met_burden<- as.data.frame(radio_for_joining %>% group_by(DMP_ID) %>% summarise(METASTATIC_BURDEN=n()))

#view met burden column
head(radio_with_met_burden)

#bind rows with the radio for joining and radio with met burden 
radio_join <- inner_join(radio_for_joining, radio_with_met_burden, by=c("DMP_ID"))

#bind rows - radiology and tropisms paper
Tropisms_and_radio_predictions <- bind_rows(radio_join, new_tropisms_for_comparisons)

head(Tropisms_and_radio_predictions)

#bar graph showing met burden
ggplot(Tropisms_and_radio_predictions,aes(x=, y=METASTATIC_BURDEN, fill=SOURCE))+geom_boxplot()+coord_flip() #+facet_wrap("Cancer.Type")
