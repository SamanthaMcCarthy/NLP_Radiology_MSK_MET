#install packages
install.packages("tidyr")

#load library
library(ggplot2)
library(dplyr)
library(tidyr)

#load data frames/ bring files into R studio
fname <- "C:/Users/Sam/Desktop/Samantha/HOPP_SummerProgram_MSK/ShahLabSpecific/metatrop_met_site_annotations_clinical_and_impact - metatrop_met_site_annotations_clinical_and_impact.csv.csv"
df_meta_trop <- read.csv(fname)
new_file <- "C:/Users/Sam/Downloads/age_at_first_mets (2).csv"
df_new_radiology <- read.csv(new_file)
file <- "C:/Users/Sam/Downloads/mskimpact_clinical_data (6).tsv"
df_clinical_sum <- read.csv(file, sep="\t")
file <- "C:/Users/Sam/Downloads/yaeger_validation_072721.csv"
df_yaeger <- read.csv(file)

#find DMP_IDs in metatrop that only overlap with radiology data
df_overlapping_DMPs <- intersect(df_meta_trop$DMP_ID, df_new_radiology$DMP_ID)

#separate df_meta_trop into pathology and billing
    #this time, select for age as well
df_path_with_age <- df_meta_trop %>% select(SOURCE, METASTATIC_SITE_BILLING_RDN, DMP_ID, AGE_AT_DX_DAYS) %>% 
  filter(DMP_ID %in% df_overlapping_DMPs) %>% distinct()
df_billing_with_age <- df_meta_trop %>% select(SOURCE, METASTATIC_SITE_BILLING_RDN, DMP_ID, AGE_AT_DX_DAYS) %>% 
  filter(DMP_ID %in% df_overlapping_DMPs) %>% distinct()

#Adding the column "Source" to the radiology
df_new_radiology$SOURCE <- "Radiology"

#Changing the column names in the radiology to match the ones in path and billing
colnames(df_new_radiology)[which(names(df_new_radiology) == "met_site")] <- "METASTATIC_SITE_BILLING_RDN"
colnames(df_new_radiology)[which(names(df_new_radiology) == "age_at_report")] <- "AGE_AT_DX_DAYS"

#Creating a data frame with an all category
df_ALL_with_age <- bind_rows(df_new_radiology, df_path_with_age, df_billing_with_age) %>%
  group_by(DMP_ID, METASTATIC_SITE_BILLING_RDN) %>% 
  summarise(min_age = min(AGE_AT_DX_DAYS)) %>% unique()

#Change the source to all
df_ALL_with_age$SOURCE <- "All"

#Creating a data frame with updated path, billing, and radio
df_all_with_age_at_dx <- bind_rows(df_new_radiology, df_path_with_age, df_billing_with_age) %>%
group_by(DMP_ID, METASTATIC_SITE_BILLING_RDN, SOURCE) %>% 
  summarise(min_age = min(AGE_AT_DX_DAYS)) %>% unique()

#Combine those two data frames so there will be path, billing, radio, and all
df_age_comparisons <- bind_rows(df_all_with_age_at_dx, df_ALL_with_age) %>% distinct()
 
#create a list of met sites in the radiology
df_only_radio_mets_list <- df_new_radiology$METASTATIC_SITE_BILLING_RDN %>% unique()

#add the cancer type from clinical sum data
df_met_sites_by_ID <- inner_join(df_age_comparisons, select(df_clinical_sum, Patient.ID, Cancer.Type), 
                                 by = c("DMP_ID" = "Patient.ID")) %>% unique()

#number of patients per cancer type
num_patients_cancer_type <- df_met_sites_by_ID %>% select(DMP_ID, Cancer.Type) %>%
  unique() %>% 
  group_by(Cancer.Type) %>% 
  summarise(Num_Patients = n()) 

top_10_cancer_types <- top_n(num_patients_cancer_type, 10, Num_Patients)$Cancer.Type

#create a df with only radio metastases
df_only_radio_mets_list <- df_new_radiology$METASTATIC_SITE_BILLING_RDN

df_radio_mets <- df_met_sites_by_ID %>% 
  filter(METASTATIC_SITE_BILLING_RDN %in% df_only_radio_mets_list)

#filter the df for cancer types with a fewer number of patients
#df_met_sites_by_id <- inner_join(df_age_comparisons, df_clinical_sum, 
                                 #by = c("DMP_ID" = "Patient.ID")) %>% group_by(Cancer.Type) %>% summarise(Num_Patients = n() > 100)

#df_more_than_100 <- df_met_sites_by_id %>% filter(Num_Patients == "TRUE")

#more_than_100_list <- df_more_than_100$Cancer.Type

#create a df - cancer types w/ more than 100 patients
df_100_patients <- df_met_sites_by_ID %>% filter(Cancer.Type %in% more_than_100_list)

#Create a density plot of age_at_dx
ggplot(df_age_comparisons, aes(x=min_age, fill=SOURCE)) +
  geom_density(alpha=0.5, position = "identity")+ 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) 

#Create a histogram of age_at_dx 
ggplot(df_age_comparisons, aes(x=min_age, fill=SOURCE)) + 
  geom_histogram(stat="count", alpha=0.5, position = "identity")+ 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) + facet_grid("SOURCE")

#Create a density plot with a facet on organ site/met site
df_met_all <- df_met_sites_by_ID %>% select(SOURCE, METASTATIC_SITE_BILLING_RDN,DMP_ID, Cancer.Type, AGE)

ggplot(df_100_patients, aes(x=min_age, fill=SOURCE)) +
  geom_density(alpha=0.5, position = "identity")+ 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(Cancer.Type~.)
  #theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) 

#add the cancer type
df_met_sites_by_ID <- inner_join(df_age_comparisons, select(df_clinical_sum, Patient.ID, Cancer.Type), 
  by = c("DMP_ID" = "Patient.ID")) %>% unique()

#create a df with only radio mets
df_radio_mets <- df_met_sites_by_ID %>% 
  filter(METASTATIC_SITE_BILLING_RDN %in% df_only_radio_mets_list)

#filter for only colorectal cancer
df_met_sites_CRC <- df_met_sites_by_ID %>% 
  filter(Cancer.Type == "Colorectal Cancer")

#filter for only crc - only radio mets
df_CRC_met_sites <- df_radio_mets %>%
  filter(Cancer.Type == "Colorectal Cancer")

#Create a density plot with a facet on metastatic site for only colorectal cancer type
ggplot(df_CRC_met_sites, aes(x=min_age, fill=SOURCE)) +
  geom_density(alpha=0.5, position = "identity")+ 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ylim(0,0.00015) +
  facet_wrap("METASTATIC_SITE_BILLING_RDN")
#theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) 

head(df_age_comparisons)

#radio and billing comparisons lists - re-figuring the data
df_met_sites_pivot <- pivot_wider(df_met_sites_by_ID, 
                names_from = SOURCE, values_from = min_age)

df_met_sites_pivot_top_ten <- df_met_sites_pivot %>% filter(Cancer.Type %in% top_10_cancer_types)

#all patients with met to each organ site 
total_num_met <- df_met_sites_by_ID %>% select(DMP_ID, METASTATIC_SITE_BILLING_RDN) %>%
  unique() %>% 
group_by(METASTATIC_SITE_BILLING_RDN) %>% 
  summarise(Num_Patients = n())


df_adrenal_radiology_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "ADRENAL_GLAND", (!is.na(Radiology) & (is.na(`ICD Billing`))))
dim(df_adrenal_radiology_first)

#adrenal data frames
df_adrenal_radiology_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "ADRENAL_GLAND", 
         (Radiology < `ICD Billing`) | (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

num_patients_adrenal_radio_first <- df_met_sites_by_ID %>% filter(DMP_ID %in% df_adrenal_radiology_first$DMP_ID) %>%
  select(DMP_ID, METASTATIC_SITE_BILLING_RDN) %>% unique() %>% 
  group_by(METASTATIC_SITE_BILLING_RDN) %>% 
  summarise(Num_Patients = n())

#scatterplot - comparing radiology and billing ages
radiology_age_vs_billing_age <- ggplot(df_met_sites_pivot, aes(Radiology, `ICD Billing`)) +
  geom_point()

#scatterplot - comparing radiology and billing ages - facet on cancer type
radiology_age_vs_billing_age_with_facet <- ggplot(df_met_sites_pivot_top_ten, aes(Radiology, `ICD Billing`)) +
  geom_point() + facet_wrap(Cancer.Type~.)

#difference in age column to the pivot
age_differences <- df_met_sites_pivot$Radiology - df_met_sites_pivot$`ICD Billing`

df_met_sites_pivot$Age_Difference = age_differences

#boxplot - difference in ages (radio/billing)

boxplot_pivot <- ggplot(df_met_sites_pivot, aes(x= factor(0), y=Age_Difference)) + 
  geom_boxplot(color ="red", outlier.shape = NA) + geom_jitter()

list_unique_mets_radio <- unique(c("LUNG", "ADRENAL_GLAND", "LYMPH", "LIVER", "MEDIASTINUM", "PERITONEUM", "CNS_BRAIN", "BONE", "OTHER"))


mets_pivot <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN %in% list_unique_mets_radio)

ggplot(mets_pivot, x=Age_Difference, y=METASTATIC_SITE_BILLING_RDN)+

boxplot_mets <- ggplot(mets_pivot, aes(x= METASTATIC_SITE_BILLING_RDN, y=Age_Difference)) + 
geom_boxplot(color ="red", outlier.shape = NA) + geom_jitter()
  
boxplot_pivot_mets <- ggplot(df_met_sites_pivot, aes(x= METASTATIC_SITE_BILLING_RDN, y=Age_Difference)) + 
  geom_boxplot(color ="red", outlier.shape = NA) + geom_jitter()


#median age difference
mean(filter(df_met_sites_pivot, !is.na(Age_Difference))$Age_Difference)

#boxplot - difference in ages (radio/billing) with Cancer Type
boxplot_pivot_with_cancer_type <- ggplot(df_met_sites_pivot_top_ten, aes(x= Cancer.Type, y=Age_Difference)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter()+theme(axis.text.x = element_text(angle=45, hjust=1))
 #+coord_flip()

 #boxplot - met site

overlapping_met_sites <- filter(df_met_sites_pivot, METASTATIC_SITE_BILLING_RDN %in% df_only_radio_mets_list)

boxplot_pivot_with_met_site <- ggplot(overlapping_met_sites, aes(x= METASTATIC_SITE_BILLING_RDN, y=Age_Difference)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter() #+coord_flip()



#filter for the positive outliers 
(df_met_sites_pivot_top_ten %>% filter(Age_Difference > 4000))$DMP_ID

#filter for the negative outliers
(df_met_sites_pivot_top_ten %>% filter(Age_Difference < -1500))$DMP_ID

#adrenal gland 
df_adrenal_radiology_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "ADRENAL_GLAND", 
         (Radiology < `ICD Billing`) | (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

df_adrenal_billing_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "ADRENAL_GLAND", 
         (`ICD Billing` < Radiology) | (!is.na(`ICD Billing`) & (is.na(Radiology))))

#boxplot and scatter plot for all of adrenal
df_adrenal <- df_met_sites_pivot %>%  filter(METASTATIC_SITE_BILLING_RDN == "ADRENAL_GLAND")

boxplot_adrenal <- ggplot(df_adrenal, aes(x= factor(0), 
 y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_adrenal <- ggplot(df_adrenal, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#adrenal radio no billing
df_adrenal_radiology_no_billing <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "ADRENAL_GLAND", (!is.na(Radiology) & (is.na(`ICD Billing`)))) 
dim(df_adrenal_radiology_no_billing)

#adrenal billing no radio 
df_adrenal_billing_no_radiology <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "ADRENAL_GLAND", (is.na(Radiology) & (!is.na(`ICD Billing`)))) 

dim(df_adrenal_billing_no_radiology)

#boxplots and scatters plot for adrenal radio first vs billing first

boxplot_adrenal_radio_first <- ggplot(df_adrenal_radiology_first, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_adrenal_radio_first <- ggplot(df_adrenal_radiology_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

boxplot_adrenal_billing_first <- ggplot(df_adrenal_billing_first, aes(x= factor(0), 
   y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_adrenal_billing_first <- ggplot(df_adrenal_billing_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#bone
df_bone_radiology_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "BONE", 
         (Radiology < `ICD Billing`) | (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

df_bone_billing_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "BONE", 
         (`ICD Billing` < Radiology) | (!is.na(`ICD Billing`) & (is.na(Radiology))))

#boxplot and scatter plot for all of bone
df_bone <- df_met_sites_pivot %>%  filter(METASTATIC_SITE_BILLING_RDN == "BONE")

boxplot_bone <- ggplot(df_bone, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_bone <- ggplot(df_bone, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#bone radio no billing
df_bone_radiology_no_billing <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "BONE", (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

dim(df_bone_radiology_no_billing)

#bone billing no radio 
df_bone_billing_no_radiology <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "BONE", (is.na(Radiology) & (!is.na(`ICD Billing`)))) 

dim(df_bone_billing_no_radiology)

#boxplots and scatter plots for bone

boxplot_bone_radio_first <- ggplot(df_bone_radiology_first, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_bone_radio_first <- ggplot(df_bone_radiology_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

boxplot_bone_billing_first <- ggplot(df_bone_billing_first, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_bone_billing_first <- ggplot(df_bone_billing_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 


#CNS_brain
df_cns_radiology_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "CNS_BRAIN", 
         (Radiology < `ICD Billing`) | (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

df_cns_billing_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "CNS_BRAIN", 
         (`ICD Billing` < Radiology) | (!is.na(`ICD Billing`) & (is.na(Radiology))))

#boxplot and scatter plot for all of CNS brain
df_cns_brain <- df_met_sites_pivot %>%  filter(METASTATIC_SITE_BILLING_RDN == "CNS_BRAIN")

boxplot_cns_brain <- ggplot(df_cns_brain, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_cns_brain <- ggplot(df_cns_brain, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#cns_brain radio no billing
df_cns_brain_radiology_no_billing <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
 "CNS_BRAIN", (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

dim(df_cns_brain_radiology_no_billing)

#cns_brain billing no radio 
df_cns_brain_billing_no_radiology <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "CNS_BRAIN", (is.na(Radiology) & (!is.na(`ICD Billing`)))) 

dim(df_cns_brain_billing_no_radiology)

#boxplots and scatter plots for cns_brain
boxplot_cns_radio_first <- ggplot(df_cns_radiology_first, aes(x= factor(0), 
   y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_cns_radio_first <- ggplot(df_cns_radiology_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

boxplot_cns_billing_first <- ggplot(df_cns_billing_first, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_cns_billing_first <- ggplot(df_cns_billing_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#liver
df_liver_radiology_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "LIVER", 
         (Radiology < `ICD Billing`) | (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

df_liver_billing_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "LIVER", 
         (`ICD Billing` < Radiology) | (!is.na(`ICD Billing`) & (is.na(Radiology))))

#boxplot and scatter plot for all of liver
df_liver <- df_met_sites_pivot %>%  filter(METASTATIC_SITE_BILLING_RDN == "LIVER")

boxplot_liver <- ggplot(df_liver, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_liver <- ggplot(df_liver, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#liver radio no billing
df_liver_radiology_no_billing <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "LIVER", (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

dim(df_liver_radiology_no_billing)

#liver billing no radio 
df_liver_billing_no_radiology <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "LIVER", (is.na(Radiology) & (!is.na(`ICD Billing`)))) 

dim(df_liver_billing_no_radiology)

#boxplots and scatterplots for liver
boxplot_liver_radio_first <- ggplot(df_liver_radiology_first, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_liver_radio_first <- ggplot(df_liver_radiology_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

boxplot_liver_billing_first <- ggplot(df_liver_billing_first, aes(x= factor(0), 
 y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_liver_billing_first <- ggplot(df_liver_billing_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#lung
df_lung_radiology_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "LUNG", 
         (Radiology < `ICD Billing`) | (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

df_lung_billing_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "LUNG", 
         (`ICD Billing` < Radiology) | (!is.na(`ICD Billing`) & (is.na(Radiology))))

#boxplot and scatter plot for all of lung
df_lung <- df_met_sites_pivot %>%  filter(METASTATIC_SITE_BILLING_RDN == "LUNG")

boxplot_lung <- ggplot(df_lung, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_lung <- ggplot(df_lung, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#lung radio no billing
df_lung_radiology_no_billing <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "LUNG", (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

dim(df_lung_radiology_no_billing)

#lung billing no radio 
df_lung_billing_no_radiology <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
 "LUNG", (is.na(Radiology) & (!is.na(`ICD Billing`)))) 

dim(df_lung_billing_no_radiology)

#boxplots and scatterplots for lung
boxplot_lung_radio_first <- ggplot(df_lung_radiology_first, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_lung_radio_first <- ggplot(df_lung_radiology_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

boxplot_lung_billing_first <- ggplot(df_lung_billing_first, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_lung_billing_first <- ggplot(df_lung_billing_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#lymph
df_lymph_radiology_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "LYMPH", 
         (Radiology < `ICD Billing`) | (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

df_lymph_billing_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "LYMPH", 
         (`ICD Billing` < Radiology) | (!is.na(`ICD Billing`) & (is.na(Radiology))))

#boxplot and scatter plot for all of lymph
df_lymph <- df_met_sites_pivot %>%  filter(METASTATIC_SITE_BILLING_RDN == "LYMPH")

boxplot_lymph <- ggplot(df_lymph, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_lymph <- ggplot(df_lymph, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#lymph radio no billing
df_lymph_radiology_no_billing <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "LYMPH", (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

dim(df_lymph_radiology_no_billing)

#lymph billing no radio 
df_lymph_billing_no_radiology <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "LYMPH", (is.na(Radiology) & (!is.na(`ICD Billing`)))) 

dim(df_lymph_billing_no_radiology)

#box plots and scatterplots for lymph 
boxplot_lymph_radio_first <- ggplot(df_lymph_radiology_first, aes(x= factor(0), 
 y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_lymph_radio_first <- ggplot(df_lymph_radiology_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

boxplot_lymph_billing_first <- ggplot(df_lymph_billing_first, aes(x= factor(0), 
                                                                y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_lymph_billing_first <- ggplot(df_lymph_billing_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#mediastinum
df_mediastinum_radiology_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "MEDIASTINUM", 
         (Radiology < `ICD Billing`) | (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

df_mediastinum_billing_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "MEDIASTINUM", 
         (`ICD Billing` < Radiology) | (!is.na(`ICD Billing`) & (is.na(Radiology))))

#boxplot and scatter plot for all of mediastinum
df_mediastinum <- df_met_sites_pivot %>%  filter(METASTATIC_SITE_BILLING_RDN == "MEDIASTINUM")

boxplot_mediastinum <- ggplot(df_mediastinum, aes(x= factor(0), 
 y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_mediastinum <- ggplot(df_mediastinum, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#mediastinum radio no billing
df_mediastinum_radiology_no_billing <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "MEDIASTINUM", (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

dim(df_mediastinum_radiology_no_billing)

#mediastinum billing no radio 
df_mediastinum_billing_no_radiology <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
  "MEDIASTINUM", (is.na(Radiology) & (!is.na(`ICD Billing`)))) 

dim(df_mediastinum_billing_no_radiology)

#boxplots and scatterplots for mediastinum 
boxplot_mediastinum_radio_first <- ggplot(df_mediastinum_radiology_first, aes(x= factor(0), 
 y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_mediastinum_radio_first <- ggplot(df_mediastinum_radiology_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

boxplot_mediastinum_billing_first <- ggplot(df_mediastinum_billing_first, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_mediastinum_billing_first <- ggplot(df_mediastinum_billing_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#peritoneum 
df_peritoneum_radiology_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "PERITONEUM", 
         (Radiology < `ICD Billing`) | (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

df_peritoneum_billing_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "PERITONEUM", 
         (`ICD Billing` < Radiology) | (!is.na(`ICD Billing`) & (is.na(Radiology))))

#boxplot and scatter plot for all of peritoneum
df_peritoneum <- df_met_sites_pivot %>%  filter(METASTATIC_SITE_BILLING_RDN == "PERITONEUM")

boxplot_peritoneum <- ggplot(df_peritoneum, aes(x= factor(0), 
 y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_peritoneum <- ggplot(df_peritoneum, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#peritoneum radio no billing
df_peritoneum_radiology_no_billing <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
 "PERITONEUM", (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

dim(df_peritoneum_radiology_no_billing)

#peritoneum billing no radio 
df_peritoneum_billing_no_radiology <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
 "PERITONEUM", (is.na(Radiology) & (!is.na(`ICD Billing`)))) 

dim(df_peritoneum_billing_no_radiology)

#boxplots and scatterplots for peritoneum 
boxplot_peritoneum_radio_first <- ggplot(df_peritoneum_radiology_first, aes(x= factor(0), 
 y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_peritoneum_radio_first <- ggplot(df_peritoneum_radiology_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

boxplot_peritoneum_billing_first <- ggplot(df_peritoneum_billing_first, aes(x= factor(0), 
y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_peritoneum_billing_first <- ggplot(df_peritoneum_billing_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#other 
df_other_radiology_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "OTHER", 
         (Radiology < `ICD Billing`) | (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

df_other_billing_first <- df_met_sites_pivot %>% 
  filter(METASTATIC_SITE_BILLING_RDN == "OTHER", 
         (`ICD Billing` < Radiology) | (!is.na(`ICD Billing`) & (is.na(Radiology))))

#boxplot and scatter plot for all of other
df_other <- df_met_sites_pivot %>%  filter(METASTATIC_SITE_BILLING_RDN == "OTHER")

boxplot_other <- ggplot(df_other, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_other <- ggplot(df_other, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#other radio no billing
df_other_radiology_no_billing <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
 "OTHER", (!is.na(Radiology) & (is.na(`ICD Billing`)))) 

dim(df_other_radiology_no_billing)

#other billing no radio 
df_other_billing_no_radiology <- df_met_sites_pivot %>% filter(METASTATIC_SITE_BILLING_RDN == 
 "OTHER", (is.na(Radiology) & (!is.na(`ICD Billing`)))) 

dim(df_other_billing_no_radiology)

#boxplots and scatterplots for other 
boxplot_other_radio_first <- ggplot(df_other_radiology_first, aes(x= factor(0), 
 y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_other_radio_first <- ggplot(df_other_radiology_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

boxplot_other_billing_first <- ggplot(df_other_billing_first, aes(x= factor(0), 
  y=Age_Difference)) + geom_boxplot(outlier.shape = NA) + geom_jitter()

scatterplot_other_billing_first <- ggplot(df_other_billing_first, aes(Radiology, `ICD Billing`)) +
  geom_point() 

#create a data frame for each of the lists 
dataframe_adrenal_radio_first <- df_met_sites_by_ID %>% 
  filter(df_adrenal_radiology_first) %>% group_by(Cancer.Type) %>% 
  summarise(Num_Patients = n())




#WITH NEW RADIOLOGY DATASET (FIRST MET INFO)
radio_with_first_site_of_met <- read.csv(file)
df_radiology <- read.csv(new_file)

radio_with_first_met_and_cancer_type <- inner_join(radio_with_first_site_of_met, select(df_clinical_sum, Patient.ID, Cancer.Type), 
  by = c("DMP_ID" = "Patient.ID")) %>% unique()

colnames(radio_with_first_met_and_cancer_type)[which(names(radio_with_first_met_and_cancer_type) 
  == "First.site.of.mets")] <- "met_site"

filter(radio_with_first_met_and_cancer_type, met_site %in% df_radiology)

radio_first_met_with_billing <- inner_join(radio_with_first_met_and_cancer_type, select(df_billing_with_age, DMP_ID) %>% unique())

filter(radio_with_first_site_of_met)

#boxplot - difference in ages (radio/billing) with Cancer Type 
boxplot_pivot_new_radiology_with_cancer_type <- ggplot(df_met_sites_pivot_top_ten, aes(x= Cancer.Type, y=Age_Difference)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter() #+coord_flip()
