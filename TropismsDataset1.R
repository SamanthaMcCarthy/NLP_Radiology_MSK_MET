#install packages
install.packages("readxl")
install.packages("Rcpp")
install.packages("gplots")
install.packages("RVAideMemoire")
install.packages("spearmanCI")
install.packages("DescTools")
install.packages("ggrepel")

#load libraries
library(dplyr)
library(reshape2)
library(readxl)
library(Rcpp)
library(tibble)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(gplots)
library(RVAideMemoire)
library(spearmanCI)
library(DescTools)

#load files

#data set from paper MSK MET
dataset <- "C:/Users/Sam/Downloads/tableS2_data.xlsx"
tropisms_dataset <- read_excel(dataset, skip=2)

#data set of radiology predictions - binary (1 or 0 metastasis to each site)
radiology_predictions_file <- "C:/Users/Sam/Downloads/metsite_predictions_age.csv"
radiology_predictions <- read.csv(radiology_predictions_file)

#data set msk met paper - clinical and MSK Impact
fname <- "C:/Users/Sam/Desktop/Samantha/HOPP_SummerProgram_MSK/ShahLabSpecific/metatrop_met_site_annotations_clinical_and_impact - metatrop_met_site_annotations_clinical_and_impact.csv.csv"
df_meta_trop <- read.csv(fname)

#data set of tumor type, genomic alteration, and statistical information
stat_file <- "C:/Users/Sam/Downloads/media-5.xlsx" 
stat_info <- read_excel(stat_file, skip=2) 

#Get all the unique DMP_IDs from the MSK-MET paper data set
unique_DMP_IDS <- unique(tropisms_dataset$patient_id)

#Filter radiology predictions data set for the DMP_IDs in the above
filtered_radiology_predictions <- filter(radiology_predictions, DMP_ID %in% unique_DMP_IDS)

#Replace old organ sites with the ones in the MSK-MET data set

    #find the number of unique DMP IDs
number_unique_DMP_IDS <- length(unique_DMP_IDS)

    #reorganize radiology data set --> change the names of the columns
df_radio_predictions <- filtered_radiology_predictions %>% select(DMP_ID, ADRENAL_GLAND = adrenal,
  BONE = bone, CNS_BRAIN = cnsbrain,LIVER = liver, LUNG = lung, LYMPH = lymphoid,
  MEDIASTINUM = mediastinum, PERITONEUM = peritoneum, OTHER = softtissue)

    #reorganize radiology data set --> 
      #create a column with metastatic site and another with yes or no 
filtered_radiology_predictions_melt <- melt(data = df_radio_predictions, id.vars = c("DMP_ID"), 
  variable.name = "METASTATIC_SITE_BILLING_RDN", value.name = "YES/NO_METASTASIS")
    
    #filter for only cases with yes/no is yes (cases with metastasis)
filtered_radio_melt <- filtered_radiology_predictions_melt %>% filter(`YES/NO_METASTASIS` == 1)

        #view above changes
        head(filtered_radio_melt)

  #add a cancer types column
        
    #create data frame with ID and cancer type
cancer_types_column <- select(tropisms_dataset, patient_id, curated_subtype_display)
    
      #view above changes
      head(cancer_types_column)

    #change column name from patient id to DMP_ID
      #in order to merge with the radiology in the future
colnames(cancer_types_column)[which(names(cancer_types_column) == "patient_id")] <- "DMP_ID"

    #create a total number of patients per cancer type column
cancer_type_num_patients <- cancer_types_column %>% group_by(curated_subtype_display) %>% 
  summarise(Number_of_Patients_per_Cancer_Type=n())
    
    #create a data frame with radiology ID and metastatic site
radio_predictions_met_and_dmp_id <- filtered_radio_melt %>% select(DMP_ID, METASTATIC_SITE_BILLING_RDN)

    #merge the radiology df (id and met site) with MSK-MEt df(id and cancer type) 
predictions_cancer_type_merge <- merge(radio_predictions_met_and_dmp_id, 
cancer_types_column, by= "DMP_ID")
          
          #view above changes
          head(predictions_cancer_type_merge)

    #add column with number of patients for each met site and cancer type combination 
num_of_patients_col <- predictions_cancer_type_merge %>% unique() %>%
  group_by(METASTATIC_SITE_BILLING_RDN, curated_subtype_display) %>% 
  summarise(Number_of_Patients = n())

          #view above changes
          head(num_of_patients_col)

    #create a total-amount-of-patients-per-cancer-type column
predictions_cancer_type_total_merge <- merge(predictions_cancer_type_merge, 
  cancer_type_num_patients, by="curated_subtype_display")

          #view above changes
          head(predictions_cancer_type_total_merge)
          
          #view previous merge
          head(predictions_cancer_type_merge)

radio_predictions_percents <- merge(predictions_cancer_type_total_merge, 
        num_of_patients_col, by="curated_subtype_display")


percents_by_cancer_type <- select (radio_predictions_percents, curated_subtype_display, DMP_ID, 
        METASTATIC_SITE_BILLING_RND.x, Number_of_Patients_per_Cancer_Type, Number_of_Patients)

percents_by_cancer_type$Percent_of_Patients = 
  percents_by_cancer_type$Number_of_Patients / 
  percents_by_cancer_type$Total_Number_of_Patients_In_The_Cancer_Type*100

merge_percents <- merge(num_of_patients_col, cancer_type_num_patients, 
        by="curated_subtype_display")
head(merge_percents)


merge_percents$Percent_of_Patients = merge_percents$Number_of_Patients / 
  merge_percents$Number_of_Patients_per_Cancer_Type*100
head(merge_percents)

#metastatic burden and FGA comparisons
df_mets_to_each_site <- group_by(radiology_predictions, DMP_ID) %>% summarise_all(sum)
df_mets_to_each_site <- tibble::column_to_rownames(df_mets_to_each_site, "DMP_ID")  
df_mets_to_each_site[df_mets_to_each_site > 0] <- 1
#df_mets_to_each_site <- df_mets_to_each_site %>% select(-age_at_report)

df_mets_to_each_site$Met_Burden_by_Patient = rowSums(df_mets_to_each_site)

df_mets_to_each_site <- tibble::rownames_to_column(df_mets_to_each_site, "DMP_ID")

#add met burden and 6+ met burden
df_tropisms_with_met_burden <- inner_join(tropisms_dataset, df_mets_to_each_site, by=c("patient_id" = "DMP_ID"))
df_tropisms_with_met_burden$Categorized_Met_Burden <- ifelse(df_tropisms_with_met_burden$Met_Burden_by_Patient >= 6, 
      "6+", df_tropisms_with_met_burden$Met_Burden_by_Patient)

#boxplot

#convert the categorized met burden to a factor
df_tropisms_with_met_burden$Categorized_Met_Burden <- 
  as.factor(df_tropisms_with_met_burden$Categorized_Met_Burden)

#find the unique values in the column 
#met_burden_pairs = combn(levels(df_tropisms_with_met_burden$Categorized_Met_Burden),2)

met_burden_pairs = combn(levels(df_tropisms_with_met_burden$Categorized_Met_Burden),2, simplify=F)

#boxplot (with the p values)
boxplot_met_burden_fga <- ggplot(df_tropisms_with_met_burden, 
  aes(x=Categorized_Met_Burden, y=fga))  + geom_boxplot()
  #geom_jitter()+ geom_boxplot(outlier.shape = NA)

#+ 
  #stat_compare_means(comparisons = met_burden_pairs, aes(group = Categorized_Met_Burden))

#spearman calculation for fga 
cor.test(df_tropisms_with_met_burden$Met_Burden_by_Patient, df_tropisms_with_met_burden$fga, 
         method = "spearman", exact=FALSE)

#create a heatmap to showcase the percentages 
merge_percents <- merge_percents %>% select(-Number_of_Patients, -Number_of_Patients_per_Cancer_Type)

merge_percents_pivot <- pivot_wider(merge_percents, names_from = METASTATIC_SITE_BILLING_RDN, 
  values_from = Percent_of_Patients)

rownames(merge_percents_pivot) <- seq_len( nrow(merge_percents_pivot) )
head(merge_percents_pivot)

merge_percents_pivot <- column_to_rownames(merge_percents_pivot, "curated_subtype_display")
list_of_order <- c("Head and Neck Squamous",
   "Adenoid Cystic Carcinoma",
   "Thyroid Papillary",
   "Thyroid Poorly Differentiated",
   "Small Cell Lung Cancer",
   "Lung Adenocarcinoma",
   "Lung Squamous Cell Carcinoma",
   "Lung Neuroendocrine",
   "Pleural Mesothelioma" , 
   "Breast Ductal HR+HER2-",
   "Breast Ductal HR+HER2+",
   "Breast Ductal HR-HER2+",
   "Breast Ductal Triple Negative",
   "Breast Lobular HR+",
   "Esophageal Adenocarinoma", 
   "Stomach Adenocarinoma",
   "Small Bowel cancer",
   "Colorectal Hypermutated",
   "Colorectal MSS",
   "Gastrointestinal Neuroendocrine",
   "Gastrointestinal Stromal",
   "Appendiceal Adenocarcinoma",
   "Anal Squamous Cell",
   "Hepatocellular Carcinoma",
   "Gallbladder Cancer",
   "Cholangio Extrahepatic",
   "Cholangio Intrahepatic",
   "Pancreatic Adenocarcinoma",
   "Pancreatic Neuroendocrine",
   "Renal Clear Cell",
   "Upper Tract Urothelial",
   "Bladder Urothelial",
   "Prostate Adenocarcinoma",
   "Testicular Non-Seminoma", 
   "Testicular Seminoma",
   "Ovarian High-Grade Serous",
   "Ovarian Low-Grade Serous",
   "Ovarian Clear Cell",
   "Uterine Carcinosarcoma",
   "Uterine Endometrioid",
   "Uterine Hypermutated",
   "Uterine Serous",
   "Cervical Squamous Cell",
   "Melanoma Cutaneous",
   "Melanoma Mucosal",
   "Melanoma Uveal",
   "Cutaneous Squamous Cell",
   "Sarcoma Leiomyo",
   "Sarcoma Lipo",
   "Sarcoma UPS/MXF")

merge_percents_pivot <- merge_percents_pivot[list_of_order,]

percents_matrix <- as.matrix(merge_percents_pivot)


lwid=c(0.2,5)
lhei=c(0.2,5)

heatmap.2(percents_matrix, dendrogram='none', Rowv=FALSE, Colv=FALSE, lwid=lwid, lhei=lhei)

           #Rowv=FALSE, Colv=FALSE get rid of color/density clustering 

#jpeg(file="heatmap.jpg")
my_palette <- colorRampPalette(c("yellow", "purple"))
heatmap.2(percents_matrix, dendrogram='none', Rowv=FALSE, Colv=FALSE, margins=c(10,10), trace="none", col=my_palette, cellnote=round(percents_matrix, 0),notecol="black")
#dev.off()


#create a boxplot of TMB and Met Burden
boxplot_met_burden_tmb <- ggplot(df_tropisms_with_met_burden, 
    aes(x=Categorized_Met_Burden, y=tmb))  + geom_boxplot()+ cor.test(df_tropisms_with_met_burden$Met_Burden_by_Patient, df_tropisms_with_met_burden$tmb, 
                                                                      method = "spearman", exact=FALSE)
#geom_jitter()+ geom_boxplot(outlier.shape = NA)

#spearman correlation

result <- cor.test(df_tropisms_with_met_burden$Met_Burden_by_Patient, df_tropisms_with_met_burden$tmb, 
  method = "spearman", exact=FALSE)

#create a dataframe with cancer type and the spearman value
cancer_type_and_spearman <- df_tropisms_with_met_burden %>% 
  select(curated_subtype_display, Met_Burden_by_Patient, fga, tmb)

cancer_type_and_spearman$Spearman_Met_Burden_and_FGA = cor.test(cancer_type_and_spearman$Met_Burden_by_Patient, df_tropisms_with_met_burden$fga, 
    method = "spearman", exact=FALSE)

#create a dataframe with cancer type and the spearman value (fga)
#r <- by(cancer_type_and_spearman, cancer_type_and_spearman$curated_subtype_display, 
        #FUN = function(X) cor(X$Met_Burden_by_Patient, X$fga, method = "spearman"))

#create a data frame with cancer type and the spearman value (fga)
Coefficients_by_cancer_type_fga <- cancer_type_and_spearman %>% group_by(curated_subtype_display) %>% 
  summarise("Coefficient" = cor(Met_Burden_by_Patient, fga, method = "spearman"))

#P_Value_by_cancer_type_fga <- cancer_type_and_spearman %>% 
  #group_by(cor.test(df_tropisms_with_met_burden$Met_Burden_by_Patient, df_tropisms_with_met_burden$fga, method = "spearman", exact=FALSE)$p.value)

P_Values_by_cancer_type_fga <- cancer_type_and_spearman %>% 
  group_by(curated_subtype_display) %>% summarise("P_Value" = cor.test(Met_Burden_by_Patient, fga, method = "spearman", exact=FALSE)$p.value)

    #creating an upper and lower bound into
#lower bound
confidence_int_fga <- spearman.ci(cancer_type_and_spearman$Met_Burden_by_Patient, cancer_type_and_spearman$fga)
confidence_int_fga$conf.int
interval_fga <- confidence_int_fga$conf.int
fga_lower <- interval_fga[1]

Lower_Bound_by_cancer_type_fga <- cancer_type_and_spearman %>% 
  group_by(curated_subtype_display) %>% summarise("Confidence_Interval_Lower_Bound" = 
  spearman.ci(cancer_type_and_spearman$Met_Burden_by_Patient, cancer_type_and_spearman$fga)$conf.int[1])

Stats_by_cancer_type_fga$Confidence_Interval_Lower_Bound = fga_lower

#upper bound
fga_upper <- interval_fga[2]

Stats_by_cancer_type_fga$Confidence_Interval_Upper_Bound = fga_upper

Upper_Bound_by_cancer_type_fga <- cancer_type_and_spearman %>% 
  group_by(curated_subtype_display) %>% summarise("Confidence_Interval_Upper_Bound" = 
  spearman.ci(cancer_type_and_spearman$Met_Burden_by_Patient, cancer_type_and_spearman$fga)$conf.int[2])

#full fga data set
fga_coefs_and_p_values <- merge(Coefficients_by_cancer_type_fga, P_Values_by_cancer_type_fga, by="curated_subtype_display")

fga_coefs_p_values_lower <- merge(fga_coefs_and_p_values, Lower_Bound_by_cancer_type_fga, by="curated_subtype_display")

fga_stat_info <- merge(fga_coefs_p_values_lower, Upper_Bound_by_cancer_type_fga, by="curated_subtype_display")

fga_stat_info$Q_Value_FDR <- p.adjust(fga_stat_info$P_Value, method = "fdr")
fga_stat_info$P_Value_Significant <- fga_stat_info$P_Value < .05
fga_stat_info$Q_Value_Significant <- fga_stat_info$Q_Value_FDR < .05

#create a data frame with cancer type and the spearman value (tmb)
Coefficients_by_cancer_type_tmb <- cancer_type_and_spearman %>% group_by(curated_subtype_display) %>% summarise("Coefficient" = cor(Met_Burden_by_Patient, tmb, method = "spearman"))

P_Values_by_cancer_type_tmb <- cancer_type_and_spearman %>% 
  group_by(curated_subtype_display) %>% summarise("P_Value" = cor.test(Met_Burden_by_Patient, tmb, method = "spearman", exact=FALSE)$p.value)

#creating an upper and lower bound into - tmb
#lower bound
confidence_int_tmb <- spearman.ci(cancer_type_and_spearman$Met_Burden_by_Patient, cancer_type_and_spearman$tmb)
confidence_int_tmb$conf.int
interval_tmb <- confidence_int_tmb$conf.int
tmb_lower <- interval_tmb[1]

Lower_Bound_by_cancer_type_tmb <- cancer_type_and_spearman %>% 
  group_by(curated_subtype_display) %>% summarise("Confidence_Interval_Lower_Bound" = 
  spearman.ci(cancer_type_and_spearman$Met_Burden_by_Patient, cancer_type_and_spearman$tmb)$conf.int[1])

#upper bound - tmb
tmb_upper <- interval_tmb[2]

Upper_Bound_by_cancer_type_tmb <- cancer_type_and_spearman %>% 
  group_by(curated_subtype_display) %>% summarise("Confidence_Interval_Upper_Bound" = 
  spearman.ci(cancer_type_and_spearman$Met_Burden_by_Patient, cancer_type_and_spearman$tmb)$conf.int[2])

#merging - tmb
tmb_coefs_and_p_values <- merge(Coefficients_by_cancer_type_tmb, P_Values_by_cancer_type_tmb, by="curated_subtype_display")

tmb_coefs_p_values_lower <- merge(tmb_coefs_and_p_values, Lower_Bound_by_cancer_type_tmb, by="curated_subtype_display")

tmb_stat_info <- merge(tmb_coefs_p_values_lower, Upper_Bound_by_cancer_type_tmb, by="curated_subtype_display")

tmb_stat_info$Q_Value_FDR <- p.adjust(tmb_stat_info$P_Value, method = "fdr")
tmb_stat_info$P_Value_Significant <- tmb_stat_info$P_Value < .05
tmb_stat_info$Q_Value_Significant <- tmb_stat_info$Q_Value_FDR < .05

spearman.ci(df_tropisms_with_met_burden$Met_Burden_by_Patient, 
        df_tropisms_with_met_burden$fga)$conf.int

#visualizing this information
#x is cancer type 

ggplot(fga_stat_info, aes(x=curated_subtype_display, y=Coefficient)) + 
  geom_pointrange(aes(ymin=Confidence_Interval_Lower_Bound, ymax=Confidence_Interval_Upper_Bound))+
  coord_flip()

SpearmanRho(x=fga_stat_info$curated_subtype_display, y=fga_stat_info$Coefficient, conf.level = NA)

ggplot(tmb_stat_info, aes(x=curated_subtype_display, y=Coefficient)) + 
  geom_pointrange(aes(ymin=Confidence_Interval_Lower_Bound, ymax=Confidence_Interval_Upper_Bound))+
  coord_flip()

ggplot(fga_stat_info, aes())


#scatter plots of q values 

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

df_radio <- df_met_radiology %>% select(DMP_ID, METASTATIC_SITE_BILLING_RDN, SOURCE)

head(df_radio)

colnames(tropisms_dataset)[which(names(tropisms_dataset) == "patient_id")] <- "DMP_ID"

df_radio_trop_merge <- inner_join(tropisms_dataset, df_radio, by=c("DMP_ID"))

df_radio <- df_radio_trop_merge %>% select(DMP_ID, METASTATIC_SITE_BILLING_RDN, met_site_count, fga, tmb, curated_subtype_display)

df_radio$SOURCE <- "Radiology_Predictions" 

head(df_radio)

#copied from code above

cancer_type_and_spearman <- df_tropisms_with_met_burden %>% 
  select(curated_subtype_display, Met_Burden_by_Patient, fga, tmb)

Coefficients_by_cancer_type_fga <- cancer_type_and_spearman %>% group_by(curated_subtype_display) %>% 
  summarise("Coefficient" = cor(Met_Burden_by_Pacor(Met_Burden_by_Patient, fga, method = "spearman")))

P_Values_by_cancer_type_fga <- cancer_type_and_spearman %>% 
  group_by(curated_subtype_display) %>% summarise("P_Value" = cor.test(Met_Burden_by_Patient, fga, method = "spearman", exact=FALSE)$p.value)

Lower_Bound_by_cancer_type_fga <- cancer_type_and_spearman %>% 
  group_by(curated_subtype_display) %>% summarise("Confidence_Interval_Lower_Bound" = 
  spearman.ci(cancer_type_and_spearman$Met_Burden_by_Patient, cancer_type_and_spearman$fga)$conf.int[1])

Upper_Bound_by_cancer_type_fga <- cancer_type_and_spearman %>% 
  group_by(curated_subtype_display) %>% summarise("Confidence_Interval_Upper_Bound" = 
  spearman.ci(cancer_type_and_spearman$Met_Burden_by_Patient, cancer_type_and_spearman$fga)$conf.int[2])

fga_coefs_and_p_values <- merge(Coefficients_by_cancer_type_fga, P_Values_by_cancer_type_fga, by="curated_subtype_display")

fga_coefs_p_values_lower <- merge(fga_coefs_and_p_values, Lower_Bound_by_cancer_type_fga, by="curated_subtype_display")

fga_stat_info <- merge(fga_coefs_p_values_lower, Upper_Bound_by_cancer_type_fga, by="curated_subtype_display")

fga_stat_info$Q_Value_FDR <- p.adjust(fga_stat_info$P_Value, method = "fdr")
fga_stat_info$P_Value_Significant <- fga_stat_info$P_Value < .05
fga_stat_info$Q_Value_Significant <- fga_stat_info$Q_Value_FDR < .05


#data frame for tropisms data 

#fga

trop_data <- tropisms_dataset %>% select(curated_subtype_display, curated_subtype, curated_subtype_abbr) %>% unique()

#colnames(stat_info)[which(names(stat_info) == "tumor")]

trop_with_stats <- merge(trop_data, stat_info, by.x = "curated_subtype", by.y = "tumor_type", all.y = TRUE)

trop_with_stats_fga <- trop_with_stats %>% filter(alteration == "fga", !is.na(curated_subtype_display)) %>% select(curated_subtype_display, curated_subtype_abbr, 
  Coefficent_Tropisms = cor.estimate, P_Value_Tropisms = cor.pval, Q_Value_Tropisms = cor.qval)

radio_and_trop_fga_stat <- merge(fga_stat_info, trop_with_stats_fga, by="curated_subtype_display")

colnames(radio_and_trop_fga_stat)[which(names(radio_and_trop_fga_stat) == "Coefficient")] <- "Coefficient_Radiology"
colnames(radio_and_trop_fga_stat)[which(names(radio_and_trop_fga_stat) == "Q_Value_FDR")] <- "Q_Value_FDR_Radiology"
colnames(radio_and_trop_fga_stat)[which(names(radio_and_trop_fga_stat) == "Coefficent_Tropisms")] <- "Coefficient_Tropisms"

ggplot(radio_and_trop_fga_stat, aes(x = Q_Value_FDR_Radiology, y = Q_Value_Tropisms)) + geom_point()

ggplot(radio_and_trop_fga_stat, aes (x=Coefficient_Radiology, y = Coefficient_Tropisms)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = curated_subtype_abbr))

#tmb

trop_data <- tropisms_dataset %>% select(curated_subtype_display, curated_subtype, curated_subtype_abbr) %>% unique()

trop_with_stats_tmb <- trop_with_stats %>% filter(alteration == "tmb", !is.na(curated_subtype_display)) %>% select(curated_subtype_display, curated_subtype_abbr, 
  Coefficent_Tropisms = cor.estimate, P_Value_Tropisms = cor.pval, Q_Value_Tropisms = cor.qval)

radio_and_trop_tmb_stat <- merge(tmb_stat_info, trop_with_stats_tmb, by="curated_subtype_display")

colnames(radio_and_trop_tmb_stat)[which(names(radio_and_trop_tmb_stat) == "Coefficient")] <- "Coefficient_Radiology"
colnames(radio_and_trop_tmb_stat)[which(names(radio_and_trop_tmb_stat) == "Q_Value_FDR")] <- "Q_Value_FDR_Radiology"
colnames(radio_and_trop_tmb_stat)[which(names(radio_and_trop_tmb_stat) == "Coefficent_Tropisms")] <- "Coefficient_Tropisms"

ggplot(radio_and_trop_tmb_stat, aes(x = Q_Value_FDR_Radiology, y = Q_Value_Tropisms)) + geom_point()

ggplot(radio_and_trop_tmb_stat, aes (x=Coefficient_Radiology, y = Coefficient_Tropisms)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = curated_subtype_abbr))



#boxplot with categorized and fga/tmb (for each cancer type and for radio vs. tropisms)
#metastatic burden and FGA comparisons

df_mets_to_each_site <- group_by(radiology_predictions, DMP_ID) %>% summarise_all(sum)
df_mets_to_each_site <- tibble::column_to_rownames(df_mets_to_each_site, "DMP_ID")  
df_mets_to_each_site[df_mets_to_each_site > 0] <- 1
#df_mets_to_each_site <- df_mets_to_each_site %>% select(-age_at_report)

df_mets_to_each_site$Met_Burden_by_Patient_Radiology = rowSums(df_mets_to_each_site)

df_mets_to_each_site <- tibble::rownames_to_column(df_mets_to_each_site, "DMP_ID")

#add met burden and 6+ met burden

radio_mets <- df_mets_to_each_site %>% select(DMP_ID, Met_Burden_by_Patient_Radiology)

radio_mets$Categorized_Met_Burden_Radiology <- ifelse(radio_mets$Met_Burden_by_Patient_Radiology >= 6, 
  "6+", radio_mets$Met_Burden_by_Patient_Radiology)

#radio_mets$SOURCE <- "Radiology_Predictions"

#fixing the tropisms data frame 
trop_met_burden <- tropisms_dataset %>% select(DMP_ID = patient_id, curated_subtype_display, 
  Met_Burden_by_Patient_Tropisms = met_site_count, fga, tmb)

trop_met_burden$Categorized_Met_Burden_Tropisms <- ifelse(trop_met_burden$Met_Burden_by_Patient_Tropisms >= 6, 
  "6+", trop_met_burden$Met_Burden_by_Patient_Tropisms)

#trop_met_burden$SOURCE <- "MSK_Tropisms_Paper"

#merge the two - way # 1 
trop_radio_met_merge <- merge(trop_met_burden, radio_mets, by = "DMP_ID") 
  
  trop_radio_final <- melt(data = trop_radio_met_merge, id.vars = c("curated_subtype_display", 
  "tmb", "fga", "DMP_ID", "Categorized_Met_Burden_Tropisms", "Categorized_Met_Burden_Radiology")) 
  
  colnames(trop_radio_final)[which(names(trop_radio_final) == "variable")] <- "SOURCE"
  colnames(trop_radio_final)[which(names(trop_radio_final) == "value")] <- "METASTATIC_BURDEN"
  
  
  #variable.name = "SOURCE", value.name = Met_Burden_by_Patient))) 
  
  #::melt(trop_radio_met_merge, id.vars=c("DMP_ID", "FGA"), measure.vars=c("Categorized_Met_Burden_Tropisms", "Categorized_Met_Burden_Radiology"))

  
#merge the two - way #2 
  trop_radio_met_merge <- merge(trop_met_burden, radio_mets, by = "DMP_ID") 
  
  trop_radio_final <- melt(data = trop_radio_met_merge, id.vars = c("curated_subtype_display", 
     "tmb", "fga", "DMP_ID", "Met_Burden_by_Patient_Tropisms", "Met_Burden_by_Patient_Radiology")) 
  
  colnames(trop_radio_final)[which(names(trop_radio_final) == "variable")] <- "SOURCE"
  colnames(trop_radio_final)[which(names(trop_radio_final) == "value")] <- "CATEGORIZED_METASTATIC_BURDEN"
  
  trop_radio_final<- trop_radio_final %>% filter(trop_radio_final$CATEGORIZED_METASTATIC_BURDEN >0)
  
  #variable.name = "SOURCE", value.name = Met_Burden_by_Patient)

  #boxplot categorized met burden with fga and fill on source 
  cat_met_burden_fga <- ggplot(trop_radio_final, 
    aes(x=CATEGORIZED_METASTATIC_BURDEN, y=fga, fill=SOURCE))+
    geom_boxplot()+ 
    #scale_fill_manual(SOURCE,values=c("MSK Tropisms Paper","Radiology Predictions"))
  #+ facet_wrap(curated_subtype_display)+
  labs(x="Metastatic Burden", y="FGA", title= "FGA Across Metastatic Burden")+
  #geom_jitter()+ geom_boxplot(outlier.shape = NA)
    theme(strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          #plot.margin = margin(10, 10, 10, 10),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black",))
  
  #boxplot categorized met burden with tmb and fill on source 
  cat_met_burden_tmb <- ggplot(trop_radio_final, 
     aes(x=CATEGORIZED_METASTATIC_BURDEN, y=tmb, fill=SOURCE))+ 
    geom_boxplot() + scale_y_continuous(trans='log10')+ annotation_logticks()+
    labs(x="Metastatic Burden", y="TMB", title= "TMB Across Metastatic Burden")+
    theme(strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          #plot.margin = margin(10, 10, 10, 10),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black",))

    #scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),labels = scales::trans_format("log10", scales::math_format(10^.x)))
    
    geom_boxplot() + scale_y_continuous(trans='log10')
  #geom_jitter()+ geom_boxplot(outlier.shape = NA)
    
    
    theme(strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.margin = margin(10, 10, 10, 10),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black",)
    )