##-------INSTALLATION PACKAGES DE BASE-------
if (!require("devtools")) install.packages("devtools", dependencies = TRUE)
if (!require("remotes")) install.packages("remotes", dependencies = TRUE)
#install claudeR avec force = TRUE pour écraser les anciennes versions
devtools::install_github("IMNMV/ClaudeR", force = TRUE)
library(ClaudeR)
install_clauder()
claudeAddin()


# Installation des packages nécessaires
install.packages(c(
  "cardx", "dplyr", "readxl", "openxlsx", "tidyverse", "gtsummary",
  "magrittr", "ggplot2", "lubridate", "ggpubr", "survival", "scales",
  "survminer", "summarytools", "MatchIt", "optmatch", "purr",
  "officer", "flextable", "gt", "mice", "googlesheets4", "cards",
  "RItools", "epiR", "tableone", "cobalt", "broom", "forcats", "dlstats", "pkgsearch", "pROC", "stats",
  "parameters", "broom.helpers", "forestplot", "kableExtra", "rsconnect", "pacman", "stringr", "knitr", "purr", "lubridate"
))

# ----LIBRARY ----
library(cardx)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyverse)
library(gtsummary)
library(magrittr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(survival)
library(survminer)
library(scales)
library(summarytools)
library(MatchIt)
library(optmatch)
library(purrr)
library(officer)
library(flextable)
library(gt)
library(mice)
library(googlesheets4)
library(cards)
library(RItools)
library(epiR)
library(tableone)
library(cobalt)
library(broom)
library(forcats)
library(dlstats)
library(pkgsearch)
library(pROC)
library(stats)
library(parameters)
library(broom.helpers)
library(forestplot)
library(kableExtra)
library(rsconnect)
library(pacman)
library(stringr)


##-------Import de la base----

gs4_auth(email = "thomas.husson56@gmail.com") 
df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/19Pt_q2Jds_2TLeVGs5D2fmSyEu8-LEltYQL-4LPT89w/edit?gid=1806996301#gid=1806996301",
  sheet = "v2"
)


##-------colonnes à inclure 1----
cols_to_include1 <- c(
  "sex_male",
  "BMI",
  "PS_anterieur",
  "motif_admission",
  "delai_admission_hospit_chirurgie",
  "delai_admission_rea_chirurgie",
  "duree_sejour_rea",
  "duree_sejour_postop",
  "diagnostic",
  "CCI6_metastatic_solid_tumor",
  "CCI6_SIDA",
  "Charlson_Comorbidity_total"
)

table1 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include1),
    missing = "no",
    type = list(
      delai_admission_hospit_chirurgie ~ "continuous",
      delai_admission_rea_chirurgie ~ "continuous",
      duree_sejour_postop ~ "continuous", # <-- forcée en continue,
      Charlson_Comorbidity_total ~ "continuous"
    ),
  )

table1

##-------colonnes à inclure 2-----
cols_to_include2 <- c(
  "sepsis_preop_YN", "IRA_YN", "DRA_YN", "choc_YN", "deficit_neurologique_YN",
  "defaillance_hepatique_YN", "CIVD_YN", "sofa_respi", "sofa_coag", "sofa_cardiovasc",
  "sofa_SNC", "sofa_hepatique", "sofa_renal", "sofa_total", "q_sofa",
  "ventilation_mecanique_YN", "vm_duree_nb_jours",
  "lactate_mmol_L", "vasopresseur_YN", "vaso_duree_nb_jours",
  "EER_YN", "EER_nb_jours",
  "GCSF_YN", "GCSF_duree_nb_jours",
  "aplasie_YN", "aplasie_duree_nb_jours",
  "delai_aplasie_J1CT_nb_jours", "Hyperleucocytose_YN", "leucocytes_G_L",
  "Hb_preop_g_dL", "Hb_preopInf10_5_g_dL_YN",
  "AlbuminLevel_admission_g_L", "Albumin_sub35", "CRP_preop", "plaquettes_G_L",
  "endoscopie_YN", "endoscopie", "aspect_endoscopique"
)

table2 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include2),
    percent = "column",
    missing = "ifany",
    type = list(
      vm_duree_nb_jours ~ "continuous",
      lactate_mmol_L ~ "continuous",
      vaso_duree_nb_jours ~ "continuous", # <-- forcée en continue,
      EER_nb_jours ~ "continuous",
      GCSF_duree_nb_jours ~ "continuous",
      leucocytes_G_L ~ "continuous",
      AlbuminLevel_admission_g_L ~ "continuous",
      CRP_preop ~ "continuous"))

table2

##-------colonnes à inclure 3-----
cols_to_include3 <- c(
  "delai_debut_symptomes_chirurgie",
  "etat_general", "diarrhee_YN", "selles_nocturnes", "imodium",
  "defense_generalisee_YN", "douleur_localisee_cote", "vomissements_YN",
  "hemorragie_YN", "transfusion_YN", "ttt_hemorragie",
  "occlusion_YN", "SNG_YN", "fievre_YN", "clostridium_YN"
)

table3 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include3),
  )

table3





##-------colonnes à inclure 3-----
cols_to_include4 <- c(
  "TDM_topographie_colite",
  "colectasie_caecum_>100mm_YN",
  "colectasie_transverse>60mm_YN",
  "epaississement_parietal_TDM_YN",
  "hemorragie_digestive_grave_YN",
  "distension_grele_YN",
  "pneumatose_parietale_YN",
  "perforation_YN",
  "pneumoperitoine_YN",
  "infiltration_graisse_YN",
  "collection_abdo_YN",
  "epanchement_YN"
)

table4 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include4),
  )

table4

##-------colonnes à inclure 5-----
cols_to_include5 <- c(
  "delai_chirurgie_diagnostic_jours",
  "laparoscopie_YN",
  "laparotomie_YN",
  "laparo_blanche_YN",
  "lavage_drainage_YN",
  "RIC_YN",
  "colectomie_subtotale_YN",
  "colectomie_droite_YN",
  "colectomie_gauche_YN",
  "intervention_hartmann_YN",
  "resection grelique",
  "moignon_rectal_YN",
  "ileostomie_YN",
  "colostomie_YN",
  "anastomose_YN",
  "drainage_YN",
  "geste_associe",
  "constatation_chirurgie"
)

table5 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include5),
    type = list(
      delai_chirurgie_diagnostic_jours ~ "continuous"
    )
  )

table5

##-------colonnes à inclure 6-----
cols_to_include6 <- c(
  "plaie_digestive_perop_YN",
  "plaie_vasculaire_perop_YN",
  "other_intraop_complication",
  "other_intraop_complication_text",
  "hemorragie_perop_YN"
)

table6 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include6)
  )

table6

##-------colonnes à inclure 7-----
cols_to_include7 <- c(
  "vasopresseur_per_op_YN",
  "vasopresseur_concentration_mg_h",
  "hb_per_op",
  "transfusion_perop_YN",
  "nb_CGR",
  "transfusion_PFC_YN",
  "PFC_nb",
  "transfusion_plaquettes_YN",
  "pq_nb",
  "anapath",
  "nutrition_parentérale",
  "nutrition_entérale"
)

table7 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include7),
    missing = "ifany",
    type=list(
      vasopresseur_concentration_mg_h ~ "continuous",
      hb_per_op ~ "continuous",
      nb_CGR ~ "continuous",
      PFC_nb ~ "continuous"
    )
  )

table7

##-------colonnes à inclure 8-----
cols_to_include8 <- c(
  "ATB_YN",
  "imidazole",
  "ureidopenicilline",
  "carbapeneme",
  "C4G",
  "aminoside",
  "C3G",
  "glycopeptide",
  "vancomycine",
  "ATB_adapte_vanco",
  "ATB_adapte_amin",
  "ATF_proba",
  "ambisome",
  "echinocandine",
  "ATF_adapte",
  "doc_microbio",
  "CMV_YN",
  "bact1",
  "bact2",
  "bact3",
  "bact_site",
  "champi",
  "champi_whole",
  "champi_site",
  "ag_aspergillaire_YN"
)

library(gtsummary)

table8 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include8)
  )

table8

##-------colonnes à inclure 9-----
cols_to_include9 <- c(
  "complications_post_op",
  "sepsis_postop_YN",
  "anastomotic_leakage",
  "intraabdominal_abcess_or_collection",
  "Bleeding_YN",
  "intra_abdominal_hematoma",
  "wound_complication",
  "radiological_drainage_for_complication",
  "peritonite",
  "reoperation_for_complication",
  "details_reoperation_for_complication",
  "stoma_for_complication",
  "ileus",
  "stoma_related_complication",
  "anastomotic_bleeding",
  "anemia_transfusion",
  "infection_urinaire",
  "insuffisance_renale",
  "acute_urinary_retention",
  "pneumopathie",
  "MTEV",
  "catheter_infection",
  "dehydratation_IV_fluids",
  "poor_control_of_pain",
  "other_sepsis",
  "ClavienDindo"
)

table9 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include9)
  )

table9


##-------colonnes à inclure 10-----
cols_to_include10 <- c(
    "Medical_complications",
    "all_septic",
    "readmission_within_30d",
    "retablissement_YN",
    "delai_chirurgie_retablissement",
    "remission_maladie_6mois_YN",
    "remission_maladie_1an_YN",
    "rechute_maladie_6mois_YN",
    "rechute_maladie_1an_YN",
    "maladie_refractaire_1an_YN",
    "last_news"
  )

table10 <- df %>% 
  tbl_summary(
    include = all_of(cols_to_include10),
    type = list(
      delai_chirurgie_retablissement ~ "continuous"
    )
  )

table10




peux tu regarder Rstudio c'est pour un travail sur 12 patients opérés d'une colite neutropénique  toutes les infos sont dans df  je veux que tu regardes caractéristiques préop lignes de traitement médicales caractéristiques chirurgicales résultats postop   il faut faire tourner les stats dans tous les sens pour essayer des associations / trucs sifgnificatifs quelqu'ils soient


et surtout si tu me donnes un script : il faut qu'il parte de df
et soit "standalone" (si tu crées des vecteurs, donne les trucs que tu as utilisé pour les créer)
