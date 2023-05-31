

# 0. Identification ----------------------------------
#Title: Code for tables of research paper on Social Dominance Orientation and Trust in Unions
#Description: The article is part of the Mini Coes "Subjective Inequality and Trade Union Support in Latin America". by the post doctoral researcher Juan Diego García Castro and Pablo Perez Ahumada.
#Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
#Responsable: Martín Venegas - Research Assistant

# Executive Summary: This script contains the code to create some of the tables that are contained in the article.
# Fecha: May 30, 2023


# 1. Load packages -----------------------------------------------------------

if (!require("pacman"))
  install.packages("pacman")  #if pacman es missing, install

pacman::p_load(tidyverse,
               sjlabelled,
               lavaan)


# 2. Load data ------------------------------------------------------------

load("input/other/executed_models.RData") # Load enviroment with models ran


# 3. Make tables ----------------------------------------------------------

# Save vector with model names
models <- gofdt$m

# Make fits paragraph
fits <- with(
  gofdt,
  paste0(
    "X2",
    "(",
    df,
    ")",
    "= ",
    round(X2, 2),
    ", ",
    "p < .001",
    "; ",
    "CFI = ",
    round(CFI, 2),
    "; ",
    "TLI = ",
    round(TLI, 2),
    "; ",
    "RMSEA = ",
    round(RMSEA, 2),
    "; ",
    "SRMR = ",
    round(SRMR, 2),
    "."
  )
)

# 3.1 Make table with fits ------------------------------------------------
table <- data.frame("Model" = models,
                    "Model Fit" = fits)

# Make vector with comparissons between models
comp1_vector <- comp1$comp

# Make comparisson paragraph
deltas1 <- with(comp1, paste0("ΔCFI = ", round(CFI_D, 2), "; ",
                              "ΔRMSEA = ", round(RMSEA_D, 2), "."))

# 3.2 Make table with comparisson 1 ---------------------------------------

comp1_tab <- data.frame("Model Comparison" = comp1_vector,
                        "Model Invariance Testing" = deltas1)

# Make vector with comparisson between models
comp2_vector <- comp2$comp

# Make comparisson paragraph
deltas2 <- with(comp2, paste0("ΔCFI = ", round(CFI_D, 2), "; ",
                              "ΔRMSEA = ", round(RMSEA_D, 2), "."))

# 3.3 Make table with comparisson 2 ---------------------------------------

comp2_tab <- data.frame("Model Comparison" = comp2_vector,
                        "Model Invariance Testing" = deltas2)

# 3.4 Make table with both comparissons -----------------------------------

comp_tab <- full_join(comp1_tab,
                      comp2_tab)


# 4. Save tables ----------------------------------------------------------
writexl::write_xlsx(table, "output/tables/fit.xlsx")
writexl::write_xlsx(comp_tab, "output/tables/comp_tab.xlsx")
