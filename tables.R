models <- gofdt$m


fits <- with(
  gofdt, paste0("X2","(",df,")","= ", round(X2,2),", ",
  "p < .001","; ",
  "CFI = ",round(CFI,3),"; ",
  "TLI = ",round(TLI,3),"; ",
  "RMSEA = ",round(RMSEA,3),"; ",
  "SRMR = ", round(SRMR,3), "."
     ))

table <- data.frame(
  "Model" = models,
  "Model Fit" = fits
)

comp1_vector <- comp1$comp
deltas1 <- with(comp1,paste0("ΔCFI = ",round(CFI_D,3),"; ",
                  "ΔRMSEA = ",round(RMSEA_D),"."))

comp1_tab <- data.frame(
  "Model Comparison" = comp1_vector,
  "Model Invariance Testing" = deltas1
)


comp2_vector <- comp2$comp
deltas2 <- with(comp2,paste0("ΔCFI = ",round(CFI_D,3),"; ",
                             "ΔRMSEA = ",round(RMSEA_D),"."))

comp2_tab <- data.frame(
  "Model Comparison" = comp2_vector,
  "Model Invariance Testing" = deltas2
)

comp_tab <- full_join(
  comp1_tab,
  comp2_tab
)

writexl::write_xlsx(table, "output/tables/fit.xlsx")
writexl::write_xlsx(comp_tab, "output/tables/comp_tab.xlsx")
