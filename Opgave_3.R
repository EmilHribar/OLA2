# Opgave 3 - Logistik regression, husholdningernes forbrugsudgift og forbrugertillid

# Opgave 3.1 - Feature Engineering, dummy variable
{ #Dan en dummy variable af den kvartalsvise årlige vækst i husholdningernes 
  # forbrugsudgift for perioden 1. kvartal 1998 til 2. kvartal 2021. Hvor ofte 
  # stiger den kvartalsvise årlige vækst i husholdningernes forbrugsudgift? Hvor
  # ofte falder den kvartalsvise årlige vækst i husholdningernes forbrugsudgift?
{
  NKN3_3_1 <- Færdig_NKN3
  
  # Ny kolonne med NA værdier
  NKN3_3_1$Op_Ned <- NA
  
  # Funktion til at afgøre om der er stigning eller fald i den årlige udvikling
  Op_Ned_Funktion <- function(column){
    diff <- column - lag(column, 1) 
    ifelse(column>= 0, "op", "ned")
  }
  
  # Anvend funktionen på data
  NKN3_3_1$Op_Ned <- Op_Ned_Funktion(NKN3_3_1$Forbrugets_årlig_udvikling)
  
  # Tæl antallet af "op" (positive stigninger)
  antal_op <- sum(NKN3_3_1$Op_Ned == "op", na.rm = TRUE)
  
  # Tæl antallet af "ned" (negative fald)
  antal_ned <- sum(NKN3_3_1$Op_Ned == "ned", na.rm = TRUE)
  
  # Find det samlede antal gyldige rækker (uden NA)
  total <- sum(!is.na(NKN3_3_1$Op_Ned))
  
  # Beregn procentdelen af "op" og "ned"
  procent_op <- (antal_op / total) * 100
  procent_ned <- (antal_ned / total) * 100
  
  # Udskriv resultaterne
  cat("Antal op:", antal_op, "(", round(procent_op, 2), "% )\n")
  cat("Antal ned:", antal_ned, "(", round(procent_ned, 2), "% )\n")
  
  #Antal op: 79 ( 80,61 % )
  #Antal ned: 19 ( 19,93 % )
  
  #Barplot med procentværdier og tal på søjlerne, y-aksen går op til 100%
  barplot_heights <- barplot(
    height = c(procent_op, procent_ned),
    names.arg = c("Positiv stigning", "Negativt fald"),
    col = c("green", "red"),
    main = "Procentvise positive og negative ændringer på forbrugets årlige udvikling",
    xlab = "Ændringer",
    ylab = "Procent (%)",
    ylim = c(0, 100)  # Sæt y-aksen fra 0 til 100
  )
  
  # Tilføj procent tallet oven på hver søjle
  text(
    x = barplot_heights, 
    y = c(procent_op, procent_ned), 
    labels = paste0(round(c(procent_op, procent_ned), 2), "%"), 
    pos = 3,  # Positioner teksten over søjlerne
    cex = 1.2,  # Tekststørrelse
    col = "black"
  )
}

}

#Opgave 3.2 - Logistisk regression og forudsigelser
{ # Lav en logistik regression med dummy variable fra opgave 1.1 og de fire 
  # indikatorer i DI’s forbrugertillidsindikator. Hvilken retning forudsiger 
  # jeres model, at den årlige vækst i husholdningernes forbrugsudgift, vil gå 
  # i 3. kvartal 2024? (Hint: svaret er enten op eller ned)
  {  
 FORV1Meta=dst_meta("FORV1")
    
    my_query <- list(
      INDIKATOR="*",
      Tid="*"
    )
    
    #hent tabel
    FORV1=dst_get_data(table = "FORV1", query = my_query)
    
    #2. Data cleaning
    library(tidyr)
    
    #Lav til lang format
    FORV1_wide <- FORV1 %>%
      pivot_wider(names_from = INDIKATOR, values_from = value)
    
    #Fjern række 1 til 303 = til 2000, måned 1
    FORV1_wide <- FORV1_wide[-(1:303), ]
    
    #Nyt navn til DF
    FORV1_3.2<-FORV1_wide
    
    #Aggregering fra måneder til kvartaler
    # Konverter måneder til kvartalers startdatoer i formatet "YYYY-MM-DD"
    FORV1_3.2$TID <- as.Date(cut(as.Date(FORV1_3.2[[1]]), "quarter"))
    
    # Udvælg de relevante kolonner
    selected_columns <- FORV1_3.2[, c(3, 5, 7, 11)]
    
    # Opret en ny dataframe med gennemsnit pr. kvartal
    FORV1_3.2 <- aggregate(selected_columns, by = list(FORV1_3.2$TID), FUN = mean, na.rm = TRUE)
    
    # Omdøb kolonnen Group.1 til TID, da det nu er tidspunktet for kvartaler
    colnames(FORV1_3.2)[1] <- "TID"
    
    # Fjern række 98 og 99, da det skal matche forbrug, som kun går til 2024K2
    FORV1_3.2 <- FORV1_3.2[-c( 99), ]
    
    colnames(FORV1_3.2)
    
    Samlet3.2 <- merge(
      FORV1_3.2[, c("TID","Familiens økonomiske situation i dag, sammenlignet med for et år siden",
                    "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
                    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
                    "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.")], 
      NKN3_3_1[, c("TID", "Op_Ned")], # Udvælg kolonner fra anden dataframe
      by.x = "TID", # Kolonnen i FORV1_færdig
      by.y = "TID" # Kolonnen i Færdig_NKN3
    )
    
    # Konverter Op_Ned til binær kode
    # Her antager vi at "op" skal være 1 og "ned" skal være 0
    Samlet3.2$Op_Ned_binær <- ifelse(Samlet3.2$Op_Ned == "op", 1, 0)
    
    # Multiple logistisk regression
    logistisk_model <- glm(
      Op_Ned_binær ~ `Familiens økonomiske situation i dag, sammenlignet med for et år siden` +
        `Danmarks økonomiske situation i dag, sammenlignet med for et år siden` +
        `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` +
        `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`, 
      data = Samlet3.2, 
      family = binomial
    )
    
    # Vis opsummering af modellen
    summary(logistisk_model)
    
    # Funktion til at beregne sandsynlighed manuelt baseret på koefficienter fra modellen
    manual_logistic_prediction <- function(X1, X2, X3, X4, model) {
      
      # Træk koefficienterne fra modellen
      intercept <- coef(model)[1]
      coef_X1 <- coef(model)[2]
      coef_X2 <- coef(model)[3]
      coef_X3 <- coef(model)[4]
      coef_X4 <- coef(model)[5]
      
      # Beregn logit-værdien
      logit_p <- intercept + (coef_X1 * X1) + (coef_X2 * X2) + (coef_X3 * X3) + (coef_X4 * X4)
      
      # Konverter logit-værdien til sandsynlighed
      p <- 1 / (1 + exp(-logit_p))
      
      # Returner sandsynligheden
      return(p)
    }
    
    #Sæt værdier for X1, X2, X3, X4 "Forkert værdi"
    X1_value <- 5  # Familiens økonomiske situation
    X2_value <- 2  # Danmarks økonomiske situation
    X3_value <- 3  # Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket
    X4_value <- 4  # Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.
    
    # Beregn sandsynligheden for "op"
    sandsynlighed_op <- manual_logistic_prediction(X1_value, X2_value, X3_value, X4_value, logistisk_model)
    
    # Udskriv resultatet
    cat("Sandsynlighed for 'op':", sandsynlighed_op, "\n")
    
    # Bestem om udfaldet er "op" eller "ned"
    if (sandsynlighed_op > 0.5) {
      cat("Forudsigelse: Op\n")
    } else {
      cat("Forudsigelse: Ned\n")
    }
    
    #Sandsynlighed for 'op': 0.9910178 
    #Forudsigelse: Op
    
    
    #Test for kun signifkant spørgsmål 
    
    # Multiple logistisk regression med kun én uafhængig variabel
    logistisk_model_opdateret_1var <- glm(
      Op_Ned_binær ~ `Danmarks økonomiske situation i dag, sammenlignet med for et år siden`, 
      data = Samlet3.2, 
      family = binomial
    )
    
    # Vis opsummering af den opdaterede model
    summary(logistisk_model_opdateret_1var)
    
    # Funktion til at beregne sandsynlighed manuelt baseret på koefficienter fra modellen med én variabel
    manual_logistic_prediction_1var <- function(X2, model) {
      # Træk koefficienterne fra modellen
      intercept <- coef(model)[1]
      coef_X2 <- coef(model)[2]
      
      # Beregn logit-værdien
      logit_p <- intercept + (coef_X2 * X2)
      
      # Konverter logit-værdien til sandsynlighed
      p <- 1 / (1 + exp(-logit_p))
      
      # Returner sandsynligheden
      return(p)
    }
    
    #Sæt værdien for X2
    X2_value <- 2  # Danmarks økonomiske situation
    
    # Beregn sandsynligheden for "op" med den opdaterede model
    sandsynlighed_op_1var <- manual_logistic_prediction_1var(X2_value, logistisk_model_opdateret_1var)
    
    # Udskriv resultatet
    cat("Sandsynlighed for 'op' med opdateret model:", sandsynlighed_op_1var, "\n")
    
    # Bestem om udfaldet er "op" eller "ned" med den opdaterede model
    if (sandsynlighed_op_1var > 0.5) {
      cat("Forudsigelse: Op\n")
    } else {
      cat("Forudsigelse: Ned\n")
    }
  }}
  #Sandsynlighed for 'op' med opdateret model: 0.9281648
  #Forudsigelse: Op
  
#Opgave 3.3 – Simpel validering af modellen
{ # Hvor ofte forudsiger jeres model i opgave 3.2, at den kvartalsvise årlige 
  # realvækst i husholdningernes forbrugsugift stiger? Hvor ofte er det så reelt
  # tilfældet, at den kvartalsvise årlige realvækst i husholdningernes 
  # forbrugsudgift stiger, set i forhold til, hvad jeres model forudsiger? 
  # (hint: hvor mange tilfælde af 1 og 1 finder jeres model?)

  # Forudsig sandsynligheder for hele datasæt (DI) 
  Samlet3.2$predicted_prob_DI <- manual_logistic_prediction(
    Samlet3.2$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`,
    Samlet3.2$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`,
    Samlet3.2$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`,
    Samlet3.2$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`,
    logistisk_model
  )
  
  
  # Konverter sandsynlighederne til binære værdier for den oprindelige model
  Samlet3.2$predicted_op_ned_DI <- ifelse(Samlet3.2$predicted_prob_DI > 0.5, 1, 0)
  
  # Confusion matrix for den oprindelige model (DI)
  confusion_matrix_DI <- table(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_op_ned_DI)
  cat("Confusion Matrix for DI modellen:\n")
  
  # Tilføj labels til confusion matrixen
  rownames(confusion_matrix_DI) <- c("Faktisk: Ned (0)", "Faktisk: Op (1)")
  colnames(confusion_matrix_DI) <- c("Forudsagt: Ned (0)", "Forudsagt: Op (1)")
  
  print(confusion_matrix_DI)
  
  # Forudsig sandsynligheder manuelt for den opdaterede model (DI_signifikant)
  Samlet3.2$predicted_prob_DI_signifikant <- manual_logistic_prediction_1var(
    Samlet3.2$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`,
    logistisk_model_opdateret_1var
  )
  
  # Konverter sandsynlighederne til binære værdier for DI_signifikant modellen
  Samlet3.2$predicted_op_ned_DI_signifikant <- ifelse(Samlet3.2$predicted_prob_DI_signifikant > 0.5, 1, 0)
  
  # Confusion matrix for den opdaterede model (DI_signifikant)
  confusion_matrix_DI_signifikant <- table(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_op_ned_DI_signifikant)
  cat("Confusion Matrix for DI_signifikant modellen:\n")
  
  rownames(confusion_matrix_DI_signifikant) <- c("Faktisk: Ned (0)", "Faktisk: Op (1)")
  colnames(confusion_matrix_DI_signifikant) <- c("Forudsagt: Ned (0)", "Forudsagt: Op (1)")
  
  print(confusion_matrix_DI_signifikant)
  
  # Funktion til beregning af accuracy, precision og recall
  beregn_metrikker <- function(confusion_matrix) {
    TP <- confusion_matrix[2, 2]
    TN <- confusion_matrix[1, 1]
    #FP <- confusion_matrix[1, 2]
    #FN <- confusion_matrix[2, 1]
    
    accuracy <- (TP + TN) / sum(confusion_matrix)
    #precision <- TP / (TP + FP)
    #recall <- TP / (TP + FN)
    
    cat("Accuracy:", round(accuracy * 100, 2), "%\n")
    #cat("Precision:", round(precision * 100, 2), "%\n")
    #cat("Recall:", round(recall * 100, 2), "%\n")
  }
  
  # Beregn metrikker for DI modellen
  cat("\nMetrikker for DI modellen:\n")
  beregn_metrikker(confusion_matrix_DI)
  
  # Beregn metrikker for DI_signifikant modellen
  cat("\nMetrikker for DI_signifikant modellen:\n")
  beregn_metrikker(confusion_matrix_DI_signifikant)
  
  library(ggplot2)
  
  # Installer nødvendige pakker hvis du ikke allerede har dem
  # install.packages("ggplot2")
  
  library(ggplot2)
  
  # Lav confusion matrix for DI modellen
  confusion_matrix_DI <- table(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_op_ned_DI)
  
  # Beregn totaler for rækker og kolonner
  row_totals <- rowSums(confusion_matrix_DI)
  col_totals <- colSums(confusion_matrix_DI)
  grand_total <- sum(confusion_matrix_DI)
  
  # Omdan confusion matrix til en dataframe til plotting
  df_confusion_DI <- as.data.frame(confusion_matrix_DI)
  colnames(df_confusion_DI) <- c("Faktisk", "Forudsagt", "Antal")
  
  # Tilføj labels for Faktisk og Forudsagt kategorier
  df_confusion_DI$Faktisk <- factor(df_confusion_DI$Faktisk, levels = c(0, 1), labels = c("Faktisk: Ned", "Faktisk: Op"))
  df_confusion_DI$Forudsagt <- factor(df_confusion_DI$Forudsagt, levels = c(0, 1), labels = c("Forudsagt: Ned", "Forudsagt: Op"))
  
  # Plot confusion matrix med totals udenfor bokse
  ggplot(data = df_confusion_DI, aes(x = Faktisk, y = Forudsagt)) +
    geom_tile(fill = "lightblue", color = "black", width = 0.9, height = 0.9) +  # Celler med lys blå farve og sorte kanter
    geom_text(aes(label = Antal), size = 8, fontface = "bold") +  # Store tal i cellerne
    # Tilføj totaler ved siden af plottet
    annotate("text", x = 2.5, y = 1, label = paste("Total:", row_totals[1]), size = 6, fontface = "bold") +
    annotate("text", x = 2.5, y = 2, label = paste("Total:", row_totals[2]), size = 6, fontface = "bold") +
    annotate("text", x = 1, y = 2.5, label = paste("Total:", col_totals[1]), size = 6, fontface = "bold") +
    annotate("text", x = 2, y = 2.5, label = paste("Total:", col_totals[2]), size = 6, fontface = "bold") +
    annotate("text", x = 2.5, y = 2.5, label = paste("Grand Total:", grand_total), size = 6, fontface = "bold") +
    labs(title = "Forskellen mellem Forudsagte og Faktiske Værdier af Forbruget",  # Titel med forklaring
         x = "Faktiske Kategorier", y = "Forudsagte Kategorier") +  # Aksel labels
    theme_minimal() +  # Simpelt tema
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Stor, fed overskrift
      axis.text.x = element_text(size = 12),  # Juster størrelsen på x-akse tekst
      axis.text.y = element_text(size = 14),  # Større skrift på Y-aksen
      axis.title = element_text(size = 14, face = "bold")  # Fed skrift til akseltitler
    ) +
    coord_fixed()  # Sørg for, at cellerne forbliver kvadratiske
  
 # __________________
  #colnames(Samlet3.2)
  
  # Installer nødvendige pakker hvis du ikke allerede har dem
   install.packages("pROC")
  
  library(pROC)
  
  # Lav en ROC-kurve for den oprindelige model (DI)
  roc_DI <- roc(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_prob_DI)
  
  # Lav en ROC-kurve for den opdaterede model (DI_signifikant)
  #roc_DI_signifikant <- roc(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_prob_DI_signifikant)
  
  # Plot ROC-kurven for den oprindelige model uden akser og etiketter
  plot(roc_DI, col = "blue", main = "ROC-kurve for DI", print.auc = TRUE,
       lwd = 2, axes = FALSE, xlab = "", ylab = "")
  #plot(roc_DI, col = "blue", main = "ROC-kurve for DI og DI_signifikant modeller", 
  #print.auc = TRUE, lwd = 2, axes = FALSE, xlab = "", ylab = "")
  
  # Tilføj ROC-kurven for DI_signifikant modellen
  #plot(roc_DI_signifikant, col = "red", add = TRUE, lwd = 2)
  
  # Tilføj akser manuelt uden etiketter
  axis(1, cex.axis = 1.5)  # X-aksen (False Positive Rate)
  axis(2, cex.axis = 1.5)  # Y-aksen (True Positive Rate)
  
  # Tilføj brugerdefinerede akseetiketter med mtext()
  mtext("False Positive Rate (1 - Specificity)", side = 1, line = 3, cex = 1.5)  # X-aksen tekst
  mtext("True Positive Rate (Sensitivity)", side = 2, line = 3, cex = 1.5)      # Y-aksen tekst
  
  # Tilføj en legend til at vise farverne
  legend("bottomright", legend = c("DI modellen (blå)", "DI_signifikant modellen (rød)"), col = c("blue", "red"), lwd = 2)
  
  # Negativ side
  # Lav en ROC-kurve for den oprindelige model (DI)
  roc_DI <- roc(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_prob_DI)
  
  # Lav en ROC-kurve for den opdaterede model (DI_signifikant)
  roc_DI_signifikant <- roc(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_prob_DI_signifikant)
  
  # Plot ROC-kurven for den oprindelige model uden akser og etiketter
  plot(roc_DI, col = "blue", main = "ROC-kurve for DI og DI_signifikant modeller (Negative)", 
       print.auc = TRUE, lwd = 2, axes = FALSE, xlab = "", ylab = "")
  
  # Tilføj ROC-kurven for DI_signifikant modellen
  plot(roc_DI_signifikant, col = "red", add = TRUE, lwd = 2)
  
  # Tilføj akser manuelt uden etiketter
  axis(1, cex.axis = 1.5)  # X-aksen (False Negative Rate)
  axis(2, cex.axis = 1.5)  # Y-aksen (True Negative Rate)
  
  # Tilføj brugerdefinerede akseetiketter med mtext() for den negative side
  mtext("False Negative Rate", side = 1, line = 3, cex = 1.5)  # X-aksen tekst
  mtext("True Negative Rate", side = 2, line = 3, cex = 1.5)   # Y-aksen tekst
  
  # Tilføj en legend til at vise farverne
  legend("bottomright", legend = c("DI modellen (blå)", "DI_signifikant modellen (rød)"), col = c("blue", "red"), lwd = 2)
}}

#3.4 Potentielle forbedringer af model
{
  
  # Baseline Cutoff på 0.5
  Samlet3.2$predicted_op_ned_baseline <- ifelse(Samlet3.2$predicted_prob_DI > 0.5, 1, 0)
  
  # Cutoff på 0.3
  Samlet3.2$predicted_op_ned_cutoff_03 <- ifelse(Samlet3.2$predicted_prob_DI > 0.45, 1, 0)
  
  # Cutoff på 0.7
  Samlet3.2$predicted_op_ned_cutoff_07 <- ifelse(Samlet3.2$predicted_prob_DI > 0.55, 1, 0)
  
  # Confusion matrix for baseline (cutoff = 0.5)
  confusion_matrix_baseline <- table(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_op_ned_baseline)
  print("Confusion Matrix - Baseline (cutoff = 0.5):")
  print(confusion_matrix_baseline)
  
  # Confusion matrix for cutoff = 0.4
  confusion_matrix_cutoff_03 <- table(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_op_ned_cutoff_03)
  print("Confusion Matrix - Cutoff 0.45:")
  print(confusion_matrix_cutoff_03)
  
  # Confusion matrix for cutoff = 0.6
  confusion_matrix_cutoff_07 <- table(Samlet3.2$Op_Ned_binær, Samlet3.2$predicted_op_ned_cutoff_07)
  print("Confusion Matrix - Cutoff 0.55:")
  print(confusion_matrix_cutoff_07)
  
  # Beregn accuracy for de forskellige cutoffs
  accuracy_baseline <- sum(diag(confusion_matrix_baseline)) / sum(confusion_matrix_baseline)
  accuracy_cutoff_03 <- sum(diag(confusion_matrix_cutoff_03)) / sum(confusion_matrix_cutoff_03)
  accuracy_cutoff_07 <- sum(diag(confusion_matrix_cutoff_07)) / sum(confusion_matrix_cutoff_07)
  
  # Print accuracy for hver cutoff
  cat("Accuracy - Baseline (cutoff 0.5):", accuracy_baseline, "\n")
  cat("Accuracy - Cutoff 0.45:", accuracy_cutoff_03, "\n")
  cat("Accuracy - Cutoff 0.55:", accuracy_cutoff_07, "\n")
  
}