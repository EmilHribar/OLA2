# Opgave 4 - Forbrug og FTI fra DST og DI, samt loops i lister
# Opgave 4.1 - Illustration af FTI
{ # Hent data fra forbrugertillidsundersøgelsen fra januar 1996 til i dag og 
  # omregns jeres data til kvartaler. Lav en grafisk illustration af jeres om-
  # regnede data for DST's FTI og kommentér på, hvornår de danske forbrugere er 
  # mest og mindst optimistiske.

{ # Hent data fra DST
 library(dkstat)

# Vi laver nu en df over alle tabeller i DST, som vi kan søge i
alltables <- dst_get_tables(lang = "da")

# Vi ved fra tidliger opgaver, at vores data hedder FORV1.

# Vi finder "FORV1", som vi skal bruge og henter den ind som liste
FORV1_Meta <- dst_meta("FORV1")

# 
my_query <- list(
  INDIKATOR="*",
  Tid="*"
)

# Vi henter tabellen som df.
FORV1=dst_get_data(table = "FORV1", query = my_query)

library(tidyr)

#Lav til lang format
Opgave_4.1 <- FORV1 %>%
  pivot_wider(names_from = INDIKATOR, values_from = value)

# Vi retter vores df til
Opgave_4.1 <- Opgave_4.1[256:nrow(Opgave_4.1),1:2 ]

# Konverter datoer til kvartaler i kolonne 1 (tid)
Opgave_4.1$Kvartal <- paste0(format(as.Date(Opgave_4.1[[1]]), "%Y"), "K", 
        ceiling(as.numeric(format(as.Date(Opgave_4.1[[1]]), "%m")) / 3))

# Opret en ny dataframe med gennemsnit pr. kvartal
Opgave_4.1 <- aggregate(cbind(Forbrugertillidsindikatoren) ~ Kvartal,
                                data = Opgave_4.1, FUN = mean, na.rm = TRUE)

# Vi retter, så der kun er 1 decimal
Opgave_4.1$Forbrugertillidsindikatoren <- round(Opgave_4.1$Forbrugertillidsindikatoren,1)

# Til slut laver vi vores plot via ggplot
# Indlæs nødvendige pakker
library(ggplot2)
library(zoo)  # Til at arbejde med kvartalsdata

# Konverter Kvartal til år-kvartal format med as.yearqtr()
#Opgave_4.1$Kvartal <- as.yearqtr(Opgave_4.1$Kvartal, format = "%Y Q%q")

# Brugerdefinerede labels til at vise årstal kun ved Q1
# custom_labels <- function(x) {
  format(x, "%Y")  # Vis kun årstal
}
{
# Lav et linjeplot med brugerdefinerede labels
  #install.packages("scales")
  library(scales)
  #install.packages("stringr")
  library(stringr)
  library(ggplot2)
  ggplot(Opgave_4.1, aes(x = Kvartal, y = Forbrugertillidsindikatoren, group = 1)) +
    geom_line(color = "#0072B2", size = 1.2) +  # Blå linje med tykkere linjestørrelse
    geom_point(size = 3, color = "#D55E00") +  # Tilføj orange punkter for hver kvartal
    labs(title = "Danskerne er mest optimistiske i 06 og 15 og mindst i 08 og 22",
         subtitle = "Gennemsnit per kvartal",  # Tilføj en underoverskrift
         x = "Kvartal",
         y = "Forbrugertillidsindikatoren (index)") +
    scale_x_discrete(breaks = Opgave_4.1$Kvartal[seq(1, length(Opgave_4.1$Kvartal), by = 4)],  
                     # Vis kun hvert 4. kvartal for bedre overskuelighed
                     labels = function(x) str_wrap(x, width = 10)) +  # Del lange labels i to linjer, hvis nødvendigt
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Gør titlen centreret og større
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),  # Tilføj en pæn underoverskrift
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Drej x-aksen labels og gør dem større
      axis.text.y = element_text(size = 12),  # Gør y-aksen labels større
      axis.title.x = element_text(size = 14, face = "bold"),  # Gør x-aksen titel større og fed
      axis.title.y = element_text(size = 14, face = "bold"),  # Gør y-aksen titel større og fed
      panel.grid.major = element_line(color = "gray80", size = 0.5)  # Lysere gridlines for renere udseende
    ) 
  }}

# Opgave 4.2 - Gennemsnit af underspørgsmål
{ #Beregn gennemsnittet for underspørgsmålet ”Set i lyset af den økonomiske 
  # situation, mener du, at det for øjeblikket er fordelagtigt at anskaffe 
  # større forbrugsgoder som fjernsyn, vaskemaskine eller lignende, eller er det
  # bedre at vente?” for perioden 1. kvartal 2000 til og med 3. kvartal 2024.
  #Vurdér jeres resultat set i forhold til spørgsmålet og svarmulighederne. 
  #(Hint: giver resultatet analytisk mening?)
  
  # Indlæsning af FORV1
  {library(dkstat)

  FORV1Meta=dst_meta("FORV1")
  
  my_query <- list(
    INDIKATOR="*",
    Tid="*")
  
  #hent tabel
  FORV1=dst_get_data(table = "FORV1", query = my_query)

  #2. Data cleaning
  library(tidyr)
  
  #Lav til lang format
  FORV1_wide <- FORV1 %>%
    pivot_wider(names_from = INDIKATOR, values_from = value)
  
  #Fjern række 1 til 303 = til 2000, måned 1
  FORV1_wide <- FORV1_wide[-(1:303), ]
  }
  
# Vi laver et nyt df med spm. 7 i kvartaler
Opgave_4.2 <- FORV1_wide[,c(1,2,7)]

# Vi laver vores df til kvartaler
  # Konverter datoer til kvartaler i kolonne 1 (tid)
  Opgave_4.2$Kvartal <- paste0(format(as.Date(Opgave_4.2[[1]]), "%Y"), "K", 
                               ceiling(as.numeric(format(as.Date(Opgave_4.2[[1]]), "%m")) / 3))
  
  # Opret en ny dataframe med gennemsnit pr. kvartal
  Opgave_4.2 <- aggregate(cbind(Opgave_4.2$Forbrugertillidsindikatoren,
              Opgave_4.2$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` ) ~ Kvartal,
              data = Opgave_4.2, FUN = mean, na.rm = TRUE)
  
  # Vi navngiver kolonnerne korrekt
colnames(Opgave_4.2)[2] <- "Forbrugertillidsindikatoren"
colnames(Opgave_4.2)[3] <- "`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`"
  
  # Vi retter, så der kun er 1 decimal
  Opgave_4.2$Forbrugertillidsindikatoren <- round(Opgave_4.2$Forbrugertillidsindikatoren,1)
  Opgave_4.2$`\`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket\``
  #<- round( Opgave_4.2$`\`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket\``,1)
  
  # Vi summerer vores spg. 7 i kvartaler.
  mean(Opgave_4.2$`\`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket\``)
  # Gns for spg. 7 er -10,4. Dette betyder at danskerne generelt er negativt stemt
  # for at anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket.
  
}

# Opgave 4.3 - De 11 grupper af forbrug
{ # Hent data for de 11 grupper af forbrug blandt husholdningerne. Hvad brugte 
  # danskerne flest penge på i 2023? Hvilken gruppe af forbruget steg mest fra 
  # 2020 til 2023? (hint: I kan ikke lægge kvartalerne sammen, når I har kædede værdier)
  
  # Hent data for de 11 grupper af forbrug blandt husholdningerne. Hvad brugte 
  # danskerne flest penge på i 2023?
  {
# Vi henter vores data via DST's API
  # Vi finder "FORV1", som vi skal bruge og henter den ind som liste
  library(dkstat)
  NKHC021_Meta <- dst_meta("NKHC021")
  
  # 
  my_query4 <- list(
    FORMAAAL = "*",
    PRISENHED = "2020-priser, kædede værdier",
    SÆSON="Sæsonkorrigeret",
    Tid = "*"
  )
  
  # Hent data som en dataframe
  NKHC021 = dst_get_data(table = "NKHC021", query = my_query4)
  
  library(tidyr)
  
  #Lav til lang format
  Opgave_4.3 <- NKHC021 %>%
    pivot_wider(names_from = FORMAAAL, values_from = value)
  
  # Rette vores df til
  Opgave_4.3 <- Opgave_4.3[-1:-40,]
  
  # Lave Tid om til dato
  Opgave_4.3$TID <- as.Date(Opgave_4.3$TID)
  
  # Vælg række 24 og kolonnerne 4 til 15
  Opg.4.3_række_2023Q1_Q4 <- Opgave_4.3[c(93:96), c(5:19)]
  
  # Beregn gennemsnittet af de to rækker (2023-01-01 og 2023-10-01)
  gennemsnit_2023 <- colMeans(Opg.4.3_række_2023Q1_Q4)
  
  # Find kolonnen med det største forbrug i 2023
  max_forbrug_index <- which.max(gennemsnit_2023)
  kategori_med_max_forbrug <- names(gennemsnit_2023)[max_forbrug_index]
  
  # Print resultaterne
  print("Gennemsnitligt forbrug i 2023 per kategori:")
  print(gennemsnit_2023)
  
  print(paste("Kategorien med det største forbrug i 2023 er:", kategori_med_max_forbrug))
  print(paste("Forbruget er:", gennemsnit_2023[max_forbrug_index]))
  }
  # Den største forbrugskategori er Boligbenyttelse
  
  # Hvilken gruppe af forbruget steg mest fra 2020 til 2023?
  {
    # Filtrer data for 2020Q1 og 2023Q4
    data_2020 <- Opgave_4.3[81:84, 5:19]  # Sørg for, at indekserne er korrekte for dine data
    data_2023 <- Opgave_4.3[93:96, 5:19]  # Ret årstallet til 2023 og kvartal til Q4, hvis nødvendigt
    
    # Beregn gennemsnittet for hvert år (da vi har kvartalsdata)
    årsgennemsnit_2020 <- colMeans(data_2020, na.rm = TRUE)
    årsgennemsnit_2023 <- colMeans(data_2023, na.rm = TRUE)
    
    # Beregn procentuelle ændringer
    procentuel_ændring <- (årsgennemsnit_2023 - årsgennemsnit_2020) / årsgennemsnit_2020 * 100
    
    # Find den kategori med den største stigning
    max_stigning_index <- which.max(procentuel_ændring)  # Find indekset for største vækst
    kategori_med_max_stigning <- colnames(Opgave_4.3)[max_stigning_index + 4]  # +4 da kolonner starter fra 5
    
    # Print resultaterne
    print("Procentuel ændring fra 2020 til 2023 for hver kategori:")
    print(procentuel_ændring)
    
    print(paste("Kategorien med den største procentuelle stigning er:", kategori_med_max_stigning))
    print(paste("Stigningen er:", round(procentuel_ændring[max_stigning_index], 2), "%"))
    
  }
  # "Kategorien med den største procentuelle stigning er: Beklædning og fodtøj 50%
  {
  # Filtrer data for 2020Q1 og 2024Q2
  data_2020Q1 <- Opgave_4.3[Opgave_4.3$TID == as.Date("2020-01-01"), 5:19]
  data_2024Q2 <- Opgave_4.3[Opgave_4.3$TID == as.Date("2024-04-01"), 5:19]
  
  # Beregn den reelle udvikling
  reel_udvikling <- data_2024Q2 - data_2020Q1
  
  # Find den kategori med den største stigning
  max_udvikling_index <- which.max(reel_udvikling)
  kategori_med_max_udvikling <- colnames(Opgave_4.3)[max_udvikling_index + 4] # +4 for at matche kolonner fra 5:19
  
  # Print resultaterne
  print("Reel udvikling fra 2020Q1 til 2024Q2 for hver kategori:")
  print(reel_udvikling)
  
  print(paste("Kategorien med den største reelle udvikling er:", kategori_med_max_udvikling))
  print(paste("Stigningen i reelle tal er:", reel_udvikling[max_udvikling_index]))
  }
  # "Kategorien med den største reelle udvikling er: Beklædning og fodtøj 4868DKK
  
  }
  
# Opave 4.4 - 22 simple lineære reggressioner
{ # Lav 22 simple lineære regressioner mellem hver af de 11 grupper i forbruget 
#(y-variable) og henholdsvis forbrugertillidsindikatoren fra DST og DI. I skal 
# gemme summary i 22 lister. I skal lave jeres regressioner fra 1. kvartal 2000 
# til og med 2. kvartal 2024.
{
# Vi starter med at merge vores df med FTI, DST og DI med Opgave 4.3
  {
  # Først skal vi rense vores df, så vi kun har FTI for DST og DI
  FORV1_4.4 <- FORV1_FTI_DST_DI[,1:3]
  
  # Beregn kvartalets startdato
  FORV1_4.4$TID <- as.Date(cut(FORV1_4.4$TID, breaks = "quarter"))
  
  # Opret en ny dataframe med gennemsnit pr. kvartal
  FORV1_4.4 <- aggregate(
    cbind(Forbrugertillidsindikatoren, Forbrugertillidsindikatoren_DI) ~ TID,
    data = FORV1_4.4,
    FUN = mean,
    na.rm = TRUE
  )
  # Vi afrunder 
  # Afrund kolonnerne til én decimal
  FORV1_4.4$Forbrugertillidsindikatoren <- round(FORV1_4.4$Forbrugertillidsindikatoren, 1)
  FORV1_4.4$Forbrugertillidsindikatoren_DI <- round(FORV1_4.4$Forbrugertillidsindikatoren_DI, 1)
  }
  
Opgave_4.4 <- merge(
  FORV1_4.4, Opgave_4.3,
  by.x = "TID",
  by.y = "TID"
)

# Vi retter vores df til, så det passer
Opgave_4.4 <- Opgave_4.4[,-4:-7]

colnames(Opgave_4.4)
# Kategorierne som skal bruges som y-variabler
forbrug_kategorier <- c("Fødevarer mv.", "Drikkevarer og tobak mv.", "Beklædning og fodtøj", 
                        "Boligbenyttelse", "Elektricitet, fjernvarme og andet brændsel", 
                        "Boligudstyr, husholdningstjenester mv.", "Medicin, lægeudgifter o.l.", 
                        "Køb af køretøjer", "Drift af køretøjer og transporttjenester", 
                        "Information og kommunikation", "Fritid, sport og kultur", "Undervisning", 
                        "Restauranter og hoteller", "Forsikring og finansielle tjenester", 
                        "Andre varer og tjenester")

# Initialiser lister til at gemme regression summaries
summary_lister <- list()

# Kør regressioner for hver kategori mod DST og DI tillidsindikatorerne
for (kategori in forbrug_kategorier) {
  # Brug backticks omkring kategorinavnet for at undgå problemer med mellemrum
  formel_DST <- as.formula(paste0("`", kategori, "` ~ Forbrugertillidsindikatoren_DST"))
  model_DST <- lm(formel_DST, data = Opgave_4.4)
  
  # Gem summary i listen
  summary_lister[[paste(kategori, "DST")]] <- summary(model_DST)
  
  # Regression med Forbrugertillidsindikatoren_DI
  formel_DI <- as.formula(paste0("`", kategori, "` ~ Forbrugertillidsindikatoren_DI"))
  model_DI <- lm(formel_DI, data = Opgave_4.4)
  
  # Gem summary i listen
  summary_lister[[paste(kategori, "DI")]] <- summary(model_DI)
}

# Nu er alle regression summaries gemt i summary_lister

summary_lister[["Boligudstyr, husholdningstjenester mv. DST"]]

summary_lister[["Beklædning og fodtøj DI"]]

summary_lister[["Fødevarer mv. DI"]]




} 
  
}

  
  
    
  
