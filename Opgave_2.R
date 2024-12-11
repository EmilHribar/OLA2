# Opgave 2 - Forbrugertillidsindikator og fremtidig vækst i husholdningernes forbrugsudgift

# Opgave 2.1) Opdatering af DI's forbrugertillidsindikator
{# Opdatér DI’s forbrugertillidsindikator med data frem til og med 2023 fra artiklen ”Forbruget
  # fortsætter fremgangen i 2016” (Baum, 2016). Lav vurdering af om forbrugertillidsindikatoren fra DI
  # fortsat er bedre end forbrugertillidsindikatoren fra DST.
}
{
# Vi finder vores data via Dst's API og klargør datasæt

{# Først skal vi indlæse vores pakke fra library
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
FORV1_wide <- FORV1 %>%
  pivot_wider(names_from = INDIKATOR, values_from = value)

#Fjern række 1 til 301
FORV1_wide <- FORV1_wide[-(1:303), ]

}

# Nu skal vi have lavet vores nye FTI for DI.

{ # Vi laver en ny df, hvor vi kan sammenligne FTI for DST og DI.
  FORV1_FTI_DST_DI <- FORV1_wide
  FORV1_FTI_DST_DI$Forbrugertillidsindikatoren_DI <- NA
  
  # Nu flytter vi vores nye kolonne til 3. kolonne.
  FORV1_FTI_DST_DI <- FORV1_FTI_DST_DI[, c(1:2, 15, 3:14)]
  
  # Vi laver nu vores FTI for DI. 
  # DI FTI bygger på kolonne 4, 6, 8 & 12
  FORV1_FTI_DST_DI$Forbrugertillidsindikatoren_DI <- aggregate(FORV1_FTI_DST_DI, c(4,6,8,12), FUN = mean())
  
  # Beregn gennemsnittet af kolonne 4, 6, 8 og 12 for hver række og gem resultatet i kolonne 3
  FORV1_FTI_DST_DI$Forbrugertillidsindikatoren_DI <- rowMeans(FORV1_FTI_DST_DI[, c(4, 6, 8, 12)], na.rm = TRUE)
  
  # Vi afrunder nu vores FTI DI til 1 decimal
  FORV1_FTI_DST_DI$Forbrugertillidsindikatoren_DI <- round(FORV1_FTI_DST_DI$Forbrugertillidsindikatoren_DI , 1)
  
}

# For at sammenligne FTI for DST og DI plotter vi vores df i et scatterplot (Ekstra)
{ # Indlæs ggplot2, hvis det ikke allerede er gjort
  library(ggplot2)
  
  # Lav et ggplot med TID som x-akse og både Forbrugertillidsindikator og Forbrugertillidsindikatoren_DI som y-akse
  ggplot(FORV1_FTI_DST_DI, aes(x = TID)) +
    geom_line(aes(y = Forbrugertillidsindikatoren, color = "Forbrugertillidsindikatoren_DST"), size = 1) +  # Første linje
    geom_line(aes(y = Forbrugertillidsindikatoren_DI, color = "Forbrugertillidsindikatoren_DI"), size = 1) +  # Anden linje
    labs(x = "Tid", y = "Forbrugertillidsindikator (DST og DI)", 
         color = "Legend") +  # Tilføj labels til akserne og en legend
    ggtitle("Udvikling i forbrugertillidsindikator 2000-2023 (DST og DI)") +  # Tilføj overskrift
    theme_minimal() +
    scale_color_manual(values = c("Forbrugertillidsindikatoren_DST" = "blue", 
                                  "Forbrugertillidsindikatoren_DI" = "red"))  # Tilføj farver til linjerne
  
  
}

# For at vurdere hvilken FTI er bedst, skal vi lave regressioner på begge tabeller (DST og DI)
#og holde dem op mod det reelle forbrug. 

# Vi beregner nu det reelle forbrug i %. 
{

  # Vi skal nu hente det relle forbrug i dkstat. ( Vi bruger samme fremgangsmåde)#
  # Vi ved fra tidligere, at den hedder NKN3
  
  # Vi finder "NKN3", som vi skal bruge og henter den ind som liste
  NKN3_Meta <- dst_meta("NKN3")
  
  # 
  my_query2 <- list(
    TRANSAKT="*",
    PRISENHED="*",
    TID="*"
  )
  
  # Vi henter tabellen som df.
  NKN3=dst_get_data(table = "NKN3", query = my_query2)
  
  #Lav til lang format
  NKN3_wide <- NKN3 %>%
    pivot_wider(names_from = TRANSAKT, values_from = value)
  
  #Vi vil kune se kolonne 2 og 3. Alle kolonner
  NKN3_wide <- NKN3_wide[102:202,c(2,3) ]
  
  # Transponér dataframen
  NKN3_wide <- as.data.frame(NKN3_wide)
  
  # Omdøb kolonnerne
colnames(NKN3_wide)[2] <- "Forbrug"
  
  # Konverter "Forbrug" til numerisk og fjern decimaler
  NKN3_wide$Forbrug <- as.numeric(NKN3_wide$Forbrug)
  
  _
  
  library(dplyr)
  
  #Beregn den årlige kvartalsvise udvikling for forbrug
  NKN3_wide <- NKN3_wide %>%
    mutate(Årlig_udvikling = (Forbrug - lag(Forbrug, 4)) / lag(Forbrug, 4) * 100)
  
  #fjern de 4 første rækker
  NKN3_wide <- NKN3_wide[-(1:4), ]
  
  #Tjek den opdaterede dataframe
  View(privat_forbrug_df)
  
}

# Vi skal nu udregne vores FTI for DST og DI i kvartaler
{
  # Først skal vi rense vores df, så vi kun har FTI for DST og DI
  FORV1_FTI_DST_DI_Q <- FORV1_FTI_DST_DI[,1:3]
  
  #Konverter datoer til kvartaler i kolonne 1 (tid)
  FORV1_FTI_DST_DI_Q$Kvartal <- paste0(format(as.Date(FORV1_FTI_DST_DI_Q[[1]]), "%Y"), "K", 
                                 ceiling(as.numeric(format(as.Date(FORV1_FTI_DST_DI_Q[[1]]), "%m")) / 3))
  
  #Opret en ny dataframe med gennemsnit pr. kvartal
  FORV1_FTI_DST_DI_Q <- aggregate(cbind(Forbrugertillidsindikatoren, Forbrugertillidsindikatoren_DI) ~ Kvartal,
                                data = FORV1_FTI_DST_DI_Q, FUN = mean, na.rm = TRUE)
 
  # Vi fjerne de to sidste kvartaler for at kunne sammenligne
  FORV1_FTI_DST_DI_Q <- FORV1_FTI_DST_DI_Q[-99,]
  FORV1_FTI_DST_DI_Q <- FORV1_FTI_DST_DI_Q[-98,]
}

# Vi lave vi regression med FTI DST op mod vores forbrug
{ # Vi laver en ny df, hvor kun FTI DST og forbrug er.
  FTI_DST_Forbrug <- merge(FORV1_FTI_DST_DI_Q$Forbrugertillidsindikatoren, NKN3_wide$Årlig_udvikling)
  colnames(FTI_DST_Forbrug) <- c("Forbrugertillidsindikatoren","Forbrug")
  
  
  # Kør regression (byttet rundt på y og x)
  FTI_DST_Forbrug_Reg <- lm(Forbrug ~ Forbrugertillidsindikatoren, data = FTI_DST_Forbrug)
  
  # Se resultaterne
  
  summary(FTI_DST_Forbrug_Reg)
  
}
}
# Opgave 2.2 Forudsigelser af forbruget
{# Beregn/forudsig den årlige realvækst i husholdningernes forbrugsudgift for
# 3. kvartal 2024 med henholdsvis DI’s forbrugertillidsindikator og forbrugertillidsindikatoren fra DST.
  #2000Q1 til 2024Q3 for FTI af DST & DI 
  #Forklaring 2024-07-01 / DST-6.5 /DI -8.2
  
  #1. DST forudsagte årlige udvikling af forbruget
  # Hent koefficienterne fra model_DST
  intercept_DST <- coef(model_DST)[1]  # Intercept
  coef_DST <- coef(model_DST)[2]  # Koeficienten for DST-indikatoren
  
  # Værdien af Forbrugertillidsindikatoren_DST for 3. kvartal 2024
  DST_value_2024Q3 <- -6.5
  
  # Forudsig den årlige udvikling af forbruget med DST-modellen
  forudsigelse_DST <- intercept_DST + coef_DST * DST_value_2024Q3
  
  #2. DI forudsagte årlige udvikling af forbruget
  # Hent koefficienterne fra model_DI
  intercept_DI <- coef(model_DI)[1]  # Intercept
  coef_DI <- coef(model_DI)[2]  # Koeficienten for DI-indikatoren
  
  # Værdien af Forbrugertillidsindikatoren_DI for 3. kvartal 2024
  DI_value_2024Q3 <- -8.2
  
  # Forudsig den årlige udvikling af forbruget med DI-modellen
  forudsigelse_DI <- intercept_DI + coef_DI * DI_value_2024Q3
  
  # Forudsigelsen:
  print(forudsigelse_DI)
  print(forudsigelse_DST)
  
  #DST-modellen forudsiger en årlig vækst i forbruget på 0.714% for 3. kvartal 2024.
  #DI-modellen forudsiger en årlig vækst i forbruget på 0.196% for samme periode.
  
  # Forbrug i 2024 Q2
  forbrug_Q2 <- 290.2
  
  # Forudsagt procentvis vækst for DST og DI
  forudsigelse_DST <- 0.714  # i procent
  forudsigelse_DI <- 0.196   # i procent
  
  # Beregn forbruget i 2024 Q3 baseret på DST
  forbrug_Q3_DST <- forbrug_Q2 * (1 + forudsigelse_DST / 100)
  
  # Beregn forbruget i 2024 Q3 baseret på DI
  forbrug_Q3_DI <- forbrug_Q2 * (1 + forudsigelse_DI / 100)
  
  # Udskriv resultaterne
  cat("Forbruget i 2024 Q3 (DST):", forbrug_Q3_DST, "\n")
  cat("Forbruget i 2024 Q3 (DI):", forbrug_Q3_DI, "\n")
  
  #Forudsagt Forbrug i 2024 Q3 (DST): 292.272
  #Forudsagt Forbrug i 2024 Q3 (DI): 290.7688
  
  }
 
#2.3 – Salg resten af året

# Beregn gennemsnittet af den årlige vækst i forbruget for alle kvartaler
gennemsnitlig_vækst <- mean(FORV1_og_NKN3_samlet$Forbrugets_årlig_udvikling, na.rm = TRUE)

# Udskriv gennemsnittet
cat("Den gennemsnitlige årlige vækst i forbruget pr. kvartal er:", round(gennemsnitlig_vækst, 2), "%\n")

#Den gennemsnitlige årlige vækst i forbruget pr. kvartal er: 1.47 %

# Filtrér data for perioden før 1. april 2021
forbrug_før_covid <- FORV1_og_NKN3_samlet %>%
  filter(TID < as.Date("2020-04-01"))

# Tjek om dataene er korrekt filtreret
cat("Antal rækker før COVID-19:", nrow(forbrug_før_covid), "\n")

# Vis de første par rækker for at sikre, at filtreringen virker
head(forbrug_før_covid)

# Hvis der er data, beregn gennemsnittet
if (nrow(forbrug_før_covid) > 0) {
  gennemsnitlig_forbrug_før_covid <- mean(forbrug_før_covid$Forbrugets_årlig_udvikling, na.rm = TRUE)
  cat("Den gennemsnitlige årlige vækst i forbruget før COVID-19 er:",+
        round(gennemsnitlig_forbrug_før_covid, 2), "%\n")
} else {
  cat("Ingen data fundet før COVID-19.\n")
}

#Den gennemsnitlige årlige vækst i forbruget før COVID-19 er: 1.5 %                                                                     


 