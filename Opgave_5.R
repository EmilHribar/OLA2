# Opgave 5 – Eurostat og API
# Opgave 5.1 - Kvartalsvis årlig realvækst for en række Eurolande
{ # Beregn den kvartalsvise årlige realvækst for husholdningernes forbrugsudgift
  # for Danmark, Belgien, Holland, Sverige, Østrig, Tyskland, Frankrig, Italien
  # og Spanien i perioden 1. kvartal 2000 til og med 2. kvartal 2024. 
  # I skal hente data vha. API’et fra Eurostat.

# Pakker til API og plotting
library(eurostat)
library(ggplot2)

# Definer de lande, vi fokuserer på
lande <- c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES")

# Opsætning af filter til at hente data fra Eurostat API
myfilter <- list(
  unit = c("CLV10_MNAC"),  # Bruger konstant pris (Real terms)
  s_adj = "SCA",           # Sæsonkorrigeret
  na_item = "P31_S14",     # Husholdningernes forbrugsudgift
  geo = lande
)

# Hent datasættet fra Eurostat via API med filtrering
data <- get_eurostat("namq_10_gdp", time_format = "date", filters = myfilter)

# Filtrer på den ønskede periode (1. kvartal 2000 til 2. kvartal 2024)
filtered_data <- subset(data, time >= as.Date("1999-01-01") & time <= as.Date("2024-06-30"))

# Konverter værdier til numerisk (hvis det ikke allerede er numerisk)
filtered_data$values <- as.numeric(filtered_data$values)

# Fjern eventuelle manglende værdier
filtered_data <- na.omit(filtered_data)

# Sortér data efter land og tid
filtered_data <- filtered_data[order(filtered_data$geo, filtered_data$time), ]

# Opret en matrix til resultater: Land, Tid, Vækst
result_matrix <- matrix(nrow = 0, ncol = 3)  
colnames(result_matrix) <- c("Land", "Tid", "Vækst")

# Loop gennem hvert land for at beregne den kvartalsvise årlige realvækst
for (land in lande) {
  
  # Filtrér data for det enkelte land
  land_data <- subset(filtered_data, geo == land)
  
  # Beregn vækstrater: De første 4 kvartaler får NA, da de mangler sammenligningsgrundlag
  land_data$growth <- c(rep(NA, 4), (land_data$values[5:nrow(land_data)] / 
                                       land_data$values[1:(nrow(land_data) - 4)] - 1) * 100)
  
  # Afrunding til to decimaler
  land_data$growth <- round(land_data$growth, 2)
  
  # Saml landets resultater i result_matrix
  country_results <- cbind(land, as.character(land_data$time), land_data$growth)
  result_matrix <- rbind(result_matrix, country_results)
}

# Konverter result_matrix til en data.frame for nemmere manipulation
result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
colnames(result_df) <- c("Land", "Tid", "Vækst")

# Konverter 'Tid' kolonnen til dato-format
result_df$Tid <- as.Date(result_df$Tid)

# Konverter 'Vækst' kolonnen til numerisk
result_df$Vækst <- as.numeric(result_df$Vækst)

# Vis de første rækker af resultatet
head(result_df)

# Plot den kvartalsvise årlige vækst for alle lande
ggplot(result_df, aes(x = Tid, y = Vækst, color = Land)) +
  geom_line() +
  labs(title = "Kvartalsvis Årlig Realvækst for Husholdningernes Forbrugsudgift",
       x = "Tid",
       y = "Vækst (%)",
       color = "Land") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-Q%q", date_breaks = "1 year") +  # Kvartalsvis label
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Facettering for at plotte hvert land separat
ggplot(result_df, aes(x = Tid, y = Vækst)) +
  geom_line() +
  facet_wrap(~ Land) +  # Opdel plottet per land
  labs(title = "Kvartalsvis Årlig Realvækst for Hvert Land",
       x = "Tid",
       y = "Vækst (%)") +
  theme_minimal()
}
#____________________

# Opgave 5.2 - Højeste kvartalsvise årlige realvækst
{ # Hvilket af de landene har gennemsnitligt haft den højeste kvartalsvise 
  # årlige realvækst i husholdningernes forbrugsudgift i perioden 1. kvartal 2000 til 2. kvartal 2024.

# Fjern NA værdier (de første 4 kvartaler for hvert land) + Laver en ny dataframe til denne opgave
result_df2 <- na.omit(result_df)

# Konverter 'Vækst' kolonnen til numerisk, hvis den ikke allerede er numerisk
result_df2$Vækst <- as.numeric(result_df2$Vækst)

# Bruger aggregate() til at beregne gennemsnitlig vækst for hvert land baseret på "Vækst"-kolonnen.
avg_growth <- aggregate(Vækst ~ Land, data = result_df2, FUN = mean)

# Find indekset for det land, der har den højeste gennemsnitlige vækst
# which.max() returnerer indekset for den største værdi i en vektor 
highest_growth_country <- avg_growth[which.max(avg_growth$Vækst), ]

# Vis resultatet
print(highest_growth_country)

# Resultat for gennemsnitlig højeste kvartalsvise årlige realvækst: 
# Land: SE
# Vækst: 2.1365
}
# __________________

# Opgave 5.3 - Coronakrisen som outlier
{# Fjerne Coronakrisen fra jeres data og find igen den gennemsnitligt kvartalsvise
  # realvækst i husholdningernes forbrugsudgift i perioden 1. kvartal 2000 til 
  # 2. kvartal 2024. I hvilket af landene har Coronakrisen haft en største effekt
  # på den gennemsnitligt kvartalsvise realvækst.

# result_df er den eksisterende dataframe med resultaterne fra opg. 5.1, danner ny dataframe til 5.3
result_df3 <- result_df

# Konverter 'Tid' kolonnen til datoformat, hvis det ikke allerede er gjort
result_df3$Tid <- as.Date(result_df3$Tid)

# Filtrer data for at fjerne Coronakrisen (Q1 2020 - Q2 2021)
result_filtered <- subset(result_df3, !(Tid >= as.Date("2020-01-01") & Tid <= as.Date("2021-06-30")))

# Beregn gennemsnitlig vækst for hvert land uden Coronakrisen
avg_growth_no_covid <- aggregate(Vækst ~ Land, data = result_filtered, FUN = mean)

# Beregn den gennemsnitlige vækst for hvert land med Coronakrisen
avg_growth_with_covid <- aggregate(Vækst ~ Land, data = result_df3, FUN = mean)

# Sammenlign gennemsnitlig vækst med og uden Coronakrisen
# Her skabes en ny data frame "comparison" ved at sammenkøre to eksisterende data frames
# merge: samler data rammerne baseret på en fælles kolonne, i dette tilfælde "Land". 
# suffixes argumentet: Når der er kolonner med samme navn i begge data rammer ("Vækst"), 
#  - tilføjer merge funktionen suffixerne "_with_covid" og "_no_covid" til kolonnenavnene i den nye data ramme, så de kan skelnes fra hinanden.
comparison <- merge(avg_growth_with_covid, avg_growth_no_covid, by = "Land", suffixes = c("_with_covid", "_no_covid"))

# Beregn ændringen i gennemsnitlig vækst
comparison$change <- comparison$Vækst_no_covid - comparison$Vækst_with_covid

# Find landet med den største ændring (den største effekt af coronakrisen)
most_affected_country <- comparison[which.max(comparison$change), ]

# Vis resultatet
print(most_affected_country)
}
# REsultat: 
# Land - AT (Østrig)
# Vækst med covid: 1.055612
# Vækst uden covid: 1.48913
# Forskellen i alt: 0.4335182

#______________________

# Opgave 5.4 - Effekt af Corona på forbruget
{# I hvilket europæiske land faldt den gennemsnitligt kvartalsvise realvækst i 
  # husholdningernes forbrugsudgift, i perioden 1. kvartal 2020 til 2. kvartal 2024, mest?

# 1. Filtrer dataene for perioden 1. kvartal 2020 til 2. kvartal 2024
covid_period <- result_df3[result_df3$Tid >= as.Date("2020-01-01") & result_df3$Tid <= as.Date("2024-06-30"), ]

# 2. Beregn den gennemsnitlige vækst for hvert land i denne periode
avg_growth_covid_period <- aggregate(Vækst ~ Land, data = covid_period, FUN = mean)

# 3. Beregn den gennemsnitlige vækst for hvert land i perioden før Corona
pre_covid_period <- result_df3[result_df3$Tid < as.Date("2020-01-01"), ]
avg_growth_pre_covid <- aggregate(Vækst ~ Land, data = pre_covid_period, FUN = mean)

# 4. Merge de to datasæt for at sammenligne væksten
comparison <- merge(avg_growth_pre_covid, avg_growth_covid_period, by = "Land", suffixes = c("_pre_covid", "_covid"))

# 5. Beregn ændringen i gennemsnitlig vækst
comparison$change <- comparison$Vækst_covid - comparison$Vækst_pre_covid

# 6. Find landet med det største fald i vækst
most_affected_country <- comparison[which.min(comparison$change), ]

# 7. Vis resultatet
print(most_affected_country)
}
# Resultat:
# Land - SE 
# Gennemsnitlig forbrugsudgift før corona: 2.43
# Gennemsnitlig forbrugsudgift under corona: 0,83
# Resultat af ændring: -1.60
# Konklusion: den gennemsnitlige vækst i husholdningernes forbrugsudgift i Sverige faldt betydeligt under COVID-19-pandemien.

# _____________
# Ekstra - Print "comparison" for at se udviklingen af væksten under covid for alle landende
Print(comparison)