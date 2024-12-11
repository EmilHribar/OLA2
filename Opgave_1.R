# OLA 2 
# Opgave 1 - Boliger og DST

# Opgave 1.1) Det første skridt 
{#  Skriv en kode, der viser hvordan, du finder en tabel, som kan give en liste
# over byer med indbyggertal via DST-pakken dkstat. 

# Find tabel via dkstat
{# Først skal vi indlæse vores pakke fra library
library(dkstat)

# Vi laver nu en df over alle tabeller i DST, som vi kan søge i
alltables <- dst_get_tables(lang = "da")

# Vi skal nu søge relevante tabeller frem. Det gør vi med dst_search
dst_search(string = "befolkning",field = "text")

# Vi finder "POSTNR1", som vi skal bruge og henter den ind som liste
BY_Meta <- dst_meta("POSTNR1")

# Lav liste med filter-variabler
# Variabler
my_query_1 <- list(
  PNR20="*",
  Tid="2024"
)

# Hent tabel og ret til
BY_str <- dst_get_data(table = "POSTNR1", query = my_query_1)

# Nu vil vi gerne splitte vores første kolonne, så vi har postnr og by separat.
# Dette gøres således:
{
  
BY_str_tp <- BY_str
  
# Fjern parenteser og teksten inde i dem
  BY_str_tp$location_cleaned <- sub("\\s*\\(.*\\)", "", BY_str_tp$PNR20)

# Split kolonnen i postnr og by
split_data_BY <- strsplit(BY_str_tp$location_cleaned, " ", fixed = TRUE)

# Udpak postnummer og by i separate kolonner
BY_str_tp$postnr <- sapply(split_data_BY, `[`, 1)
BY_str_tp$by <- sapply(split_data_BY, function(x) paste(x[-1], collapse = " "))

# Fjern den midlertidige kolonne
BY_str_tp$location_cleaned <- NULL
BY_str_tp$PNR20 <- NULL
BY_str_tp$TID <- NULL

# Til sidst navngiver jeg min 1. kolonne "indbyggertal"
colnames(BY_str_tp)[1] <- "indbyggertal"

# Vi fjerner hele landet
BY_str_tp <- BY_str_tp[-1,]

}
}}

# Opave 1.2) Kategorivariable
{ # Lav en kategorivariabel i R ud fra følgende kriterier: 
  # bycat = (c("landsby"=250,"lille by"=1000, "almindelig by"=2500, 
  # "større by"=10000, "storby"=50000)) og anvend den på listen over byer.
# ___________________________________________
  # Vi laver nu en funktion, som kan sætte vores byer i bås.
  bycat = (c("landsby 0-2.500"<=2500,"lille by 2.500-10.000"<=10000,
        "almindelig by10.000-25.000"<=25000, "større by 25.000-100.000"<=100000,
        "storby over 500.000"<=500000))
  
  # For at København ikke bliver kategoriseret som en anden by end "storby"
  # så lægger vi indbyggertallene sammen og giver alle Københavns bydele samme
  # indbyggertal. Det samme gør vi for Aarhus.
  {
    # Beregn samlet indbyggertal for København, Aarhus og Odense
    kbh_sum <- BY_str_tp %>%
      filter(postnr >= 1000 & postnr <= 2451) %>%
      summarise(indbyggertal = sum(indbyggertal, na.rm = TRUE)) %>%
      pull(indbyggertal)
    
    aarhus_sum <- BY_str_tp %>%
      filter(postnr >= 8000 & postnr <= 8270) %>%
      summarise(indbyggertal = sum(indbyggertal, na.rm = TRUE)) %>%
      pull(indbyggertal)
    
    odense_sum <- BY_str_tp %>%
      filter(postnr >= 5000 & postnr <= 5270) %>%
      summarise(indbyggertal = sum(indbyggertal, na.rm = TRUE)) %>%
      pull(indbyggertal)
    
    # Tildel summen til alle postnumre for hver by
    BY_str_tp <- BY_str_tp %>%
      mutate(indbyggertal = case_when(
        postnr >= 1000 & postnr <= 2451 ~ kbh_sum,
        postnr >= 8000 & postnr <= 8270 ~ aarhus_sum,
        postnr >= 5000 & postnr <= 5270 ~ odense_sum,
        TRUE ~ indbyggertal  # Behold andre byer uændrede
      ))

  }
  
  # Vi laver nu en ny kolonne til denne funktion i vores df BY2_by_str_merge
  BY_str_tp$By_størrelse <- NA
  
  # Vi kan nu bruge cut funktionen til at indele byerne i de forskellige kategorier
  BY_str_tp$By_størrelse <- cut(
    BY_str_tp$indbyggertal,  # Kolonnen med indbyggertal
    breaks = c(-Inf, 2500, 10000, 25000, 100000, 500000, Inf),  # Definer grænserne for hver kategori
    labels = c("landsby", "lille by", "almindelig by", "større by", "storby", "hovedstad"),  # Navnene på kategorierne
    right = FALSE  # Gør grænserne venstre-lukkede (så "2500" tilhører "lille by")
  )
  opgave_1.2 <- BY_str_tp
}

# Opgave 1.3) Merge de to dataframes
{ # Indlæs filen med boliger og tilpas de to dataframes,
  # så du kan merge de to sammen via variablen ”by”
  #således at du får kategorien bycat med i dit bolig-datasæt.
 # _________________________________
  
  # Vi indlæser nu vores boligfil, som findes i data folderen
  library(readr)
  boligsiden <- read_csv("Data/boligsiden.csv")
  View(boligsiden)
  
  # Nu ønsker vi kun at se kolonne 1, 4 og 5
  boligsiden <- boligsiden[,c(4,5,6)]
  
  # Fjern rækker med NA i kolonnerne pris, postnr eller by
  boligsiden <- na.omit(boligsiden)
  
  # Vi merger nu BY_str_tp og boligsiden baseret på postnr og postnr
  opgave_1.3_merged_data <- merge(BY_str_tp,boligsiden, by.x = "postnr",by.y = "postnr")

  # Fjern outliers
  colnames(opgave_1.3_merged_data)
  # Behold rækker hvor kvmpris er mindre eller lig med 150000
  opgave_1.3_merged_data <- opgave_1.3_merged_data[opgave_1.3_merged_data$kvmpris <= 100.000, ]
  
  }

# Opgave 1.4 - Plot
# Din merge skal producer en dataframe og et plot som vist nedenfor.(Se OLA2)
{ # Først renser jeg mit merged df, så det ligner det i OLA 2
  # Kolonnerne er arrangeret således: "Bynavn", "Værdi", "Indbygger", "Bystørrelse"
  Opgave_1.4 <- opgave_1.3_merged_data[,c(2,4,5,6)]
  colnames(Opgave_1.4)
  
  # Jeg omrokerer mine kolonner
  Opgave_1.4 <- Opgave_1.4[,c("by.y","kvmpris","indbyggertal","By_størrelse")]
  

  # Nu plotter vi vores df som et bar plot via ggplot
  library(dplyr)
  library(ggplot2)
  
  # Beregn gennemsnitlig kvadratmeterpris pr. bystørrelse
  avg_kvmpris <- Opgave_1.4 %>%
    group_by(By_størrelse) %>%
    summarise(gennemsnit_kvmpris = mean(kvmpris, na.rm = TRUE))
  
  # Plot baseret på gennemsnit
  ggplot(avg_kvmpris, aes(x = By_størrelse, y = gennemsnit_kvmpris, fill = By_størrelse)) + 
    geom_bar(stat = "identity") +  # Skaber et bar plot baseret på gennemsnitsværdier
    theme_minimal() +              # Bruger et minimalistisk tema
    labs(
      title = "Kvadratmeterprisen er dyrest i hovedstaden",  # Tilføj overskrift
      x = "Bykategori", 
      y = "Kvadratmeterpris, tusinde kr.", 
      fill = "Bytype"
    ) +  
    scale_y_continuous(labels = function(x) x / 1) +  # Konverterer y-aksen til tusinde kroner
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotér x-aksens tekst for bedre læsbarhed
  
}  


