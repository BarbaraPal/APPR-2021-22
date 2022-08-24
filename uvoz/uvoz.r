# 2. faza: Uvoz podatkov
sl <- readr::locale("sl", decimal_mark=",", grouping_mark=".")

pretvornik.regij <- function(){ # 2 regiji (Posavska in Jugovzhodna Slovenija) imata različno poimenovanje v različnih virih
  regije.slo = dplyr::tibble(
    regija = c(
      "Gorenjska",
      "Goriška",
      "Jugovzhodna",
      "Koroška",
      "Obalno-kraška",
      "Osrednjeslovenska",
      "Podravska",
      "Pomurska",
      "Spodnjeposavska",
      "Posavska",
      "Primorsko-notranjska",
      "Savinjska",
      "Zasavska",
      "SLOVENIJA"
    ),
    statisticna_regija = c(
      "Gorenjska",
      "Goriška",
      "Jugovzhodna Slovenija",
      "Koroška",
      "Obalno-kraška",
      "Osrednjeslovenska",
      "Podravska",
      "Pomurska",
      "Posavska",
      "Posavska",
      "Primorsko-notranjska",
      "Savinjska",
      "Zasavska",
      "SLOVENIJA"
    )
  )
  return(regije.slo)
}

# UVOZ

## PRIPADNOST OBČIN REGIJAM
uvoz.obcine.regije <- function(){
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- rvest::session(link) %>% 
    rvest::read_html()
  tabela <- stran %>% 
    rvest::html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",") %>% 
    dplyr::select("Statistična regija", "Občina")
  tabela[162, 2] <- "Sveta Trojica v Slov. goricah*"
  tabela[163, 2] <- "Sveti Andraž v Slov. goricah"
  tabela[165, 2] <- "Sveti Jurij v Slov. goricah"
  tabela[62, 2] <- "Kanal"
  names(tabela)[1] <- "regija"
  names(tabela)[2] <- "obcine"
  tabela <- dplyr::right_join(pretvornik.regij(), 
                       tabela, 
                       by="regija") %>% 
    dplyr::select(-regija)   # spremenjeno poimenovanje dveh regij - zdaj se ujema z ostalimi podatki
  return(tabela)
}

## PREBIVALSTVO
uvoz.prebivalstvo <- function(){
  prebivalstvo <- readr::read_csv2("podatki/prebivalstvo.csv", skip=2,
                            locale=locale(encoding="Windows-1250"),
                            col_types = cols(
                              .default = col_guess(),
                              SPOL = col_skip()
                            ))
  names(prebivalstvo)[1] <- "statisticna_regija"
  prebivalstvo <- prebivalstvo %>%
    tidyr::pivot_longer(-c("statisticna_regija"), 
                 names_to = "leto", 
                 values_to = "stevilo_prebivalcev") %>%
    dplyr::mutate(leto = stringr::str_replace_all(leto, " Starost - SKUPAJ", ""))
  return(prebivalstvo)
}

## ŠTEVILO GRADBENIH DOVOLJENJ
uvoz.stevilo.gradbenih.dovoljenj <- function(){
  stevilo.gradbenih.dovoljenj <- readr::read_csv2("podatki/dovoljenja-za-gradnjo.csv", 
                                           skip=2,
                                           locale=locale(encoding="Windows-1250"))
  names(stevilo.gradbenih.dovoljenj)[1] <- "statisticna_regija"
  names(stevilo.gradbenih.dovoljenj)[3] <- "tip_stavbe"
  stevilo.gradbenih.dovoljenj <- stevilo.gradbenih.dovoljenj %>%
                                  tidyr::pivot_longer(-c(statisticna_regija, INVESTITOR, tip_stavbe), 
                                               names_to = "x", 
                                               values_to = "vrednost") %>%
                                  tidyr::separate(col = "x",
                                    into = c("leto", "stevilo/povrsina_v_m2", "tip"),
                                    sep = c(" ", " "))
  stevilo.gradbenih.dovoljenj <- dplyr::select(stevilo.gradbenih.dovoljenj, 
                                        -c(INVESTITOR, tip_stavbe))
  return(stevilo.gradbenih.dovoljenj)
}

## INDEKSI CEN STANOVANJSKIH NEPREMIČNIN
uvoz.indeksi.cen.stan.nepremicnin <- function(){
  indeksi.cen.stan.nepremicnin <- readr::read_csv2("podatki/indeksi-cen-stanovanjskih-nepremicnin.csv", 
                                            skip=2,
                                            locale=locale(encoding="Windows-1250"),
                                            na= "...")
  names(indeksi.cen.stan.nepremicnin)[1] <- "Stanovanjske nepremičnine"
  indeksi.cen.stan.nepremicnin <- indeksi.cen.stan.nepremicnin %>% 
    tidyr::pivot_longer(!`Stanovanjske nepremičnine`, 
                 names_to = "leto", 
                 values_to = "povprecje_cetrtletij_glede_na_2015") %>%
    tidyr::separate(col = "leto",
             into = c("leto", "x"),
             sep = " ") %>%
    dplyr::select(-x)
  return(indeksi.cen.stan.nepremicnin)
}

## INDEKSI GRADBENIH STROŠKOV
uvoz.indeksi.gradbenih.stroskov <- function(){
  indeksi.gradbenih.stroskov <- readr::read_csv2("podatki/indeksi-gradbenih-stroskov.csv", 
                                          skip=2,
                                          locale=locale(encoding="Windows-1250"))
  indeksi.gradbenih.stroskov <- tidyr::separate(indeksi.gradbenih.stroskov,
                                         col = "ČETRTLETJE",
                                         into = c("leto", "četrtletje"),
                                         sep = "Q")
  indeksi.gradbenih.stroskov$leto <- as.integer(indeksi.gradbenih.stroskov$leto)
  nova.df <- data.frame(
    leto = indeksi.gradbenih.stroskov$leto,
    "stroški skupaj" = indeksi.gradbenih.stroskov$`Gradbeni stroški - SKUPAJ`,
    "stroški materiala" = indeksi.gradbenih.stroskov$`Stroški materiala`,
    "stroški dela" = indeksi.gradbenih.stroskov$`Stroški dela`
  )
  return(nova.df)
}

## INDEKSI CEN ŽIVLJENJSKIH POTREBŠČIN
uvoz.indeksi.cen.zivljenjskih.potrebscin <- function(){
  tabela <- readxl::read_excel("podatki/indeksi-cen-zivljenjskih-potrebscin.xlsx", 
                       col_types = c("guess", "numeric", "numeric"),
                       col_names = c("x", "leto", "indeks glede na leto 2015"),
                       skip = 3,
                       n_max = 11) %>%
    dplyr::select(-x)
  tabela$leto <- as.integer(tabela$leto)
  
  return(tabela)
} 
## OCENA DOKONČANIH STANOVANJ PO OBČINAH IN PO REGIJAH
uvoz.ocena.dokoncanih.stanovanj.po.obcinah <- function(){
  ocena.dokoncanih.stanovanj.po.obcinah <- readr::read_csv2("podatki/ocena-dokoncanih-stanovanj-po-obcinah.csv", 
                                                     skip=2,
                                                     locale=locale(encoding="Windows-1250"), 
                                                     na = "-") %>%
                                            tidyr::pivot_longer(-c("OBČINE", "MERITVE"), 
                                                         names_to = "x", 
                                                         values_to = "vrednosti") %>%
                                            tidyr::separate(col = "OBČINE",
                                                      into = c("obcine", "y"),
                                                      sep = "/") %>%
                                            tidyr::separate(col = "x",
                                                     into = c("leto", "vrsta"),
                                                     sep = " ") %>%
                                            dplyr::select(-y)
  return(ocena.dokoncanih.stanovanj.po.obcinah)
}

uvoz.ocena.dokoncanih.stanovanj.skupno.regije <- function(){
  ocena.dokoncanih.stanovanj.po.obcinah <- uvoz.ocena.dokoncanih.stanovanj.po.obcinah() %>% 
    dplyr::filter(vrsta == "Stanovanja") %>% 
    dplyr::select(-vrsta)
  ocena.dokoncanih.stanovanj.skupno.regije <- dplyr::right_join(ocena.dokoncanih.stanovanj.po.obcinah, 
                                                        uvoz.obcine.regije(),
                                                        by="obcine")
  ocena.dokoncanih.stanovanj.skupno.regije <- ocena.dokoncanih.stanovanj.skupno.regije %>%
    dplyr::group_by(statisticna_regija, leto, MERITVE) %>% 
    dplyr::summarise(vrednosti = sum(vrednosti, 
                              na.rm = TRUE))
  return(ocena.dokoncanih.stanovanj.skupno.regije)
}

## SELITVE PREBIVALSTVA
uvoz.selitve.prebivalstva <- function(){
  selitve.prebivalstva <- readr::read_csv2("podatki/selitve-prebivalstva.csv",
                                    skip=2,
                                    locale=locale(encoding="Windows-1250"))
  names(selitve.prebivalstva)[1] <- "statisticna_regija"
  selitve.prebivalstva <- selitve.prebivalstva %>% 
    tidyr::pivot_longer(!statisticna_regija,
                 names_to = "x",
                 values_to = "stevilo") %>%
    tidyr::separate(col = "x",
             into = c("leto", "priseljeni/odseljeni"),
             sep = " ")
  selitve.prebivalstva <- selitve.prebivalstva %>%
    tidyr::pivot_wider(names_from = "priseljeni/odseljeni",
                values_from = "stevilo")
  selitve.prebivalstva$leto <- as.integer(selitve.prebivalstva$leto)
  return(selitve.prebivalstva)
}

# KUPOPRODAJNI POSLI
uvoz.kupoprodajni.posli <- function(podatki1, podatki2){
  leto1 <- readr::read_csv2(podatki1, locale = locale(encoding = "UTF-8"))
  leto2 <- readr::read_csv2(podatki2, locale = locale(encoding = "UTF-8"))
  leto <- base::merge(leto1,
                leto2,
                by="ID Posla") %>% 
    dplyr::select(`Pogodbena cena / Odškodnina`, 
           `Občina`, 
           `Leto izgradnje dela stavbe`, 
           `Stavba je dokončana`, 
           `Dejanska raba dela stavbe`, 
           `Uporabna površina`)
  leto$`Dejanska raba dela stavbe` <- as.factor(leto$`Dejanska raba dela stavbe`)
  leto <- leto %>% dplyr::filter(`Dejanska raba dela stavbe` == str_subset(leto$`Dejanska raba dela stavbe`, 
                                                                    "^11[0-9]"), 
                          `Stavba je dokončana` == 1,
                          `Pogodbena cena / Odškodnina` >= 1,
                          `Uporabna površina` != 0,
                          `Leto izgradnje dela stavbe` != "NA",
                          `Leto izgradnje dela stavbe` >= 100
                          ) %>%
    dplyr::select(- c(`Stavba je dokončana`, 
               `Dejanska raba dela stavbe`))
  return(leto)
}


# TABELE

## TABELA 1

tabela.1 <- function(){
  tabela1.1 <- dplyr::full_join(uvoz.stevilo.gradbenih.dovoljenj(),
                       uvoz.prebivalstvo(),
                       by = c("statisticna_regija", "leto"))
  stevilo_stanovanj <- tabela1.1 %>% 
    dplyr::filter(tip == "stanovanj") %>%
    dplyr::select(c(statisticna_regija, 
             leto, 
             vrednost,
             stevilo_prebivalcev))
  names(stevilo_stanovanj)[3] <- "stevilo_stanovanj"
  povrsina_stanovanj <- tabela1.1 %>%
    dplyr::filter(`stevilo/povrsina_v_m2` == "Površina") %>%
    dplyr::select(c(statisticna_regija, 
             leto, 
             vrednost, 
             stevilo_prebivalcev))
  names(povrsina_stanovanj)[3] <- "povrsina_stanovanj"
  tabela1.1 <- dplyr::full_join(stevilo_stanovanj,
                       povrsina_stanovanj,
                       by= c("statisticna_regija", "leto", "stevilo_prebivalcev"))
  tabela1.2 <- dplyr::full_join(uvoz.ocena.dokoncanih.stanovanj.skupno.regije(), 
                        uvoz.prebivalstvo(), 
                        by = c("statisticna_regija", "leto")) %>%
    tidyr::pivot_wider(names_from = MERITVE, values_from = vrednosti) %>%
    dplyr::select(-"NA")
  names(tabela1.2)[4] <- "povrsina_ocena_dokoncanih"
  names(tabela1.2)[5] <- "stevilo_ocena_dokoncanih"
  tabela <- dplyr::full_join(tabela1.1,
                      tabela1.2,
                      by = c("leto", "statisticna_regija", "stevilo_prebivalcev"))
  tabela$statisticna_regija <- as.factor(tabela$statisticna_regija)
  tabela$leto <- as.integer(tabela$leto)
  return(tabela)
}

shrani.tabela1 <- tabela.1() %>% 
  dplyr::filter(statisticna_regija != "SLOVENIJA") %>%
  readr::write_csv("podatki/shrani-st-izdanih-gradb-dovoljenj-in-ocena-dokoncanih-stanovanj.csv", 
            na= "NA",
            append = FALSE,
            col_names = TRUE)

# TABELA 2
tabela.2 <- function(){
  tabela2.1 <- dplyr::full_join(uvoz.ocena.dokoncanih.stanovanj.po.obcinah(), 
                       uvoz.obcine.regije(),
                       by = c("obcine")) %>%
    dplyr::filter(obcine != "SLOVENIJA") %>%
    dplyr::select(-obcine) %>%
    dplyr::group_by(MERITVE, leto, vrsta, statisticna_regija) %>%
    dplyr::summarise(vrednosti = sum(vrednosti, na.rm = TRUE))
  
  tabela2.2 <- dplyr::full_join(tabela2.1,
                         uvoz.prebivalstvo(),
                         by = c("statisticna_regija", "leto")) %>%
    dplyr::filter(statisticna_regija != "SLOVENIJA")
  tabela2.2$leto <- as.integer(tabela2.2$leto)
  tabela2.2$vrsta <- factor(tabela2.2$vrsta, 
                            levels = c("Enosobna", 
                                       "Dvosobna", 
                                       "Trisobna", 
                                       "Štirisobna", 
                                       "Pet-", 
                                       "Stanovanja"),
                            labels = c("Enosobna", 
                                       "Dvosobna",
                                       "Trisobna", 
                                       "Štirisobna", 
                                       "Pet ali večsobna",
                                       "Stanovanja"))
  tabela2.2$statisticna_regija <- as.factor(tabela2.2$statisticna_regija)
  tabela2.2 <- tabela2.2 %>% 
    tidyr::pivot_wider(names_from = MERITVE, values_from = vrednosti)
  return(tabela2.2)
}  

shrani.tabela2 <- tabela.2() %>%
  readr::write_csv("podatki/shrani-ocena-dokoncanih-stanovanj-po-vrstah-stanovanj.csv",
            na= "NA",
            append = FALSE,
            col_names = TRUE)

# TABELA 3
tabela.3 <- function(){
  x.1 <- tabela.1() %>% 
    dplyr::filter(statisticna_regija == "SLOVENIJA") %>%
    dplyr::select(c(leto, stevilo_stanovanj))
  x.2 <- dplyr::full_join(x.1, 
                   uvoz.indeksi.cen.zivljenjskih.potrebscin(),
                   by = "leto")
  x.3 <- uvoz.indeksi.gradbenih.stroskov() %>%
    dplyr::group_by(`leto`) %>% 
    dplyr::summarize(`indeks skupnih stroškov` = mean(`stroški.skupaj`),
              `indeks stroškov materiala` = mean(`stroški.materiala`),
              `indeks stroškov dela` = mean(`stroški.dela`))
  tabela <- dplyr::full_join(x.2, x.3, by = "leto")
  tabela$`indeks glede na leto 2015` <- tabela$`indeks glede na leto 2015` * 10
  colnames(tabela) <- c("leto", 
                        "stevilo_stanovanj", 
                        "indeks cen življenjskih potrebščin", 
                        "indeks gradbenih stroškov (skupaj)", 
                        "indeks stroškov materiala", 
                        "indeks stroškov dela")
  return(tabela)
}

shrani.tabela3 <- tabela.3() %>%
  readr::write_csv("podatki/shrani-indeksi-gradbenih-stroskov-in-stroskov-zivljenjskih-potrebscin.csv",
            na= "NA",
            append = FALSE,
            col_names = TRUE)
# TABELA 4
tabela.4 <- function(){
  tabela4.1 <- uvoz.indeksi.cen.stan.nepremicnin()
  tabela4.1$leto <- as.integer(tabela4.1$leto)
  tabela4.1$`Stanovanjske nepremičnine` <- factor(tabela4.1$`Stanovanjske nepremičnine`,
                                                levels = c("1 Stanovanjske nepremičnine - SKUPAJ",
                                                           "1.1 Nove stanovanjske nepremičnine",
                                                           "1.1.1 Nova stanovanja",
                                                           "1.1.2 Nove družinske hiše",
                                                           "1.2 Rabljene stanovanjske nepremičnine",
                                                           "1.2.1 Rabljena stanovanja, Slovenija",
                                                           "1.2.1.1 Rabljena stanovanja, Ljubljana-občina",
                                                           "1.2.1.2 Rabljena stanovanja, preostala Slovenija",
                                                           "1.2.1.2.1 Rabljena stanovanja, Mestna obcina Maribor",
                                                           "1.2.1.2.2 Rabljena stanovanja, preostala Slovenija (brez mestnih obcin Ljubljana in Maribor)",
                                                           "1.2.2 Rabljene družinske hiše"),
                                                labels = c("1 Stanovanjske nepremičnine - SKUPAJ",
                                                           "Nove stanovanjske nepremičnine",
                                                           "Nova stanovanja",
                                                           "Nove družinske hiše",
                                                           "Rabljene stanovanjske nepremičnine",
                                                           "Rabljena stanovanja, Slovenija",
                                                           "Rabljena stanovanja, Ljubljana-občina",
                                                           "Rabljena stanovanja, preostala Slovenija",
                                                           "Rabljena stanovanja, Mestna obcina Maribor",
                                                           "Rabljena stanovanja, preostala Slovenija (brez mestnih obcin Ljubljana in Maribor)",
                                                           "Rabljene družinske hiše"))
  tabela4.1$povprecje_cetrtletij_glede_na_2015 <- tabela4.1$povprecje_cetrtletij_glede_na_2015 / 10
  return(tabela4.1)
}
shrani.tabela4 <- tabela.4() %>%
  readr::write_csv("podatki/shrani-indeksi-stan-nepremicnin.csv", 
            na= "NA", 
            append = FALSE, 
            col_names = TRUE)

# TABELA 5
tabela.5 <- function(){
  tabela5.1 <- shrani.tabela1 %>%
    dplyr::select(-stevilo_prebivalcev)
  tabela5.2 <- dplyr::full_join(tabela5.1, 
                         uvoz.selitve.prebivalstva()) %>%
    dplyr::filter(statisticna_regija != "SLOVENIJA")
  tabela5.2$statisticna_regija <- as.factor(tabela5.2$statisticna_regija)
  tabela5.2$leto <- as.integer(tabela5.2$leto)
  return(tabela5.2)
}

shrani.tabela5 <- tabela.5() %>%
  readr::write_csv("podatki/shrani-migracije-med-regijami.csv", 
            na= "NA", 
            append = FALSE, 
            col_names = TRUE)

# TABELA 6
leto2010 <- uvoz.kupoprodajni.posli(podatki1 = "podatki/2010-1.csv",
                                   podatki2 = "podatki/2010-2.csv")
leto2020 <- uvoz.kupoprodajni.posli(podatki1 = "podatki/2020-1.csv",
                                   podatki2 = "podatki/2020-2.csv")

povezava.obcine.regije <- data.frame(`statisticna_regija` = uvoz.obcine.regije()$statisticna_regija, 
                                     `Občina` = toupper(uvoz.obcine.regije()$obcine)) %>% 
  dplyr::mutate(`Občina` = stringr::str_replace_all(`Občina`, 
                                    " - ",
                                    "-"), 
         `Občina` = stringr::str_replace(`Občina`,
                                "SVETA TROJICA V SLOV. GORICAH\\*", 
                                "SV. TROJICA V SLOV. GORICAH"))

kupoprodajni.posli.2010 <- dplyr::left_join(leto2010, povezava.obcine.regije, by = "Občina") %>% 
  dplyr::select(-`Občina`)
kupoprodajni.posli.2010$`statisticna_regija` <- as.factor(kupoprodajni.posli.2010$`statisticna_regija`)
kupoprodajni.posli.2020 <- dplyr::left_join(leto2020, povezava.obcine.regije, by = "Občina") %>% 
  dplyr::filter(`Občina` != "NA") %>%
  dplyr::select(-`Občina`)
kupoprodajni.posli.2020$`statisticna_regija` <- as.factor(kupoprodajni.posli.2020$`statisticna_regija`)

  
povprecne.cene.na.kvadratni.meter.po.regijah <- function(posli){
  tabela <- posli %>% 
    dplyr::group_by(`statisticna_regija`) %>% 
    dplyr::mutate(`Cena na kvadratni meter` = `Pogodbena cena / Odškodnina` / `Uporabna površina`) %>%
    dplyr::summarize(`Povprečna cena/m2` = mean(`Cena na kvadratni meter`))
  return(tabela)
}

podatki.zemljevid1.1 <- povprecne.cene.na.kvadratni.meter.po.regijah(kupoprodajni.posli.2010) %>%
  tibble::add_column(leto = as.integer(2010))
podatki.zemljevid1.3 <- povprecne.cene.na.kvadratni.meter.po.regijah(kupoprodajni.posli.2020) %>%
  tibble::add_column(leto = as.integer(2020))

tabela6.1 <- podatki.zemljevid1.1 %>% 
  tibble::add_row(`statisticna_regija` = podatki.zemljevid1.3$`statisticna_regija`,
          `Povprečna cena/m2` = podatki.zemljevid1.3$`Povprečna cena/m2`,
          `leto` =  podatki.zemljevid1.3$`leto`)

povprecne.cene.na.kvadratni.meter.po.regijah.mlajse.od.10.let <- function(leto, posli){
  tabela <- posli %>% 
    dplyr::group_by(`statisticna_regija`) %>% 
    dplyr::filter(`Leto izgradnje dela stavbe` >= leto - 10) %>%
    dplyr::mutate(`Cena na kvadratni meter` = `Pogodbena cena / Odškodnina` / `Uporabna površina`) %>%
    dplyr::summarize(`Povprečna cena/m2 (mlajše od 10let)` = mean(`Cena na kvadratni meter`))
  return(tabela)
}

podatki.zemljevid2.1 <- povprecne.cene.na.kvadratni.meter.po.regijah.mlajse.od.10.let(2010, kupoprodajni.posli.2010) %>%
  tibble::add_column(leto = as.integer(2010))
podatki.zemljevid2.3 <- povprecne.cene.na.kvadratni.meter.po.regijah.mlajse.od.10.let(2020, kupoprodajni.posli.2020) %>%
  tibble::add_column(leto = as.integer(2020))

tabela6.2 <- podatki.zemljevid2.1 %>%
  tibble::add_row(`statisticna_regija` = podatki.zemljevid2.3$`statisticna_regija`,
          `Povprečna cena/m2 (mlajše od 10let)` = podatki.zemljevid2.3$`Povprečna cena/m2 (mlajše od 10let)`,
          `leto` =  podatki.zemljevid2.3$`leto`)


povprecna.starost.oddanih.nepremicnin.po.regijah <- function(posli){
  tabela <- posli %>%
    dplyr::group_by(`statisticna_regija`) %>%
    dplyr::summarize(`Povprečna starost stanovanjske nepremičnine` = round(mean(`Leto izgradnje dela stavbe`)))
  return(tabela)
}

podatki.zemljevid3.1 <- povprecna.starost.oddanih.nepremicnin.po.regijah(kupoprodajni.posli.2010) %>%
  tibble::add_column(leto = as.integer(2010))
podatki.zemljevid3.3 <- povprecna.starost.oddanih.nepremicnin.po.regijah(kupoprodajni.posli.2020) %>%
  tibble::add_column(leto = as.integer(2020))

tabela6.3 <- podatki.zemljevid3.1 %>% 
  tibble::add_row(`statisticna_regija` = podatki.zemljevid3.3$`statisticna_regija`,
          `Povprečna starost stanovanjske nepremičnine` = podatki.zemljevid3.3$`Povprečna starost stanovanjske nepremičnine`,
          `leto` =  podatki.zemljevid3.3$`leto`)

shrani.tabela6 <- dplyr::full_join(tabela6.1, tabela6.2, tabela6.3, by = c("statisticna_regija", "leto")) %>%
  readr::write_csv("podatki/shrani-kupoprodajni-posli.csv", 
            na= "NA", 
            append = FALSE, 
            col_names = TRUE)

# TABELA 7 - DODATNA TABELA - ZA PRIMERJAVO MED REGIJAMI
uvoz.kako.gospodinjstva.prezivijo.s.svojimi.prihodki <- function(){
  tabela <- readr::read_csv2("podatki/kako-gospodinjstva-prezivijo-s-svojimi-prihodki.csv", 
                      skip=2,
                      locale=locale(encoding="Windows-1250")
                      ) %>%
    tidyr::pivot_longer(-c(`STATISTIČNA REGIJA`, LETO),
                 names_to = "Stopnja",
                 values_to = "Delež ljudi")
  tabela$`STATISTIČNA REGIJA` <- as.factor(tabela$`STATISTIČNA REGIJA`)
  tabela$LETO <- as.integer(tabela$LETO)
  tabela$Stopnja <- factor(tabela$Stopnja, levels = c("Zelo težko", 
                                                      "Težko",
                                                      "Z manjšimi težavami",
                                                      "Dokaj lahko", 
                                                      "Lahko",
                                                      "Zelo lahko"))
  return(tabela)
}

shrani.tabela7 <- uvoz.kako.gospodinjstva.prezivijo.s.svojimi.prihodki() %>% readr::write_csv("podatki/shrani-kako-gospodinjstva-prezivijo-s-svojimi-prihodki.csv", 
                                                                                       na= "NA", 
                                                                                       append = FALSE, 
                                                                                       col_names = TRUE)
