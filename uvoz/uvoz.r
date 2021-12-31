# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

pretvornik.regij <- function(){ # 2 regiji (Posavska in Jugovzhodna Slovenija) imata različno poimenovanje v različnih virih
  regije.slo = tibble(
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
  stran <- session(link) %>% 
    read_html()
  tabela <- stran %>% 
    html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",") %>% 
    select("Statistična regija", "Občina")
  tabela[162, 2] <- "Sveta Trojica v Slov. goricah*"
  tabela[163, 2] <- "Sveti Andraž v Slov. goricah"
  tabela[165, 2] <- "Sveti Jurij v Slov. goricah"
  tabela[62, 2] <- "Kanal"
  names(tabela)[1] <- "regija"
  names(tabela)[2] <- "obcine"
  tabela <- right_join(pretvornik.regij(), 
                       tabela, 
                       by="regija") %>% 
    select(-regija)   # spremenjeno poimenovanje dveh regij - zdaj se ujema z ostalimi podatki
  return(tabela)
}

## PREBIVALSTVO
uvoz.prebivalstvo <- function(){
  prebivalstvo <- read_csv2("podatki/prebivalstvo.csv", skip=2,
                            locale=locale(encoding="Windows-1250"),
                            col_types = cols(
                              .default = col_guess(),
                              SPOL = col_skip()
                            ))
  names(prebivalstvo)[1] <- "statisticna_regija"
  prebivalstvo <- prebivalstvo %>%
    pivot_longer(-c("statisticna_regija"), names_to = "leto", values_to = "stevilo_prebivalcev") %>%
    mutate(leto = str_replace_all(leto, " Starost - SKUPAJ" , ""))
  return(prebivalstvo)
}

## ŠTEVILO GRADBENIH DOVOLJENJ
uvoz.stevilo.gradbenih.dovoljenj <- function(){
  stevilo.gradbenih.dovoljenj <- read_csv2("podatki/dovoljenja-za-gradnjo.csv", 
                                           skip=2,
                                           locale=locale(encoding="Windows-1250"))
  names(stevilo.gradbenih.dovoljenj)[1] <- "statisticna_regija"
  names(stevilo.gradbenih.dovoljenj)[3] <- "tip_stavbe"
  stevilo.gradbenih.dovoljenj <- stevilo.gradbenih.dovoljenj %>%
                                  pivot_longer(-c(statisticna_regija, INVESTITOR, tip_stavbe), names_to = "x", values_to = "vrednost") %>%
                                  separate(col = "x",
                                    into = c("leto", "stevilo/povrsina_v_m2", "tip"),
                                    sep = c(" ", " "))
  stevilo.gradbenih.dovoljenj <- select(stevilo.gradbenih.dovoljenj, -c(INVESTITOR, tip_stavbe))
  return(stevilo.gradbenih.dovoljenj)
}

## INDEKSI CEN STANOVANJSKIH NEPREMIČNIN
uvoz.indeksi.cen.stan.nepremicnin <- function(){
  indeksi.cen.stan.nepremicnin <- read_csv2("podatki/indeksi-cen-stanovanjskih-nepremicnin.csv", 
                                            skip=2,
                                            locale=locale(encoding="Windows-1250"),
                                            na= "...")
  names(indeksi.cen.stan.nepremicnin)[1] <- "stanovanjske_nepremicnine"
  indeksi.cen.stan.nepremicnin <- indeksi.cen.stan.nepremicnin %>% 
    pivot_longer(!stanovanjske_nepremicnine, 
                 names_to = "leto", 
                 values_to = "povprecje_cetrtletij_glede_na_2015") %>%
    separate(col = "leto",
             into = c("leto", "x"),
             sep = " ") %>%
    select(-x)
  return(indeksi.cen.stan.nepremicnin)
}

## INDEKSI GRADBENIH STROŠKOV
uvoz.indeksi.gradbenih.stroskov <- function(){
  indeksi.gradbenih.stroskov <- read_csv2("podatki/indeksi-gradbenih-stroskov.csv", 
                                          skip=2,
                                          locale=locale(encoding="Windows-1250"))
  return(indeksi.gradbenih.stroskov)
}

## OCENA DOKONČANIH STANOVANJ PO OBČINAH IN PO REGIJAH
uvoz.ocena.dokoncanih.stanovanj.po.obcinah <- function(){
  ocena.dokoncanih.stanovanj.po.obcinah <- read_csv2("podatki/ocena-dokoncanih-stanovanj-po-obcinah.csv", 
                                                     skip=2,
                                                     locale=locale(encoding="Windows-1250"), 
                                                     na = "-") %>%
                                            pivot_longer(-c("OBČINE", "MERITVE"), 
                                                         names_to = "x", 
                                                         values_to = "vrednosti") %>%
                                            separate(col = "OBČINE",
                                                      into = c("obcine", "y"),
                                                      sep = "/") %>%
                                            separate(col = "x",
                                                     into = c("leto", "vrsta"),
                                                     sep = " ") %>%
                                            select(-y)
  return(ocena.dokoncanih.stanovanj.po.obcinah)
}

uvoz.ocena.dokoncanih.stanovanj.skupno.regije <- function(){
  ocena.dokoncanih.stanovanj.po.obcinah <- uvoz.ocena.dokoncanih.stanovanj.po.obcinah() %>% 
    filter(vrsta == "Stanovanja") %>% 
    select(-vrsta)
  ocena.dokoncanih.stanovanj.skupno.regije <- right_join(ocena.dokoncanih.stanovanj.po.obcinah, 
                                                        uvoz.obcine.regije(),
                                                        by="obcine")
  ocena.dokoncanih.stanovanj.skupno.regije <- ocena.dokoncanih.stanovanj.skupno.regije %>%
    group_by(statisticna_regija, leto, MERITVE) %>% 
    summarise(vrednosti = sum(vrednosti, 
                              na.rm = TRUE))
  return(ocena.dokoncanih.stanovanj.skupno.regije)
}

## SELITVE PREBIVALSTVA
uvoz.selitve.prebivalstva <- function(){
  selitve.prebivalstva <- read_csv2("podatki/selitve-prebivalstva.csv",
                                    skip=2,
                                    locale=locale(encoding="Windows-1250"))
  names(selitve.prebivalstva)[1] <- "statisticna_regija"
  selitve.prebivalstva <- selitve.prebivalstva %>% 
    pivot_longer(!statisticna_regija,
                 names_to = "x",
                 values_to = "stevilo") %>%
    separate(col = "x",
             into = c("leto", "priseljeni/odseljeni"),
             sep = " ")
  selitve.prebivalstva <- selitve.prebivalstva %>%
    pivot_wider(names_from = "priseljeni/odseljeni",
                values_from = "stevilo")
  return(selitve.prebivalstva)
}


# TABELE

## TABELA 1

tabela11 <- function(){
  tabela1 <- full_join(uvoz.stevilo.gradbenih.dovoljenj(),
                       uvoz.prebivalstvo(),
                       by = c("statisticna_regija", "leto"))
  stevilo_stanovanj <- tabela1 %>% 
    filter(tip == "stanovanj") %>%
    select(c(statisticna_regija, leto, vrednost, stevilo_prebivalcev))
  names(stevilo_stanovanj)[3] <- "stevilo_stanovanj"
  povrsina_stanovanj <- tabela1 %>%
    filter(`stevilo/povrsina_v_m2` == "Površina") %>%
    select(c(statisticna_regija, leto, vrednost, stevilo_prebivalcev))
  names(povrsina_stanovanj)[3] <- "povrsina_stanovanj"
  tabela1 <- full_join(stevilo_stanovanj,
                       povrsina_stanovanj,
                       by= c("statisticna_regija", "leto", "stevilo_prebivalcev"))
  return(tabela1)
}
tabela111 <- function(){
  tabela11 <- full_join(uvoz.ocena.dokoncanih.stanovanj.skupno.regije(), 
                        uvoz.prebivalstvo(), 
                        by = c("statisticna_regija", "leto")) %>%
    pivot_wider(names_from = MERITVE, values_from = vrednosti) %>%
    select(-"NA")
  names(tabela11)[4] <- "povrsina_ocena_dokoncanih"
  names(tabela11)[5] <- "stevilo_ocena_dokoncanih"
  return(tabela11)
}

shrani.tabela1 <- full_join(tabela11(),
                            tabela111(),
                            by = c("leto", "statisticna_regija", "stevilo_prebivalcev")) %>% 
  filter(statisticna_regija != "SLOVENIJA") %>%
  write_csv("podatki/shrani-st-izdanih-gradb-dovoljenj-in-ocena-dokoncanih-stanovanj.csv", 
            na= "NA", 
            append = FALSE, 
            col_names = TRUE)

# TABELA 2
tabela2 <- function(){
  tabela2 <- full_join(uvoz.ocena.dokoncanih.stanovanj.po.obcinah(), 
                       uvoz.obcine.regije(),
                       by = c("obcine")) %>%
    filter(obcine != "SLOVENIJA") %>%
    select(-obcine) %>%
    group_by(MERITVE, leto, vrsta, statisticna_regija) %>%
    summarise(vrednosti = sum(vrednosti, na.rm = TRUE))
  return(tabela2)
}  
shrani.tabela2 <- tabela2 %>% write_csv("podatki/shrani-ocena-dokoncanih-stanovanj-po-vrstah-stanovanj.csv",
                            na= "NA",
                            append = FALSE,
                            col_names = TRUE)

# TABELA 3

# TABELA 4
shrani.tabela4 <- uvoz.indeksi.cen.stan.nepremicnin() %>%
  write_csv("podatki/shrani-indeksi-stan-nepremicnin.csv", na= "NA", append = FALSE, col_names = TRUE)

# TABELA 5
