# 3. faza: Vizualizacija podatkov
graf.kako.gospodinjstva.prezivijo.s.svojimi.prihodki.po.regijah <- shrani.tabela7 %>%
  filter(LETO == 2020) %>%
  ggplot(
    aes(x = `STATISTIČNA REGIJA`,y = `Delež ljudi`, fill = Stopnja)
  ) +
  geom_bar(
    stat = "summary", fun = median, position = position_fill(reverse = TRUE)
  ) +
  labs(title = "Kako gospodinjstva preživijo s svojimi prihodki",
       x = "Statistične regije",
       y = "Delež",
       fill = "Stopnja"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8))

graf.stevilo.izdanih.gradbenih.dovoljenj.na.1000.prebivalcev.po.regijah <- shrani.tabela1 %>%
  ggplot(
    mapping = aes(x = leto, 
                  y = 1000 * stevilo_stanovanj/stevilo_prebivalcev, 
                  fill = statisticna_regija)
  ) +
  geom_bar(stat="identity", 
           position = position_dodge(width = 0.75)) +
  labs(title = "Število izdanih gradbenih dovoljenj na 1000 prebivalcev po regijah",
       x = "Leto",
       y = "Število/1000 prebivalcev",
       fill = "Statistična regija"
       ) +
  theme_bw() +
  scale_x_continuous(breaks = c(2010:2020)) +
  theme(axis.text.x = element_text(angle = 90, size = 8))

graf.ocena.dokoncanih.stanovanj.na.1000.prebivalcev.po.regijah <- shrani.tabela1 %>%
  filter(stevilo_ocena_dokoncanih != "Posavska") %>%  # Ni podatkov za Posavsko regijo
  ggplot(
    mapping = aes(x = leto,
                  y = 1000 * stevilo_ocena_dokoncanih/stevilo_prebivalcev,
                  fill = statisticna_regija)
  ) +
  geom_bar(stat="identity", 
           position = position_dodge(width = 0.75), 
           na.rm = FALSE) +
  labs(title = "Ocena dokončanih stanovanj na 1000 prebivalcev po regijah", 
       y = "Ocena/1000 prebivalcev",
       fill = "Statistična regija",
       caption = "(Brez Posavske regije)") + 
  scale_x_continuous(breaks = c(2010:2020)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8))

graf.indeksi <- shrani.tabela3 %>% 
  pivot_longer(!leto, names_to = "vrsta", values_to = "stevilo" ) %>%
  filter(vrsta != "stevilo_stanovanj") %>%
  ggplot(
    mapping = aes(x = leto, y = stevilo, color = vrsta)
  )+ 
  geom_point() +
  geom_line(linetype = 2) +
  scale_x_continuous(breaks = c(2010:2020)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  labs(title = "Primerjava indeksa cen življenjskih potrebščin z gradbenimi indeksi",
       x = "Leto",
       y = "Indeks (leto 2015 = 1000)",
       fill = "Vrsta indeksa"
  )

graf.gradbena.dovoljenja <- shrani.tabela3 %>%
  select(leto, stevilo_stanovanj) %>%
  ggplot(
    mapping = aes(x = leto, y = stevilo_stanovanj)
  ) +
  geom_line(linetype = 2) +
  geom_point() +
  scale_x_continuous(breaks = c(2010:2020)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  labs(title = "Število izdanih gradbenih dovoljenj za stanovanjske objekte",
       x = "Leto",
       y = "Število izdanih gradbenih dovoljenj"
  )

graf.stevilo.gradbenih.dovoljenj.po.vrstah <- shrani.tabela2 %>%
  filter(vrsta != "Stanovanja") %>%
  group_by(leto, statisticna_regija) %>%
  ggplot(
    mapping = aes(x = vrsta, y = 1000 * `Število`/stevilo_prebivalcev)
  ) +
  labs(title = "Število izdanih gradbenih dovoljenj na 1000 prebivalcev po vrstah stanovanj",
       y = "Število/1000 prebivalcev",
       x = "Vrsta") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.75)) +
  facet_wrap(~ statisticna_regija)

graf.povprecna.povrsina.stanovanj.po.vrstah <- shrani.tabela2 %>%
  filter(vrsta != "Stanovanja") %>%
  ggplot(
    mapping = aes(x = vrsta, y = `Površina [m2]` / `Število`, fill = statisticna_regija)
  ) +
  geom_boxplot() +
  labs(title = "Povprečna površina stanovanjskih objektov glede na vrsto",
       y = "Površina [m2]",
       x = "Vrsta") 

shrani.tabela5 %>%
  group_by(leto, statisticna_regija) %>%
  filter(statisticna_regija == "Pomurska") %>%
  ggplot(
    mapping = aes(x = leto, y = stevilo_ocena_dokoncanih/(Priseljeni - Odseljeni), fill = Priseljeni - Odseljeni)
  ) +
  labs(title = "Ocena števila dokončanih stanovanj glede na razliko med priseljenimi in odseljenimi v Pomurski regiji",
       y = "Ocena števila dokončanih stanovanj/|(Priseljeni - Odseljeni)|") +
  scale_x_continuous(breaks = c(2010:2020)) +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  geom_bar(stat = "identity")

funkcija.grafa.tabele.4 <- function(podatek1, podatek2, naslov){
  shrani.tabela4 %>% 
    filter(`Stanovanjske nepremičnine` ==  podatek1 |
             `Stanovanjske nepremičnine` == podatek2
           ) %>%
    ggplot(
      aes(x = leto, y = povprecje_cetrtletij_glede_na_2015, color = `Stanovanjske nepremičnine`)
    ) +
    geom_point() +
    geom_line(linetype = 1) +
    labs(title = naslov,
         x = "Leto",
         y = "Indeks (leto 2015 = 1000)") +
    scale_x_continuous(breaks = c(2010:2020)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 8))
}

graf.nove.stare.indeks <- funkcija.grafa.tabele.4("Nove stanovanjske nepremičnine", 
                                           "Rabljene stanovanjske nepremičnine",
                                           "Indeks cen novih in rabljenih stanovanjskih nepremičnin")

graf.nova.stanovanja.in.hise.indeks <- funkcija.grafa.tabele.4("Nova stanovanja", 
                                                        "Nove družinske hiše",
                                                        "Indeks cen novih stanovanj in novih družinskih hiš")

graf.rabljena.stanovanja.in.hise.indeks <- funkcija.grafa.tabele.4("Rabljena stanovanja, Slovenija", 
                                                                   "Rabljene družinske hiše",
                                                                    "Indeks cen rabljenih stanovanj in rabljenih družinskih hiš")

graf.rabljena.stanovanja.v.lj.in.preostali.slo.indeks <- funkcija.grafa.tabele.4("Rabljena stanovanja, preostala Slovenija", 
                                                               "Rabljena stanovanja, Ljubljana-občina",
                                                               "Indeks cen rabljenih stanovanj v Ljubljanski občini in v preostali Sloveniji")


zemljevid <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                             "SVN_adm1", mapa = "zemljevid", encoding = "UTF-8")

zemljevid$NAME_1 <- c("Gorenjska", "Goriška","Jugovzhodna Slovenija", "Koroška", "Primorsko-notranjska", "Obalno-kraška", "Osrednjeslovenska", "Podravska", "Pomurska", "Savinjska", "Posavska", "Zasavska")
zemljevid <- fortify(zemljevid)
names(zemljevid)[12] <- "statisticna_regija"


zemljevid.povprecna.cena.na.kvadratni.meter <- function(podatek){
  zemljevid <- shrani.tabela6 %>%
    left_join(zemljevid, by = "statisticna_regija") %>%
    filter(leto == podatek) %>%
    ggplot() +
    geom_polygon(
      mapping = aes(long, lat, group = group, fill = `Povprečna cena/m2`),
      color = "grey"
    ) +
    coord_map() +
    scale_fill_binned() +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.background = element_rect(colour = "grey", fill = "white")
    ) +
    labs(title = paste0("Povprečna cena prodanega stanovanjskega objekta za kvadratni meter v letu ", podatek))
  return(zemljevid)
}

zemljevid.povprecna.cena.na.kvadratni.meter.2010 <- zemljevid.povprecna.cena.na.kvadratni.meter("2010")
zemljevid.povprecna.cena.na.kvadratni.meter.2020 <- zemljevid.povprecna.cena.na.kvadratni.meter("2020")
