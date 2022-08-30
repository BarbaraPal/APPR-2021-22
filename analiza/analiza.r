# 4. faza: Napredna analiza podatkov

##################################################################################################
# LINEARNA REGRESIJA 
##################################################################################################

podatki.za.linearno.regresijo <- tibble(shrani.tabela3[2], shrani.tabela3[4])
names(podatki.za.linearno.regresijo)[2] <- 'gradbeni_stroski' 
  
g <- ggplot(podatki.za.linearno.regresijo, aes(x=gradbeni_stroski, y=stevilo_stanovanj)) + geom_point() + 
  labs(title = "Število izdanih gradbenih dovoljenj glede na indeks gradbenih stroškov") + xlab("Indeks gradbenih stroškov") + ylab("Število izdanih gradbenih dovoljenj") + 
  theme(axis.line = element_line(colour = "blue"))+
  theme_bw()

lin <- lm(data=podatki.za.linearno.regresijo, stevilo_stanovanj ~ gradbeni_stroski)
lin
napovedi <- predict(lin, tibble(gradbeni_stroski=seq(min(podatki.za.linearno.regresijo$gradbeni_stroski), max(podatki.za.linearno.regresijo$gradbeni_stroski))))

g + geom_line(data = tibble(gradbeni_stroski=seq(min(podatki.za.linearno.regresijo$gradbeni_stroski), max(podatki.za.linearno.regresijo$gradbeni_stroski)), napovedi = napovedi),
              aes(x = gradbeni_stroski, y = napovedi), col = "blue")

kvadrat <- lm(data=podatki.za.linearno.regresijo, stevilo_stanovanj ~ I((gradbeni_stroski)^2))
kvadrat
g + geom_smooth(method="lm", formula = y ~ x + I(x^2), color="blue")

z <- lowess(podatki.za.linearno.regresijo$gradbeni_stroski, podatki.za.linearno.regresijo$stevilo_stanovanj)
g + geom_line(data=as.data.frame(z), aes(x=x, y=y), color="blue")

mls <- loess(data=podatki.za.linearno.regresijo, stevilo_stanovanj ~ (gradbeni_stroski))
g + geom_smooth(method="loess", color = "blue")

sapply(list(lin, kvadrat, mls, z), function(x) mean((x$residuals^2)))  

which.min(sapply(list(lin, kvadrat, mls), function(x) mean((x$residuals^2)))) # tu sem ugotovila da ima najmanjšo napako mls in zato v končnem grafu izberemo metoso loess

analiziran.graf <- g + geom_smooth(method="loess", formula = y ~ x, color = "blue")

##################################################################################################
# RAZVRŠČANJE V SKUPINE 
##################################################################################################

podatki.za.razvrscanje <- shrani.tabela1 %>%
  group_by(statisticna_regija) %>%
  dplyr::summarize(stevilo_stanovanj = sum(stevilo_stanovanj), stevilo_ocena_dokoncanih = sum(stevilo_ocena_dokoncanih))

# funkcije uporabljene pri razvrščanju v skupine

hc.kolena = function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  # k.visina je tabela s štirimi stolpci
  # (1) k, število skupin
  # (2) višina združevanja
  # (3) sprememba višine pri združevanju
  # (4) koleno: ali je točka koleno?
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    # sprememba višine
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    # ali se je intenziteta spremembe dovolj spremenila?
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}

hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr::select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

diagram.kolena = function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "black"
    )+
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "black"
    )+
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "red", size = 2.5
    )+
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}

obrisi = function(podatki, hc = TRUE, od = 2, do = NULL) {
  n = nrow(podatki)
  if (is.null(do)) {
    do = n - 1
  }
  razdalje = dist(podatki)
  k.obrisi = tibble()
  for (k in od:do) {
    if (hc) {
      o.k = hclust(razdalje) %>%
        cutree(k) %>%
        silhouette(razdalje)
    } else {
      set.seed(42) # zato, da so rezultati ponovljivi
      o.k = kmeans(podatki, k)$cluster %>%
        silhouette(razdalje)
    }
    k.obrisi = k.obrisi %>% bind_rows(
      tibble(
        k = rep(k, n),
        obrisi = o.k[, "sil_width"]
      )
    )
  }
  k.obrisi$k = as.ordered(k.obrisi$k)
  k.obrisi
}

obrisi.povprecje = function(k.obrisi) {
  k.obrisi.povprecje = k.obrisi %>%
    group_by(k) %>%
    summarize(obrisi = mean(obrisi))
}

obrisi.k = function(k.obrisi) {
  obrisi.povprecje(k.obrisi) %>%
    filter(obrisi == max(obrisi)) %>%
    summarize(k = min(k)) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

diagram.obrisi = function(k.obrisi) {
  ggplot() +
    geom_boxplot(
      data = k.obrisi,
      mapping = aes(x = k, y = obrisi)
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = k, y = obrisi),
      color = "red"
    ) +
    geom_line(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = as.integer(k), y = obrisi),
      color = "red"
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi) %>%
        filter(obrisi == max(obrisi)) %>%
        filter(k == min(k)),
      mapping = aes(x = k, y = obrisi),
      color = "blue"
    ) +
    xlab("število skupin (k)") +
    ylab("obrisi (povprečje obrisov)") +
    ggtitle(paste("Maksimalno povprečje obrisov pri k =", obrisi.k(k.obrisi))) +
    theme_classic() 
}

# razvrščanje po metodi k-tih voditeljev

r.hc = podatki.za.razvrscanje[, -1] %>% obrisi(hc = TRUE)
r.km = podatki.za.razvrscanje[, -1] %>% obrisi(hc = FALSE)

r.hc.plt <- diagram.obrisi(r.hc)
r.km.plt <- diagram.obrisi(r.km)  

# obe predlagata k = 5
diagram.skupine = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    dplyr::rename(skupina = ...4)
  
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = stevilo_stanovanj, y = stevilo_ocena_dokoncanih, color = skupina
      )
    ) +
    geom_point() +
    geom_label(label = oznake, size = 2) +
    scale_color_hue() +
    theme_classic()
  
  for (i in 1:k) {
    d = d + geom_encircle(
      data = podatki %>%
        filter(skupina == i)
    )
  }
  d
}

set.seed(72)
k = 5
skupine = podatki.za.razvrscanje[, -1] %>%
  kmeans(centers = k) %>%
  getElement("cluster") %>%
  as.ordered()
diagram5 <- diagram.skupine(podatki.za.razvrscanje, podatki.za.razvrscanje$`statisticna_regija`, skupine, k)


prostorski.diagram.skupine = function(drzave, skupine, k) {
  drzave %>%
    bind_cols(skupine) %>%
    dplyr::select(`statisticna_regija` = `statisticna_regija`, skupina = ...4) %>%
    left_join(
      zemljevid,
      by = "statisticna_regija"
    ) %>%
    ggplot() +
    geom_polygon(
      mapping = aes(long, lat, group = statisticna_regija , fill = skupina),
      color = "grey"
    ) +
    scale_fill_brewer() +
    coord_map() +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

set.seed(73)

k = 5
skupine = podatki.za.razvrscanje[, -1] %>%
  kmeans(centers = k) %>%
  getElement("cluster") %>%
  as.ordered()
diagram.zemljevid5 <- prostorski.diagram.skupine(podatki.za.razvrscanje, skupine, k)

# hierarhično razvrščanje

X <- podatki.za.razvrscanje[,-1] %>% as.matrix() %>% scale()
dendrogram  <- dist(X) %>% hclust(method = "ward.D")

r <-  hc.kolena(dendrogram)
diagram.kolena <- diagram.kolena(r) # kolena so pri 2, 3, 4

set.seed(73)
k = 2
skupine <- podatki.za.razvrscanje[, -1] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()

tabela.skupine.hierarh <- podatki.za.razvrscanje %>%
  mutate(Skupine = as.numeric(skupine))

prostorski.diagram.hierarhicno2 <- ggplot() +
  aes(x = long, y = lat, group = group, fill = factor(Skupine)) +
  geom_polygon(data = tabela.skupine.hierarh %>% 
                 right_join(zemljevid, by = "statisticna_regija"),
               color = 'grey') +
  scale_fill_brewer() +
  coord_map() +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

set.seed(73)
k = 3
skupine <- podatki.za.razvrscanje[, -1] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()

tabela.skupine.hierarh <- podatki.za.razvrscanje %>%
  mutate(Skupine = as.numeric(skupine))

prostorski.diagram.hierarhicno3 <- ggplot() +
  aes(x = long, y = lat, group = group, fill = factor(Skupine)) +
  geom_polygon(data = tabela.skupine.hierarh %>% 
                 right_join(zemljevid, by = "statisticna_regija"),
               color = 'grey') +
  scale_fill_brewer() +
  coord_map() +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

set.seed(73)
k = 4
skupine <- podatki.za.razvrscanje[, -1] %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  cutree(k = k) %>%
  as.ordered()

tabela.skupine.hierarh <- podatki.za.razvrscanje %>%
  mutate(Skupine = as.numeric(skupine))

prostorski.diagram.hierarhicno4 <- ggplot() +
  aes(x = long, y = lat, group = group, fill = factor(Skupine)) +
  geom_polygon(data = tabela.skupine.hierarh %>% 
                 right_join(zemljevid, by = "statisticna_regija"),
               color = 'grey') +
  scale_fill_brewer() +
  coord_map() +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

##################################################################################################
# NAPOVEDNI MODEL
##################################################################################################

# funkcije
ucenje = function(podatki, formula, algoritem) {
  switch(
    algoritem,
    lin.reg = lm(formula, data = podatki),
    log.reg = glm(formula, data = podatki, family = "binomial"),
    ng = ranger(formula, data = podatki)
  )
}

napovedi = function(podatki, model, algoritem) {
  switch(
    algoritem,
    lin.reg = predict(model, podatki),
    log.reg = ifelse(
      predict(model, podatki, type = "response") >= 0.5,
      1, -1
    ),
    ng = predict(model, podatki)$predictions
  )
}

napaka_regresije = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(yn.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (yn - yn.hat) ^ 2
    ) %>%
    dplyr::select(izguba) %>%
    unlist() %>%
    mean()
}

napaka_razvrscanja = function(podatki, model, algoritem) {
  podatki %>%
    bind_cols(yd.hat = napovedi(podatki, model, algoritem)) %>%
    mutate(
      izguba = (yd != yd.hat)
    ) %>%
    dplyr::select(izguba) %>%
    unlist() %>%
    mean()
}

# Razreži vektor x na k enako velikih kosov
razbitje = function(x, k) {
  # Razreži indekse vektorja na k intervalov
  razrez = cut(seq_along(x), k, labels = FALSE)
  # Razbij vektor na k seznamov
  # na osnovi razreza intervalov
  split(x, razrez)
}

pp.razbitje = function(n, k = 5, stratifikacija = NULL, seme = NULL) {
  # najprej nastavimo seme za naključna števila, če je podano
  if (!is.null(seme)) {
    set.seed(seme)
  }
  # če ne opravljamo stratifikacije, potem vrnemo navadno razbitje
  # funkcijo sample uporabimo zato, da naključno premešamo primere
  if (is.null(stratifikacija)) {
    return(razbitje(sample(1:n), k))
  }
  # če pa opravljamo stratifikacijo, razbitje izvedemo za vsako
  # vrednost spremenljive stratifikacija posebej in nato
  # podmnožice združimo v skupno razbitje
  r = NULL
  for (v in levels(stratifikacija)) {
    # Če smo pri prvi vrednosti vzpostavimo razbitje
    if (is.null(r)) {
      # opravimo razbitje samo za primere z vrednostjo v
      r = razbitje(sample(which(stratifikacija == v)), k)
    } else {
      # opravimo razbitje za vrednost v
      # in podmnožice združimo s trenutnim razbitjem
      r.v = razbitje(sample(which(stratifikacija == v)), k)
      for (i in 1:k) {
        r[[i]] = c(r[[i]], r.v[[i]])
      }
    }
  }
  r
}

precno.preverjanje = function(podatki, razbitje, formula, algoritem, razvrscanje) {
  # pripravimo vektor za napovedi
  if (razvrscanje) {
    pp.napovedi = factor(rep(1, nrow(podatki)), levels = c(-1,1))
  } else {
    pp.napovedi = rep(0, nrow(podatki))
  }
  # gremo čez vse podmnožice Si razbitja S
  for (i in 1:length(razbitje)) {
    # naučimo se modela na množici S \ Si
    model = podatki[ -razbitje[[i]], ] %>% ucenje(formula, algoritem)
    # naučen model uporabimo za napovedi na Si
    pp.napovedi[ razbitje[[i]] ] = podatki[ razbitje[[i]], ] %>% napovedi(model, algoritem)
  }
  if (razvrscanje) {
    mean(pp.napovedi != podatki$pricakov)
  } else {
    mean((pp.napovedi - podatki$pricakov) ^ 2)
  }
}

# Funkciji Predictor$new moramo povedati kako napovedujemo z modelom naključnih gozdov 
pfun = function(model, newdata) {
  predict(model, data = newdata, predict.all = FALSE)$predictions
}

zamakni <- function(x, n){c(rep(NA, n), x)[1:length(x)]}


# IZBIRA MODELA
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
osrednjeslovenska <- shrani.tabela1 %>% filter(statisticna_regija == 'Osrednjeslovenska')

# glede na preteklih 5 let:
naredi.df.5 <- function(x){
  data.frame(pricakov  = x,
             "Leto 2020"  = zamakni(x, 1),
             "Leto 2019" = zamakni(x, 2),
             "Leto 2018" = zamakni(x, 3),
             "Leto 2017" = zamakni(x,4),
             "Leto 2016" = zamakni(x,5))
}

df.5 <- drop_na(naredi.df.5(osrednjeslovenska$stevilo_stanovanj))
ucni.5 = pp.razbitje(nrow(df.5))

precno.preverjanje(df.5, ucni.5, pricakov ~ ., "ng", FALSE)
precno.preverjanje(df.5, ucni.5, pricakov ~ ., "lin.reg", FALSE)

# glede na pretekla 4 leta:
naredi.df.4 <- function(x){
  data.frame(pricakov  = x,
             "Leto 2020"  = zamakni(x, 1),
             "Leto 2019" = zamakni(x, 2),
             "Leto 2018" = zamakni(x, 3),
             "Leto 2017" = zamakni(x, 4))
}
df.4 <- drop_na(naredi.df.4(osrednjeslovenska$stevilo_stanovanj))
ucni.4 = pp.razbitje(nrow(df.4))

precno.preverjanje(df.4, ucni.4, pricakov ~ ., "ng", FALSE)
precno.preverjanje(df.4, ucni.4, pricakov ~ ., "lin.reg", FALSE)

# glede na pretekla 3 leta:
naredi.df.3 <- function(x){
  data.frame(pricakov  = x,
             "Leto 2020"  = zamakni(x, 1),
             "Leto 2019" = zamakni(x, 2),
             "Leto 2018" = zamakni(x, 3))
}

df.3 <- drop_na(naredi.df.3(osrednjeslovenska$stevilo_stanovanj))
ucni.3 = pp.razbitje(nrow(df.3))

precno.preverjanje(df.3, ucni.3, pricakov ~ ., "ng", FALSE)
precno.preverjanje(df.3, ucni.3, pricakov ~ ., "lin.reg", FALSE)

# napaka je vsepovsod manjša pri uporabi algoritma nakjučnih gozdov


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# NAPOVEDOVANJE Z METODO NAKLJUČNIH GOZDOV
# napovedovanje glede na preteklih 5 leta:

df.5 <- naredi.df.5(osrednjeslovenska$stevilo_stanovanj)
model.5 = ranger(formula = pricakov ~ ., data = df.5 %>% drop_na())

n = nrow(df.5)

df.5.2 = df.5
for(i in 1:3){
  df.5.2 = naredi.df.5(c(df.5.2$pricakov, NA))
  napoved = predict(model.5, data = df.5.2[n+i,])$predictions
  df.5.2[n+i, 1] = napoved
}

# napovedi za naslednja 3 leta:
napovedi.5 = df.5.2[(n+1):(n+3),1]
osrednjeslovenska.z.napovedjo5 <- osrednjeslovenska %>% dplyr::select(leto, stevilo_stanovanj)
zadnje_leto <- 2020
for (i in 1:3) {
  novo_leto <- as.integer(zadnje_leto + i)
  nov_podatek <- as.integer(napovedi.5[i])
  osrednjeslovenska.z.napovedjo5[nrow(osrednjeslovenska.z.napovedjo5) + 1, ] <-
    list(novo_leto, nov_podatek)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# prečno preverjanje napovedovanja glede na pretekla 4 leta:
df.5 <- df.5 %>% drop_na()

ucni <- pp.razbitje(df.5, stratifikacija = df.5$pricakov)

X <- df.5[,-1]

reg.pred = Predictor$new(
  model.5,
  data = X, y = df.5$pricakov,
  predict.fun = pfun
)

reg.moci = FeatureImp$new(reg.pred, loss = "mse")

reg.moci.5.plot <- plot(reg.moci) + theme_bw()
#-------------------------------------------------------------------------
# napovedovanje glede na prejšnja 4 leta:

df.4 <- naredi.df.4(osrednjeslovenska$stevilo_stanovanj)
model.4 = ranger(formula = pricakov ~ ., data = df.3 %>% drop_na())

n = nrow(df.4)

df.4.2 = df.4
for(i in 1:4){
  df.4.2 = naredi.df.4(c(df.4.2$pricakov, NA))
  napoved = predict(model.4, data = df.4.2[n+i,])$predictions
  df.4.2[n+i, 1] = napoved
}

# napovedi za naslednja 3 leta:
napovedi.4 = df.4.2[(n+1):(n+3),1]

osrednjeslovenska.z.napovedjo4 <- osrednjeslovenska %>% dplyr::select(leto, stevilo_stanovanj)
zadnje_leto <- 2020
for (i in 1:3) {
  novo_leto <- as.integer(zadnje_leto + i)
  nov_podatek <- as.integer(napovedi.4[i])
  osrednjeslovenska.z.napovedjo4[nrow(osrednjeslovenska.z.napovedjo4) + 1, ] <-
    list(novo_leto, nov_podatek)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# prečno preverjanje napovedovanja glede na pretekla 4 leta:
df.4 <- df.4 %>% drop_na()

ucni <- pp.razbitje(df.4, stratifikacija = df.4$pricakov)

X <- df.4[,-1]

reg.pred = Predictor$new(
  model.4,
  data = X, y = df.4$pricakov,
  predict.fun = pfun
)

reg.moci = FeatureImp$new(reg.pred, loss = "mse")

reg.moci.4.plot <- plot(reg.moci) + theme_bw()
reg.moci.4.plot
#-------------------------------------------------------------------------
# napovedovanje glede na prejšnje 3 leta:

df.3 <- naredi.df.3(osrednjeslovenska$stevilo_stanovanj)
model.3 = ranger(formula = pricakov ~ ., data = df.3 %>% drop_na())

n = nrow(df.3)

df.3.2 = df.3
for(i in 1:3){
  df.3.2 = naredi.df.3(c(df.3.2$pricakov, NA))
  napoved = predict(model.3, data = df.3.2[n+i,])$predictions
  df.3.2[n+i, 1] = napoved
}

# napovedi za naslednja 3 leta:
napovedi.3 = df.3.2[(n+1):(n+3),1]

osrednjeslovenska.z.napovedjo3 <- osrednjeslovenska %>% dplyr::select(leto, stevilo_stanovanj)
zadnje_leto <- 2020
for (i in 1:3) {
  novo_leto <- as.integer(zadnje_leto + i)
  nov_podatek <- as.integer(napovedi.3[i])
  osrednjeslovenska.z.napovedjo3[nrow(osrednjeslovenska.z.napovedjo3) + 1, ] <-
    list(novo_leto, nov_podatek)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# prečno preverjanje napovedovanja glede na pretekla 3 leta:

df.3 <- df.3 %>% na.omit()

ucni.3 <- pp.razbitje(df.3, stratifikacija = df.3$pricakov)

X.3 <- df.3[,-1]

reg.pred.3 = Predictor$new(
  model.3,
  data = X.3, y = df.3$pricakov,
  predict.fun = pfun
)

reg.moci.3 = FeatureImp$new(reg.pred.3, loss = "mse")

reg.moci.3.plot <- plot(reg.moci.3) + theme_bw()
reg.moci.3.plot

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# graf napovedovanja:
graf.napovedovanje <- ggplot(osrednjeslovenska.z.napovedjo3 %>% filter(leto %in% c(2010:2020)), 
                                    aes(x = leto, y = stevilo_stanovanj)) +
  geom_line(color = "black") + 
  + 
  osrednjeslovenska.z.napovedjo3 %>%
  filter(leto %in% c(2020:2023)) %>%
  geom_line(
    mapping = aes(x = leto, y = stevilo_stanovanj),
    color = 'green',
    size = 0.75
  ) +
  osrednjeslovenska.z.napovedjo4 %>%
  filter(leto %in% c(2020:2023)) %>%
  geom_line(
    mapping = aes(x = leto, y = stevilo_stanovanj),
    color = 'blue',
    size = 0.75
  ) +
  osrednjeslovenska.z.napovedjo5 %>%
  filter(leto %in% c(2020:2023)) %>%
  geom_line(
    mapping = aes(x = leto, y = stevilo_stanovanj),
    color = "red",
    size = 0.75
  ) +
  labs(
    x = "Leto",
    y = "Število",
    title = "Napovedni model števila gradbenih dovoljenj za Osrednjeslovensko regijo"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = 2010:2023) 