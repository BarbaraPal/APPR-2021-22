# 3. faza: Vizualizacija podatkov

shrani.tabela1 %>%
  ggplot(
    mapping = aes(x = leto, y = 1000 * stevilo_stanovanj/stevilo_prebivalcev, fill = statisticna_regija)
  ) +
  geom_bar(stat="identity", position = position_dodge(width = 0.75)) +
  labs(title = "Število izdanih gradbenih dovoljenj na 1000 prebivalcev po regijah",
       x = "Leto",
       y = "Število/1000 prebivalcev"
       ) +
  theme_bw() +
  scale_x_continuous(breaks = c(2010:2020)) +
  theme(axis.text.x = element_text(angle = 90, size = 8))

shrani.tabela1 %>%
  filter(stevilo_ocena_dokoncanih != 'Posavska') %>%  # Ni podatkov za Posavsko regijo
  ggplot(
    mapping = aes(x = leto, y = 1000 * stevilo_ocena_dokoncanih/stevilo_prebivalcev, fill = statisticna_regija)
  ) +
  geom_bar(stat="identity", position = position_dodge(width = 0.75), na.rm = FALSE) +
  labs(title = 'Ocena dokončanih stanovanj na 1000 prebivalcev po regijah', y = 'ocena/1000 prebivalcev', caption = '(Brez Posavske regije)') + 
  scale_x_continuous(breaks = c(2010:2020)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8))

shrani.tabela2 %>%
  filter(vrsta != "Stanovanja", leto == '2010') %>%
  group_by(leto, statisticna_regija) %>%
  ggplot(
    mapping = aes(x = vrsta, y = 1000 * `Število`/stevilo_prebivalcev)
  ) +
  labs(title = 'Število stanovanj na 1000 prebivalcev po vrstah stanovanj', y = "število/1000 prebivalcev") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.75)) +
  facet_wrap(~ statisticna_regija)

shrani.tabela2 %>%
  filter(vrsta != "Stanovanja", statisticna_regija != 'Posavska', leto == '2010') %>%   # Ni podatkov za Posavsko regijo
  group_by(leto, statisticna_regija) %>%
  ggplot(
    mapping = aes(x = vrsta, y = `Površina [m2]`/`Število`)
  ) +
  labs(title = 'Povprečna površina stanovanj glede na vrsto', y = "površina/število", caption = '(Brez Posavske regije)') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.75)) +
  facet_wrap(~ statisticna_regija)

shrani.tabela4 %>%
  filter(stanovanjske_nepremicnine == c('1.1.1 Nova stanovanja', '1.1.2 Nove družinske hiše')) %>%
  ggplot(
    mapping = aes(x = leto, y = povprecje_cetrtletij_glede_na_2015 - 10000, col = stanovanjske_nepremicnine)
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  labs(title = 'Indeksi cen stanovanjskih nepremičnin glede na leto 2015',
       y = 'Indeksi cen stanovanjskih nepremičnin glede na leto 2015 - 10000') +
  geom_point() +
  geom_line()

shrani.tabela4 %>%
  filter(stanovanjske_nepremicnine == c('1.2.1.1 Rabljena stanovanja, Ljubljana-občina', '1.2.1.2 Rabljena stanovanja, preostala Slovenija')) %>%
  ggplot(
    mapping = aes(x = leto, y = povprecje_cetrtletij_glede_na_2015 - 10000, col = stanovanjske_nepremicnine)
  ) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  labs(title = 'Indeksi cen rabljenih stanovanjskih nepremičnin glede na leto 2015',
       y = 'Indeksi cen stanovanjskih nepremičnin glede na leto 2015 - 10000') +
  geom_point() +
  geom_line()

shrani.tabela5 %>%
  group_by(leto, statisticna_regija) %>%
  filter(statisticna_regija == 'Pomurska') %>%
  ggplot(
    mapping = aes(x = leto, y = stevilo_ocena_dokoncanih/(Priseljeni - Odseljeni))
  ) +
  labs(title = 'Ocena števila dokončanih stanovanj glede na razliko med priseljenimi in odseljenimi v Pomurski regiji',
       y = 'Ocena števila dokončanih stanovanj/(Priseljeni - Odseljeni)') +
  scale_x_continuous(breaks = c(2010:2020)) +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  geom_bar(stat = "identity")

shrani.tabela5 %>%
  group_by(leto, statisticna_regija) %>%
  filter(statisticna_regija == 'Pomurska') %>%
  ggplot(
    mapping = aes(x = leto, y = stevilo_stanovanj/(Priseljeni - Odseljeni))
  ) +
  labs(title = 'Število gradbenih dovoljenj glede na razliko med priseljenimi in odseljenimi v Pomurski regiji',
       y = 'Število grabenih dovoljenj/(Priseljeni - Odseljeni)') +
  scale_x_continuous(breaks = c(2010:2020)) +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  geom_bar(stat = "identity")

shrani.tabela5 %>% group_by(leto, statisticna_regija) %>%
  ggplot(
  mapping = aes(x = leto, y = Priseljeni - Odseljeni)
) + 
  geom_point() + 
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  scale_x_continuous(breaks = c(2010:2020)) +
  facet_wrap(~ statisticna_regija)


