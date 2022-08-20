# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza stanovanjske situacije v Sloveniji

Analizirala bom stanovanjsko situacijo v Sloveniji v letih 2010-2020. Najprej me bo zanimalo število novih stanovanj glede na število prebivalcev posamezne statistične regije in vrsta teh stanovanj, potem pa se bom osredotočila na iskanje povezav med številom novih stanovanj glede na indekse gradbenih stroškov, indekse cen življenskih potrebščin in indekse cen stanovanjskih nepremičnin. Na koncu me bo zanimala še podrobnejša analiza izdanih gradbenih dovoljenj in ocena dokončanih stanovanj po statističnih regijah, glede na migracije med regijami.

### Tabele
1. tabela: Število izdanih gradbenih dovoljenj za stanovanjske objekte in ocena dokončanih stanovanj
* statistična regija (factor)
* leto (integer)
* število prebivalcev (integer)
* število gradbenih dovoljenj (integer)
* ocena dokončanih stanovanj (integer)

2.tabela: Ocena dokončanih stanovanj po vrstah stanovanj
* statistična regija (factor)
* leto (integer)
* število prebivalcev (integer)
* ocena dokončanih stanovanj po vrstah stanovanj (integer)

3.tabela: Število izdanih gradbenih dovoljenj za stanovanjske objekte in ocena dokončanih stanovanj glede na indekse gradbenih stroškov ter inflacijo
* leto (integer)
* indeksi gradbenih stroškov skupaj(double)
* indeksi stroškov gradbenega materiala(double)
* indeksi stroškov dela(double)
* indeksi cen življenskih potrebščin (double)
* število gradbenih dovoljenj (integer)

4.tabela: Indeksi cen stanovanjskih nepremičnin glede na stanje nepremičnin (novo/rabljeno) in njihovo lokacijo
* indeksi cen stanovanjskih nepremičnin (double)
* stanje nepremičnin (factor)
* lokacija (factor)

5.tabela
* statistična regija (factor)
* leto (integer)
* število priseljenih (integer)
* število odseljenih (integer)
* število gradbenih dovoljenj (integer)
* ocena dokončanih stanovanj (integer)

6.tabela: kupoprodajni posli v letih 2010, 2015, 2020
* statistična regija (factor)
* leto (integer)
* Povprečna cena/m2 (double)
* Povprečna cena/m2 za nepremičnine mlajše od 10 let (double)

### Viri
* [Statistični urad Republike Slovenije](https://pxweb.stat.si/SiStat/sl)
* [Geodetska uprava Republike Slovenije](https://egp.gu.gov.si/egp/)
* [Wikipedia](https://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji)

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
