library(dplyr)
library(PogromcyDanych)

# Nazwy kolumn
colnames(auta2012)

# 1. Która Marka występuje najczęściej w zbiorze danych auta2012?
# Odp: Volkswagen  22826
auta2012 %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  top_n(1, liczba)

# 2. Spośród aut marki Toyota, który model występuje najczęściej.
# Odp: Yaris   1552
auta2012 %>%
  filter(Marka=="Toyota") %>%
  group_by(Model) %>%
  summarise(liczba = n()) %>%
  top_n(1, liczba)

# 3. Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
# Odp: 114958
auta2012 %>%
  filter(grepl("diesel", Rodzaj.paliwa)) %>%
  count()

# 4. Jakiego koloru auta mają najmniejszy medianowy przebieg?
# Odp: bialy-metallic             60000
auta2012 %>%
  group_by(Kolor) %>%
  summarise(Mediana.przebiegu=median(Przebieg.w.km, na.rm=TRUE)) %>%
  top_n(1, desc(Mediana.przebiegu))

# 4. Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?
# Odp: Volkswagen   1679
auta2012 %>%
  filter(Rok.produkcji=="2007") %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  top_n(1, liczba)

# 5. Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.
# Odp: Hiace             30000
# TODO: to chyba da się łatwiej
Srednie.ceny <- auta2012 %>%
  filter(Marka=="Toyota", Rok.produkcji %in% c("2007", "2008")) %>%
  group_by(Model, Rok.produkcji) %>%
  summarise(Srednia.cena.w.PLN = mean(Cena.w.PLN))

Srednie.ceny.2007 <- Srednie.ceny %>%
  filter(Rok.produkcji=="2007")

Srednie.ceny.2008 <- Srednie.ceny %>%
  filter(Rok.produkcji=="2008")

Srednie.ceny.2007 %>%
  inner_join(Srednie.ceny.2008, by="Model") %>%
  mutate(Roznica.cen.w.PLN = Srednia.cena.w.PLN.x - Srednia.cena.w.PLN.y) %>%
  summarise(Roznica.cen.w.PLN = mean(Roznica.cen.w.PLN)) %>%  # bez summarise nie działało top_n
  top_n(1, Roznica.cen.w.PLN)
  

# 6. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najdroższa?
# Odp: Volvo   505744.9
auta2012 %>%
  filter(Rok.produkcji=="2007", grepl("diesel", Rodzaj.paliwa)) %>%
  top_n(1, Cena.w.PLN) %>%
  select(Marka, Cena.w.PLN)

# 7. Ile jest aut z klimatyzacją?
# Odp: 162960
auta2012 %>%
  filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>%
  count()

# 8. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?
# Odp: Volkswagen  13317
auta2012 %>%
  filter(KM>100) %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  top_n(1, liczba)

# 9. Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?
# Odp: Camry            31647.
# TODO: to chyba da się łatwiej
Srednie.ceny.rodzaj.paliwa <- auta2012 %>%
  filter(Marka=="Toyota") %>%
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(Srednia.cena.w.PLN = mean(Cena.w.PLN))

Srednie.ceny.benzyna <- Srednie.ceny.rodzaj.paliwa %>%
  filter(grepl("diesel", Rodzaj.paliwa))

Srednie.ceny.diesel <- Srednie.ceny.rodzaj.paliwa %>%
  filter(grepl("benzyna", Rodzaj.paliwa))

Srednie.ceny.benzyna %>%
  inner_join(Srednie.ceny.diesel, by="Model") %>%
  mutate(Roznica.cen.w.PLN = abs(Srednia.cena.w.PLN.x - Srednia.cena.w.PLN.y)) %>%
  summarise(Roznica.cen.w.PLN = mean(Roznica.cen.w.PLN)) %>%  # bez summarise nie działało top_n
  top_n(1, Roznica.cen.w.PLN)

# 10. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?
# Odp: Skoda        562
auta2012 %>%
  filter(Rok.produkcji=="2007", grepl("diesel", Rodzaj.paliwa)) %>%
  top_n(1, desc(Cena.w.PLN)) %>%
  select(Marka, Cena.w.PLN)

# 11. W jakiej marce klimatyzacja jest najczęściej obecna?
# Odp: Volkswagen  17145
auta2012 %>%
  filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  top_n(1, liczba)

# 12. Gdy ograniczyć się tylko do aut o cenie ponad 50 000 PLN, która Marka występuje najczęściej w zbiorze danych auta2012?
# Odp: Audi    4374
auta2012 %>%
  filter(Cena.w.PLN>50000) %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  top_n(1, liczba)

# 13. Spośród aut marki Toyota, który model ma największy medianowy przebieg?
# Odp: Urban Cruiser             11000
auta2012 %>%
  filter(Marka=='Toyota') %>%
  group_by(Model) %>%
  summarise(Mediana.przebiegu=median(Przebieg.w.km, na.rm=TRUE)) %>%
  top_n(1, desc(Mediana.przebiegu))

# 14. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najdroższy?
# Odp: inny   505744.9
auta2012 %>%
  filter(Rok.produkcji=="2007", grepl("diesel", Rodzaj.paliwa)) %>%
  top_n(1, Cena.w.PLN) %>%
  select(Model, Cena.w.PLN)

# 15. W jakim modelu klimatyzacja jest najczęściej obecna?
# Odp: Passat   6124
auta2012 %>%
  filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>%
  group_by(Model) %>%
  summarise(liczba = n()) %>%
  top_n(1, liczba)
  
# 16. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?
# Odp: BMW     1217
auta2012 %>%
  filter(Przebieg.w.km<50000, grepl("diesel", Rodzaj.paliwa)) %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  top_n(1, liczba)

# 17. Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?
# Odp: Land Cruiser            101566.
auta2012 %>%
  filter(Rok.produkcji=="2007", Marka=="Toyota") %>%
  group_by(Model) %>%
  summarise(Srednia.cena.w.PLN=mean(Cena.w.PLN)) %>%
  top_n(1, Srednia.cena.w.PLN)

# 18. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najtańszy?
# Odp: Octavia        562
auta2012 %>%
  filter(Rok.produkcji=="2007", grepl("diesel", Rodzaj.paliwa)) %>%
  top_n(1, desc(Cena.w.PLN)) %>%
  select(Model, Cena.w.PLN)

# 19. Jakiego koloru auta mają największy medianowy przebieg?
# Odp: bordowy            175500
auta2012 %>%
  group_by(Kolor) %>%
  summarise(Mediana.przebiegu=median(Przebieg.w.km, na.rm=TRUE)) %>%
  top_n(1, Mediana.przebiegu)
