# Instalacja pakietu (jeśli jeszcze nie zainstalowany)
install.packages("lavaan")

# Wczytanie pakietu
library(lavaan)

data("HolzingerSwineford1939")
head(HolzingerSwineford1939)  # Podgląd danych
?HolzingerSwineford1939

# Definicja modelu CFA
model <- '
  Visual  =~ x1 + x2 + x3
  Textual =~ x4 + x5 + x6
  Speed   =~ x7 + x8 + x9
'

# Dopasowanie modelu CFA
fit <- cfa(model, data = HolzingerSwineford1939)

# Wyświetlenie wyników CFA
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Wybrane wskaźniki dopasowania
fitMeasures(fit, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr"))


# Instalacja pakietu semPlot (jeśli potrzebne)
install.packages("semPlot")

# Wczytanie pakietu
library(semPlot)

# Wizualizacja modelu CFA
semPaths(fit, "standardized", whatLabels = "std", layout = "tree", edge.color = "black", nCharNodes = 0)
#^ prostokąty - zm. obserwowalne, kółka - zm. nieobserwowalne
