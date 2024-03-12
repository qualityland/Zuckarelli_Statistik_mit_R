# §§§§§§§§§§§§§§§§§§§§§§§§§§  KAPITEL 5 - DATEN DESKRIPTIV ANALYSIEREN  §§§§§§§§§§§§§§§§§§§§§§§§§§



################ ---- Bevor Sie beginnen ... ---- ################

# Setzen Sie Ihr Arbeitsverzeichnis (achtung: ändern!) und legen Sie den Beispieldatensatz dort ab
setwd("C:/users/Name/MeinArbeitsVerzeichnis")

# Den Beispqieldatensatz einlesen
x <- read.table("daten_final.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", na.strings = "")




################ ---- Statistische Kennzahlen in R ---- ################


######## ---- Lagemaße in R ---- ########

# Einige deskriptive Übersichtsstatistiken für unsere Variable EXPEND, die Staatsausgabenquote, anzeigen
summary(x$EXPEND)

# Wir speichern das Rückgabeobjekt von summary() in einer Variablen
deskriptiv <- summary(x$EXPEND)
deskriptiv  			# Anzeige des Rückgabeobjekts von summary()

# Das Rückgabeobjekt enthält die einzelnen deskriptiven Statistiken, auf die wir einzeln zugreifen 
# können; entweder über das gespeicherte Objekt ...
deskriptiv[3]
# ... oder mit dieser Anweisung, die ebenfalls auf das Rückgabeobjekt von summary(x$EXPEND) zugreift
# (das in diesem Moment neu berechnet wird)
summary(x$EXPEND)[3]

# Die deskriptiven Statistiken können natürlich auch einzeln berechnet werden. Dafür bietet R eine 
# ganze Reihe von Funktionen
min(x$EXPEND)			# Das Minimum
quantile(x$EXPEND, 0.25)# Das 25 %-Quantil (= 1. Quartil)
median(x$EXPEND)		# Der Median, das heißt, das 50 %-Quantil oder das 3. Quartil
mean(x$EXPEND)			# Das arithemtische Mittel
quantile(x$EXPEND, 0.75)# Das 75 %-Quantil (= 3. Quartil)
max(x$EXPEND)			# Das Maximum

# Ein genauerer Blick auf die Funktion quantile(), die Quantile der Verteilung der Variablen bestimmt
quantile(x$EXPEND, c(0.10,0.25,0.5,0.75,0.9))

# Nun könnte uns interessieren, welches Land  die höchste Staatsausgabenquote hat. Es ist...Tuvalu!
x$COUNTRY[x$EXPEND == max(x$EXPEND)]

# Wie man sieht, wenn man den Datensatz absteigend nach der Höhe der Staatsausgabenquote sortiert,
# ist Tuvalu aber eher ein Ausreißer. 
# Hier zeigen wir mit Hilfe der Funktion head() nur die ersten 6 Beobachtungen an. Die Angabe n=6
# beim Aufruf von head() hätten wir eigentlich nicht gebraucht, denn 6 ist der Standardwert dieses
# optionalen Arguments
head(x[order(x$EXPEND, decreasing = TRUE),c(2,3)], n = 6)



######## ---- Streuungsmaße in R ---- ########

# Wir berechnen die Spannweite der Variable EXPEND
max(x$EXPEND) - min(x$EXPEND)

# Und nun die mittlere absolute Abweichung vom arithmetischen Mittel; dazu ermitteln wir zunächst die
# absoluten Abweichungen vom arithmetischen Mittel ...
abs(x$EXPEND - mean(x$EXPEND))
# ... und teilen dann durch die Zahl der Beobachtungen
maa.expend <- sum(abs(x$EXPEND - mean(x$EXPEND))) / length(x$EXPEND)
# Und das gibt uns diese mittlere absolute Abweichung
maa.expend				# Anzeige

# Die können wir uns auch als Funktion definieren. In einer Anweisung ...
maa <- function(w) { return(sum(abs(w - mean(w))) / length(w))}
# ... oder in zwei Anweisungen (dann aber die Anweisungen durch Semikolon getrennt, sonst kann die
# R-Konsole das nicht verarbeiten
maa <- function(w) { z <- sum(abs(w - mean(w))) / length(w); 
return(z) }

# Wenn man im Skriptmodus arbeitet, sind solche Funktion natürlich bequemer zu handhaben
maa <- function(w) 
{ 
  z<-sum(abs(w - mean(w))) / length(w)
  return(z) 
}

# Probieren wir unsere Funktion für die mittlere absolute Abweichung vom arithmetischen Mittel doch 
# gleich einmal aus!
maa(x$EXPEND)

# Und mit der Staatsausgabenquote
maa(x$UNEMPLOY)			# Liefert ein Missing (NA) als Ergebnis

# Offensichtlich müssen wir unsere Funktion so modifizieren, dass sie mit Missings umgehen kann, sonst
# ist ihr Ergebnis selbst jedes mal NA, wenn ihr Argument, also die Variable, auf die sie angewendet
# wird, ein Missing enthält
maa<-function(w) 
{ 
  z<-sum(abs(w[!is.na(w)] - mean(w, na.rm = TRUE))) / length(w[!is.na(w)])
  return(z) 
}

# Wieviele Missings gibt es denn in der Variable UNEMPLOY?
length(x$UNEMPLOY[is.na(x$UNEMPLOY)])

# Hier also die modifizierte Version der Funktion, die Missings von der Berechnung ausschließt
maa <- function(w) 
{ 
  z <- sum(abs(w[!is.na(w)] - mean(w, na.rm = TRUE))) / length(w[!is.na(w)])
  return(z) 
}

# Dann kommt unsere Funktion maa auch mit der Variable UNEMPLOY klar, die Missings enthält
maa(x$UNEMPLOY)

# Jetzt zu Streuungsmaßen, die in R standardmäßig als Funktion angeboten werden. 
# Wir berechnen die Varianz ...
var(x$EXPEND)

# ... und die Standardabweichung
sd(x$EXPEND)

# Die Quadratwurzel der Varianz ist bekanntlich die Standardabweichung; das läßt sich leicht
# nachrechnen
sqrt(var(x$EXPEND))

# Wir versuchen einmal, die Varianz nachzurechnen
sum((x$EXPEND - mean(x$EXPEND))^2) / length(x$EXPEND)   # Das Ergenis weicht von var(EXPEND) ab

# R berechnet die Stichprobenvarianz. Wir müssen also teilen durch N-1 statt durch N
sum((x$EXPEND - mean(x$EXPEND))^2) / (length(x$EXPEND) - 1)

# Wenn wir auch diese Berechnung gegen etwaige Missings "immunisieren" wollen...
sum((x$EXPEND[!is.na(x$EXPEND)] - mean(x$EXPEND, na.rm = TRUE))^2) / (length(x$EXPEND[!is.na(x$EXPEND)]) - 1)



######## ---- Zusammenhangsmaße in R ---- ########

# Wir berechnen den (Pearson'schen) Korrelationskoeffizienten der Arbeitslosenquote und des 
# Staatsausgabenanteils, einmal für den gesamten Datensatz...
cor(x$UNEMPLOY, x$EXPEND, use = "pairwise.complete.obs")

# ... einmal für die Länder mit hohem Einkommen ...
cor(x$UNEMPLOY[x$INCOMEGROUP == "High income: OECD" | x$INCOMEGROUP == "High income: nonOECD"], x$EXPEND[x$INCOMEGROUP == "High income: OECD" | x$INCOMEGROUP == "High income: nonOECD"], use = "pairwise.complete.obs")

# ... und dann für die Subgruppe der OECD-Länder
cor(x$UNEMPLOY[x$INCOMEGROUP == "High income: OECD"], x$EXPEND[x$INCOMEGROUP == "High income: OECD"], use = "pairwise.complete.obs")

# Das gleiche nochmal mit Spearmans Rangkorrelationskoeffizient
cor(x$UNEMPLOY[x$INCOMEGROUP == "High income: OECD" | x$INCOMEGROUP == "High income: nonOECD"], x$EXPEND[x$INCOMEGROUP == "High income: OECD" | x$INCOMEGROUP == "High income: nonOECD"], use = "pairwise.complete.obs", method = "spearman")
cor(x$UNEMPLOY[x$INCOMEGROUP == "High income: OECD"], x$EXPEND[x$INCOMEGROUP == "High income: OECD"], use = "pairwise.complete.obs", method = "spearman")



######## ---- Daten gruppiert analysieren ---- ########

# Wenn wir das arithmetische Mittel der Staatsausgabenquote nach Einkommensklassen der Länder bestimmen
# wollen, könnten wir so vorgehen
mean(x$EXPEND[x$INCOMEGROUP == "High income: OECD"])
mean(x$EXPEND[x$INCOMEGROUP == "High income: nonOECD"])
mean(x$EXPEND[x$INCOMEGROUP == "Upper middle income"])
mean(x$EXPEND[x$INCOMEGROUP == "Lower middle income"])
mean(x$EXPEND[x$INCOMEGROUP == "Low income"])

# Das ist aber umständlich. Einfacher geht es mit der Funktion tapply(), die eine Funktion (Argument FUN) auf Daten 
# anwendet, die automatisch mit Hilfe eines Faktors (Argument INDEX) gruppiert werden
tapply(x$EXPEND, INDEX = x$INCOMEGROUP, FUN = mean)

# So können wir auch die Beobachtungen in den einzelnen Kategorien zählen
tapply(x$EXPEND, INDEX = x$INCOMEGROUP, FUN = length)

# Auch kombinierte Kategorien aus mehreren Faktoren sind möglich. Diese Faktoren müssen dann mit der Funktion list()
# zu einer Liste zusammengestellt werden
tapply(x$EXPEND, INDEX = list(x$INCOMEGROUP, x$DEMOCRATIC), FUN = mean)

# So dagegen funktioniert es nicht
tapply(x$EXPEND, INDEX = x$INCOMEGROUP, x$DEMOCRATIC, FUN = mean)	# Achtung: Fehlermeldung!

# Probieren wir das gleiche einmal mit CPIVAL, dem Index der wahrgenommenen Korruption. Das ergibt lauter
# Missings, weil wir in tapply() der Funktion mean() nicht das entscheidende Argument na.rm übergeben können
tapply(x$CPIVAL, INDEX = x$INCOMEGROUP, FUN = mean)

# Daher definieren wir uns selbst eine Funktion, die mean() mit dem Argument na.rm aufruft
mean.na <- function(z) { return(mean(z,na.rm = TRUE)) }
# Diese Funktion können wir dann in tapply() als Argument FUN verwenden
tapply(x$CPIVAL, INDEX = x$INCOMEGROUP, FUN = mean.na)

# Wir zählen die Ausprägungen in den unterschiedlichen Kategorien, die durch zwei Faktoren/logische 
# Variablen, INCOMEGROUP und DEMOCRATIC, aufgespannt werden
table(x$INCOMEGROUP, x$DEMOCRATIC, useNA = "ifany")		# Anders als bei tapply() müssen die Faktoren hier nicht zu
														# einer Liste zusammengestellt werden
tapply(x$EXPEND, INDEX = list(x$INCOMEGROUP, x$DEMOCRATIC),FUN = length)