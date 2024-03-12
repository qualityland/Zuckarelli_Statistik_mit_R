# §§§§§§§§§§§§§§§§§§§§§§§§§§  KAPITEL 8 - ERGEBNISSE PRÄSENTIEREN  §§§§§§§§§§§§§§§§§§§§§§§§§§



################ ---- Bevor Sie beginnen ... ---- ################


# Packages laden
library(stargazer)		# Für stargazer()

# Wenn Sie das Package stargazer noch nicht installiert haben, müssen Sie dies zunächst tun:
# install.packages("stargazer", dependencies=TRUE)

# Setzen Sie Ihr Arbeitsverzeichnis (achtung: ändern!) und legen Sie den Beispieldatensatz dort ab
setwd("C:/users/Name/MeinArbeitsVerzeichnis")

# Den Beispqieldatensatz einlesen
x <- read.table("daten_final.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", na.strings = "")




################ ---- Tabellen ---- ################

# Wir erzeugen zunächst zwei Regressionsmodelle, um deren Ergebnisse in einer Tabelle darzustellen
m1 <- lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)
m2 <- lm(EXPEND ~ GROWTH + POPSENIORS + DEMOCRATIC, data = x)

# Die Tabellendarstellung übernimmt die Funktion stargazer() aus dem gleichnamigen Package
# Die Ausgabe soll im Textformat (ASCII) erfolgen. Möglich wären für type auch "latex" und "html"
stargazer(m1, m2, type = "text")




################ ---- Grafiken ---- ################


######## ---- Histogramme ---- ########

# Ein erstes einfaches Histogramm: Wir lassen uns die Verteilung der Staatsausgabenquote EXPEND anzeigen
hist(x$EXPEND)

# Die Zahl der (dann gleich breiten) Intervalle läßt sich manuell einstellen
hist(x$EXPEND, breaks = 30)

# Alternativ können Sie auch die Grenzen der Intervalle festlegen
hist(x$EXPEND, breaks = seq(0, 100, by = 5))

# Das Standard-Histogramm können wir optisch noch etwas aufbessern, zum Beispiel, indem Titel und Achsenbeschriftungen
# hinzugefügt und die Farbe und Schraffierung der Histogramm-Balken geändert werden
hist(x$EXPEND, breaks = seq(0, 100, by = 5), main = "Government Expenditures", xlab = "Expenditures as % of GDP", ylab = "Number of countries", density = 30, col = "gray")



######## ---- Scatterplots (Punktewolken) ---- ########

# Beispiel eines einfachen Scatterplots. Die Variable, die auf der X-Achse dargestellt werden soll, wird der Funktion
# plot() stets zuerst übergeben
plot(x$POPSENIORS, x$EXPEND)

# Welcher Bereich auf der X-Achse dargestellt werden soll, kann manuell mit Hilfe des Arguments xlim festgelegt werden
plot(x$POPULATION, x$EXPEND, xlim = c(0,50))

# Jetzt geht es ans Beschriften. Wir erzeugen zunächst einen einfachen Scatterplot
plot(x$POPSENIORS, x$EXPEND)
# Dann fügen wir mit Hilfe der Funktion text() die ISO-Codes der Länder hinzu, und zwar in kleinerer Schrift (Hälfte
# der Standardgröße, Argument cex) sowie linksbündig und vertikal etwas nach oben versetzt ausgerichtet.
# Das erste Element des Arguments adj ist die horizontale Ausrichtung (0 bedeutet linksbündig, 0,5 zentriert und 1 rechtbündig),
# das zweite Element die vertikale Ausrichtung
text(x$POPSENIORS, x$EXPEND, labels = x$ISO, cex = 0.5, adj = c(0,-1))

# Mit der Funktion locator() können wir die Koordinaten eines Punktes im Diagramm bestimmen, in wir die Funktion aufrufen
# (mit der Zahl der zu bestimmenden Datenpunkte als Argument) und dann entsprechend viele Punkte im Diagramm anklicken
locator(1)

# Wenn wir eine kategoriale Variable mit der Funktion plot() darstellen, erzeugt R automatisch einen Boxplot
plot(x$INCOMEGROUP, x$EXPEND)



######## ---- Boxplots ---- ########

# Wie bereits zuvor gesehen, erzeugt plot() automatisch einen Boxplot, wenn die Variable auf der X-Achse kategorial ist
plot(x$INCOMEGROUP, x$EXPEND)

# Boxplots können Sie aber auch explizit mit Hilfe der Funktion boxplot() generieren. Dabei wird eine Formel als Arrguent
# übergeben, die abhängige Variable (Y-Achse) steht links der Tilde (~), die unabhängige Variable (X-Achse) rechts davon
boxplot(x$EXPEND ~ x$INCOMEGROUP)

# Das Argument range bestimmt, wo genau die Whiskers liegen sollen. Hier setzen wir sie einfach auf die Extremwerte
boxplot(x$EXPEND ~ x$INCOMEGROUP, range=0)

# Wie alle Grafik-Funktionen bietet auch boxplot eine ganze Reihe von Möglichkeiten, die Darstellung optisch attraktiver
# zu machen
boxplot(x$EXPEND ~ x$INCOMEGROUP, range = 0, horizontal = TRUE, varwidth = TRUE, names = c("OECD", "Other high", "Low","Low-mid", "Up-mid"), col = c("gray84", "gray84", "gray84","gray84", "skyblue1", "gray84", "gray84", "gray84"), border = "gray45", xlab = "Gov. expenditures in % of GDP", ylab = "Income group", main = "Distribution of government expenditures")



######## ---- Globale Grafikparameter einstellen ---- ########

# Wir sichern zu zunächst die globalen Grafikparameter in einer Variable, um nichts "kaputtzumachen"
param.backup <- par()
# Das Rückgabeobjekt der Grafikparameterfunktion par() ist eine Liste aller Parameter
param.backup			# Anzeige

# Diese Parameter können wir über par() auch setzen, zum Beispiel die Schriftgröße (ps), die Größe des
# Haupttitels relativ zur Standardschriftgröße (cex.main), die Farbe des Haupttitels (col.main)
# und den Schriftstil des Haupttitels (font.main), den wir hier auf fett setzen (2)
par(ps = 10, cex.main = 1.4, col.main = "#0000FF", font.main = 2)

# Hier sehen Sie, wie sich diese Änderungen auswirken
plot(x$POPSENIORS, x$EXPEND, main = "Staatsausgaben in Abhängigkeit vom Seniorenanteil", col.main = "red")

# Am Ende können wir wieder aufräumen und alle Parameter wieder auf die Ausgangswerte zurücksetzen.
# Dabei werden ein paar Warnungen ausgegeben, denn einige der Grafikparameter, die wir eben mit par()
# ausgelesen hatten, sind read-only, können also nur gelesen aber nicht gesetzt werden. Auch diese sind
# natürlich in unserem Listenobjekt param.backup enthalten.
par(param.backup)



######## ---- Mehrere Grafiken kombinieren ---- ########

# Wir bereiten R darauf vor, dass wir ein Schema von Grafiken mit einer Zeile und zwei Spalten
# haben wollen, dass wir also zwei Grafiken nebeneinander setzen wollen
par(mfcol = c(1,2))

# Dann erzeugen wir die Grafiken, die R automatisch in das Anordnungsschema einfügt
plot(x$POPCHILDREN, x$EXPEND, col = "red", xlab = "Anteil Jugendliche", ylab = "Staatsausgabenquote")
plot(x$POPSENIORS, x$EXPEND, col = "blue", xlab = "Anteil Senioren", ylab = "Staatsausgabenquote") 

# Jetzt setzen wir noch einen Titel zentriert über beiden Grafiken
# Damit der Titel nicht über den oberen Rand hinausgeht, setzen wir zunächst die outer margins über das 
# Argument oma
par(oma = c(0,0,4,0))
title("Staatsausgabenquote und Bevölkerungsstruktur", outer = TRUE)

# Jetzt können wir mfcol wieder zurücksetzen. Die nächste Grafik wird dann wieder auf der maximalen
# Anzeigefläche dargestellt
par(mfcol=c(1,1))



######## ---- Elemente zu Grafiken hinzufügen ---- ########

# Ein einfaches Regressionsmodell als Beispiel
m <- lm(EXPEND ~ POPSENIORS, data = x)

# Wir zeigen die Punktewolke der abhängigen und unabhängigen Variablen an
plot(x$POPSENIORS, x$EXPEND)

# Jetzt zeichnen wir die Regressionsgerade ein, einmal mit lines(), einmal mit abline(), einmal mit curve()
lines(x$POPSENIORS[as.integer(names(m$fitted.values))], m$fitted.values)
abline(a = m$coefficients[1], b = m$coefficients[2])
curve(m$coefficients[1] + m$coefficients[2]*x, from = 0, to = 25, add = TRUE)

# Die Funktion curve() kann nicht nur Geraden zeichnen, sondern ganz beliebige bivariate funktionale
# Zusammenhänge
curve(x^3, from = -10, to = 10)

# Wir zeichnen zwei dicke blaue Punkte an den Koordinaten (20,60) und (30,25) ein...
points(x = c(20,30),y = c(60,25), col = "blue", lwd = 2)

# ...und beschriften einen dieser Punkte
text(20.5, 60, labels = "Punkt-Beschreibung", col = "blue", adj = c(0,0.5))