# §§§§§§§§§§§§§§§§§§§§§§§§§§  KAPITEL 4 - DATEN VORBEREITEN  §§§§§§§§§§§§§§§§§§§§§§§§§§



################ ---- Bevor Sie beginnen ... ---- ################

# Setzen Sie Ihr Arbeitsverzeichnis (Achtung: ändern!) und legen Sie den Beispieldatensatz dort ab
setwd("C:/users/Name/MeinArbeitsVerzeichnis")

# Den Beispqieldatensatz einlesen
x <- read.table("daten_final.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", na.strings = "")

# Bitte führen Sie die folgenden Schritte aus, um die Teildatensätze zu erzeugen, die in diesem Kapitel verwendet werden
expendit <- x[c(1:4),c(1:3)]
economics <- x[c(1:4),c(1,4,6)]
firstfour <- x[c(1:4),c(1:3)]
secondfour <- x[c(5:8),c(1:3)]
cpi.data <- data.frame(x$ISO[c(1:4,6)], x$CPIVAL[c(1:4,6)], x$CPIRANK[c(1:4,6)])
names(cpi.data) <- c("ISO", "CPIVAL", "CPIRANK")
cpi.data <- cpi.data[order(cpi.data$ISO, decreasing = TRUE),]
exp.data <- x[c(1:6),c(1:3)]




################ ---- Datensätze zusammenführen ---- ################


######## ---- Datensätze mit gleicher Struktur ---- ########

# Ein Blick auf die Daten
expendit  			# Datensatz anzeigen
economics 			# Datensatz anzeigen

# Wir verbinden die beiden Datensätze, indem wir ihre Variablen (also Spalten) mit cbind() "nebeneinanderstellen"
fulldata <- cbind(expendit, economics)
fulldata  			# Anzeigen

# Eine Alternative, um die Verdopplung von Variablen zu vermeiden, die in beiden Dataframes vorkommen ist,
# genau die Variablen aus beiden Datensätzen zu spezfizieren, die in den neuen, kombinierten Datensatz eingehen sollen 
fulldata <- cbind(expendit,economics$GDPTOTAL, economics$UNEMPLOY)
fulldata  			# Anzeigen

# Wir benennen die Variablen economics$GDPTOTAL und economics$UNEMPLOY im neuen Datensatz fulldata um
names(fulldata)		# Anzeigen
names(fulldata)[4] <- "GDPTOTAL"
names(fulldata)[5] <- "UNEMPLOY"

# Jetzt wollen wir die *Beobachtungen* (nicht die Variablen, wie eben) von zwei Datensätzen kombinieren. 
# Zunächst wieder ein Blick auf die Ausgangsdaten
firstfour
secondfour

# Wir verbinden die beiden Datensätze, indem wir ihre Beobachtungen (also Zeilen) mit rbind() "hintereinanderhängen"
firsteight <- rbind(firstfour, secondfour)
firsteight 			# Anzeigen

# Sowohl für cbind() als auch für rbind() ist eine gewisse strukturelle Gleichheit der Daten notwendig.
# Deshalb funktioniert diese Operation nicht
economics <- x[c(1:5),c(1,4,6)]			# Wir selektieren den economics-Datensatz so, dass er eine Zeile mehr hat
										# als der expendit-Datensatz
fulldata <- cbind(expendit,economics)   # Achtung: Fehlermeldung!



######## ---- Datensätze mit unterschiedlicher Struktur ---- ########

# Ausgangsdaten anzeigen
cpi.data
exp.data

# Jetzt kombinieren wir die beiden Datensätze anhand der Werte ihrer gemeinsamen Variable ISO
combined <- merge(exp.data, cpi.data, by = "ISO")
combined  			# Ergebnis anzeigen

# Wenn die Variablen in den Datensätzen unterschiedlich heißen, dann müssen wir beide Namen angeben
names(cpi.data)[1] <- "COUNTRYCODE" # Variable vorübergehend umbenennen, indem wir das erste Element
									# des Namensvektors überschreiben
combined <- merge(exp.data, cpi.data, by.x = "ISO", by.y = "COUNTRYCODE")
combined  			# Ergebnis anzeigen
names(cpi.data)[1] <- "ISO"   # Wieder "zurückumbenennen"

# Dieses Mal wollen die Datensätze so verbinden, dass *alle* Beoachtungen aus exp.data im Ergebnisdatensatz
# auftauchen, auch dann, wenn kein passendes "Gegenstück" in cpi.data gefunden worden ist (die Werte der
# Variablen aus cpi.data sind dann Missings (NA)
combined <- merge(exp.data, cpi.data, by = "ISO", all.x = TRUE)
combined  			# Ergebnis anzeigen




################ ---- Daten selektieren ---- ################


######## ---- Selektion mit festen Indexwerten ---- ########

# Wir Schauen uns den Gesamtdatensatz nochmal an ...
combined

# ... im besonderen die Variable EXPEND
combined$EXPEND

# Wir greifen das dritt Element der Variable EXPEND heraus (dritte "Zeile" innerhalb der Variable)
combined$EXPEND[3]

# Wir greifen das dritte und das sechste Element der Variable EXPEND heraus (dritte und sechste 
# "Zeile" innerhalb der Variable)
combined$EXPEND[c(3,6)]

# Hier wird versucht, auf eine zweite Dimension des Vektors EXPEND zuzugreifen. 
# Er hat allerdings nur eine Dimension, denn er ist eine Variable, das heißt, eine Spalte, unseres 
# Datensatzes
combined$EXPEND[3,6]	# Achtung: Fehlermeldung!

# Dieser Ansatz funktioniert aber auf dem Gesamtdatensatz. Der ist nämlich eine Tabelle mit Zeilen und 
# Spalten, also ein zweidimensionales Gebilde. 
# Hier greifen wir auf das dritte Element der dritten Variablen in unserem Datensatz zu; das sind die
# Staatsausgaben von Albanien. Äquivalente Anweisung: combined$EXPEND[3]
combined[3,3]

# Das dritte und das sechste Element der dritten Variable des Datensatzes anzeigen: Die Staatsausgaben 
# von Albanien und Algerien
combined[c(3,6),3]

# Das dritte und das sechste Element (dritte und sechste Zeile, also Beobachtung) unseres Datensatzes für 
# die erste, zweite, dritte, vierte und fünfte Spalte (also Variable) anzeigen
combined[c(3,6),c(1,2,3,4,5)]

# Alle Variablen anzeigen für die dritte und sechste Beobachtung unseres Datensatzes (Variablenindex 
# hinter dem Komma ist leer, das heißt, *alle* Variablen (alle Spalten) werden selektiert
combined[c(3,6),]

# Alle Elemente der dritten Variable anzeigen, das heißt, die dritte Variable (Staatsausgabenquote, EXPEND) 
# für alle "Zeilen" des Datensatzes. Der Zeilenindex vor dem Komma ist leer, daher werden alle Zeilen selektiert
combined[,3]

# Das gleiche für die dritte und vierte Variable, die Staatsausgaben und den Wert des Index für die
# wahrgenommene Korruption
combined[,c(3,4)]

# Den gesamten Datensatz anzeigen: Durch Weglassen sowohl der Zeilen- als auch der Spaltenindizes werden sowohl
# alle Zeilen, als auch alle Spalten selektiert, also der gesamte Datensatz
combined[,]

# Selektieren mit Hilfe eines Index: Zunächst wird eine Variable v definiert, die dann als Index zur Selektion 
# bestimmter Zeilen des Datensatzes verwendet wird
v<-c(2:5)
combined[v,]




######## ---- Selektion mit Bedingungen ---- ########

# Alle Zeilen selektieren, für die der Staatsausgabenanteil größer als 30 % ist und für diese 
# Zeilen (= Beobachtungen) alle Variablen anzeigen (leerer Spaltenindex hinter dem Kommma)
combined[combined$EXPEND > 30,] 

# Die gerade verwendendete Bedingung ergibt einen Vektor von Wahrheitswerten, der anzeigt, für welche Zeilen im 
# Datesatz die Bedingung erfüllt ist 
combined$EXPEND > 30
class(combined$EXPEND > 30)		# tatsächlich eine logischer Ausdruck

# Alle Zeilen selektieren, für die der Staatsausgabenanteil größer als 30 % UND der Indexwert der 
# wahrgenommenen Korruption kleiner als 20 ist
combined[combined$EXPEND > 30 & combined$CPIVAL < 20,]

# Alle Zeilen selektieren, für die der Staatsausgabenanteil größer als 30 %  ODER der Indexwert der 
# wahrgenommen Korruption kleiner als 20 ist
combined[combined$EXPEND > 30 | combined$CPIVAL < 20,]

# Alle Zeilen selektieren, für die der Staatsausgabenanteil größer als 30 % ist ODER zugleich der Indexwert der 
# wahrgenommen Korruption kleiner als 20 UND der Indexwert kein Missing ist. Dadurch wird vehindert, dass R für 
# Missings bei der Variable CPIVAL leere Datenzeilen im Selektionsergebnis anzeigt
combined[combined$EXPEND > 30 | (combined$CPIVAL < 20 & !is.na(combined$CPIVAL)),]

# Speichern der Selektionsergebnisse als neuer Datensatz
selection <- combined[combined$EXPEND > 30 & combined$CPIVAL < 20,]




################ ---- Daten rekodieren ---- ################


# Unseren Beispieldatensatz für die folgenden Beispiele etwas einschränken
xauswahl <- x[c(62, 110, 143, 170, 178),c(1:3,7,16)]

# Wir überzeugen uns, dass INCOMEGROUP tatächlich ein Faktor ist
class(xauswahl$INCOMEGROUP)

# Die Levels, das heißt, die möglichen Ausprägungen, von INCOMEGROUP anzeigen
levels(xauswahl$INCOMEGROUP)

# Achtung: Fehler! Der erste Versuch der Recodierung schlägt fehl, weil "High income" aktuell noch kein gültiges
# Level der Variable INCOMEGROUP ist
xauswahl[xauswahl$INCOMEGROUP == "High income: OECD" | xauswahl$INCOMEGROUP == "High income: nonOECD",4] <- "High income"

# Das funktioniert also nicht; deshalb erst mal wieder die vorherigen Datenstand wieder herstellen:
xauswahl <- x[c(62, 110, 143, 170, 178),c(1:3,7,16)]
# So geht es richtig: Wir manipulieren die Levels, das heißt, die Ausprägungen der Variable INCOMEGROUP. 
# Dadurch werden die bisherige zweite und dritte Ausprägung zusammengefaßt zu "High income", das von nun an ein
# gültiges Level unserer Faktorvariable ist. Jetzt könnten wir also Level auch in Zuweisungen wie der 
# obigen verwenden
levels(xauswahl$INCOMEGROUP)[1] <- "High income"
levels(xauswahl$INCOMEGROUP)[2] <- "High income"

# Ursprünglichen Datensatz wiederherstellen
xauswahl <- x[c(62, 110, 143, 170, 178),c(1:3,7,16)]

# Dieses mal weisen wir allen Beobachtungen, die als Ausprägung der Variable INCOMEGROUP das Level "Lower middle income" 
# haben, das Level "Low income" zu. Das funktioniert, denn das Level "Low income" existiert bereits, die Faktorvariable
# INCOMEGROUP ist also gewissermaßen darauf vorbereitet, diese Ausprägung anzunehmen
xauswahl[xauswahl$INCOMEGROUP == "Lower middle income", 4] <- "Low income"

# Alternativ kann die Variable auch direkt angesprochen werden, statt der etwas umständlichen Selektion über 
# den Gesamtdatensatz
xauswahl$INCOMEGROUP[xauswahl$INCOMEGROUP == "Lower middle income"] <- "Low income"




################ ---- Daten klassieren ---- ################


# Datensatz für unsere Beispiele erzeugen
xauswahl <- x[c(62, 110, 143, 170, 178),c(1:3,16)]

# Eine neue Variable WOMENPARL.K, die den Frauenanteil im Parlament in vier Klassen einteilt und
# diese Klassen benennt, anlegen
xauswahl$WOMENPARL.K <- cut(xauswahl$WOMENPARL, breaks = c(0, 10, 25, 50, 100), labels = c("sehr niedrig", "niedrig", "mittel", "hoch"))
xauswahl  			# Anzeigen

# Es geht auch ohne dass man den Klassen explizit Namen zuweist. In diesem Fall verwendet R Standardnamen
xauswahl$WOMENPARL.K <- cut(xauswahl$WOMENPARL, breaks = c(0, 10, 25, 50, 100))
xauswahl  			# Anzeigen

# Standardmäßig sind die Intervalle in R links offen und rechts geschlossen (also z.B. 10 < x <= 25, 
# mit right=FALSE läßt sich das aber ändern
xauswahl$WOMENPARL.K<-cut(xauswahl$WOMENPARL, breaks=c(0, 10, 25, 50, 100), right=FALSE)
xauswahl  			# Anzeigen

# Statt die Funktion cut() zu verwenden, kann die Klassierung natürlich mit den bereits bekannten Techniken
# auch manuell vorgenommen werden, wenn auch mit ungleich höherem Aufwand 
xauswahl$WOMENPARL.K2[xauswahl$WOMENPARL < 10] <- "Sehr niedrig"
xauswahl$WOMENPARL.K2[xauswahl$WOMENPARL >= 10 & xauswahl$WOMENPARL <= 25] <- "Niedrig"
xauswahl$WOMENPARL.K2[xauswahl$WOMENPARL <  10] <- "Sehr niedrig"
xauswahl$WOMENPARL.K2[xauswahl$WOMENPARL >= 10 & xauswahl$WOMENPARL < 25] <- "Niedrig"
xauswahl$WOMENPARL.K2[xauswahl$WOMENPARL >= 25 & xauswahl$WOMENPARL < 50] <- "Mittel"
xauswahl$WOMENPARL.K2[xauswahl$WOMENPARL > 50] <- "Hoch"
xauswahl  			# Anzeigen

# WOMENPARL.K2, das Ergebnis unserer Klassierung, ist eine Zeichenketten-Variable
class(xauswahl$WOMENPARL.K2)
# Wir hätten aber gerne einen Faktorvariable
xauswahl$WOMENPARL.K2 <- as.factor(xauswahl$WOMENPARL.K2)
class(xauswahl$WOMENPARL.K2)

# Natürlich kann man auch eine binäre Variable erzeugen; hier im Beispiel eine, die anzeigt, ob ein Land
# zu den Ländern mit höchstens 25 % Frauenanteil im Parlament gehört
xauswahl$WOMENPARL.KLEINER25 <- xauswahl$WOMENPARL <= 25	# xauswahl$WOMENPARL <= 25 ist ein logischer
															# Ausdruck, sein Ergebnis wird der neuen Variable
															# WOMENPARL.KLEINER25 zugewiesen



															
################ ---- Duplikate bereinigen ---- ################


# Zunächst erzeugen wir wieder einen Auszug aus unserem Datensatz für unsere Beispiele
xauswahl <- x[c(62, 110, 143, 170, 178, 143),c(1:3,16)]

# Gibt es komplette Duplikate (also vollständig identische Beobachtungen) im Datensatz?
duplicated(xauswahl)  	# Saudi-Arabien ist doppelt, das zweite Vorkommen wird der beiden wird mit TRUE markiert

# Wir bereinigen den Datensatz um die Duplikate
xauswahl_ohnedupl <- xauswahl[!duplicated(xauswahl),]

# Einfacher geht es mit Hilfe der Funktion unique
unique(xauswahl)

# Beide Funktionen lassen sich auch auf einzelne Variablen anwenden
unique(xauswahl$COUNTRY)
duplicated(xauswahl$COUNTRY)




################ ---- Daten sortieren ---- ################


# Datensatz für die folgenden Beispiele präparieren
xauswahl <- x[c(62, 110, 143, 170, 178),c(1:3,16)]
xauswahl$WOMENPARL.K <- cut(xauswahl$WOMENPARL, breaks = c(0, 10, 25, 50, 100), labels = c("sehr niedrig", "niedrig", "mittel", "hoch"))

# Eine Variable nach der Größe ihrer Beobachtungen zu sortieren, ist einfach
sort(xauswahl$WOMENPARL)

# Standardmäßig sortiert R aufsteigend. Wir können aber auch absteigend sortieren lassen
sort(xauswahl$WOMENPARL, decreasing = TRUE)

# Das Sortieren von ganzen Dataframes ist weniger intuitiv. Zunächst erzeugen wir eine Indexvariable,
# die die Nummern der Beobachtungen, sortiert nach der Sortier-Variable (also WOMENPARL) enthält
ind <- order(xauswahl$WOMENPARL)
# Dann selektieren wir den Datensatz so, dass die Beobachtungen in der richtigen, nämlich der durch die Index-
# Variable bestimmten, Reihenfolge herausgegriffen werden
xauswahl[ind,]

# Die beiden Anweisungen lassen sich natürlich auch kombinieren; hier sortieren wir primär nach dem Frauenanteil im Parlament
# und nachrangig (bei gleichem Frauenanteil) nach der Höhe der Staatsausgabenquote
xauswahl[order(xauswahl$WOMENPARL, xauswahl$EXPEND, decreasing = TRUE),]




################ ---- Geänderten Datensatz speichern ---- ################


# Wir speichern den geänderten Datensatz zur späteren Verwendung in einer CSV-Datei
write.table(xauswahl,file = "datensatz.csv", sep = ";", na = "", dec = ",", row.names = FALSE)