# §§§§§§§§§§§§§§§§§§§§§§§§§§  KAPITEL 6 - LINEARE REGRESSION: KONTINUIERLICHE DATEN ANALYSIEREN (INFERENZSTATISTIK I)  §§§§§§§§§§§§§§§§§§§§§§§§§§



################ ---- Bevor Sie beginnen ... ---- ################


# Packages laden
library(car)			# u.a. für linearHypothesis(), vcov(), vif()
library(lmtest)			# u.a. für coeftest(), bgtest(), bptest(), gqtest(), dwtest(), reset(), lrtest()
library(sandwich)		# u.a. für vcovHC(), vcovHAC()
library(tseries)		# Für jarque.bera.test()
library(nnet)			# Für multinom()
library(MASS)			# für polr()

# Wenn Sie die Packages noch nicht installiert haben, müssen Sie dies zunächst tun. Als Beispiel die
# Installation des Packages lmtest:
# install.packages("lmtest", dependencies=TRUE)

# Setzen Sie Ihr Arbeitsverzeichnis (achtung: ändern!) und legen Sie den Beispieldatensatz dort ab
setwd("C:/users/Name/MeinArbeitsVerzeichnis")

# Den Beispqieldatensatz einlesen
x <- read.table("daten_final.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", na.strings = "")




################ ---- Lineare Regression in R ---- ################


######## ---- Ein erstes Regressionsmodell in R ---- ########

# Unser erstes Regressionsmodell!
lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)

# Achtung Fehler: In diesem Modell sind linke und rechte Seite der Modellgleichung
# durch ein Gleichheitszeichen statt durch eine Tilde getrennt
lm(EXPEND = UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)

# Wir speichern die Modellgleichung in einer Variable
modell <- EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC
# Diese Variable ist vom Typ formula
class(modell)

# Jetzt ist der Aufruf von lm() viel kompakter
lm(modell, data = x)

# Es geht auch ohne data-Argument, wenn Sie die Variablen in der üblichen datensatz$variable-
# Notation angeben
lm(formula = x$EXPEND ~ x$UNEMPLOY + x$POPSENIORS + x$POPCHILDREN + x$DEMOCRATIC)

# Wie lassen uns mehr Details zu den Schätzergebnissen anzeigen
summary(lm(formula = EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x))

# Gleicher Effekt: Rückgabewert von lm() in einer eigenen Variable speichern und dann mit summary()
# auswerten
resultat <- lm(formula = EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)
summary(resultat)

# Wieviele Missings stecken in unseren unabhängigen Variable?
# So finden Sie es heraus, hier für UNEMPLOY:
NROW(x$UNEMPLOY[is.na(x$UNEMPLOY)] == TRUE)

# Da R die Konstanten TRUE mit 1 und FALSE codiert, könnne wir die Zahl der Fälle, in denen is.na()
# TRUE zurückgibt und daher ein Missing vorliegt, auch einfach durch Summation zählen
sum(is.na(x$UNEMPLOY))
sum(is.na(x$POPSENIORS))
sum(is.na(x$POPCHILDREN))
sum(is.na(x$DEMOCRATIC))



######## ---- Weitere Beispiele für Regressionsmodelle ---- ########

# Ein Modell mit GROWTH statt der Variablen UNEMPLOY, die sehr viele Missings aufwies und so dazu führte, 
# dass viele Beobachtungen der Stichprobe nicht in der Schätzung des Modells berücksichtigt werden konnten
summary(lm(formula = EXPEND ~ GROWTH + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x))

# Wir ersetzen POPCHILDREN und POPSENIORS durch die Dependency Rate, die die Summe beider Variablen ist 
x$DEPENDENCY <- x$POPCHILDREN + x$POPSENIORS 
summary(lm(formula = EXPEND ~ UNEMPLOY + DEPENDENCY + DEMOCRATIC, data = x))

# Die beiden Komponenten der Dependency Rate weisen eine hohe negative Korrelation auf.
# Deshalb ist kaum ein Effekt der Dependency Rate erkennbar, wohl aber der beiden Einzelvariablen
cor(x$POPCHILDREN, x$POPSENIORS, use = "pairwise.complete.obs")

# Jetzt erstellen wir eine Prognose. Dazu schätzen wir zunächst unser Standardmodell und weisen das Ergebnis 
# einer Variablen zu
m <- lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)

# Dann erzeugen wir mit der Funktion predict() die Prognose. Als neuen Satz an Daten für die erklärenden
# Variablen nehmen wir der Bequemlichkeit halber den Ausgangsdatensatz
prog <- predict(m, newdata = x)
# Wir bauen eine Dataframe, der die prognostizierten Werte und den Vektor m$fitted.values 
# nebeneinanderstellt. Dieser Vektor ist Element des Rückgabeobjekts von m und enthält die geschätzten
# Werte der abhängigen Variable, in diesem Fall also die gleichen Werte wie die unserer Prognose 
diff <- data.frame(prog[!is.na(prog)], m$fitted.values)
# Davon, dass die Werte tatsächlich gleich sind, können wir uns leicht mit Hilfe unseres 
# Dataframes leicht überzeugen
head(diff)				# Anzeige



######## ---- Ein genauerer Blick auf die Funktion lm ---- ########

# Entweder wir zeigen die Detailergebnisse der Modellschätzung direkt an ...
summary(lm(formula = EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x))
# ... oder wir speichern den Rückgabewert von lm() erst mal in einer eigenen Variable
resultat <- lm(formula = EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)

# Die ist vom Typ lm
class(resultat)

# Dahinter verbirgt sich letztlich aber wieder eine Liste
typeof(resultat)

# Das Rückgabeobjekt von lm() beinhaltet eine ganze Reihe interessanter Informationen zur Modellschätzung
names(resultat)

# Unter anderem enthält es einen named vector mit den Koeffizientenschätzern
resultat$coefficients

# Nicht nur das Rückgabeobjekt von lm ist übrigens eine Liste, auch unser Dataframe ist eine Liste,
# und zwar eine Liste gleich langer Vektoren, unserer Variablen
class(x)				# Die "Klasse" des Objekts ist data.frame...
typeof(x)				# ... aber dahinter steht letztlich nichts weiter als eine Liste

# resultat$coefficients ist, wie bereits gesehen, ein named vector, bei dem die Elemente jeweils einen Namen
# besitzen. Solche named vectors können Sie ganz leicht selbst erzeugen. Hier ein einfaches Beispiel
vektor <- c(1,2)
names(vektor) <- c("Erstes Element", "Zweites Element")

# Auch der Zugriff auf einzelne Elemente des named vectors ist sehr leicht zu bewerkstelligen.
# Um auf den UNEMPLOY-Koeffizientenschätzer zuzugreifen, können Sie resultat$coefficients entweder
# so indizizeren...
resultat$coefficients[2]
# ... oder aber über seinen Namen, also so
resultat$coefficients["UNEMPLOY"]

# Der Durchschnitt der geschätzten Residuen ist gleich Null. in R gibt es einen ganz, ganz kleinen
# Rundungseffekt
mean(resultat$residuals)

# Mit Hilfe der Residuen können Sie sehr leicht den Schätzer für die Residuenvarianz nachrechnen 
# und daraus den wichtigen Standardfehler der Residuen bestimmen
sigma.2 <- sum(resultat$residuals^2) / (NROW(resultat$residuals) - NROW(resultat$coefficients))
sqrt(sigma.2)

# So reduzieren Sie den Datensatz auf die Beobachtungen, die auch wirklich in die Schätzung des
# Modells eingegangen sind
x.benutzt <- x[as.integer(names(resultat$residuals)),]

# Mit demselben Ansatz können wir die Werte unserer unabhängigen Variable EXPEND bestimmen, die 
# im Modell berücksichtigt worden sind
EXPEND.benutzt <- x$EXPEND[as.integer(names(resultat$residuals))]

# Mit diesen läßt sich nun leicht das Bestimmtheitsmaß R^2 des Modells nachrechnen.
# Zunächst die Gesamtvariation der unabhängigen Variable (total sum of squares, TSS)
TSS <- sum((EXPEND.benutzt - mean(EXPEND.benutzt))^2)
# Dann die erklärte Variation der unabhängigen Variable (explained sum of squares, ESS)
ESS <- sum((resultat$fitted.values - mean(resultat$fitted.values))^2)
# Mit TSS und ESS ist es nur noch ein einfacher Schritt
r.2 <- ESS / TSS



######## ---- Ein genauerer Blick auf die Funktion summary ---- ########

# Bisher haben wir uns die Ergebnisse von summary(lm(...)) immer direkt angeschaut. Dieses Mal 
# speichern wir den Rückgabewert von summary() in einer eigenen Variablen
sum.resultat <- summary(lm(formula = EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x))

# Das Rückgabeobjekt von summary(), das wir in unserer Variable sum.resultat gespeichert haben, beinhaltet
# eine ganze Reihe von Daten...
names(sum.resultat)

# ...darunter auch eine Matrix mit Informationen zu den geschätzten Modellkoeffizienten. Das ist die Matrix 
# der Informationen, die auch angezeigt wird, wenn Sie in der R-Konsole summary(lm(...)) aufrufen
sum.resultat$coefficients

# Um auf den in der Matrix enthaltenen Standardfehler des Koeffizientenschätzers der Variable UNEMPLOY 
# zuzugreifen, können Sie die Matrix entweder über die Zeilen- bzw. Spaltennummern indizieren,
# oder durch Angaben der Namen der Zeilen bzw. Spalten, oder einer Mischung aus beidem
sum.resultat$coefficients[2,2]
sum.resultat$coefficients["UNEMPLOY", 2]
sum.resultat$coefficients["UNEMPLOY", "Std. Error"]

# Achtung Fehler: Hier ist das "error" kleingeschrieben! Die Groß- und Kleinschreibung der Indizes
# muss aber exakt der Groß- und Kleinschreibung entsprechen, wie sie auch in der coefficients-Matrix
# verwendet wird
sum.resultat$coefficients["UNEMPLOY", "Std. error"]

# Mit Hilfe der Matrix können Sie sich zum Beispiel leicht den Vektor der Standardfehler aller 
# Koeffizienten anschauen. Lassen Sie dazu einfach den Zeilenindex (also den Index des Koeffizienten) frei
sum.resultat$coefficients[,"Std. Error"]

# Vergleichen Sie nochmal die Daten, die Ihnen durch das Rückgabeobjekt von lm() selbst sowie das
# Rückgabeobjekt von summary(lm(...)) zur Verfügung gestellt werden; 
resultat <- lm(formula = EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)
names(resultat)
sum.resultat <- summary(resultat)
names(sum.resultat)

# Wie nicht anders zu erwarten, ist das Rückgabe-Objekt der Funktion summary, auch wenn 
# es hochoffiziell als summary.lm daherkommt, letztlich doch eine Liste:
class(sum.resultat)
typeof(sum.resultat)




################ ---- Hypothesentests in R ---- ################


######## ---- Hypothesentests einzelner Parameter ---- ########

# Zunächst nochmal das Modell berechnen, dessen Koeffizientenschätzer wir für die Tests heranziehen wollen
m <- lm(formula = EXPEND ~ GROWTH + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)
summary(m)				# Anzeige

# Jetzt berechnen wir den t-Wert unseres Koeffizienten DEMOCRATIC unter Gültigkeit der Nullhypothese.
# Dazu zunächst das summary-Rückgabeobjekt in der Variable s speichern.
s <- summary(m)
# Aus den darin enthaltenen Werten der Koeffizientenschätzer und ihrer Standardfehler lässt sich dann 
# ganz einfach der t-Wert errechnen
t <- (s$coefficients["DEMOCRATICTRUE", "Estimate"] + 3) / s$coefficients["DEMOCRATICTRUE", "Std. Error"]

# Alternativ: Zugriff auf die Elemente coefficients-Matrix des Rückgabeobjekts s nicht über die Namen, 
# sondern über die Nummern von Zeile und Spalte des jeweiligen Werts in der Matrix
t <- (s$coefficients[5,1] + 3) / s$coefficients[5,2]

# Wir wahrscheinlich ist es, bei Gültigkeit der Nullhypothese, dass der Koeffizient von DEMOCRATIC größer 
# oder gleich -3 ist, einen t-Wert zu erhalten, der größer ist als unser t-Wert? 
# Nachschauen in der t-Verteilung, natürlich mit der richtigen Zahl von Freiheitsgarden (N-K-1)
pt(t,161)

# Auf diese Art lassen sich auch die standardmäßig von R angebotenen Hypothesentests, dass die Parameter des 
# Regressionsmodells in der Grundgesamtheit gleich 0 sind, nachrechnen
t <- s$coefficients["DEMOCRATICTRUE", "Estimate"] / s$coefficients["DEMOCRATICTRUE", "Std. Error"]
# Achtung: Wie bestimmen das Signifikanzniveau eines zweiseitigen Hypothesentests!
2 * pt(t,161)



######## ---- Gleichzetige Hypothesentests mehrerer Parameter ---- ########

# Wir bauen als erstes die Koeffizinenten Matrix für unseren Test zusammen...
gamma <- cbind(c(0,2), c(1,-1), c(3,0), c(0,0), c(0,0))
# ... und die festen Werte, gegen die die Linearkombinationen von Koeffizinenten getestet werden sollen
theta <- c(7,1)

# Wir schätzen als Beispiel ein einfaches Regressionsmodell, dessen Koeffizienten wir dann dem eben 
# definierten Test unterziehen
m <- lm(EXPEND ~ GROWTH + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)

# Die eigentliche Testdurchführung bewerkstelligen wir mit Hilfe von linearHypothesis(); diese Funktion
# führt einen F-Test auf Basis unseres linearen Gleichungssystems, gegeben durch die Matrix gamma und 
# den Vektor theta, durch
linearHypothesis(model = m, hypothesis.matrix = gamma, rhs = theta)

# Es geht aber auch einfacher: Man kann die zu testenden Linearkombinationen von Koeffizienten auch
# einfach verbal beschreiben
linearHypothesis(model = m, c("GROWTH + 3*POPSENIORS = 7", "2*(Intercept) - GROWTH = 1"))

# Aber Achtung: Dabei müssen die Koeffizienten genauso geschrieben werden, wie im Output der Funktion lm().
# In diesem Beispiel fehlen die Klammern um "(Intercept)"
linearHypothesis(model = m, c("GROWTH + 3*POPSENIORS = 7", "2*Intercept-GROWTH = 1"))

# Auch Hypothesen, die nur einen einzelnen Koeffizienten betreffen, lassen sich so testen. Zum Beispiel die
# Standard-Hypothese, dass der Wert eines Koeffizienten in der Grundgesamtheit gleich 0 ist. Das Ergebnis
# dieses Tests ist identisch mit dem Ergebnis des entsprechenden t-Tests
linearHypothesis(model = m, c("GROWTH = 0"))

# Die Funktion linearHypothesis() gibt, wie die meisten der komplexeren Funktionen, ein Listen-Objekt zurück
ftest <- linearHypothesis(model = m, c("GROWTH = 0"))
# Was darin enthalten ist, kann man sich leicht anschauen:
names(ftest)
# Unter anderem enthalten ist natürlich der Wert der F-Teststatistik. im Falle des Test nur eines einzelnen
# Koeffizienten wie eben (Test GROWTH=0) ist dessen Quadratwurzel gerade gleich dem Wert der 
# entsprechenden t-Testsatistik; probieren Sie es aus und vergleichen Sie den folgenden Wert mit dem Wert
# der t-Teststatistik, den die Funktion lm() ausgibt
sqrt(ftest$F[2])

# Solange nur eine einzelne Linearkombination von Koeffizienten gestestet wird, kann man dazu auch den 
# t-Test bemühen. Die Berechnung des t-Werts kann allerdings etwas mühsamer sein, weil man den Standardfehler
# der Linearkombination unter Gültigkeit der Nullhypothese benötigt. Dazu wieder benötigt man die Varianz-
# Kovarianz-Matrix der Koeffizientenschätzer, die mit Hilfe der Funktion() vcov berechnet werden kann
t <- (m$coefficients["POPSENIORS"] + m$coefficients["POPCHILDREN"] - 1) / sqrt(vcov(m)["POPSENIORS", "POPSENIORS"] + vcov(m)["POPCHILDREN", "POPCHILDREN"] + 2*vcov(m)["POPSENIORS", "POPCHILDREN"])
# Wir befreien den Vektor t von seinem Namen (eine rein "optische" Maßnahme)...
t <- unname(t)
# ... und berechnen den zugehörigen p-Wert
pt(t, 161, lower.tail = FALSE)




################ ---- Regression auf kategoriale Variablen ---- ################


######## ---- Kategoriale Variablen als Regressoren ---- ########

# Sie haben, vermutlich ohne es bewußt zur Kenntnis zu nehmen, bereits zuvor Regressionsmodelle
# geschätzt, in denen einige Regressoren kategoriale Variablen, das heißt, Faktoren oder logische
# Variablen waren. 
# Hier ein Beispiel mit der logischen Variable DEMOCRATIC, die im Zusammenhang mit Regressionsmodellen 
# dann auch als Dummy-Variable bezeichnet wird
summary(lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x))

# Ein weiteres Beispiel: Die Regression der Staatsausgabenquote EXPEND auf die Einkommensklassifikation
# INCOMEGROUP. INCOEMGROUP ist eine Faktor-Variable. R wandelt die Kategorien in einzelne Dummy-Variablen um,
# läßt dabei aber eine Kategorie außen vor, die sogenannte Basiskategorie
summary(lm(EXPEND ~ INCOMEGROUP, data = x))



######## ---- Die Basiskategorie verstehen und interpretieren ---- ########

# R muss bei Faktoren, aber auch bei Dummy-Variablen (also logischen Variablen) immer eine Kategorie
# auslassen, die Basiskategorie. Sie können R natürlich zwingen, alle Kategorien aufzunehmen, hier 
# im Beispiel die beiden Kategrien des Wahrheitswerts DEMOCRATIC. Dazu erzeugen wir zunächst eine neue 
# Dummy-Variable DEMOCARTIC.2, die immer dann TRUE ist, wenn DEMOCRATIC FALSE ist, und umgekehrt
x$DEMOCRATIC.2 <- !x$DEMOCRATIC
# Beide Variablen nehmen wir in unser Regressionsmodell auf. Das geht aber nicht gut. R läßt einen der
# beiden Dummy-Regressoren bei der Schätzung unberücksichtigt, weil die beiden Variablen perfekt 
# voneinander abhängig sind, und so ihre beiden Effekte nicht voneinander getrennt berechnet werden können
summary(lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC + DEMOCRATIC.2, data = x))

# Weil also bei kategorialen Variablen stets eine Ausprägung von den anderen abhängig ist, können nicht
# alle Kategorien spearat geschätzt werden, die Basiskategorie wird ausgelassen. Welche das ist, können
# Sie aber selbst festlegen. Wir wollen hier im Beispiel alle Effekte relativ zur Basiskategorie "Low
# income" messen.
x$INCOMEGROUP.2 <- relevel(x$INCOMEGROUP, ref = "Low income")
# Das führt dann zu diesen Ergebnissen, die sich jeweils relativ zur Basiskategorie "Low income"
# interpretieren
summary(lm(EXPEND ~ INCOMEGROUP.2, data = x))

# Die perfekte lineare Abhängigkeit (Multikollinearität) der unterschiedlichen Levels der kategorialen, 
# unabhängigen Variable wird erst durch die Konstante in der Regressionsgleichung möglich. Also lassen
# wir diese einfach bei der Schätzung weg.
# Dazu definieren wir uns zunächst eine Reihe von Dummies, die die verschiedenen Levels unserer Variable
# INCOMEGROUP abbilden
x$INC.HighOECD <- x$INCOMEGROUP == "High income: OECD"
x$INC.HighNonOECD <- x$INCOMEGROUP == "High income: nonOECD"
x$INC.UpperMiddle <- x$INCOMEGROUP == "Upper middle income"
x$INC.LowerMiddle <- x$INCOMEGROUP == "Lower middle income"
x$INC.Low <- x$INCOMEGROUP == "Low income"
# Diese Dummy-Variablen sind logische Werte (TRUE/FALSE). Wir nutzen den Umstand aus, das R TRUE als 1 
# FALSE als 0 codiert und wandeln die logischen Variablen in Integer-(Ganzzahl)-Variablen um
x$INC.HighOECD <- as.integer(x$INC.HighOECD)
x$INC.HighNonOECD <- as.integer(x$INC.HighNonOECD)
x$INC.UpperMiddle <- as.integer(x$INC.UpperMiddle)
x$INC.LowerMiddle <- as.integer(x$INC.LowerMiddle)
x$INC.Low <- as.integer(x$INC.Low)
# Mit diesen neuen Regressoren schätzen wir jetzt unser Modell. Die Konstante nehmen wir dabei durch
# Hinzufügen von "-1" aus die Modellformel heraus (sie wird von R standardmäßig hinzugefügt und muss 
# explizit entfernt werden, wenn man ein Modell ohne Konstante schätzen will).
# Jetzt erhalten wir Koeffizienten für jedes Level unserer kategorialen Variable. Dese Koeffizienten 
# interpretieren sich als erwartete Staatsausgabenquote für Länder der betreffenden Kategorie
summary(lm(EXPEND ~ INC.HighNonOECD + INC.HighOECD + INC.UpperMiddle + INC.LowerMiddle + INC.Low - 1, data = x))
# Dass das stimmt, läßt sich leicht überprüfen, zum Beispiel für das Level "High income: OCED"
mean(x$EXPEND[x$INCOMEGROUP == "High income: OECD"], na.rm = TRUE)



################ ---- Verletzung der Annahmen des linearen Regressionsmodells ---- ################


######## ---- Heteroskedastizität ---- ########

# Wir gehen erneut von folgenden Modell aus:
m <- lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)

# Grafische Diagnose von Heteroskedastizität.
# Gibt es einen Zusammenhang zwischen den Residuen und den unabhängigen Variablen ...?
plot(x$UNEMPLOY[as.integer(names(m$residuals))], m$residuals)
plot(x$POPCHILDREN[as.integer(names(m$residuals))], m$residuals)
# ... oder vielleicht der abhängigen Variable?
plot(x$EXPEND[as.integer(names(m$residuals))], m$residuals)

# Formale Tests auf Heteroskedastizität.
# Der Breusch-Pagan-Test (Package lmtest). Standardmäßig wird  ein Zusammenhang der Residuen mit 
# den *unabhängigen* Variablen untersucht
bptest(m, studentize = FALSE)

# Alternativ kann, indem eine einseitige Formel als Argument angegeben wird, auch ein möglicher 
# Zusammenhang der Residuen mit der *abhängigen* Variable analysiert werden
bptest(m, ~EXPEND, studentize = FALSE, data = x)

# Der Goldfeld-Quandt-Test. Die Variable, von der vermutet wird, dass sie einen Zusammenhang mit den 
# Residuen aufweist, wird als einseitige Formel übergeben
gqtest(m, order.by = ~UNEMPLOY[as.integer(names(m$residuals))], data = x)
# Der Teil der Beobachtungen, der beim Goldfeld-Quandt-Test "in der Mitte" ausgelassen wird, nachdem
# alle Beobachtungen nach der Größe der interessierenden unabhängigen Variable sortiert worden sind,
# kann mit dem Argument fraction festgelegt werden, in diesem Beispiel auf 20 %
gqtest(m, order.by = x$UNEMPLOY[as.integer(names(m$residuals))], fraction = 0.2, data = x)

# Um die Auswirkungen des Problems der Heteroskedastizität auf unsere Hypothesentests zu beheben, 
# können wir den normalen Hypothesentest auf Gleichheit der Koeffizienten mit 0 auf Basis von 
# White's heteroskedastizitätskonsistenten Standardfehlern durchführen. Die berechnet uns die Funktion
# vcovHC (aus dem Package sandwich) auf Basis für unser Regressionsmodells m
coeftest(m, vcov. = vcovHC(m, type = "HC3"))



######## ---- Multikollinearität ---- ########

# Um das Problem der perfekten Multikollinearität zu verdeutlichen, erzeugen wir eine neue Variable, 
# die eine Linearkombination der existieren Variable UNEMPLOY ist
x$UNEMPLOY.LIN = 3 + 1.5*x$UNEMPLOY
# Beziehen wir beide Variablen als Regressoren in ein Regressionsmodell ein, kann nur der Koeffizient 
# einer der beiden Variablen geschätzt werden
summary(lm(EXPEND ~ UNEMPLOY + UNEMPLOY.LIN, data = x))

# In der Realität gibt es selten perfekte Multikollinearität, sehr häufig aber imperfekte.
# Wenn wir vermuten, dass zwischen zwei Variablen unseres Datensatzes ein (imperfekter) linearer 
# Zusammenhang besteht, läßt sich das natürlich leicht mit Hilfe des Korrelationskoeffizienten untersuchen
cor(x$POPSENIORS, x$POPCHILDREN, use = "pairwise.complete.obs", method = "pearson")
# Der zusammenhang muss natürlich nicht zwischen *zwei* Variablen bestehen, es kann auch ein linearer 
# Zusammenhang mehrerer Regressoren sein. Solch ein Zusammenhäng läßt sich mit einer Hilfregression
# feststellen
summary(lm(POPSENIORS ~ UNEMPLOY + POPCHILDREN + DEMOCRATIC, data = x))

# Multikollinearität führt zu größeren Standardfehlern. Wieviel größer die Standardfehler gegenüber dem
# Fall ohne Multikollinearität sind, beziffert der Variance-inflating factor (VIF), der mit der gleich-
# namigen Funktion (Package car) berechnet werden kann
m <- lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)
vif(m)



######## ---- Nicht normalverteilte Störgrößen ---- ########

# Wir gehen wieder von unserem Standardmodell als Beispiel aus
m <- lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)

# Zu Untersuchung, ob die Residuen normalverteilt sind, verwenden wir die Funktion jarque.bera.test() aus dem
# Package tseries. Sie führt den populären Jarque-Bera-Test auf Normalverteilung der Residuen durchführt.
# Dieser Test vergleicht die Schiefe und die Wölbung der Residuenverteilung mit der Schiefe und Wölbung der 
# Normalverteilung
jarque.bera.test(m$residuals)



######## ---- Autokorrelation ---- ########

# Grafische Analyse.
# Zu Diagnose von Autokorrelation können Sie sich die Residuen grafisch anschauen und so festzustellen
# versuchen, ob eine Zusammenhang zwischen den Residuen besteht
m <- lm(EXPEND ~ UNEMPLOY + POPCHILDREN + DEMOCRATIC, data = x[order(x$POPSENIORS),])
plot(m$residuals)

# Formale Tests.
# Alternativ kann der bekannte Durbin-Watson-Test eingesetzt werden (lmtest) zur Verfügung gestellt wird.
# Er untersucht, ob je zwei aufeinanderfolgende Residuen im Sinne eines autoregressiven Prozesses 1. Ordnung 
# zusammenhängen
dwtest(m)

# Der Breusch-Godfrey-Test (ebenfalls aus dem lmtest-Package) ist allgemeiner. Er berücksichtigt nicht 
# nur autoregressive Prozesse 1. Ordnung, sondern kann autoregressive Zusammenhänge höherer Ordung
# aufdecken; die maximale Ordnung wird durch das Argument order angegeben, im Beispiel 5
bgtest(m, order = 5)

# Zur Behebung der Auswirkungen von Autokorrelation auf die Ergebnisse statistischer Hypothesentests werden
# Newey-West-Standardfehler verwendet. Ebenso wie im Fall der Heteroskedastizität werden dabei zunächst
# die korrigierten Standardfehler berechnet, in diesem Fall mit der Funktion vcovHAC() (HAC = "autocorrelation
# and heteroscedasticity consistent"). Diese werden anschließend der Funktion coeftest() übergeben, um
# den Standardtest der Hypothesen, dass je ein Koeffizient gleich 0 ist, durchzuführen
coeftest(m, vcov. = vcovHAC(m))



######## ---- Spezifikationsfehler ---- ########

# Um die Diagnose irrtümlich ausgelassener Variablen zu demonstrieren, berechnen wir zuächst die zweiten
# und dritten Potenzen von POPSENIORS
x$POPSENIORS2 <- x$POPSENIORS^2
x$POPSENIORS3 <- x$POPSENIORS^3

# Diese Variablen gehen in folgendes einfache Modell ein, von dem wir annehmen wollen, es sei das 
# "wahre" Modell (also das Modell des Zusammenhangs in der Grundgesamtheit)
m1 <- lm(EXPEND ~ POPSENIORS + POPSENIORS2 + POPSENIORS3, data = x)
# Wir gehen aber davon aus, dass wir irrtümlicherweise ein Modell schätzen, in dem die zweiten und 
# dritten Potenzen von POPSENIORS fehlen
m2 <- lm(EXPEND ~ POPSENIORS, data = x)

# Testen können wir diese Art der Fehlspezifikation mit Ramsey's RESET-Verfahren, das von der Funktion
# reset() im Package lmtest bereitgestellt word
reset(m2)
# Die Potenzen der unabhängigen Variablen, die beim RESET-Verfahren berücksichtigt werden sollen, können
# wir mit dem Argment "power" bestimmen, zum Beispiel also für die zweiten, dritten und vierten Potenzen ...
reset(m2, power = 2:4)
# ... oder so
reset(m2, power = c(2,3,4))

# Gehen wir nun davon aus, dass dasfolgende Modell den "wahren" Zusammenhang in der Grundgesamtheit 
# abbildet, ...
m1 <- lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)
# ... wir aber irrtümlich dieses Modell schätzen, also POPSENIORS als Regressor auslassen
m2 <- lm(EXPEND ~ UNEMPLOY + POPCHILDREN + DEMOCRATIC, data = x)
# Dieses Modell können wir nun mit dem Durbin-Watson-Test daraufhin untersuchen, ob seine Residuen einen
# Zusammenhang mit einer Variablen, von der wir vermuten, wir könnten sie vergessen haben, aufweisen,
# in unserem Beispiel POPSENIORS. Dazu müssen wir zunächst das Modell nocheinmal schätzen, wobei wir 
# die Beobachtungen nach der "verdächtigen" Variable sortieren
m2 <- lm(EXPEND ~ UNEMPLOY + POPCHILDREN + DEMOCRATIC, data = x[order(x$POPSENIORS),])
# Dann können wir den Durbin-Watson-Test (ebenfalls aus dem lmtest-Package) anwenden, der uns schon bei 
# Diagnose von Autokorrelation begegent ist. Sein Ergebnis deutet tatsächlich auf Fehlspezifikation hin
dwtest(m2)
# Beziehen wir die fehlende Variable POPSENIORS dagegen in das Modell ein und unterziehen dieses Modell
# erneut dem Durbin-Watson-Test, dann kann die Hypothese, dass keine Fehlspezifikation vorliegt, nicht
# mehr zurückgewiesen werden
m3 <- lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + 	DEMOCRATIC, data = x[order(x$POPSENIORS),])
dwtest(m3)




################ ---- Entwicklung von Regressionsmodellen - Ein paar Tipps ---- ################


# Wir schätzen uns ein Beispielmodell
m <- lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN, data=x)

# Dann untersuchen wir den Effekt des Auslassens einer einzelnen Beobachtung aus dem Modell
# Absoluter Effekt auf die Koeffizienten ...
dfbeta(m)
# ... Effekt in Standardabweichungen der Koeffizienten
dfbetas(m)

# Aus dem Datensatz eine Teilsprichprobe ziehen, um die Robustheit der Ergebisse zu validieren
ind <- sample(1:NROW(x$COUNTRY), size = 150)	# 150 Zufällige Indexwerte genererieren
x.sample <- x[ind,]								# Damit unseren Datensatz indizieren und so
												# 150 Beobachtungen ziehen
NROW(x.sample$COUNTRY)							# Unser neuer Datensatz hat tatsächlich 150
												# Beobachtungen. Damit könnten wir nun prüfen,
												# ob unsere bisherigen Ergebnisse auch in der Teil-
												# Stichprobe halten und in diesem Sinne "robust" sind