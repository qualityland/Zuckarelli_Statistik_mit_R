# §§§§§§§§§§§§§§§§§§§§§§§§§§  KAPITEL 7 - KATEGORIALE DATEN ANALYSIEREN (INFERENZSTATISTIK II)  §§§§§§§§§§§§§§§§§§§§§§§§§§



################ ---- Das lineare Wahrscheinlichkeitsmodell ---- ################

# In diesem Abschnitt geht es um abhängige Variablen, die kategorial sind, also Faktoren und logische
# Werte (Dummies). Wir schaffen uns dazu zunächst einmal eine kategoriale abhängige Variable auf Basis 
# unserer Variable EXPEND. Die neue Variable x$EXPHIGHER teilt alle Länder in "Staatsausgabenquote 
# größer als Median" (1) und "Staatsusgabenquote kleiner als oder gleich Median" (0)
x$EXPHIGHER <- 0
x$EXPHIGHER[x$EXPEND > median(x$EXPEND, na.rm = TRUE)] <- 1

# Dann regressieren wir die neue Variable auf den Seniorenanteil. Et voilà: Ein lineares
# Wahrscheinlichkeitsmodell!
m <- lm(EXPHIGHER ~ POPSENIORS, data = x)
summary(m)




################ ---- Logit- und Probit-Modelle ---- ################


######## ---- Abhängige kategoriale Variablen mit zwei Kategorien ---- ########

# Wir schätzen ein einfaches Logit-Modell. Die abhängige Variable ist dabei EXPHIGHER, das wir im
# vorangegangen Abschnitt berechnet hatten
logitm <- glm(EXPHIGHER ~ POPSENIORS, data = x, family = binomial("logit"))
summary(logitm)				# Anzeige
# Die Koeffizienten geben Auskunft darüber, wie sich das Chancenverhältnis (Odds ratio) ändert, wenn die
# unabhängige Variable um 1 Einheit erhöht wird. Der Faktor, um den sich das Chancenverhältnis in diesem
# Fall ändert, läßt sich leicht errechnen
exp(logitm$coefficients[2])

# Damit läßt sich das neue Chancenverhältnis nach einer Erhöhung von POPSENIORS um einen Prozentpunkt 
# bestimmen, wobei wir hier davon ausgehen, dass das Chancenverhältnis anfänglich bei 0,5 gelegen hat 
OR <- 0.5 * exp(logitm$coefficients[2])
# Aus dem Chancenverhältnis läßt sich durch Umstellen die Wahrscheinlichkeit errechnen
pi <- OR / (1 + OR)

# Logit-Modelle können natürlich mehr als nur eine unabhängige Variable wie in unserem einfachen Beispiel 
# oben haben
logitm2 <- glm(EXPHIGHER ~ POPSENIORS + GROWTH + DEMOCRATIC, data = x, family = binomial("logit"))
summary(logitm2)				# Anzeige

# Analog zum F-Test bei klassischen linearen Regressionsmodellen erlaubt es der Likelihood Ratio-Test,
# der von der Funktion lrtest() im Package lmtest zur Verfügung gestellt wird, die Frage zu untersuchen,
# ob das MOodell ingesamt einen Beitrag zur Erklärung der unabhängigen Variable leistet
lrtest(logitm2)

# Auch ein Äquivalent für das Bestimmtheitsmaß R^2 gibt es: McFadden's Pseudo R^2. Es errechnet sich aus
# den Residual deviances, die im Rückgabeobjekt der Funktion glm() enthalten sind
1 - logitm2$deviance / logitm2$null.deviance



######## ---- Abhängige kategoriale Variablen mit mehreren Kategorien ---- ########


#### ---- Modelle mit nominalen abhängigen Variablen ---- #####

# Der Einfachheit halber fassen wir die beiden "High income"-Kategorien (OECD/Non-OECD) zu einer zusammen
levels(x$INCOMEGROUP)[1] <- "High income"
levels(x$INCOMEGROUP)[2] <- "High income"
levels(x$INCOMEGROUP)				# Anzeige

# Mit der Funktion multinom() aus dem nnet-Package können wir unser erstes multinomiales Modell schätzen
logitpoly <- multinom(INCOMEGROUP ~ DEMOCRATIC + URBAN, data = x)
summary(logitpoly)

# Potenziert man die Eulersche Zahl e mit den Modell-Koeffizienten, erhält man die multiplikativen Effekte
# einer Erhöhung der jeweiligen unahängigen Variable auf das Chancenverhältnis. Beispiel hier: Effekt des
# Urbanisierungsgrades auf das Chancenverhältnis für "Low income" (relativ zur Basiskategorie "High income")
exp(summary(logitpoly)$coefficients[1,3])
# Das können wir uns auch für alle Koeffizienten auf einmal anschauen
exp(summary(logitpoly)$coefficients)

# Die Basiskategorie läßt sich natürlich ändern. Das geschieht mit der Funktion relevel()
x$INCOMEGROUP <- relevel(x$INCOMEGROUP, ref = "Low income")

# Wenn wir nicht an Chancenverältnissen, sondern an absoluten Wahrscheinlichkeiten interessiert sind,
# könnten wir uns die geschätzten Wahrscheinlichkeiten für unsere Beobachtungen anschauen
logitpoly$fitted.values
# Jetzt wollen wir eine Vorhersage für einen anderen Datensatz von unabhängigen Variablen erstellen. 
# Dazu generieren wir uns zunächst als Beispiel einen neuen Datensatz, der identisch mit unserem alten ist, 
# außer dass wir den Urbanisierungsgrad bei jeder Beobachtung um einen Prozentpunkt erhöhen
xplus1 <- x							# Datensatz kopieren
xplus1$URBAN <- xplus1$URBAN + 1	# URBAN um einen Prozentpunkt erhöhen

# Dann machen wir eine Vorhersage auf Basis dieses neuen Datensatzes mit Hilfe der Funktion predict().
# Das Argument type="probs" legt fest, dass wir die absoluten Wahrscheinlichkeiten sehen wollen, ...
res <- predict(logitpoly, newdata = xplus1, type = "probs")
# ... wir hätten uns aber auch die geschätzten Kategorien der abhängigen Variable ausgeben lassen können
# (Argument type="class")
predict(logitpoly, newdata = xplus1, type = "class")

# Jetzt berechnen wir die Chancenverhältnisse für "Low income" (relativ zur Basiskategorie "High income")
# vor und nach der Erhöhung des Urbanisierungsgrades um einen Prozentpunkt
or.vor <-logitpoly$fitted.values[1,2] / logitpoly$fitted.values[1,1]
or.nach <- res[1,2] / res[1,1]
# Wie man sieht, hat sich das Chancenverhältnis mit dem Faktor verändert, der der als Exponent von e 
# gemommene Koeffizient des Urbanisierungsgrades ist
or.nach/or.vor
exp(summary(logitpoly)$coefficients["Low income", "URBAN"])



#### ---- Modelle mit ordinalen abhängigen Variablen ---- #####

# Wir wollen ein geordnetes Logit-Modell schätzen. Dazu müssen zunächst die Faktorlevels der abhängigen
# Variable mit der Funktion ordered() in die richtige Reihenfolge gebracht werden
x$INCOMEGROUP2 <- ordered(x$INCOMEGROUP, c("High income", "Upper middle income", "Lower middle income", "Low income"))

# Dann kann das geordnete Logit-Modell mit der Funktion polr() aus dem Package MASS geschätzt werden
logitorder <- polr(INCOMEGROUP2 ~ POPSENIORS + URBAN, data = x, method = "logistic")
summary(logitorder)					# Anzeige

# Wie zeigen uns die pro Beobachtung in unserem Datensatz geschätzten Wahrscheinlichkeiten für die 
# verschiedenen Kategorien der abhängigen Variable an. Analog zu den multinomialen, also ungeordneten
# Logit-Modellen, könnten wir uns mit Hilfe der Funktion predict() für einen neuen Datensatz (Argument 
# newdata) die geschätzten Wahrscheinlichkeiten für die verschiedenen Kategeorien der abhängigen Variable
# (Argument type="probs") oder die erwarteten Kategorien selbst (Argument type="class") für jede
# Beobachtung anzeigen lassen
logitorder$fitted.values

# p-Werte berechnen (ähnlicher Ansatz wie bei nominalen (ungeordneten) Logit-Modellen mit multinom() im
# vorangegangenen Abschnitt
t <- summary(logitorder)$coefficients[,"t value"]
p <- pnorm(abs(t), lower.tail = FALSE) * 2