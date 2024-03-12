# §§§§§§§§§§§§§§§§§§§§§§§§§§  KAPITEL 3 - MIT DATEN ARBEITEN  §§§§§§§§§§§§§§§§§§§§§§§§§§




################ ---- Einfache Variablen und Zuweisungen ---- ################


######## ---- Numerische Variablen ---- ########

# Eine erste Zuweisung: Wir weisen der Variablen x den Wert 1 zu
x <- 1

# Es geht aber auch so herum
1 -> y

# Anschauen des Werts durch Eingeben der Variablen
x
y

# Das geht aber nur, wenn die Variable auch existiert
z   					# Achtung: Existiert nicht, daher Fehlermeldung!

# Zahlen am Anfang von Variablennamen sind unzulässig
2maleins <- 2*1   		# Achtung: Fehlermeldung!

# Ein Punkt am Anfang ist aber zulässig ...
.nullkommnull <- 0.0
# ... ein Unterstrich hingegen nicht
_pi <- 3.1415926535



######## ---- Zeichenketten ---- ########

# Zwueisung einer Zeichenketten-Variable (character)
name.hund <- "Wuffi"

# Wert wieder anzeigen
name.hund

# Die zugwiesene Zeichenkette muss in Anführungszeichen stehen, sonst denkt R, wir wollten der einen
# Variable eine andere Variable zuweisen
name.hund <- Wuffi		# Achtung: Fehlermeldung, weil die *Variable* Wuffi nicht existiert!



######## ---- Logische Werte ---- ########

# Das hier ist ein logischer Ausdruck, er kann entweder wahr (TRUE) oder falsch (FALSE) sein
name.hund == "Wauzi"	# Doppeltes Gleichheitszeichen ist Vergleichsoperator

# Den (logischen) Wert dieses Ausdrucks können wir in einer Variable speichern ...
z <- name.hund == "Wauzi"
# ... und uns davon überzeugen, dass sie tatsächlich das Ergebnis des logischen Ausdrucks aufgenommen hat
z

# Natürlich können wir logischen Variablen Werte auch direkt zuweisen, mit Hilfe der Konstanten TRUE und
# FALSE
z <- TRUE

# Achtung: TRUE und FALSE dürfen nicht in Anführungszeichen stehen, R macht Ihre Variable sonst zu einer 
# Zeichenketten-Variable
z <- "TRUE"   			# Ist eine Zeichenkettenvariable, keine logische Variable

# Weil R die logischen Werte TRUE und FALSE intern durch 1 und 0 repräsentiert, kann man mit logischen 
# Variablen auch rechnen
z <- TRUE
z*5						# =1, weil TRUE den Zahlenwert 1 hat



######## ---- Faktoren ---- ########

# Wir legen uns für die folgenden Beispiele einen Faktor an; Sie müssen sich hier nicht weiter 
# damit beschäftigen, was genau dabei passiert
hunde.rasse <- as.factor("Golden Retriever")
levels(hunde.rasse) <- c("Boxer", "Golden Retriever", "Dalmatiner", "Dackel")


# Wir lassen uns die Levels, das heißt, die Ausprägungen, der Faktorvariable anzeigen ...
levels(hunde.rasse)

# ... und den eigentlichen Inhalt der Variablen hunde.rasse, also die einzelnen Werte
hunde.rasse				# Anzeigenn

# Nun weisen wir der Faktorvariable hunde.rasse einen neuen Wert zu und schauen uns das Ergebnis an
hunde.rasse <- "Boxer"
hunde.rasse				# Anzeigen

# Achtung: Schreiben wir die Ausprägung nicht ganz genau so, wie das Level heißt, wird die Variable
# zu einer Zeichenketten-Variable, weil der Wert nicht in das Ausprägungsschema paßt
hunde.rasse <- "boxer"	# Klein geschrieben
# Die Anzeige der Levels verschwindet, weil jetzt character-Variable statt factor-Variable
hunde.rasse



######## ---- Datentypen von Variablen ermittelen und konvertieren ---- ########

# Mit Zeichenketten kann R nicht rechnen
x <- "10"
x*5						# Achtung: Fehlermeldung!

# Wir können die Variable aber in eine numerische Variablen konvertieren; danach ist auch die Rechnung
# möglich
class(x) <- "numeric"
x*5

# Ein anderes Beispiel für eine Konverierung
x <- "TRUE"   			# Das ist eine Zeichenketten-Variable, keine logische Variable!
x   					# Anzeigen

# Der Typ bestätigt: Es ist eine character-Variable, enthält also eine Zeichenkette
class(x)

# Jetzt ändern wir den Typ, dass die Variable eine logische Variable wird
class(x) <- "logical"

# Dann wird auch der Wert von x als logischer Wert erkannt
x   					# Anzeigen
class(x)  # In der Tat hat sich der Typ der Variablen gändert

# Konvertieren können Sie Variablen auch mit den as."type"-Funktionen. Ein Beispiel mit as.logical
wahr <- "TRUE"
class(wahr)   			# Zuerst eine Zeichenketten-Variable
wahr <- as.logical(wahr)# Konvertierung
wahr					# Anzeigen
class(wahr)   			# Jetzt eine logische Variable



######## ---- Variablen löschen ---- ########

# Anzeigen, welche Variablen aktuell im Speicher von R existieren:
ls()

# Dann löschen wir die Variable hunde.rasse, die wir eben angelegt haben
rm(hunde.rasse)

# Sie können sich leicht überzeugen, dass die Variable tatsächlich verschwunden ist
ls()




################ ---- Vektoren ---- ################


######## ---- Vektoren anlegen ---- ########

# Wir legen einen ganz einfachen Vektor aus numerischen Werten an ...
x <- c(5,3,7)
x   					# Anzeigen des Vektorinhalts

# ... und einen, der aus logischen Werten besteht ...
sieliebtmich <- c(TRUE, TRUE, TRUE, FALSE, TRUE)
sieliebtmich   			# Anzeigen

# ... einen Vektor aus drei Einzelvariablen ...
x <- 5
y <- c(3)				# Wenn Ihre Variable nur einen Wert enthält, können Sie wahlweise die Funktion
						# c() verwenden, oder auch nicht
z <- 7
koordinate <- c(x,y,z)	# Beim Anlegen eines Vektors mit mehreren Elementen ist die Verwendung von c()
						# zwingend notwendig
koordinate   			# Anzeigen

# ... und einen aus drei Vektoren mit je zwei Elementen
x <- c(1,2)
y <- c(3,4,5)
z <- c(x,y)
z   					# Anzeigen

# Dieser Vektor...
z <- c(5, "Eine Zeichenkette", TRUE)
z   					# Anzeigen
# ... ist ein Vektor von Zeichenketten, ein character-Vektor, denn das ist der "kleinste gemeinsame
# Nenner" seiner drei Elemente
class(z)

# Genauso dieser Fall, wo ein numerischer Vektor der "kleinste gemeinsame Nenner" ist
z <- c(5, FALSE, TRUE)
z   					# Anzeigen

# Die Länge eines Vektors, das heißt, die Zahl seiner Elemente, bestimmen Sie mit der Funktion NROW() ...
NROW(z)
# ... oder mit der Funktion length()
length(z)

# Sie können der Länge aber auch einen Wert zuweisen; mit der folgenden Anweisung verkürzen Sie den
# Vektor auf zwei Elemente ...
length(z) <- 2
z   					# Anzeigen
# ... oder verlängern ihn auf 5 Elemente, wobei die "neuen" Elemente mit Missings (NA) aufgefüllt werden
length(z) <- 5
z   					# Anzeigen



######## ---- Mit Missings umgehen ---- ########


# Mit Missings läßt sich nicht rechnen: Hier weisen machen wir zunächst die Variable x zu einem Missing
x <- NA
# Dann versuchen wir, sie mit 5 zu multiplizieren; das Ergebnis ist wieder ein Missing 
x*5

# Mit Hilfe der Funktion is.na läßt sich leicht feststellen, ob eine Variable ein Missing ist
is.na(x)

# Das funktioniert nicht nur bei Variablen, die nur einen Wert enthalten, sondern auch bei Vektoren
z <- c(5, 0, NA, NA, NA)
is.na(z)



######## ---- Auf einzelne Elemente eines Vektors zugreifen ---- ########


# Wir nehmen wieder den Vektor von eben ...
z <- c(5, 0, NA, NA, NA)
# ... und greifen auf sein erstes ....
z[1]				# Anzeigen
# ... und sein fünftes Element zu
z[3]				# Anzeigen

# Das Zuweisen von Werten zu einzelnen Elementen funktioniert analog
z[5] <- 7
z   				# Anzeigen

# Unser Vektor besteht aus fünf Elementen; was passiert, wenn wir versuchen, auf ein Element zuzugreifen,
# das gar nicht existiert?
z[8]				# Es wird ein Missing (NA) angezeigt

# Als Indizes, mit denen wir Elemente aus einem Vektor selektieren, können wir auch andere 
# Variablen verwenden
index <- c(3,5)
z[index]			# Anzeigen

# So wie in der nächsten Anweisung geht es allerdings nicht. Der Vektor hat nur eine Dimension, die 
# vewendete Indizierung deutet aber auf zwei Dimensionen, also auf eine Matrix, hin
z[3,5] 				# Achtung: Fehlermeldung!

# So dagegen funktioniert es, weil wir nur eine Dimension ansprechen, aus dieser aber zwei Elemente 
# herausgreifen
z[c(3,5)]			# Wir greifen auf das dritte und das fünfte Elemente zu; durch das Verbinden zu einem
					# Vektor zeigen wir R, dass die beiden Elemente zur gleichen Dimension gehören
					# (nämlich zu der einen, die der Vektor hat)

# Ein Index kann auch aus logischen Werten bestehen. Jeder logische Wert gibt dann an, ob das 
# Vektorelement, das an dieser Stelle steht, selektiert werden soll (TRUE) oder nicht (FALSE)
index <- c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
z[index]			# Anzeigen

# Mit Hilfe eines solchen Index von logischen Werten können wir zum Beispiel auch alle Elemente des 
# Vektors selektieren, die kein Missing sind. Der Vektor von Wahrheitswerten ist hier der ausgewertete
# logische Asudruck is.na(z) == FALSE, der für jedes Vektorelement entweder TRUE oder FALSE sein kann
z[is.na(z) == FALSE]

# Nochmal Schritt für Schritt: is.na liefert TRUE für Missings, FALSE für Nicht-Missings
is.na(z)			# Anzeigen
# is.na(z) == FALSE prüft, ob der Rückgabewert von is.na(z) FALSE ist, ob also kein Missing vorliegt.
# Anders ausgedrückt: is.na(z) == FALSE ist dort (für diejenigen Elemente des Vektors v) TRUE, wo is.na(z) FALSE ist
is.na(z) == FALSE
# Mit diesem Vektor von logischen Werten hätte man auch manuell selektieren können
z[c(TRUE, TRUE, FALSE, FALSE, TRUE)]

# Einfacher noch schreibt sich is.na(z) == FALSE mit dem Negationsoperator, der den Wahrheitswert einer 
# logischen Aussage herumdreht
!TRUE				# Ist FALSE
# Damit schreibt sich unsere Indizierung, die im Vektor z nach Elementen sucht, die keine Missings sind
z[!is.na(z)]		# Anzeigen




################ ---- Dataframes ---- ################


# Wir erzeugen uns einen Datensatz. Dazu legen wir zunächst eine Reihe von Vektoren an, aus denen sich 
# unser neuer Datensatz zusammensetzen soll
vorname <- c("Hanna", "Peter", "Sophie", "Ulrike", "Thomas")
alter <- c(21, 20, 22, 20, 19)
semester <- c(3, 3, 5, 3, 1)
# Dann erzeugen wir einen neuen Dataframe aus den drei Einzelvektoren
studenten <- data.frame(vorname, alter, semester)

# Die Vektoren in einem Dataframe sind alle geich lang! Das ist im folgenden Beispiel nicht gegeben,
# deshalb erhalten wir hier eine Fehlermeldung
vorname <- c("Hanna", "Peter", "Sophie", "Ulrike", "Thomas")
alter <- c(21, 20, 22, 20) 	# Dieser Vektor ist um ein Element zu kurz
semester <- c(3, 3, 5, 3, 1)
studenten <- data.frame(vorname, alter, semester)	# Achtung: Fehlermeldung

# Den Inhalt des Dataframes können wir uns direkt in der R-Konsole anschauen ...
studenten
# ... oder aber - etwas schicker - im R-Editor
View(studenten)

# Häufig werden Sie auf die einzelnen Variablen eines Dataframes zugreifen müssen. Hier lautet die
# Notation datensatz$variable. Folgende Anweisung zeigt uns also die Variable Alter (in einem 
# Extra-Fenster des R-Editors) an
View(studenten$alter)

# Mit dieser Notation können Sie auch auf die Variable zugreifen, um Berechnungen durchzuführen, 
# zum Beispiel, um das Durchschnittsalter zu ermitteln ...
mean(studenten$alter)
# ... oder, um einzelnen Elementen einer Variable einen Wert zuzuweisen, hier etwa der vierten Beobachtung 
# (das ist Ulrike) das Alter 21
studenten$alter[4] <- 21

# Da ein Dataframe letztlich eine Tabelle ist, können Sie auch über die Zeilen- und Spaltennummer
# auf eine einzelne Datenpunkte zugreifen
studenten[4,2]   	# Wiederum das Alter von Ulrike (vierte Zeile, zweite Spalte)

# Nicht nur die Anzeige von Datensätzen unterstützen die R-Editoren, auch ihre Bearbeitung
edit(studenten)

# Wie Sie oben gesehen haben, müssen Sie den Datensatz aber nicht unbedingt  mit edit() öffnen, 
# um ihn zu bearbeiten. 
# Jetzt fügen wir dem Datensatz eine Variable hinzu, die anzeigt, ob ein Student im dritten Semester ist
studenten$drittes.semester <- studenten$semester == 3
# Diese Variable ist natürlich ein logischer Wert, ein Wahrheitswert
class(studenten$drittes.semester)

# Und so löschen wir die eben angelegte Variable wieder
studenten$drittes.semester <- NULL

# So wie in der folgenden Anweisung geht das Löschen übrigens nicht; rm() löscht nur *ganze* Objekte aus 
# dem R-Speicher, nicht aber einzelne Variablen eines Dataframes, genausowenig einzelne Elemente eines 
# Vektors
rm(studenten$drittes.semester)  # Achtung: Fehlermeldung!




################ ---- Einlesen von Daten nach R ---- ################


# Der erste Versuch, unseren Beispieldatensatz einzulesen
x <- read.table("daten_final.csv", header = TRUE, sep = ";", dec = ",")  # Gibt ein paar Warnungen

# Besser ist es so...
x <- read.table("daten_final.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", na.strings = "")
# ... und kürzer, das heißt ohne die Namen der Argumente der Funktion read.table(), so
x <- read.table("daten_final.csv", TRUE, ";", "\"", ",")

# Sie können auch Daten zum Beispiel aus Excel einlesen ...
x <- read.xls("daten.xslx", sheet = "Final")  	# Setzt voraus, dass die Datei daten.xlsx tatsächlich in
												# Ihrem Arbeitsverzeichnis existiert
# ... und sogar aus der Zwischenablage
x <- readClipboard()


# Einen einfachen Überlick über die Variablen im Datensatz verschaffen Sie sich mit der Funktion names()
names(x)