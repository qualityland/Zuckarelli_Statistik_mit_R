# §§§§§§§§§§§§§§§§§§§§§§§§§§  KAPITEL 9 - PROGRAMMIEREN MIT R  §§§§§§§§§§§§§§§§§§§§§§§§§§



################ ---- Bevor Sie beginnen ... ---- ################


# Setzen Sie Ihr Arbeitsverzeichnis (achtung: ändern!) und legen Sie den Beispieldatensatz dort ab
setwd("C:/users/Name/MeinArbeitsVerzeichnis")

# Den Beispqieldatensatz einlesen
x <- read.table("daten_final.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", na.strings = "")




################ ---- Funktionen ---- ################


# Wir greifen als Beispiel auf die Funktion zurück, die die mittlere absolute Abweichung berechnet.
# Es gibt viele Möglichkeiten, Ihren Code mit Zeilenumbrüchen zu strukturieren...
maa <- function(w) { return(sum(abs(w - mean(w))) / length(w)) }
# ...oder...
maa <- function(w) 
{ 
  return(sum(abs(w - mean(w))) / length(w)) 
}
#...oder....
maa <- function(w) { 
  return(sum(abs(w - mean(w))) / length(w)) 
}

# Wird die Funktion mit einem Argument aufgerufen, wird sie tatsächlich ausgeführt, ....
maa(c(-1,3,9,24))
# .... wird sie dagegen ohne Klammern aufgerufen, werden die R-Anweisungen angezeigt, aus denen
# die Funktion besteht
maa

# Funktionen müssen keine Argumente haben
gruessmich <- function()
{
  return("Hallo Welt")
}
# Aber auch Funktionen ohne Argumente müssen mit Klammern aufgerufen werden
gruessmich()

# Funktionen sind, wie eigentlich alles in R, Objekte
class(maa)

# Auch Operatoren sind Objekte, zum Beispiel der Zuweisungsoperator
'<-'(v, 2)

# Unsere Funktion maa() können wir so umbauen, dass man per Argument festlegen kann, wie sie auf
# Missings (NA) reagieren soll
maa <- function(w, ohne.na) 
{ 
  abw <- w[!is.na(w)] - mean(w, na.rm = ohne.na)
  N <- sum(!is.na(w))
  res <- sum(abs(abw)) / N
  return(res)
}



######## ---- Der Funktionskopf ---- ########

# Hier nochmal die Funktion maa() zur Berechnung der mittleren absoluten Abweichung aus dem letzten Abschnitt
maa <- function(w, ohne.na) 
{ 
  abw <- w[!is.na(w)] - mean(w, na.rm = ohne.na)
  N <- sum(!is.na(w))
  res <- sum(abs(abw)) / N
  return(res)
}

# Sie kann so aufgerufen werden
maa(c(1,2,3), TRUE)

# Oder die Argumente werden über ihre Namen angesprochen
maa(w = c(1,2,3), ohne.na = TRUE)

# Wenn man beim Aufruf keine Argumentenamen verwendet, muss man die Reihenfolge einhalten.
# Dieser Aufruf tut daher nicht ganz das, was er soll
maa(TRUE, c(1,2,3))

# Wenn man aber Argumentenamen verwendet, ist die Reihenfolge der Argumente beim Funktionsaufruf egal
maa(ohne.na = TRUE, w = c(1,2,3))

# Da Funktionen in R Objekte sind, können auch Funktionen anderen Funktionen als Argument übergeben werden
# Ein Beispiel dafür ist die Funtion tapply()
tapply(x$EXPEND, INDEX = x$INCOMEGROUP, FUN = mean)

# Argumente können mit Standardwerten versehen und so zu optionalen Argumenten gemacht werden
maa <- function(w, ohne.na = TRUE)
{ 
  abw <- w[!is.na(w)] - mean(w, na.rm = ohne.na)
  N <- sum(!is.na(w))
  res <- sum(abs(abw)) / N
  return(res)
}

# Optionale Argumente können beim Funktionsaufruf weggelassen werden, R verwendet dann den Standardwert
maa(c(1,2,3))

# Ist aber kein Standardwert festgelegt...
maa <- function(w, ohne.na)
{ 
  abw <- w[!is.na(w)] - mean(w, na.rm = ohne.na)
  N <- sum(!is.na(w))
  res <- sum(abs(abw)) / N
  return(res)
}

# ... führt das Weglassen eines Arguments beim Funktionsaufruf zu einem Fehler
maa(c(1,2,3))



######## ---- Der Funktionsrumpf ---- ########

# Die Funktion cat() wird dazu verwendet, Dinge auf dem Bildschirm auszugeben. Die ihr übergebenen 
# Argumente werden standardmäßig durch Leerzeichen separiert angezeigt
cat("Hallo", "Welt")

# Es kann aber auch ein anderes Separatorzeichen gewählt werden
cat("Hallo", "Welt", sep = "|")

# Wir nutzen cat(), um in unserer Funktion maa() eine Ausgabe auf dem Bildschirm zu vorzunehmen
maa <- function(w, ohne.na = TRUE) 
{ 
  abw <- w[!is.na(w)] - mean(w, na.rm = ohne.na)
  N <- sum(!is.na(w))
  res <- sum(abs(abw)) / N
  cat("Missings entfernen: ", ohne.na, "\n", "Anzahl Missings: ", sum(is.na(w)), "\n", "MAA: ", res, "\n\n", sep = "")
  return(res)
}

# Und so sieht das dann aus:
z <- maa(c(1,NA,3,4))

# Im nächsten Schritt erzeugen wir ein zusammengesetzes Ergebnisobjekt als Liste, das wir dann zurückgeben
maa <- function(w, ohne.na = TRUE) 
{ 
  abw <- w[!is.na(w)] - mean(w, na.rm = ohne.na)
  N <- sum(!is.na(w))
  res <- sum(abs(abw)) / N
  cat("Missings entfernen: ", ohne.na, "\n", "Anzahl Missings: ", sum(is.na(w)), "\n", "MAA: ", res, "\n\n", sep = "")
  ergebnis <- list(MAA = res, Missings = ohne.na)
  return(ergebnis)
}

# Das Rückgabeobjekt, das wir in z speichern, wird auch noch mal angezeigt, zusätzlich zu den
# Bildschirmausgaben, die die Funktion ohnehin schon macht
maa(c(1,2,3))

# Das läßt sich verhindern, indem für die Rückgabe nicht die Funktion return, sondern invisible verwendet wird
maa <- function(w, ohne.na = TRUE) 
{ 
  abw <- w[!is.na(w)] - mean(w, na.rm = ohne.na)
  N <- sum(!is.na(w))
  res <- sum(abs(abw)) / N
  cat("Missings entfernen: ", ohne.na, "\n", "Anzahl Missings: ", sum(is.na(w)), "\n", "MAA: ", res, "\n\n", sep = "")
  ergebnis <- list(MAA = res, Missings = ohne.na)
  invisible(ergebnis)
}

# Jetzt werden nur noch die Ausgaben der Funktion selbst angezeigt, das Rückgabeobjekt wird 
# nicht dargestellt
maa(c(1,2,3))

# Beispiel einer Funktion mit einer variablen Argumentenliste. Die Funktion gibt die Anzahl der ihr
# übergebenen Argumente aus und zeigt das zweite Argument an
zeige.2 <- function(...)
{
  args <- list(...)
  cat("Anzahl der Argumente: ", length(args), "\n")
  cat("Zweites Argument: ", args[[2]])
}




################ ---- Kontrollstrukturen ---- ################


######## ---- Wenn-Dann-Entscheidungen (if-Konstrukte) ---- ########

# Wir schreiben uns eine Funtion, die die Quadratwurzel ihres Arguments zieht und dabei prüft,
# ob das Argument kleiner 0 ist
qwurzel <- function(x)
{
  if(x >= 0)
  {
    return(sqrt(x)) 
  }
  else
  {
    cat("Fehler: Der Wert", x, "ist kleiner als Null. Daher kann keine Qudratwurzel bestimmt werden.")
  }
}

# Prüfen wir, ob die Funktion tut, was sie soll ...
qwurzel(3)
qwurzel(-3)   		# Fehlermeldung

# Jetzt wollen wir zusätzlich den Fall abfangen, dass das Argument x ein Missing ist
qwurzel <- function(x)
{
  if (!is.na(x))
  {
    if(x >= 0)
    {
      return(sqrt(x)) 
    }
    else
    {
      cat("Fehler: Der Wert", x, "ist kleiner als Null. Daher kann keine Qudratwurzel bestimmt werden.")
    }
  }
}

# Übrigens: Die geschweiften Klammern um die Anweisungsblöcke von if und else können Sie auch weglassen, wenn sie 
# nur eine Anweisung enthalten; das sieht dann mit unserer Funktion qwurzel so aus:
qwurzel <- function(x)
{
  if (!is.na(x))
  {
    if(x >= 0) return(sqrt(x)) 
    else cat("Fehler: Der Wert", x, "ist kleiner als Null. Daher kann keine Qudratwurzel bestimmt werden.")
    
  }
}

# Was passiert, wenn wir unsere Funkion qwurzel() mit einem Vektor aufrufen, der nicht nur ein Element 
# hat, sondern zwei? Antwort: Sie läuft ganz normal durch, obwohl das zweite Element des Vektors 
# negativ ist; R prüft nur das erste!
qwurzel(c(4,-4))

# Deshalb modifizieren wir unsere Funktion noch einmal, und zwar so, dass sie alle Fälle abfängt, 
# in denen irgendein Element des Vektors x ein Missing ist
qwurzel <- function(x)
{
  if (sum(is.na(x)) == 0)
  {
    res <- sqrt(x)
    res[is.nan(res)] <- NA
    
    cat("Ergebnis: ", res, "\n")
    if(sum(is.na(res)) != 0)
    {
      cat("Warnung: Der Vektor x enthält", sum(is.na(res)), 
          "Element(e) kleiner Null.")
    }
    invisible(res)
  }
}



######## ---- Abgezählte Schleifen (for) ---- ########

# Diese Funktion summiert die (ersten) vier Elemente des ihr als Arguments übergebenen Vektors auf und
# gibt nach jedem Element den "aktuellen Stand" der Summe aus
summe.kumulativ <- function(v)
{
  s <- v[1]
  cat("Summe nach 1. Element: ", s, "\n", sep = "")
  s <- s + v[2]
  cat("Summe nach 2. Element: ", s, "\n", sep = "")
  s <- s + v[3]
  cat("Summe nach 3. Element: ", s, "\n", sep = "")
  s <- s + v[4]
  cat("Summe nach 4. Element: ", s, "\n", sep = "")
  invisible(s)
}

# Einfacher und flexibler geht es mit einer for-Schleife
summe.kumulativ <- function(v)
{
  s <- 0
  for(i in 1:length(v))
  {
    s <- s + v[i]
    cat("Summe nach ",i ,". Element: ", s, "\n", sep = "")
  }
  invisible(s)
}



######## ---- Bedingte Schleifen (while) ---- ########

# Beispiel einer Funktion, die einen Vektor nach Missings durchsucht un die erste Position eines 
# Missings meldet
finde.NA <- function(v)
{
  i <- 1
  while(is.na(v[i]) == FALSE & i <= length(v))
  {
    i <- i + 1
  }
  if(i == length(v) + 1)
  {
    cat("Es wurde kein Missing gefunden.")
  }
  else
  {
    cat("Das erste Missing steht an Position ", i, ".", sep = "")
  }
}




################ ---- Ein ausführliches Beispiel ---- ################

signifikant<-function(m,level=0.05, hc="auto")
{
  # Prüfen, ob die benötigen Packages geladen sind, und, falls nein, nachladen  
  require("lmtest", quietly=TRUE)
  require("sandwich", quietly=TRUE)
  
  use.hc<-FALSE # Statusvariable, die angibt, ob heteroskedastizitätskonsistente Standardfehler verwendet wurden
  
  # Fehlerstatus initialisieren
  err_1<-FALSE
  err_2<-FALSE
  err_3<-FALSE
  
  # Fehlerbehandlung: Je nach Fehler wird ein Fehlerstatus gesetzt
  if(class(m)!="lm") err_1<-TRUE
  if(!is.numeric(level)) err_2<-TRUE
  if(!(hc=="auto" | hc=="yes" | hc=="no")) err_3<-TRUE
  
  # Der Kern der Funktion: Wenn kein Fehler aufgetreten ist....
  if(!(err_1 | err_2 | err_3))
  {
    if(hc=="auto")
    {
      # Breusch-Pagan-Test auf Heteroskedastizität auswerten
      if(bptest(m)$p.value<level) 
      {
        # Varianz-Kovarianzmatrix auf Basis heteroskedastizitätskonsistenter Standardfehler
        vcov.matrix<-vcovHC(m)
        use.hc<-TRUE # Statusvariable setzen
      }
      else 
      { 
        # Nicht-korrigierte ("normale") Standardfehler berechnen
        vcov.matrix<-vcov(m)
      }
    }
    else
    {
      if(hc=="yes") 
      {
        # Varianz-Kovarianzmatrix auf Basis heteroskedastizitätskonsistenter Standardfehler
        vcov.matrix<-vcovHC(m)
        use.hc<-TRUE  # Statusvariable setzen
      }
      else vcov.matrix<-vcov(m)
    }
    
    # Koeffiziententest durchführen
    ctest<-coeftest(m, .vcov=vcov_matrix)
    
    # Ein paar Basisinformationen anzeigen
    cat("\nSignifikanzlevel:", level,"\n")
    if(use.hc==TRUE) cat("Test nutzt heteroskedastizitätskonsistenter Standardfehler (HC3).\n")
    cat("Nullhypothese: Koeffizient=0\n\n")
    cat("Koeffizient\tSignifikant\n")
    
    # Vektor als Rückgabewert erzeugen
    val<-c()
    # Alle Koeffizienten durchlaufen...
    for(i in 1:NROW(m$coefficients))
    {
      if(ctest[i,4]<level) res<-"ja"
      else res<-"nein"
      # Ausgabe der Ergebnissse
      cat(names(m$coefficients)[i],"\t",res,"\n")
      
      # Rückgabewerte der Funktion vorbereiten (named vector)
      val[i]<-res=="ja"
      names(val)[i]<-names(m$coefficients)[i]
    }
    cat("\n")
    # Funktionswert zurückgeben
    invisible(val)
  }
  # Wenn (mindestens) ein Fehler aufgetreten ist, Fehlermeldung(en) anzeigen
  else
  {
    if(err_1) cat("Das Argument m muss ein lm-Objekt sein.\n")
    if(err_2) cat("Das Argument level muss numerisch sein (z.B. 0.05 für 5 %).\n")
    if(err_3) cat("'", hc, "' ist kein zulässiger Wert für das Argument hc. Zulässig sind 'auto', 'yes', 'no'.", sep="")
  }  
}


# Und jetzt: Ausprobieren! Dazu schätzen wir uns zunächst ein Regressionsmodell
m <- lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)

# Dann testen wir die Koeffizienten des Modells zum Signifikanzniveau von 10 Prozent. Dabei lassen wir die Funktion selbst
# entscheiden, ob die Verwendung heteroskedastizitätskonsistenter Standardfehler angebracht ist, oder nicht
erg <- signifikant(m, 0.01, "auto")

# Dann schauen wir einmal, was uns unsere Funktion zurückgegeben hat
erg


# Zum Abschluss "verschlanken" wir unsere Funktion signifikant ein wenig, indem wir einzelne Teile in eigene 
# Funktionen auslagern; wir schaffen zwei "Hilfsfunktionen", berechnung.ergebnis und anzeige.ergebnis, die der unserer 
# "Hauptfunktion" signifikant einen Teil Arbeit abnehmen und diese so übersichtlicher und leichter lesbar machen.

# Hilfsfunktion 1: Diese Funktion führt die eigentlichen Berechnungen aus
berechnung.ergebnis<-function(hc, m, level)
{
  use.hc<-FALSE # Statusvariable, die angibt, ob heteroskedastizitätskonsistente Standardfehler verwendet wurden
  
  if(hc=="auto")
  {
    # Breusch-Pagan-Test auf Heteroskedastizität auswerten
    if(bptest(m)$p.value<level) 
    {
      # Varianz-Kovarianzmatrix auf Basis heteroskedastizitätskonsistenter Standardfehler
      vcov.matrix<-vcovHC(m)
      use.hc<-TRUE # Statusvariable setzen
    }
    else 
    { 
      # Nicht-korrigierte ("normale") Standardfehler berechnen
      vcov.matrix<-vcov(m)
    }
  }
  else
  {
    if(hc=="yes") 
    {
      # Varianz-Kovarianzmatrix auf Basis heteroskedastizitätskonsistenter Standardfehler
      vcov.matrix<-vcovHC(m)
      use.hc<-TRUE  # Statusvariable setzen
    }
    else vcov.matrix<-vcov(m)
  }
  
  # Koeffiziententest durchführen
  ctest<-coeftest(m, .vcov=vcov_matrix)
  
  # Rückgabeobjekt erzeugen: Eine Liste mit dem Ergebnis der Funktion coeftest als erstem Element 
  # und der Statusvariable use.hc als zweitem Element
  erg<-list(ctest,use.hc)
  invisible(erg)
}


# Hilfsfunktion 2: Diese Funktion zeigt das Ergebnis auf dem Bildschirm an und erzeugt das Rückgabeobjekt, dass dann
# von unserer Funktion signifikant zurückgegeben wird
ausgabe.ergebnis<-function(m, ctest, level)
{      
  # Vektor als Rückgabewert erzeugen
  val<-c()
  
  # "Spalten-Überschriften" erzeugen (getrennt durch Tabulator-Sprung)
  cat("Koeffizient\tSignifikant\n")
  
  # Alle Koeffizienten durchlaufen...
  for(i in 1:NROW(m$coefficients))
  {
    if(ctest[i,4]<level) res<-"ja"
    else res<-"nein"
    # Ausgabe der Ergebnissse
    cat(names(m$coefficients)[i],"\t",res,"\n")
    
    # Rückgabewerte der Funktion vorbereiten (named vector)
    val[i]<-res=="ja"
    names(val)[i]<-names(m$coefficients)[i]
  }
  cat("\n")
  
  invisible(val)
}


# Unsere Hauptfunktion, von der aus die beiden Hilfsfunktionen berechnung.ergebnis und ausgabe.ergebnis
# aufgerufen werden
signifikant<-function(m,level=0.05, hc="auto")
{
  # Prüfen, ob die benötigen Packages geladen sind, und, falls nein, nachladen  
  require("lmtest", quietly=TRUE)
  require("sandwich", quietly=TRUE)

  # Fehlerstatus initialisieren
  err_1<-FALSE
  err_2<-FALSE
  err_3<-FALSE
  
  # Fehlerbehandlung: Je nach Fehler wird ein Fehlerstatus gesetzt
  if(class(m)!="lm") err_1<-TRUE
  if(!is.numeric(level)) err_2<-TRUE
  if(!(hc=="auto" | hc=="yes" | hc=="no")) err_3<-TRUE
  
  # Der Kern der Funktion: Wenn kein Fehler aufgetreten ist....
  if(!(err_1 | err_2 | err_3))
  {
    
    # Die statistische Analyse durchführen; das Ergebnis ist eine Liste, die in der Variable ergebnis gespeichert wird
    ergebnis<-berechnung.ergebnis(hc, m, level)
    
    # Ein paar Basisinformationen anzeigen
    cat("\nSignifikanzlevel:", level,"\n")
    # Achtung: Das Rückgabeobjekt von berechnung.ergebnis ist eine Liste, auf deren zweites Element (use.hc) wir hier zugreifen
    if(ergebnis[[2]]==TRUE) cat("Test nutzt heteroskedastizitätskonsistenter Standardfehler (HC3).\n")
    cat("Nullhypothese: Koeffizient=0\n\n")
    
    # Rückgabeobjekt erzeugen und Ergebnisse anzeigen. Dabei ist (ergebnis[[1]] das erste Element des (Listen-)Rückgabewerts
    # der Hilfsfunktion berechnung.ergebnis, also ctest
    val<-ausgabe.ergebnis(m, ergebnis[[1]], level)
    
    # Funktionswert zurückgeben
    invisible(val)
  }
  # Wenn (mindestens) ein Fehler aufgetreten ist, Fehlermeldung(en) anzeigen
  else
  {
    if(err_1) cat("Das Argument m muss ein lm-Objekt sein.\n")
    if(err_2) cat("Das Argument level muss numerisch sein (z.B. 0.05 für 5 %).\n")
    if(err_3) cat("'", hc, "' ist kein zulässiger Wert für das Argument hc. Zulässig sind 'auto', 'yes', 'no'.", sep="")
  }  
}


# Dieser strukturelle umbau führt aber am Ende zum gleichen Ergebnis:
m <- lm(EXPEND ~ UNEMPLOY + POPSENIORS + POPCHILDREN + DEMOCRATIC, data = x)
erg <- signifikant(m, 0.01, "auto")
erg # Anzeige