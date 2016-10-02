## cachematrix.R umfasst zwei Funktionen die dazu dienen inverse Matrizen zu berechnen. Da Dieser Prozess 
## bei grossen Datenmengen sehr rechenintensiv ist, cached die Funktion die Resultate von bereits invertierten
## Funktionen. Wenn also eine Matrize invertiert werden soll die bereits einmal berechnet wurde, wird das
## Resultat nicht noch einmal berechnet, sondern direkt aus dem Cache geladen.

## Beschreibung Funktion makeCacheMatrix:
## x ist eine reguläre invertierbare Matrix
## Resultat ist eine Liste mit 4 Funktionen die als Input für die Funktion cacheSolve verwendet wird:
## Funktion 1: Festlegen der Matrix
## Funktion 2: Abholen der Matrix
## Funktion 3: Festlegen der inversen Matrix
## Funktion 4: Abholen der inversen Matrix

# Funktion makeCacheMatrix anlegen
makeCacheMatrix <- function(x = matrix()) {
        # inverse Matrix auf NULL setzen
        invM <- NULL
        
        # Funktion 1: Festlegen der Matrix
        setM <- function(y){
                x <<- y
                invM <<- NULL
        }
        
        # Funktion 2: Abholen der Matrix
        getM <- function() x
        
        # Funktion 3: Festlegen der inversen Matrix
        setIM <- function(invers)  invM <<- invers
        
        # Funktion 4: Abholen der inversen Matrix
        getIM <- function() invM
        
        # Liste mit den 4 Funktionen anlegen
        list(setM=setM, getM=getM, setIM=setIM, getIM=getIM)
}


## x ist der Output der Funktion makeCacheMatrix (eine Funktion)
## Das Resultat der Funktion ist die inverse Matrix der Matrix welche der Funktion makeCacheMatrix übergeben 
## wurde.

# Funktion cacheSolve anlegen
cacheSolve <- function(x, ...) {
        
        # Überprüft, ob die Matrix schon einmal invertiert wurde. Falls ja werden die Daten aus dem 
        # Cache verwendet.
        invM <- x$getIM()
        if (!is.null(invM)) {
                message("verwende Daten aus dem Cache")
                return(invM)
        }
        
        # Falls die Matrix noch nicht invertiert wurde wird hier die Invertierung berechnet und anschliessend
        # gecached.
        DatenMatrix <- x$getM()
        invM <- solve(DatenMatrix, ...)
        x$setIM(invM)
        invM
}
