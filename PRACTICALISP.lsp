;-------------------------------------------------------------------------------
; Structs
;-------------------------------------------------------------------------------

(defstruct CharacterData
    hints   ; string list
    image   ; byte list
)

(defstruct PlayerScore
    score               ; integer
    charactersGuessed   ; integer
)

(defstruct GameData
    playerName      ; string
    playerScore     ; integer
    characterNames  ; string list
    characterData   ; CharacterData
    currentScore    ; integer
    usedTries       ; integer
)

;-------------------------------------------------------------------------------
; Data management
;-------------------------------------------------------------------------------

;-----------
; Constants
;-----------

(setq data--IMG_DIR          "data/images/")
(setq data--HINT_DIR         "data/hints/")
(setq data--NAMES_FILE_PATH  "data/PERSONAJES.txt")
(setq data--SCORES_FILE_PATH "data/PUNTUACIONES.txt")

;--------------------
; Internal functions
;--------------------

(defun data--read-img-file (filePath)
    (setq file  (open filePath 
                    :if-does-not-exist nil
                    :element-type      '(unsigned-byte 8)
                )
    )
    (unless file (return-from data--read-img-file nil))
    (setq img nil)
    (dotimes (i 40000)
        (cond
            ((= (read-byte file) 0)
                (setq img (cons 0 img))
            )
            (t
                (setq img (cons 1 img))
            )
        )
    )
    (close file)
    (return-from data--read-img-file (reverse img))
)

(defun data--read-text-file (filePath)
    (setq file  (open filePath
                    :if-does-not-exist nil
                )
    )
    (unless file (return-from data--read-text-file nil))
    (setq line (read-line file nil nil))
    (setq fileContent nil)
    (loop 
        (when (null line) (return))
        (setq fileContent (cons line fileContent))
        (setq line (read-line file nil nil))
    )
    (close file)
    (return-from data--read-text-file (reverse fileContent))
)

;------------------
; Public functions
;------------------

(defun data-load-names ()
    (return-from data-load-names (data--read-text-file data--NAMES_FILE_PATH))
)

(defun data-load-character (characterName)
    (setq imageFilePath (concatenate 'string data--IMG_DIR characterName ".img"))
    (setq img (data--read-img-file imageFilePath))
    (unless img (return-from data-load-character nil))

    (setq hintsFilePath (concatenate 'string data--HINT_DIR characterName ".txt"))
    (setq hints (data--read-text-file hintsFilePath))
    (unless hints (return-from data-load-character nil))

    (return-from data-load-character (make-CharacterData
                                         :hints hints
                                         :image img
                                     )
    )
)

(defun data-save-score (playerName playerScore)
    (setq file  (open data--SCORES_FILE_PATH 
                    :direction         :output
                    :if-exists         :append
                    :if-does-not-exist :create
                )
    )
    (unless file (return-from data-save-score nil))
    (format file "~5Dpts - ~2D personajes (~A)~%" 
        (PlayerScore-score             playerScore)
        (PlayerScore-charactersGuessed playerScore)
        playerName
    )
    (close file)
    (return-from data-save-score t)
)

;-------------------------------------------------------------------------------
; Graphics management
;-------------------------------------------------------------------------------
(defun load-ui-files ()
    (load "portada.lsp")
    (load "interfaz.lsp")
    (load "noAcertadoInterfaz.lsp")
    (load "acertadoInterfaz.lsp")
    (load "ganadoInterfaz.lsp")
    (load "perdidoInterfaz.lsp")
)

(defun fillRect(x y w h) 
	(setq starty (- 374 y)); y desde arriba
	(setq endy (- starty h)); de arriba a abajo
	(setq endx (+ x w)); endx=x+w
	(do	(	
			(yloop starty (- yloop 1))
		)
		(
			(= yloop endy)

		)
			(move x yloop)
			(draw endX yloop)
		
	)
)
;Dado un string imprime en el medio relativo a la culumna en la fila dada
;la variable lastCol es opcional, para establecer la ultima columna

(defun printRelMid (str col fila &optional (lastCol 81))
	(let*
		(
			(maxColForPrint (- lastCol col))
			(colLeft (- maxColForPrint (length str )))
			(medioRelativo (/ colLeft 2) )
		)
		(goto-xy (ceiling (+ col medioRelativo)) fila)		
		(format t str )		
		(values)
	)
  
)

; Funciones para imprimir los datos en la interfaz
;-------------------------------------------------
(defun print-intento-sobrante (var)
	(color 80 80 80 )
	(printRelMid (format nil "~D" var) 46 6 56) 
    (color 0 0 0 ) 	
	(values)
)

(defun print-intento-utilizado (var)
	(color 80 80 80 )
	(printRelMid (format nil "~D" var) 63 6 72)
    (color 0 0 0 ) 	
	(values)
)

(defun print-puntuacion-personaje (var)
	(color 80 80 80 )	
	(printRelMid  (format nil "~D" var) 38 11 72)
    (color 0 0 0 ) 	
	(values)
)

(defun print-punto-acumulado (var)
  	(color 80 80 80 )	
	(printRelMid  (format nil "~D" var) 46 16 54)
    (color 0 0 0 ) 	
	(values)
)

(defun print-personaje-acumulado (var)
	(color 80 80 80 )
	(printRelMid (format nil "~D" var) 66 16 71)
    (color 0 0 0 ) 	
	(values)
)

; igual que antes pero para imprimi al final cuando gana el juego
(defun print-punto-acumulado-final (var)
  	(color 80 80 80 )	
	(printRelMid  (format nil "~D" var) 30 18 40)
    (color 0 0 0 ) 	
	(values)
)

(defun print-personaje-acumulado-final (var)
	(color 80 80 80 )
	(printRelMid (format nil "~D" var) 48 18 56)
    (color 0 0 0 ) 	
	(values)
)


(defun print-pista (str)
	(goto-xy 16 19)
	(format t  str )
	(values)
)

(defun print-info (str)
	(goto-xy 10 21)
	(format t  str )
	(values)
)

(defun print-nombre-personaje (str)
    (color 80 80 80)
    (printRelMid str 8 18 31)
    (color 0 0 0 ) 	
	(values)
)


(defun get-line-xy () 
  (goto-xy 10 23)
  (read-char)
  (read-line )
)

;----------------------------------------------------------------
; each is used to print the image partially:
;   - If each = 1 then the image is completely drawn
;   - If each = x then the image is drawn every x lines
;
; WARNING: each must never be set to 0
(defun print-img (img each initialX initialY)
    (dotimes (y 200)
        (move initialX (- initialY y))
        (dotimes (x 200)
            (when (= 0 (mod y each))
                (cond 
                    ((= (car img) 0)
                        (draw (+ initialX x 1) (- initialY y))
                    )
                    (t
                        (move (+ initialX x 1) (- initialY y))
                    )
                )
            )
            (setq img (cdr img))
        )
    )
    (return-from print-img t)
)


;dibuja el titulo
(defun render-game-title ()
    (titulo)
)

;dibuja puntaciones y personajes acumuladas
(defun print-player-score (playerScore)
    (print-punto-acumulado (PlayerScore-score playerScore))
    (print-personaje-acumulado (PlayerScore-charactersGuessed playerScore))
    (return-from print-player-score t)
)

;dibuja puntaciones y personajes acumuladas al final del juego 
(defun print-score-when-end (playerScore)
    (print-punto-acumulado-final (PlayerScore-score playerScore))
    (print-personaje-acumulado-final (PlayerScore-charactersGuessed playerScore))
    (return-from print-score-when-end t)
)


(defun render-error-ui (errorMessage)
    (cls)
    (fondo)
    (render-game-title)    
    (printRelMid   (format nil "ERROR: ~A" errorMessage) 0 10)
    (printRelMid   "Para solucionar: habla con el programador!" 0 12)
    (return-from render-error-ui t)
)

(defun render-player-name-ui ()
    (cls)
    (portada)
    (nombre)
)

;Dibuja la interfaz principal del juego
(defun render-game-ui (usedTries totalTries currentScore playerScore hint image)
    (cls)
    (interfaz)
    (render-game-title)
    (print-intento-utilizado usedTries)
    (print-intento-sobrante (- totalTries usedTries))
    (print-puntuacion-personaje currentScore)
    (print-player-score playerScore)
    (print-pista hint)
     (case usedTries
        (0  (print-info "Primer intento ^.^"))
        (1  (print-info "Segundo intento ^.^"))
        (2  (print-info "Tercero....."))
        (3  (print-info "Cuarto....."))
        (4  (print-info "La mitad -.-"))
        (5  (print-info "Sexto....."))
        (6  (print-info "Septimo...."))
        (7  (print-info "Chico/a, Espabila"))
        (8  (print-info "Penultimo intento, mira bien la imagen!"))
        (9  (print-info "Ultimo intento, Quires ver que pasa si no aciertas?"))
      )
    (print-img image (- totalTries usedTries) 52 (- 374 64))
    (return-from render-game-ui t)
)

;Dibuja cuando el jugador no ha acertado el personaje
(defun render-not-guessed-character-ui (characterName characterImage playerScore)
    (cls)
    (no-acertado-interfaz)
    (render-game-title)
    (print-info (format nil "El personaje es: ~A" characterName))
    (print-nombre-personaje characterName)
    (print-img characterImage 1 52 (- 374 64))
    (print-punto-acumulado (PlayerScore-score playerScore))
    (print-personaje-acumulado (PlayerScore-charactersGuessed playerScore))
    (return-from render-not-guessed-character-ui t)
)
;Dibuja cuando el jugador ha acertado el personaje
(defun render-guessed-character-ui (characterName characterImage playerScore)
    (cls)
    (acertado-interfaz)
    (render-game-title)
    (print-info (format nil "Correcto! El personaje es: ~A" characterName))
    (print-nombre-personaje characterName)
    (print-img characterImage 1 52 (- 374 64))
    (print-punto-acumulado (PlayerScore-score playerScore))
    (print-personaje-acumulado (PlayerScore-charactersGuessed playerScore))
    (return-from render-guessed-character-ui t)
)

;Dibuja cundo el jugador ha termido todos los personajes, "JUEGO GANADO"
(defun render-win-ui (playerScore)
    (cls)
    (fondo)
    (render-game-title) 
    (ganado-interfaz)
    (print-score-when-end playerScore)
    (return-from render-win-ui t)
)

;Dibuja cundo el jugador ha termido todos los personajes, "JUEGO PERDIDO"
(defun render-lose-ui (playerScore)
    (cls)
    (fondo)
    (render-game-title) 
    (perdido-interfaz)
    (print-score-when-end playerScore)
    (return-from render-lose-ui t)
)

;Dibuja una vez termina el juego si el usuario desea reptir el juego
(defun render-gameover-ui ()
    (cls)
    (fondo)
    (render-game-title)    
    (printRelMid   "--FINAL DEL JUEGO--" 0 10)
    (printRelMid   "Presiona cualquier tecla para jugar de nuevo o CTRL+C para salir" 0 12)
    (return-from render-gameover-ui t)
)

;-------------------------------------------------------------------------------
; Game handling
;-------------------------------------------------------------------------------

;-----------
; Constants
;-----------

(setq MAX_TRIES           10)
(setq MAX_CHARACTER_SCORE 100)

;-----------
; Functions
;-----------

(defun get-input (line column)
    (goto-xy line column)
    (return-from get-input (read-line))
)

(defun get-player-name ()
    (cls)
    (render-player-name-ui)
    (format t ">")
    (return-from get-player-name (get-input 28 16))
)

(defun init-game-data ()
    (setq characterNames (data-load-names))
    (unless characterNames
        (render-error-ui "No se ha podido leer el fichero de nombres")
        (return-from init-game-data nil)
    )
    (return-from init-game-data (make-GameData
                                    :playerName     (get-player-name)
                                    :playerScore    (make-PlayerScore 
                                                        :score             0 
                                                        :charactersGuessed 0
                                                    )
                                    :characterNames characterNames
                                )
    )
)

(defun load-character (gameData)
    (setq characterName (car (GameData-characterNames gameData)))
    (setq characterData (data-load-character characterName))
    (unless characterData
        (render-error-ui 
            (concatenate 'string
                "No se han podido cargar los datos del personaje "
                "'" characterName "'"
            )
        )
        (return-from load-character nil)
    )
    (setf (GameData-characterData gameData) characterData)
    (setf (GameData-currentScore gameData) MAX_CHARACTER_SCORE)
    (setf (GameData-usedTries gameData) 0)
    (return-from load-character gameData)
)

(defun check-answer (gameData input)
    (return-from check-answer 
        (string-equal (car (GameData-characterNames gameData)) input)
    )
)

(defun guess-character (gameData)
    (loop
        (setq usedTries (GameData-usedTries gameData))
        (render-game-ui
            usedTries
            MAX_TRIES
            (GameData-currentScore gameData)
            (GameData-playerScore gameData)
            (nth usedTries 
                (CharacterData-hints (GameData-characterData gameData))
            )
            (CharacterData-image (GameData-characterData gameData))
        )
        (setq input (get-input 10 23))
        (when (check-answer gameData input) 
            (return)
        )
        (setf (GameData-usedTries gameData) (1+ usedTries))
        (setf (GameData-currentScore gameData) 
            (- (GameData-currentScore gameData) 10)
        )
        (when (= (GameData-usedTries gameData) MAX_TRIES)
            (return)
        )
    )
    (return-from guess-character gameData)
)

(defun update-player-score (gameData)
    (setq currentScore (GameData-currentScore gameData))
    (setq playerScore (GameData-playerScore gameData))
    (when (> currentScore 0)
        (setf (PlayerScore-score playerScore) 
            (+ (PlayerScore-score playerScore) currentScore)
        )
        (setf (PlayerScore-charactersGuessed playerScore)
            (1+ (PlayerScore-charactersGuessed playerScore))
        )
        (setf (GameData-playerScore gameData) playerScore)
    )
    (return-from update-player-score gameData)
)

;-------------------------------------------------------------------------------
; Program start
;-------------------------------------------------------------------------------

(defun inicio ()
    (read-line) ; Skip the program invocation output so it is not read as input
    (load-ui-files)
    (loop
        (setq gameData (init-game-data))
        (unless gameData (return-from inicio nil))
        (setq gameData (load-character gameData))
        (unless gameData (return-from inicio nil))
        (loop
            (setq gameData (guess-character gameData))
            (setq gameData (update-player-score gameData))
            (cond
                ; Character not guessed
                ((= 0 (GameData-currentScore gameData))
                    (render-not-guessed-character-ui
                        (car (GameData-characterNames gameData))
                        (CharacterData-image (GameData-characterData gameData))
                        (GameData-playerScore gameData)
                    )
                    (goto-xy 10 23)
                    (read-line)    
                )
                ; Character guessed
                (t
                    (render-guessed-character-ui
                        (car (GameData-characterNames gameData))
                        (CharacterData-image (GameData-characterData gameData))
                        (GameData-playerScore gameData)
                    )
                    (goto-xy 10 23)
                    (read-line)
                )
            )
            ; remove character
            (setf (GameData-characterNames gameData) 
                (cdr (GameData-characterNames gameData))
            )
            (when (null (GameData-characterNames gameData)) ; all characters guessed
                (if (= 0 (PlayerScore-score (GameData-playerScore gameData)))
                    (render-lose-ui (GameData-playerScore gameData))
                    (render-win-ui (GameData-playerScore gameData))
                )
                (read-line)
                (return)
            )
            (setq gameData (load-character gameData))
            (unless gameData (return-from inicio nil))
        )
        (data-save-score
            (GameData-playerName gameData)
            (GameData-playerScore gameData)
        )
        (render-gameover-ui)
        (read-line)
    )
)