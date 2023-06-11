#lang racket

;archivos
;(define inputCPP "file.cpp")
;(define inputPascal "file.pas")
(define inputFilename "input.py")
(define outputFilename "output.html")
;strings
(define tab "   ")
;expresiones regex
(define reserved #px"\\b(?:and|as|assert|break|class|continue|def|del|elif|else|except|finally|for|from|global|if|import|in|is|lambda|not|or|pass|raise|return|try|while|with|yield)\\b")
;(define literals_string #px"(\"[^\"]*\"|'[^']*')")
(define literals_string #px"[\"'][^\"']*(?:[0-9]+[^\"']*)*[\"']")
;(define literals_string #rx"[\"'][^\"]*[\"']")
;(define comments #rx"^#.*$")
(define comments #px"#[^\n]*")
(define operators #px"\\+=|\\+|-|\\*|/|%|=*|!=|<*|>*|<=|>=|\\^")
(define builtInFunctions #px"\\b(len|range|print|input|str|int|float|bool|list|tuple|dict|set)\\b")
(define separators #px"[()\\[\\]{}:,;.\\s]+")
(define variables #px"[[:alpha:]][[:word:]]*")
(define literals_numerics #px"(0b|0x||[[:digit:]]+e?)[[:digit:]]+.?[[:digit:]]*L{0,2}")
(define break #px"\\||\\(|\\)|[^([:alnum:]|.)]")
(define breakMultiple #px"(\\||\\(|\\)|[^([:alnum:]|#|.|\'|\"|_|?)])+")

(define (multipleString string times)
  (if (= times 0)
      ""
      (string-append string (multipleString string (- times 1)))
      )
  )

;generate HTML file
(define (generateHtmlBeginning output)
  (define fOut(open-output-file output #:exists 'replace))
  (fprintf fOut
           ;
           "<!DOCTYPE html>
           <html>
           
           <head>
           <h1>prueba 424</h1>
           <link rel=\"stylesheet\" href=\"styles.css\">
           </head>
           
           <body>
")
  (close-output-port fOut)
  )

(define (generateHtmlEnd output)
  (define fOut(open-output-file output #:exists 'append))
  (fprintf fOut
           "</body>

           </html>")
  (close-output-port fOut)
)

(define (hasComment line)
  (not (equal? (regexp-match comments line) #f))
  )

(define (formatSpacesHTML token)
  (string-replace (string-replace token " " "&nbsp;") "\r" "")
  )

(define (classifyType token)
  (cond
    [(regexp-match-exact? literals_string token) "sring-literal"]
    [(regexp-match-exact? operators token) "operator"]
    [(regexp-match-exact? reserved token) "reserved"]
    [(regexp-match-exact? builtInFunctions token) "builtin-function"]
    [(regexp-match-exact? comments token) "comment"]
    [(regexp-match-exact? separators token) "separator"]
    [(regexp-match-exact? variables token) "variable"]
    [(regexp-match-exact? literals_numerics token) "numeric-literal"]
    [else "normal"]
    )
  )

(define (classifyDescription token)
  (cond
    [(regexp-match-exact? reserved token)
     (string-append "Palabra reservada: " token)]
    [(regexp-match-exact? builtInFunctions token)
     (string-append "FunEst: " token)]
    [(regexp-match-exact? separators token)
     (string-append "Separador: " token)]
    [(regexp-match-exact? comments token)
     (string-append "Comment: " token)]
    [(regexp-match-exact? variables token)
     (string-append "Variable: " token)]
    [(regexp-match-exact? operators token)
     (string-append "Operator: " token)]
    [(regexp-match-exact? literals_numerics token)
     (string-append "Num: " token) ]
    [(regexp-match-exact? literals_string token) "String: "]
     [else "Nada:"]))

(define (formatTokensHTML token)
  (string-append (multipleString tab 4)
                 "<span title=\"" (classifyDescription token) "\" class=\"" (classifyType token) "\">" (formatSpacesHTML token) "</span>\n"
     )
 )

(define (fromListOfTokens tokens sepTokens)
  (cond
    [(and (empty? tokens) (empty? sepTokens)) ""]
    [(and (empty? tokens)(not (empty? sepTokens)))
     (string-append
      (formatTokensHTML (car sepTokens))
      (fromListOfTokens tokens (cdr sepTokens))
      )
     ]
    [(and (not (empty? tokens)) (empty? sepTokens))
     (string-append
      (formatTokensHTML (car tokens))
      (fromListOfTokens (cdr tokens) sepTokens)
      )
     ]
    [else
     (string-append
      (formatTokensHTML (car tokens))
      (formatTokensHTML (car sepTokens))
      (fromListOfTokens (cdr tokens) (cdr sepTokens))
      )]
    )
  )

(define (solveOnceNoComment line)
  (define sepTokens (regexp-match* breakMultiple line))
  (define tokens (string-split line breakMultiple))
  (cond
    [(and (empty? sepTokens) (empty? tokens))""]
    [(and (not (empty? tokens)) (not(empty? sepTokens)))
     (if (> (length sepTokens) (length tokens))
         (string-append
          (formatTokensHTML (car sepTokens))
          (fromListOfTokens tokens (cdr sepTokens))
          )
         (fromListOfTokens tokens sepTokens)
         )
     ]
    [(not (empty? tokens))
     (string-append
      (formatTokensHTML (car tokens))
      (fromListOfTokens (cdr tokens) sepTokens)
      )
     ]
    [else
     (string-append
      (formatTokensHTML (car sepTokens))
      (fromListOfTokens tokens (cdr sepTokens))
      )
     ]
    )
  )

(define (solve line)
  (if (hasComment line)
      (string-append
       (solveOnceNoComment (string-trim line comments))
       (formatTokensHTML (car (regexp-match comments line)))
       )
      (solveOnceNoComment line)
      )
  )

(define (processLines fIn)
  (define line (read-line fIn))
  (if (eof-object? line)
      ""
      (string-append
       (multipleString tab 3) "<p class=\"spaces\">\n"
       (solve line)
       (multipleString tab 3) "</p>\n"
       (processLines fIn)
       )
     )
  )

(define (processCode input output)
  (define fIn (open-input-file input))
  (define fOut (open-output-file output #:exists 'append))
  (fprintf fOut (string-append
                 tab "<h1 class=\"title\">" input "<h1>\n"
                 ;(multipleString tab 2) "</div>\n"
              )
      )
  (fprintf fOut(string-append
                (multipleString tab 2) "<div class = \"code\">\n"
                )
           )
  (fprintf fOut(processLines fIn))
  (fprintf fOut (string-append
                 (multipleString tab 2) "</div>\n"
                 )
           )
  
  (close-output-port fOut)
  (close-input-port fIn)
  )

(generateHtmlBeginning outputFilename)
;(processCode inputCPP outputFilename)
;(processCode inputPascal outputFilename)
(processCode inputFilename outputFilename)
(generateHtmlEnd outputFilename)
