(require 'url)
(require 'json)

(setq my-stocks '("petr4" "itub4" "lame4"))
      
(defun my-wallet-alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (helm-spotify-plus-alist-get (cdr symbols)
		 (assoc (car symbols) alist))
    (cdr alist)))

(defun my-wallet-request (a-url)
  "Function to request an json given a correct A-URL."
  (with-current-buffer
      (url-retrieve-synchronously a-url)
    (goto-char url-http-end-of-headers)
    (json-read)))

(defun my-wallet-info-string (stock)
  (let ((prefix "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20%28%22")
	(suffix ".SA%22%29&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback=")
	dailychange)  
    (setq dailychange (my-wallet-alist-get '(query results quote PercentChange)
					   (my-wallet-request (concat prefix (concat stock suffix)))
					   ))
    (format "%s : %s" stock dailychange)
    )
  )

;;(my-wallet-info-string "petr4")

(defun my-wallet-info-buffer (stock-list)
  (let ((inhibit-read-only t))
    (switch-to-buffer-other-window "*my-wallet*")
    (erase-buffer)
    (mapcar (lambda (stock)
	      (insert (my-wallet-info-string stock))
	      (insert "\n")
	      ) stock-list)
    (special-mode)
    ))

;;(my-wallet-info-buffer my-stocks)

