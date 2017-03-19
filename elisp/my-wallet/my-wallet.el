(require 'url)
(require 'json)

(setq my-wallet-stocks '("cgas5" "qual3" "flry3" "parc3" "smle3" "csan3" "petr4" "itub4" "lame4" "vale3"))
      
(defun my-wallet-alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (my-wallet-alist-get (cdr symbols)
		 (assoc (car symbols) alist))
    (cdr alist)))

(defun my-wallet-stock-request (stock)
  "Function to request an json to Yahoo Finance given a stock name."
  (let ((prefix "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20%28%22")
	(suffix ".SA%22%29&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback=")
	a-url)
    (setq a-url (concat prefix (concat stock suffix)))
    (with-current-buffer
      (url-retrieve-synchronously a-url)
    (goto-char url-http-end-of-headers)
    (json-read))))

(defun my-wallet-request (a-url)
  "Function to request an json given a correct A-URL."
  (with-current-buffer
      (url-retrieve-synchronously a-url)
    (goto-char url-http-end-of-headers)
    (json-read)))


(defun my-wallet-return-string-right-color (value)
  (if (string-match-p "-" value)
      (propertize value 'font-lock-face '(:foreground "red"))
      (propertize value 'font-lock-face '(:foreground "green")))
  )

(defun my-wallet-info-string (stock)
  (let ((prefix "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20%28%22")
	(suffix ".SA%22%29&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback=")
	stock-info
	daily-change
	fifty-days-change
	two-hundred-days-change
	change-from-year-low)
    (setq stock-info (my-wallet-request (concat prefix (concat stock suffix))))
    (setq daily-change (my-wallet-alist-get '(query results quote PercentChange)
					   stock-info
					   ))
    (setq fifty-days-change (my-wallet-alist-get '(query results quote PercentChangeFromFiftydayMovingAverage)
					   stock-info
					   ))
    (setq two-hundred-days-change (my-wallet-alist-get '(query results quote PercentChangeFromTwoHundreddayMovingAverage)
					   stock-info
					   ))
    (setq change-from-year-low (my-wallet-alist-get '(query results quote PercentChangeFromYearLow)
					   stock-info
					   ))
    (setq change-from-year-high (my-wallet-alist-get '(query results quote PercebtChangeFromYearHigh)
					   stock-info
					   ))

    (format "%-12s %10s %10s %10s %10s %10s"
	    stock
	    (my-wallet-return-string-right-color daily-change)
	    (my-wallet-return-string-right-color fifty-days-change)
	    (my-wallet-return-string-right-color two-hundred-days-change)
	    (my-wallet-return-string-right-color change-from-year-low)
	    (my-wallet-return-string-right-color change-from-year-high)	    
	    )
    )
  )

;;(my-wallet-info-string "petr4")

(defun my-wallet-header ()
  "Inserts a header in buffer containing current date, market date and stock-header"
  (let (trade-info)
  (setq trade-info (my-wallet-stock-request (car my-wallet-stocks)))
  (insert (format "Current date: %s \n" (format-time-string "%m/%d/%Y")))
  (insert (format "%s \n\n"
		  (if (my-wallet-alist-get '(query results quote TradeDate)
					   trade-info
					   )
		      (concat (propertize "Open Market" 'font-lock-face '(:foreground "green" :weight 'bold))
			      (format " (Trade Date: %s)" (my-wallet-alist-get '(query results quote TradeDate)
					   trade-info
					   ))
			      )
		     (concat (propertize "Closed Market" 'font-lock-face '(:foreground "red" :weight 'bold))
			      (format " (Last Trade Date: %s)" (my-wallet-alist-get '(query results quote LastTradeDate)
					   trade-info
					   ))
			      )
		    ;;(propertize "Closed Market" 'font-lock-face '(:foreground "red" :weight 'bold))
		      )
		  ))
  (insert (format "%-12s %10s %10s %10s %10s %10s" "Stock" "Day" "15d" "200d" "YearLow" "YearHigh")))
  )

(defun my-wallet-info-buffer (stock-list)
  (let ((inhibit-read-only t))
    (switch-to-buffer-other-window "*my-wallet*")
    (erase-buffer)
    (my-wallet-header)
    (insert "\n")
    (mapcar (lambda (stock)
	      (insert (my-wallet-info-string stock))
	      (insert "\n")
	      ) stock-list)
    (special-mode)
    ))

(my-wallet-info-buffer my-wallet-stocks)
