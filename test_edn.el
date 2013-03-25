(require 'ert)
(require 'edn)
(require 'dash)
;; (require 'websocket)

(ert-deftest edn-test-number-encode ()
  (let ((input 150)
        (output "150"))
    (should (string-equal (edn-encode input) output))))

(ert-deftest edn-test-string-encode ()
  (let ((input "hello world")
        (output "\"hello world\""))
    (should (string-equal (edn-encode input) output))))

(ert-deftest edn-test-simple-list-encode ()
  (let ((input (list "hello" "world"))
        (output "(\"hello\" \"world\")"))
    (should (string-equal (edn-encode-list input) output))))

(ert-deftest edn-test-alist-p ()
  (should (edn-alist-p '((:hello . world)
                         (:hola  . mundo))))
  ;;(should (null (edn-alist-p '(:hello world how))))
  )

(let ((it '(:hell "world" "blah")))
  (or (not (consp (cdr it)))
      (equal 2 (length it))))


(ert-deftest edn-test-alist-encode ()
  (let ((input (list '(:hello . "world") '("hola" . 123)))
        (output "{:hello \"world\" \"hola\" 123}"))))

(ert-deftest edn-test-simple-vector-encode ()
  (let ((input ["hello" "world"])
        (output "[\"hello\" \"world\"]"))
    (should (string-equal (edn-encode-vector input) output))))

(ert-deftest edn-test-timestamp-encode ()
  (let ((edn-encode-timestamp t)
        (input '(20788 63147))
        (output "#inst \"2013-03-04T11:31:55-08:00\""))
    (should (string-equal (edn-encode input) output))))

(ert-deftest edn-test-true-encode ()
  (let ((input t)
        (output "true"))
    (should (string-equal (edn-encode input) output))))

(ert-deftest edn-test-encode-with-slash ()
  (let ((input "/hello/world")
        (output "\"/hello/world\""))
    (should (string-equal (edn-encode input) output))))

(ert-deftest edn-test-nil-encode ()
  (let ((input nil)
        (output "nil"))
    (should (string-equal (edn-encode input) output))))

;; (ert-deftest edn-test-complex-1-encode ()
;;   (let ((edn-encode-timestamp t)
;;         (input (list
;;                 :is-modified t
;;                 :last-save-time "2013-03-04T11:31:55-08:00"
;;                 :file nil
;;                 :major-mode "emacs-lisp-mode"
;;                 :buffer-name "pulse.el"
;;                 :default-directory "/home/roman/Projects/pulse/"
;;                 :read-only nil))
;;         (edn-encode input))))

;; (ert-make-test-unbound 'edn-test-complex-1-encode)



;; (ert-deftest test-list-serialization ()
;;   ""
;;   (let ((list ))
;;     (sleep-for 0.1)
;;     (should (websocket-openp conn))
;;     (--dotimes 5 (websocket-send-text conn (format "Hello World %i" it)))
;;     (websocket-close conn)))
