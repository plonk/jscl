;; JSCL is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; JSCL is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with JSCL.  If not, see <http://www.gnu.org/licenses/>.

(/debug "loading repl-node/repl.lisp!")

(defvar *rl*)

(defun node-init ()
  (setq *standard-output*
        (make-stream
         :write-fn (lambda (string)
                     (#j:process:stdout:write string))))
  (setq *rl* (#j:readline:createInterface #j:process:stdin #j:process:stdout))
  (welcome-message)
  (let ((*root* *rl*)
        (input ""))
    (#j:setPrompt (format nil "~A> " (package-name *package*)))
    (#j:prompt)
    (#j:on "line"
           (lambda (line)
             (%js-try
              (progn
                (handler-case
                    (progn
                      (setq input (concatenate 'string input line "
"))
                      (loop
                         (multiple-value-bind
                               (exp nextpos)
                             (read-from-string input)
                           (let ((results (multiple-value-list
                                           (eval-interactive exp))))
                             (dolist (result results)
                               (print result)))
                           (setq input (subseq input nextpos)))))
                  (error (err)
                    (let ((args (!condition-args err)))
                      (cond
                        ((equal args '("End of file")) nil)
                        ((equal args '("Unexpected EOF")))
                        (t (progn
                             (setq input "")
                             (format t "ERROR: ")
                             (apply #'format t args)
                             (terpri))))))))
              (catch (err)
                (let ((message (or (oget err "message") err)))
                  (format t "ERROR[!]: ~a~%" message))))
             ;; Continue
             (let ((*root* *rl*))
               (if (every #'jscl::whitespacep input)
                   (#j:setPrompt (format nil "~A> " (package-name *package*)))
                   (#j:setPrompt ""))
               (#j:prompt))
             ))))


(node-init)
