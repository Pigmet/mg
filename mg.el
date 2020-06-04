(require 'myfn) ;; for mylet 
(require 's)
(require 'cl)
(require 'dash)
(require 'ido)

;;This file contains Elisp functions for interaction with git.

(defmacro mg-with-buffer
    (buffer &rest body)
  "Evaluates body in buffer and switch to it. The content of the buffer gets erased before the evaluation happens."
  `(progn
     (with-current-buffer ,buffer (erase-buffer)  ,@body)
     (switch-to-buffer-other-window ,buffer)))

(defun mg--display-text-buff
    (buff s)
  "Print string s in buffer buff."
  (with-current-buffer buff
    (erase-buffer)
    (insert s))
  (switch-to-buffer-other-window buff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialization and adding. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mg-buff (generate-new-buffer "mg"))

(defun mg-init ()
  "Initializes git repository in the current directory."
  (interactive)
  (message "%s"(shell-command-to-string "git init")))

(defun mg-add-all()
  (interactive)
  (when (y-or-n-p "add all?")
    (message "%s"
	     (shell-command-to-string "git add --all"))))

(defun mg-add-current-file ()
  (interactive)
  (mylet [file (buffer-file-name)]
	 (when (y-or-n-p (format "add %s ?" file))
	   (message "%s"
		    (shell-command-to-string (format "git add %s" file))))))

;;;;;;;;;
;; log ;;
;;;;;;;;;

(setq log-buff (generate-new-buffer "*mg log*"))

(defun mg--buffer-output (buff s)
  (with-current-buffer buff
    (erase-buffer)
    (insert s)
    (beginning-of-buffer))
  (switch-to-buffer-other-window buff))

(defun mg-log ()
  (interactive)
  (mg--buffer-output log-buff (shell-command-to-string "git log")))

(defun mg-log-oneline ()
  (interactive)
  (mg--buffer-output log-buff
		     (shell-command-to-string
		      "git log --oneline --graph --decorate")))

;;;;;;;;;;;;
;; status ;;
;;;;;;;;;;;;

(setq status-buff (generate-new-buffer "*mg status*"))

(defun mg-status ()
  (interactive)
  (mylet [res   (shell-command-to-string "git status")]
	 (mg--buffer-output status-buff  res))  )

;; FIXME: insecure behavior of format.

(setq commit-buff (generate-new-buffer "*mg-commit*"))

(defun mg-commit-all ()
  (interactive)
  (save-some-buffers)
  (mylet [msg (read-string "Enter commit message: ")
	      msg (format "git commit -a -m \"%s\"" msg)
	      res (shell-command-to-string msg)]
	 (mg-with-buffer commit-buff
			 (save-excursion
			   (insert res)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; checkout commits  ;;
;;;;;;;;;;;;;;;;;;;;;;;

(setq checkout-buff (generate-new-buffer "*mg checkout*"))

(defun mg--get-hashes ()
  (mylet [re (rx "commit"
		 (+ space)
		 (group-n 1 (+ alnum)))]
	 (-map
	  '-second-item
	  (s-match-strings-all
	   re
	   (shell-command-to-string "git log")))))

(defun mg--log-pretty-lines ()
  (s-split "\n"
	   (shell-command-to-string
	    "git log --pretty=oneline --abbrev-commit")))

(defun mg--insert-checkout-button (hash label dir)
  (insert-text-button label
		      'action
		      (lexical-let ((hash hash)
				    (dir dir)
				    (label label))
			(-lambda (_)
			  (mylet [default-directory dir]
				 (when (y-or-n-p
					(format "Checkout %s?" label))
				   (shell-command
				    (format "git checkout %s" hash))))))))


(defun mg-checkout-commit ()
  "Checkouts a previous commit."
  (interactive)
  (mylet [coll-hash (mg--get-hashes)
		    coll-label (mg--log-pretty-lines)
		    dir default-directory]
	 (with-current-buffer checkout-buff
	   (erase-buffer)
	   (insert "Click to checkout:\n\n")
	   (loop for hash in coll-hash
		 for label in coll-label
		 do
		 (mg--insert-checkout-button hash label dir)
		 (insert "\n"))
	   (beginning-of-buffer))
	 (switch-to-buffer-other-window checkout-buff)))

;; checkout master

(defun mg-checkout-master ()
  (interactive)
  (when (y-or-n-p "Switch to master?")
    (message (shell-command-to-string "git checkout master"))))

(defun mg--current-branch-name()
  "Returns the current branch name."
  (mylet [re (rx (and "branch" (+ space) (group-n 1 (+ alnum))))
	     s (shell-command-to-string "git status")]
	 (-> (s-match re s)
	     -second-item)))

;;;;;;;;;;;;
;; branch ;;
;;;;;;;;;;;;

(setq branch-buff (generate-new-buffer "*mg branch*"))

(defun mg-view-branches()
  (interactive)
  (mylet [s (shell-command-to-string "git branch -a")]
	 (with-current-buffer branch-buff
	   (erase-buffer)
	   (insert "Branches:\n\n")
	   (insert s)
	   (beginning-of-buffer))
	 (switch-to-buffer-other-window branch-buff)))

(defun mg-new-branch ()
  "Creates a new branch and switch to it."
  (interactive)
  (mylet [br (read-string "Enter branch name: ")]
	 (when (y-or-n-p (format "Create new branch %s and switch to it?" br))
	   (message (shell-command-to-string
		     (format "git checkout -b %s" br))))))

(defun mg--list-branch-strings ()
  "Returns list of branches."
  (->>
   (shell-command-to-string "git branch -a")
   (s-split (rx (+ space)))
   (-map (lambda (s) (s-chop-suffix "*" s )))
   (-map 's-trim)
   (-remove (lambda (s) (-> s length (eq 0))))
   ))

(defun mg-branch-list ()
  (interactive)
  (mylet [s (s-join ", " (mg--list-branch-strings))]
	 (message s)))

;; branch deletion

(setq mg-delete-buff (generate-new-buffer "mg delete"))

(defun mg--delete-branch-impl (br option)
  "Deletes the branch br. Returns the git message."
  (mylet [res   (shell-command-to-string
		 (format "git branch %s %s" option br))]
	 (message res)))

(defun mg--delete-branch-standard (br)
  (mg--delete-branch-impl "-d" br))

(defun mg--delete-branch-hard (bf)
  (mg--delete-branch-impl br "-D"))

(defun mg-delete-branch ()
  "Executes the git command: git branch -d (branch)"
  (interactive)
  (mylet [coll (mg--list-branch-strings)
	       br (ido-completing-read "Choose branch:" coll)]
	 (when (y-or-n-p (format "Delete %s ?" br))
	   (mg--delete-branch-standard br))))

(defun mg-delete-branch-hard ()
  "Executes the git command: git branch -D (branch)"
  (interactive)
  (mylet [coll (mg--list-branch-strings)
	       br (ido-completing-read "Choose branch:" coll)]
	 (when (y-or-n-p (format "Delete %s ?" br))
	   (mg--delete-branch-hard br))))

(setq mg-branch (generate-new-buffer "*mg-branch*"))

(defun mg-branch-checkout ()
  (interactive)
  (mylet [branches (mg--list-branch-strings)
		   br (ido-completing-read "Choose branch: " branches)
		   res (shell-command-to-string
			(format "git checkout %s" br))]
	 (message "%s" res)))

(defalias 'mg-switch-branch 'mg-branch-checkout)

;;;;;;;;;;;;;
;; merging ;;
;;;;;;;;;;;;;

(defun mg--merge-impl (br)
  (mylet [res (shell-command-to-string (format "git merge %s" br))]
	 (message res)))

(defun mg-merge ()
  "Merge a branch to the current line of development."
  (interactive)
  (mylet [branches (mg--list-branch-strings)
		   br (ido-completing-read "Choose branch to merge." branches)
		   cur-br (mg--current-branch-name)]
	 (when (y-or-n-p
		(format "merge branch %s to %s ?" br cur-br ))
	   (mg--merge-impl br))))

;;;;;;;;;;;;
;; remote ;;
;;;;;;;;;;;;

(defun mg--list-remotes()
  (->>
   (shell-command-to-string "git remote ")
   (s-split "\n")
   (-filter (lambda (s) (-> s length (> 0) )))))

(defun mg-view-remotes ()
  (interactive)
  (message  (s-join ", " (mg--list-remotes))))

(defun mg-remove-remote ()
  (interactive)
  (mylet [coll (mg--list-remotes)
	       rm (ido-completing-read "Choose remote to delete: " coll)]
	 (when (y-or-n-p (format "Delete remote %s ?" rm))
	   (shell-command
	    (format "git remote rm %s" rm))
	   (message (format "remote %s was removed." rm)))))

(defun mg-remote-how-to-push-manually ()
  "Inserts shell command for adding a remote."
  (interactive)
  (insert "git push -u origin master"))

(defun mg-push-remote-master ()
  "Note: To see how to push manually from shell, call mg-how-to-push-manually."
  (interactive)
  (mylet [target (ido-completing-read "Choose remote: " (mg--list-remotes))]
	 (message (shell-command-to-string
		   (format "git push -u %s master" target)))))

;; tagging

(setq mg-tag-buffer (generate-new-buffer"*mg-tag*"))

(setq mg-specific-tag-buffer
      (generate-new-buffer "*mg-specific-tag*"))

(defun mg-create-tag ()
  (interactive)
  (mylet [v (read-string "version:")
	    msg (read-string "message:")]
	 (shell-command (format "git tag -a %s -m \"%s\"" v msg))
	 (message "new tag %s was created." v)))

(defun mg--get-coll-tag (buf)
  "Returns coll of tags."
  (mylet [s (with-current-buffer buf
	      (shell-command-to-string "git tag"))]
	 (->> s
	      (s-split "\n")
	      (-map 's-trim)
	      (-remove (-lambda (s)(= 0 (length s))))
	      reverse)))

(defun mg--show-tag-impl (buf tag)
  (mylet [s (with-current-buffer buf
	      (shell-command-to-string
	       (format "git show %s" tag)))]
	 (with-current-buffer
	     mg-specific-tag-buffer
	   (erase-buffer)
	   (save-excursion
	     (insert s)))
	 (switch-to-buffer-other-window
	  mg-specific-tag-buffer)))

(defun mg-show-list-tag ()
  (interactive)
  (mylet [buf (current-buffer)
	      coll (mg--get-coll-tag buf)]
	 (with-current-buffer
	     mg-tag-buffer
	   (erase-buffer)
	   (save-excursion
	     (loop for tag in coll
		   do
		   (insert "\n")
		   (insert-text-button
		    tag
		    'action
		    (lexical-let ((buf buf)
				  (tag tag))
		      (-lambda (b)
			(mg--show-tag-impl buf tag)))))))
	 (switch-to-buffer-other-window mg-tag-buffer)))

(defun mg--checkout-tag-impl (buf tag)
  (when (y-or-n-p (format "checkout %s ?" tag))
    (with-current-buffer buf
      (message (shell-command-to-string
		(format "git checkout %s" tag))))))

(defun mg-checkout-tag ()
  (interactive)
  (mylet [buf (current-buffer)
	      coll (mg--get-coll-tag buf)]
	 (with-current-buffer
	     mg-tag-buffer
	   (erase-buffer)
	   (save-excursion
	     (loop for tag in coll
		   do
		   (insert "\n")
		   (insert-text-button
		    tag
		    'action
		    (lexical-let ((buf buf)
				  (tag tag))
		      (-lambda (b)
			(mg--checkout-tag-impl buf tag)))))))
	 (switch-to-buffer-other-window mg-tag-buffer)))



;;;;;;;;;;;
;; other ;;
;;;;;;;;;;;

(setq mg-reset-buff (generate-new-buffer "mg reset"))

(defun mg-reset-hard ()
  "Undoes the last commit."
  (interactive)
  (when (y-or-n-p "reste hard?")
    (mylet [res (shell-command-to-string "git reset --hard")]
	   (mg--display-text-buff mg-reset-buff res))))

(defun mg-reset-mixed()
  (interactive)
  (when (y-or-n-p "reste hard?")
    (mylet [res (shell-command-to-string "git reset --mixed")]
	   (mg--display-text-buff mg-reset-buff res))))

(defun mg-print-username()
  (interactive)
  (message
   (shell-command-to-string "git config --global user.name")))

(defun mg-copy-ssh-to-clipboard()
  (interactive)
  (mylet [s (shell-command-to-string "cat ~/.ssh/id_rsa.pub")]
	 (kill-new s)
	 (message (format "Copied \n\n %s" s))))

(defun mg-pull-rebase()
  (interactive)
  (mylet [res (shell-command-to-string "git pull --rebase")]
	 (message res)))

(defun mg-push ()
  (interactive)
  (mylet [res (shell-command-to-string "git push")]
	 (message res)))

;; clearn

(setq mg-clean-buff (generate-new-buffer "mg-clean"))

(defun mg--untracked-files()
  (mylet [re (rx "Would remove" (+ space) (group-n 1 (+ (not (any "\n")))))]
	 (-map '-last-item
	       (s-match-strings-all
		re
		(shell-command-to-string "git clean -n")))))

(defun mg-list-untracked-files()
  (interactive)
  (mylet [files (mg--untracked-files)]
	 (message (s-join " " files))))

(defun mg-execute-clean ()
  "Removes all untracked files."
  (interactive)
  (mylet [files (mg--untracked-files)
		ask-user (format "Remove %s" (s-join " " files))]
	 (when (y-or-n-p ask-user)
	   (mylet [s (shell-command-to-string "git clean -f")]
		  (message s)))))

;; stash

(setq mg-message-buff (generate-new-buffer "mg-message"))

(defun mg-stash ()
  (interactive)
  (mylet [s (shell-command-to-string "git stash")]
	 (with-current-buffer mg-message-buff
	   (erase-buffer)
	   (insert s))
	 (switch-to-buffer-other-window mg-message-buff)))

(defun mg-stash-pop ()
  (interactive)
  (mylet [s (shell-command-to-string "git stash")]
	 (with-current-buffer mg-message-buff
	   (erase-buffer)
	   (insert s))
	 (switch-to-buffer-other-window mg-message-buff)))

(defun mg-stash-list()
  (interactive)
  (mylet [s (shell-command-to-string "git stash list")]
	 (with-current-buffer mg-message-buff
	   (erase-buffer)
	   (insert s))
	 (switch-to-buffer-other-window mg-message-buff)))

(defun mg-stash-clear ()
  (interactive)
  (when (y-or-n-p "clear all stashes?")
    (shell-command "git stash clear")
    (message "cleared all stashes")))

;; remove

(defun mg-remove-current-file-cashed ()
  (interactive)
  (mylet [f (-> (buffer-file-name) (file-name-nondirectory))]
	 (when (y-or-n-p (format "remove %s cached?" f))
	   (message (shell-command-to-string (format "git rm --cached %s" f))))))

(defun mg-remove-current-file ()
  (interactive)
  (mylet [f (-> (buffer-file-name) (file-name-nondirectory))]
	 (when (y-or-n-p (format "remove %s (also from filesystem)?" f))
	   (message (shell-command-to-string (format "git rm  %s" f))))))

(provide 'mg)


