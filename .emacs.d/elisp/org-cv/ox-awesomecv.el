;;; ox-awesomecv.el --- LaTeX awesomecv Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Diego Zamboni <diego@zzamboni.org> based on work by Oscar Najera <hi AT oscarnajera.com DOT com>
;; Keywords: org, wp, tex

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library implements a LaTeX awesomecv back-end, derived from the
;; LaTeX one.

;;; Code:
(require 'ox-latex)
(require 'org-cv-utils)

;; Install a default set-up for awesomecv export.
(unless (assoc "awesomecv" org-latex-classes)
  (add-to-list 'org-latex-classes
               '("awesomecv"
                 "\\documentclass{awesome-cv}\n[NO-DEFAULT-PACKAGES]"
                 ("\\cvsection{%s}" . "\\cvsection{%s}")
                 ("\\cvsubsection{%s}" . "\\cvsubsection{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\cvparagraph{%s}" . "\\cvparagraph{%s}"))))

;;; User-Configurable Variables

(defgroup org-export-cv nil
  "Options specific for using the awesomecv class in LaTeX export."
  :tag "Org awesomecv"
  :group 'org-export
  :version "25.3")

;;; Define Back-End
(org-export-define-derived-backend 'awesomecv 'latex
;  :menu-entry
;  (?l 1
;      ((?w "AwesomeCV format" (lambda (a s v b) (org-export-to-file 'awesomecv (org-export-output-file-name ".tex"))))))
  :options-alist
  '((:latex-class "LATEX_CLASS" nil "awesomecv" t)
    (:cvstyle "CVSTYLE" nil "classic" t)
    (:cvcolor "CVCOLOR" nil "awesome-emerald" t)
    (:cvcolorizelinks "CVCOLORIZELINKS" nil nil t)
    (:cvunderlinelinks "CVUNDERLINELINKS" nil nil t)
    (:mobile "MOBILE" nil nil parse)
    (:homepage "HOMEPAGE" nil nil parse)
    (:address "ADDRESS" nil nil newline)
    (:photo "PHOTO" nil nil t)
    (:photostyle "PHOTOSTYLE" nil nil t)
    (:gitlab "GITLAB" nil nil parse)
    (:github "GITHUB" nil nil parse)
    (:leanpub "LEANPUB" nil nil parse)
    (:linkedin "LINKEDIN" nil nil parse)
    (:twitter "TWITTER" nil nil parse)
    (:stackoverflow "STACKOVERFLOW" nil nil split)
    (:extrainfo "EXTRAINFO" nil nil parse)
    (:with-email nil "email" t t)
    (:fontdir "FONTDIR" nil "fonts/" t)
    (:latex-title-command "LATEX_TITLE" nil "\\makecvheader" t)
    (:cvhighlights "CVHIGHLIGHTS" nil "true" t)
    (:quote "QUOTE" nil nil t)
    (:firstname "FIRSTNAME" nil nil t)
    (:lastname "LASTNAME" nil nil t)
    (:cvfooter_left "CVFOOTER_LEFT" nil nil t)
    (:cvfooter_middle "CVFOOTER_MIDDLE" nil nil t)
    (:cvfooter_right "CVFOOTER_RIGHT" nil nil t))
  :translate-alist '((template . org-awesomecv-template)
                     (headline . org-awesomecv-headline)
                     (plain-list . org-awesomecv-plain-list)
                     (item . org-awesomecv-item)
                     (property-drawer . org-awesomecv-property-drawer)))

;;;; Template
;;
;; Template used is similar to the one used in `latex' back-end,
;; excepted for the table of contents and awesomecv themes.

(defun org-awesomecv-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
        (spec (org-latex--format-spec info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
          (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler.
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info nil t)
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
         (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))

     (format "\\fontdir[%s]\n" (plist-get info :fontdir))
     (format "\\colorlet{awesome}{%s}\n" (plist-get info :cvcolor))
     (format "\\setbool{acvSectionColorHighlight}{%s}\n" (plist-get info :cvhighlights))
     (let ((cvcolorizelinks (plist-get info :cvcolorizelinks))
           (cvunderlinelinks (plist-get info :cvunderlinelinks)))
       (concat
        (when (and (org-string-nw-p cvcolorizelinks)
                   (not (string-equal cvcolorizelinks "false")))
          (format "\\colorizelinks%s\n"
                  (if (not (string-equal cvcolorizelinks "true"))
                    (format "[%s]" cvcolorizelinks) "")))
        (when (and (org-string-nw-p cvunderlinelinks)
                   (not (string-equal cvunderlinelinks "false")))
          (format "\\underlinelinks%s\n"
                  (if (not (string-equal cvunderlinelinks "true"))
                      (format "[%s]" cvunderlinelinks) "")))))
     ;; Author. If FIRSTNAME or LASTNAME are not given, try to deduct
     ;; their values by splitting AUTHOR on white space.
     (let* ((author (split-string (org-export-data (plist-get info :author) info)))
            (first-name-prop (org-export-data (plist-get info :firstname) info))
            (last-name-prop (org-export-data (plist-get info :lastname) info))
            (first-name (or (org-string-nw-p first-name-prop) (car author)))
            (last-name (or (org-string-nw-p last-name-prop) (cadr author))))
       (format "\\name{%s}{%s}\n" first-name last-name))

     ;; Title
     (format "\\position{%s}\n" title)

     ;; photo
     (let* ((photo (plist-get info :photo))
            (photo-style (plist-get info :photostyle))
            (style-str (if photo-style (format "[%s]" photo-style) "")))
       (when (org-string-nw-p photo) (format "\\photo%s{%s}\n" style-str photo)))

     ;; address
     (let ((address (org-export-data (plist-get info :address) info)))
       (when (org-string-nw-p address)
         (format "\\address{%s}\n" (mapconcat (lambda (line)
                                                (format "%s" line))
                                              (split-string address "\n") " -- "))))
     ;; email
     (let ((email (and (plist-get info :with-email)
                       (org-export-data (plist-get info :email) info))))
       (when (org-string-nw-p email)
         (format "\\email{%s}\n" email)))

     ;; Other pieces of information
     (mapconcat (lambda (info-key)
                  (let ((info (org-export-data (plist-get info info-key) info)))
                    (when (org-string-nw-p info) (format "\\%s{%s}\n"
                                                         (substring (symbol-name info-key) 1)
                                                         info))))
                '(:mobile
                  :homepage
                  :github
                  :gitlab
                  :leanpub
                  :linkedin
                  :twitter
                  :skype
                  :reddit
                  :extrainfo)
                "")

     ;; Stack overflow requires two values: ID and name
     (let* ((so-list (plist-get info :stackoverflow))
            (so-id (when so-list (car so-list)))
            (so-name (when so-list (cadr so-list))))
       (when (and (org-string-nw-p so-id) (org-string-nw-p so-name))
         (format "\\stackoverflow{%s}{%s}\n" so-id so-name)))

     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
            (format-spec template spec)))

     ;; Document start.
     "\\begin{document}\n\n"

     ;; Title command.
     (let* ((title-command (plist-get info :latex-title-command))
            (command (and (stringp title-command)
                          (format-spec title-command spec))))
       (org-element-normalize-string
        (cond ((not (plist-get info :with-title)) nil)
              ((string= "" title) nil)
              ((not (stringp command)) nil)
              ((string-match "\\(?:[^%]\\|^\\)%s" command)
               (format command title))
              (t command))))
     ;; Footer command
     (let* ((footer-left (format-spec (or (plist-get info :cvfooter_left) "") spec))
            (footer-mid  (format-spec (or (plist-get info :cvfooter_middle) "") spec))
            (footer-right (format-spec (or (plist-get info :cvfooter_right) "") spec)))
       (when (not (string= "" (concat footer-left footer-mid footer-right)))
         (format "\\makecvfooter{%s}{%s}{%s}\n" footer-left footer-mid footer-right)))

     ;; Document's body.
     contents
     ;; Creator.
     (and (plist-get info :with-creator)
          (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "\\end{document}")))

;;;; Produce latex code for a right-float image
(defun org-awesomecv--cventry-right-img-code (file)
  (if file
    (format "\\begin{wrapfigure}{r}{0.15\\textwidth}
  \\raggedleft\\vspace{-4.0mm}
  \\includegraphics[width=0.1\\textwidth]{%s}
\\end{wrapfigure}" file) ""))

;;;; Individual cventry/cvsubentry/cvemployer/cvschool headlines
(defun org-awesomecv--format-cventry (headline contents info)
  "Format HEADLINE as as cventry.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (let* ((entrytype (org-element-property :CV_ENV headline))
         (title (org-export-data (org-element-property :title headline) info))
         (date (org-element-property :DATE headline))
         (from-date (or (org-element-property :FROM headline) date))
         (to-date (or (org-element-property :TO headline) date))
         (employer (or (org-element-property :ORGANIZATION headline)
                       (org-element-property :SCHOOL headline)
                       (org-element-property :EMPLOYER headline)
                       (org-element-property :EVENT headline)
                       (org-element-property :POSITION headline) ""))
         (location (or (org-element-property :LOCATION headline) ""))
         (right-img (org-element-property :RIGHT_IMG headline))
         (label (or (org-element-property :LABEL headline) nil))
         (label-str (if label (format "%s\\hfill{}" label) ""))
         ;; Other Coverletter properties
         (recipient  (or (org-element-property :RECIPIENT headline) ""))
         (letter-dateformat (org-element-property :DATEFORMAT headline))
         (letter-date
          (format "\\letterdate{%s}"
                  (if date
                   (format "%s" (org-awesomecv-org-timestamp-to-dateformat date letter-dateformat t))
                   "\\today")))
         (letter-opening (if (org-element-property :LETTER_OPENING headline)
                           (format "\\letteropening{%s}" (org-element-property :LETTER_OPENING headline))
                           ""))
         (letter-closing (if (org-element-property :LETTER_CLOSING headline)
                           (format  "\\letterclosing{%s}" (org-element-property :LETTER_CLOSING headline))
                           ""))
         (letter-signature (if (org-element-property :LETTER_SIGNATURE headline)
                               (format  "\\lettersignature{%s}" (org-element-property :LETTER_SIGNATURE headline))
                           ""))
         (letter-attached (if (org-element-property :LETTER_ATTACHED headline)
                            (format "\\letterenclosure[Attached]{%s}" (org-element-property :LETTER_ATTACHED headline))
                            "")))

    (cond
     ((string= entrytype "cvemployer")
      (format "\n\\cventry{%s}{%s}{}{}{}\n%s\n"
              title
              (format "%s\\hfill %s" (org-cv-utils--format-time-window from-date to-date) location)
              contents)
      )
     ((string= entrytype "cventry")
      (format "\n\\cventry\n{%s}\n{%s}\n{%s}\n{%s}\n{%s%s}\n"
              employer
              location
              title
              (org-cv-utils--format-time-window from-date to-date)
              (org-awesomecv--cventry-right-img-code right-img)
              contents))
     ((string= entrytype "cvsubentry")
      (format "\n\\cvsubentry\n{%s}\n{%s}\n{%s%s}\n"
              title
              (format "%s%s" label-str (org-cv-utils--format-time-window from-date to-date))
              (org-awesomecv--cventry-right-img-code right-img)
              contents))
     ((string= entrytype "cvschool")
      (format "\n\\cventry\n{%s}\n{%s}\n{%s}\n{%s}\n{%s%s}\n"
              title
              location
              employer
              (org-cv-utils--format-time-window from-date to-date)
              (org-awesomecv--cventry-right-img-code right-img)
              contents))
     ((string= entrytype "cvhonor")
      (format "\n\\cvhonor\n{%s}\n{%s}\n{%s}\n{%s}\n"
              title
              employer
              location
              (org-cv-utils--format-time-window from-date to-date)))
     ;; Coverletter sections
     ((string= entrytype "letterheader")
      (format "\\recipient\n  {%s}\n  {%s%s%s}\n\n%s\n%s\n%s\n%s\n%s\n"
              recipient
              employer
              (if (and employer location) "\\\\" "")
              location
              letter-date
              letter-opening
              letter-closing
              letter-signature
              letter-attached))
     ((string= entrytype "cvletter")
      (format "\n\\lettertitle{%s}\n\\makelettertitle\n\n\\begin{cvletter}\n%s\n\\end{cvletter}\n\\makeletterclosing"
              title
              contents))
     ((string= entrytype "cvletter_notitle")
      (format "\n\\makelettertitle\n\n\\begin{cvletter}\n%s\n\\end{cvletter}\n\\makeletterclosing"
              contents))
     ((string= entrytype "lettersection")
      (format "\n\\lettersection{%s}\n%s"
              title
              contents)))))

;;;; Headlines of type "cventries"
(defun org-awesomecv--format-cvenvironment (environment headline contents info)
  "Format HEADLINE as as a cventries/cvhonors environment.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (format "%s\n\\begin{%s}\n%s\\end{%s}\n"
          (org-export-with-backend 'latex headline nil info)
          environment contents environment))

;;;; Headline
(defun org-awesomecv-headline (headline contents info)
  "Transcode HEADLINE element into awesomecv code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((environment (let ((env (org-element-property :CV_ENV headline)))
                         (or (org-string-nw-p env) "block")))
          (pagebreak (org-string-nw-p (org-element-property :PAGEBREAK headline))))
      (concat
       (when pagebreak "\\clearpage\n")
       (cond
        ;; is a cv entry or subentry
        ((or (string= environment "cventry")
             (string= environment "cvsubentry")
             (string= environment "cvemployer")
             (string= environment "cvschool")
             (string= environment "cvhonor")
             (string= environment "cvletter")
             (string= environment "cvletter_notitle")
             (string= environment "lettersection")
             (string= environment "letterheader")
             )
         (org-awesomecv--format-cventry headline contents info))
        ((or (string= environment "cventries") (string= environment "cvhonors"))
         (org-awesomecv--format-cvenvironment environment headline contents info))
        ((org-export-with-backend 'latex headline contents info)))))))

;;;; Plain List, to intercept and transform "cvskills" lists

(defun org-awesomecv-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to LaTeX.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((cv-env (org-entry-get (org-element-property :begin plain-list) "CV_ENV" nil))
         (parent-type (car (org-element-property :parent plain-list))))
    (cond
     ((string= cv-env "cvskills")
      (format "\\begin{cvskills}\n%s\\end{cvskills}" contents))
     ((and (eq parent-type 'section) (or (string= cv-env "cventry") (string= cv-env "cvsubentry") (string= cv-env "cvschool")))
      (format "\\begin{cvitems}\n%s\\end{cvitems}" contents))
     (t
      (org-latex-plain-list plain-list contents info)))))

;;;; Item, to intercept and transform "cvskills" lists

(defun org-awesomecv-item (item contents info)
  (let* ((cv-env (org-entry-get (org-element-property :begin item) "CV_ENV" t))
         (tag (let ((tag (org-element-property :tag item)))
                (and tag (org-export-data tag info)))))
    (if (and (string= cv-env "cvskills") tag)
        (format "\\cvskill{%s}{%s}\n" tag (org-trim contents))
      (org-latex-item item contents info))
    )
  )

;;;; Property Drawer, to avoid exporting them even when the option is set

(defun org-latex-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to AwesomeCV.
This does not make sense in the AwesomeCV format, so it only
returns an empty string."
  nil)


(defun org-awesomecv-org-timestamp-to-dateformat (date_str &optional FORMAT-STRING ORDINAL)
"Format orgmode timestamp DATE_STR into a date format FORMAT-STRING.
ORDINAL returns the date as an ordinal number, specified as %E in format.
Uses defaults that are consistent with awesomecv.
Other strings are just returned unmodified

e.g. <2002-08-12 Mon> => August 12th, 2012
today => today"
  (if (string-match (org-re-timestamp 'active) date_str)
      (let* ((dte (org-parse-time-string date_str))
             (time (encode-time dte))
             (day-format (if ORDINAL "%E" "%e"))
             (format-string-0 (or FORMAT-STRING
                                (if (eql calendar-date-style 'american)
                                    (format "%%B %s, %%Y" day-format)
                                  (format "%s %%B, %%Y" day-format))))
             (day-raw (format-time-string "%eth" time))
             (day-ordinal
              (let ((r0 "\\([04-9]\\|1[0-9]\\)th$")
                    (r1 "\\([1]\\)th$"   )
                    (r2 "\\([2]\\)th$"   )
                    (r3 "\\([3]\\)th$"   )
                    )
                (cond
                 ((string-match r0 day-raw) (replace-regexp-in-string r0 "\\1th" day-raw))
                 ((string-match r1 day-raw) (replace-regexp-in-string r1 "\\1st" day-raw))
                 ((string-match r2 day-raw) (replace-regexp-in-string r2 "\\1nd" day-raw))
                 ((string-match r3 day-raw) (replace-regexp-in-string r3 "\\1rd" day-raw )))))
             (format-string (replace-regexp-in-string "%E" day-ordinal format-string-0 't)))
        (format-time-string format-string time))
    date_str))


(provide 'ox-awesomecv)
;;; ox-awesomecv ends here
