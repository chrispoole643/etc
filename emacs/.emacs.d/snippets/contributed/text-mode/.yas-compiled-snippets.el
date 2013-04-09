;;; Compiled snippets and support files for `text-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
                     '(("article" "\\documentclass[11pt]{article}\n\n\\usepackage{graphicx,amsmath,amssymb,subfigure,url,xspace}\n\\newcommand{\\eg}{e.g.,\\xspace}\n\\newcommand{\\bigeg}{E.g.,\\xspace}\n\\newcommand{\\etal}{\\textit{et~al.\\xspace}}\n\\newcommand{\\etc}{etc.\\@\\xspace}\n\\newcommand{\\ie}{i.e.,\\xspace}\n\\newcommand{\\bigie}{I.e.,\\xspace}\n\n\\title{${1:title}}\n\\author{${2:Author Name}}\n\n\\begin{document}\n\\maketitle\n\n\n\\bibliographystyle{${3:plain}}\n\\bibliography{${4:literature.bib}}\n\n\\end{document}\n" "\\documentclass{article} ..." nil
                        ("skeleton")
                        nil nil nil nil)
                       ("beamer" "\\documentclass[xcolor=dvipsnames]{beamer}\n\n\\usepackage{graphicx,subfigure,url}\n\n% example themes\n\\usetheme{Frankfurt}\n\\usecolortheme{seahorse}\n\\usecolortheme{rose}\n\n% put page numbers\n% \\setbeamertemplate{footline}[frame number]{}\n% remove navigation symbols\n% \\setbeamertemplate{navigation symbols}{}\n\n\\title{${1:Presentation Title}}\n\\author{${2:Author Name}}\n\n\\begin{document}\n	\n\\frame[plain]{\\titlepage}\n	\n\\begin{frame}[plain]{Outline}\n	\\tableofcontents\n\\end{frame}\n	\n\\section{${3:Example Section}}\n\\begin{frame}{${4:Frame Title}}\n\n\\end{frame}\n\n\\end{document}\n" "\\documentclass{beamer} ..." nil
                        ("skeleton")
                        nil nil nil nil)
                       ("begin" "\\begin{${1:$$(yas/choose-value (mapcar 'car (LaTeX-environment-list)))}}\n$0\n\\end{$1}" "\\begin{environment} ... \\end{environment}" nil
                        ("environments")
                        nil nil nil nil)
                       ("bib" "\\bibliographystyle{plain}\n\\bibliography{$1}$0" "\\bibliography" nil
                        ("misc")
                        nil nil nil nil)
                       ("big" "\\\\${1:$$(yas/choose-value '(\"big\" \"Big\" \"bigg\" \"Bigg\"))}l( $0  \\\\$1r)" "\\bigl( ... \\bigr)" nil
                        ("math")
                        nil nil nil nil)
                       ("bigop" "\\\\big${1:$$(yas/choose-value '(\"oplus\" \"otimes\" \"odot\" \"cup\" \"cap\" \"uplus\" \"sqcup\" \"vee\" \"wedge\"))}_{$2}^{$3}$0\n" "\\bigop_{n}^{}" nil
                        ("math")
                        nil nil nil nil)
                       ("binom" "\\binom{${1:n}}{${2:k}}\n" "\\binom{n}{k}" nil
                        ("math")
                        nil nil nil nil)
                       ("block" "\\begin{${1:$$(yas/choose-value '(\"block\" \"exampleblock\" \"alertblock\"))}}{${2:Block Title}}\n\n\\end{$1}\n" "\\begin{*block} ... \\end{*block}" nil
                        ("environments")
                        nil nil nil nil)
                       ("case" "\\begin{cases}\n$0 \\\\\\\\\n\\end{cases}\n" "\\begin{cases} ... \\end{cases}" nil
                        ("math")
                        nil nil nil nil)
                       ("cha" "\\chapter{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\chapter" nil
                        ("sections")
                        nil nil nil nil)
                       ("cha*" "\\chapter*{${1:name}}\n$0" "\\chapter*" nil
                        ("sections")
                        nil nil nil nil)
                       ("coprod" "\\coprod_{$1}^{$2}$0\n" "\\coprod_{n}^{}" nil
                        ("math")
                        nil nil nil nil)
                       ("desc" "\\begin{description}\n\\item[$0]\n\\end{description}\n" "\\begin{description} ... \\end{description}" nil
                        ("environments")
                        nil nil nil nil)
                       ("doc" "\\documentclass[$2]{${1:$$(yas/choose-value '(\"article\" \"report\" \"book\" \"letter\"))}}\n\n\\begin{document}\n$0\n\\end{document}\n" "\\documentclass" nil nil nil nil nil nil)
                       ("enum" "\\begin{enumerate}\n\\item $0\n\\end{enumerate}\n" "\\begin{enumerate} ... \\end{enumerate}" nil
                        ("environments")
                        nil nil nil nil)
                       ("eq" "\\begin{equation}\n$>$0\n\\end{equation}\n" "\\begin{equation} ... \\end{equation}" nil
                        ("math")
                        nil nil nil nil)
                       ("eqs" "\\begin{${1:$$(yas/choose-value '(\"align\" \"align*\" \"multline\" \"gather\" \"subequations\"))}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0\n\\end{$1}\n" "\\begin{align} ... \\end{align}" nil
                        ("math")
                        nil nil nil nil)
                       ("fig" "\\begin{figure}[htbp]\n  \\centering\n  $0\n  \\caption{${1:caption}}\n  \\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n\\end{figure}\n" "\\begin{figure} ... \\end{figure}" nil
                        ("environments")
                        nil nil nil nil)
                       ("frac" "\\frac{${1:numerator}}{${2:denominator}}$0" "\\frac{numerator}{denominator}" nil
                        ("math")
                        nil nil nil nil)
                       ("frame" "\\begin{frame}{${1:Frame Title}}\n\n\\end{frame}\n" "\\begin{frame} ... \\end{frame}" nil
                        ("environments")
                        nil nil nil nil)
                       ("graphics" "\\includegraphics[width=${1:\\linewidth}]{${2:file}}" "\\includegraphics" nil nil nil nil nil nil)
                       ("href" "\\href{${1:url}}{${2:text}}$0" "\\href{url}{text}" nil
                        ("environments")
                        nil nil nil nil)
                       ("int" "\\\\${1:$$(yas/choose-value '(\"int\" \"oint\" \"iint\" \"iiint\" \"iiiint\" \"idotsint\"))}{$2}^{$3}$0\n" "\\int_{n}^{}" nil
                        ("math")
                        nil nil nil nil)
                       ("it" "\\item $0" "\\item" nil
                        ("environments")
                        nil nil nil nil)
                       ("item" "\\begin{itemize}\n\\item $0\n\\end{itemize}\n" "\\begin{itemize} ... \\end{itemize}" nil
                        ("environments")
                        nil nil nil nil)
                       ("letter" "\\documentclass{letter}\n\\signature{${1:Foo Bar}}\n\\address{${2:Address line 1 \\\\\\\\ \nAddress line 2 \\\\\\\\\nAddress line 3}}\n\\begin{document}\n \n\\begin{letter}\n{${3:Recipient's address}}\n\n\\opening{Dear ${4:Sir}:}\n\n$0\n \n\\closing{Yours Sincerely,}\n \n\\end{letter}\n \n\\end{document}\n\n" "\\documentclass{letter} ..." nil
                        ("skeleton")
                        nil nil nil nil)
                       ("lim" "\\lim_{$1}$0\n" "\\lim_{n}" nil
                        ("math")
                        nil nil nil nil)
                       ("math" "\\[\n$1\n\\]\n" "displaymath \\[ ... \\]" nil
                        ("math")
                        nil nil nil nil)
                       ("matrix" "\\begin{${1:$$(yas/choose-value '(\"pmatrix\" \"bmatrix\" \"Bmatrix\" \"vmatrix\" \"Vmatrix\" \"smallmatrix\"))}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0\n\\end{$1}\n\n" "\\begin{matrix} ... \\end{}" nil
                        ("math")
                        nil nil nil nil)
                       ("minipage" "\\begin{minipage}[${1:htbp}]{${2:1.0}${3:\\linewidth}}\n  $0\n\\end{minipage}" "\\begin{minipage}[position][width] ... \\end{minipage}" nil
                        ("environments")
                        nil nil nil nil)
                       ("par" "\\paragraph{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\paragraph" nil
                        ("sections")
                        nil nil nil nil)
                       ("prod" "\\prod_{$1}^{$2}$0\n" "\\prod_{n}^{}" nil
                        ("math")
                        nil nil nil nil)
                       ("sec" "\\section{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\section" nil
                        ("sections")
                        nil nil nil nil)
                       ("sec*" "\\section*{${1:name}}\n$0" "\\section*" nil
                        ("sections")
                        nil nil nil nil)
                       ("ssub" "\\subsubsection{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\subsubsection" nil
                        ("sections")
                        nil nil nil nil)
                       ("ssub*" "\\subsubsection*{${1:name}}\n$0" "\\subsubsection*" nil
                        ("sections")
                        nil nil nil nil)
                       ("sub" "\\subsection{${1:name}}\n\\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n$0" "\\subsection" nil
                        ("sections")
                        nil nil nil nil)
                       ("subfig" "\\subfigure[${1:caption}]{\n  \\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n  $0\n}\n" "\\subfigure" nil
                        ("environments")
                        nil nil nil nil)
                       ("sub*" "\\subsection*{${1:name}}\n$0" "\\subsection*" nil
                        ("sections")
                        nil nil nil nil)
                       ("sum" "\\sum_{$1}^{$2}$0\n" "\\sum_{n}^{}" nil
                        ("math")
                        nil nil nil nil)
                       ("table" "\\begin{table}[htbp]\n  \\centering\n  \\begin{tabular}{${3:format}}\n    $0\n  \\end{tabular}\n  \\caption{${1:caption}}\n  \\label{${2:\"waiting for reftex-label call...\"$(unless yas/modified-p (reftex-label nil 'dont-insert))}}\n\\end{table}\n" "\\begin{table} ... \\end{table}" nil
                        ("environments")
                        nil nil nil nil)
                       ("url" "\\url{${1:$$(yas/choose-value '(\"http\" \"ftp\"))}://${2:address}}$0" "\\url" nil
                        ("environments")
                        nil nil nil nil)
                       ("use" "\\usepackage[$2]{$1}$0" "\\usepackage" nil
                        ("misc")
                        nil nil nil nil)
                       ("verb" "\\begin{verbatim}\n$0\n\\end{verbatim}\n" "\\begin{verbatim} ... \\end{verbatim}" nil
                        ("environments")
                        nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
                     '(("!env" "#!/usr/bin/env `(file-name-nondirectory (getenv \"SHELL\"))`\n" "#!/usr/bin/env $SHELL" nil nil nil nil nil nil)
                       ("case" "case ${1:word} in\n  ${2:pattern} )\n    $0;;\nesac" "case ... esac" nil nil nil nil nil nil)
                       ("elif" "elif [[ ${1:condition} ]]; then\n  ${0:statements}" "elif ..." nil nil nil nil nil nil)
                       ("for" "for ${1:condition}; do\n  ${0:statements}\ndone" "for ... done" nil nil nil nil nil nil)
                       ("forin" "for ${1:i} in ${2:words}; do\n  ${0:statements}\ndone" "for ... in ... done" nil nil nil nil nil nil)
                       ("here" "<<${1:TOKEN}\n$0\n${1:TOKEN}" "Here Document" nil nil nil nil nil nil)
                       ("if" "if ${1:condition}; then\n  ${0:statements}\nfi" "if ... fi" nil nil nil nil nil nil)
                       ("ift" "if [[ ${1:condition} ]]; then\n  ${0:statements}\nfi" "if [[ test ]] ... fi" nil nil nil nil nil nil)
                       ("until" "until [[ ${1:condition} ]]; do\n  ${0:statements}\ndone" "until ... done" nil nil nil nil nil nil)
                       ("while" "while [[ ${1:condition} ]]; do\n  ${0:statements}\ndone" "while ... done" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Fri Apr  5 17:43:55 2013
