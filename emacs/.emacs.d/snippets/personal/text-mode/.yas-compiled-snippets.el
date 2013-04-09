;;; Compiled snippets and support files for `text-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
                     '(("date" "`(format-time-string \"%d %b %Y\")`" "Date" nil nil nil nil nil nil)
                       ("datever" "`(format-time-string \"%Y%m%d\")` $0\n" "DateVer" nil nil nil nil nil nil)
                       ("license.bsd" "`(insert comment-start)`Copyright (c) ${1:`(format-time-string \"%Y\")`}, ${2:`(insert user-full-name)`} <${3:`(insert user-mail-address)`}>\n`(insert comment-start)`All rights reserved.\n`(insert comment-start)`\n`(insert comment-start)`Redistribution and use in source and binary forms, with or without modification,\n`(insert comment-start)`are permitted provided that the following conditions are met:\n`(insert comment-start)`\n`(insert comment-start)`- Redistributions of source code must retain the above copyright notice, this\n`(insert comment-start)`  list of conditions and the following disclaimer.\n`(insert comment-start)`\n`(insert comment-start)`- Redistributions in binary form must reproduce the above copyright notice, this\n`(insert comment-start)`  list of conditions and the following disclaimer in the documentation and/or\n`(insert comment-start)`  other materials provided with the distribution.\n`(insert comment-start)`\n`(insert comment-start)`- Neither the name of the ${4:Organization} nor the names of its contributors may\n`(insert comment-start)`  be used to endorse or promote products derived from this software without\n`(insert comment-start)`  specific prior written permission.\n`(insert comment-start)`\n`(insert comment-start)`THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\" AND\n`(insert comment-start)`ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\n`(insert comment-start)`WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\n`(insert comment-start)`DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR\n`(insert comment-start)`ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES\n`(insert comment-start)`(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\n`(insert comment-start)`LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON\n`(insert comment-start)`ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n`(insert comment-start)`(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\n`(insert comment-start)`SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n\n$0" "BSD" nil
                        ("Licenses")
                        nil nil nil nil)
                       ("license.gpl2" "`(insert comment-start)`Copyright (C) ${1:`(format-time-string \"%Y\")`} ${2:`(insert user-full-name)`} <${3:`(insert user-mail-address)`}>\n`(insert comment-start)`\n`(insert comment-start)`This program is free software; you can redistribute it and/or modify it under\n`(insert comment-start)`the terms of the GNU General Public License as published by the Free Software\n`(insert comment-start)`Foundation; either version 2 of the License, or (at your option) any later\n`(insert comment-start)`version.\n`(insert comment-start)`\n`(insert comment-start)`This program is distributed in the hope that it will be useful, but WITHOUT ANY\n`(insert comment-start)`WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A\n`(insert comment-start)`PARTICULAR PURPOSE. See the GNU General Public License for more details.\n`(insert comment-start)`\n`(insert comment-start)`You should have received a copy of the GNU General Public License along with\n`(insert comment-start)`this program; if not, write to the Free Software Foundation, Inc., 59 Temple\n`(insert comment-start)`Place, Suite 330, Boston, MA 02111-1307 USA\n\n$0" "GPLv2" nil
                        ("Licenses")
                        nil nil nil nil)
                       ("license.gpl3" "`(insert comment-start)`Copyright (C) ${1:`(format-time-string \"%Y\")`} ${2:`(insert user-full-name)`} <${3:`(insert user-mail-address)`}>\n`(insert comment-start)`\n`(insert comment-start)`This program is free software: you can redistribute it and/or modify it under\n`(insert comment-start)`the terms of the GNU General Public License as published by the Free Software\n`(insert comment-start)`Foundation, either version 3 of the License, or (at your option) any later\n`(insert comment-start)`version.\n`(insert comment-start)`\n`(insert comment-start)`This program is distributed in the hope that it will be useful, but WITHOUT ANY\n`(insert comment-start)`WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A\n`(insert comment-start)`PARTICULAR PURPOSE.  See the GNU General Public License for more details.\n`(insert comment-start)`\n`(insert comment-start)`You should have received a copy of the GNU General Public License along with\n`(insert comment-start)`this program.  If not, see <http://www.gnu.org/licenses/>.\n\n$0" "GPLv3" nil
                        ("Licenses")
                        nil nil nil nil)
                       ("license.mit" "`(insert comment-start)`Copyright (c) ${1:`(format-time-string \"%Y\")`} ${2:`(insert user-full-name)`} <${3:`(insert user-mail-address)`}>\n`(insert comment-start)`\n`(insert comment-start)`Permission is hereby granted, free of charge, to any person obtaining a copy of\n`(insert comment-start)`this software and associated documentation files (the \"Software\"), to deal in\n`(insert comment-start)`the Software without restriction, including without limitation the rights to\n`(insert comment-start)`use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of\n`(insert comment-start)`the Software, and to permit persons to whom the Software is furnished to do so,\n`(insert comment-start)`subject to the following conditions:\n`(insert comment-start)`\n`(insert comment-start)`The above copyright notice and this permission notice shall be included in all\n`(insert comment-start)`copies or substantial portions of the Software.\n`(insert comment-start)`\n`(insert comment-start)`THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR\n`(insert comment-start)`IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS\n`(insert comment-start)`FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR\n`(insert comment-start)`COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER\n`(insert comment-start)`IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN\n`(insert comment-start)`CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.\n\n$0" "MIT" nil
                        ("Licenses")
                        nil nil nil nil)
                       ("m1.yasnippet" "`(dotimes (i 80) (insert (replace-regexp-in-string \" \" \"\" comment-start)))`\n`(dotimes (i 2) (insert (replace-regexp-in-string \" \" \"\" comment-start)))` ${1:Title}\n`(dotimes (i 2) (insert (replace-regexp-in-string \" \" \"\" comment-start)))` ${1:$(make-string (string-width text) ?\\=)}\n`(dotimes (i 2) (insert (replace-regexp-in-string \" \" \"\" comment-start)))`\n`(dotimes (i 2) (insert (replace-regexp-in-string \" \" \"\" comment-start)))` ${2:Description}\n`(dotimes (i 80) (insert (replace-regexp-in-string \" \" \"\" comment-start)))`\n\n$0" "Title" nil
                        ("Code Sections")
                        nil nil nil nil)
                       ("m2.yasnippet" "`(dotimes (i 80) (insert (replace-regexp-in-string \" \" \"\" comment-start)))`\n`(dotimes (i 2) (insert (replace-regexp-in-string \" \" \"\" comment-start)))` ${1:Title}\n`(dotimes (i 2) (insert (replace-regexp-in-string \" \" \"\" comment-start)))` ${1:$(make-string (string-width text) ?\\-)}\n`(dotimes (i 2) (insert (replace-regexp-in-string \" \" \"\" comment-start)))`\n`(dotimes (i 2) (insert (replace-regexp-in-string \" \" \"\" comment-start)))` ${2:Description}\n`(dotimes (i 80) (insert (replace-regexp-in-string \" \" \"\" comment-start)))`\n\n$0" "Section" nil
                        ("Code Sections")
                        nil nil nil nil)
                       ("m3.yasnippet" "`(dotimes (i 80) (insert (replace-regexp-in-string \" \" \"\" comment-start)))`\n`(dotimes (i 2) (insert (replace-regexp-in-string \" \" \"\" comment-start)))` ${1:Title}\n`(dotimes (i 80) (insert (replace-regexp-in-string \" \" \"\" comment-start)))`\n\n$0" "Section (no desc.)" nil
                        ("Code Sections")
                        nil nil nil nil)
                       ("sig.cheers" "Cheers\n\n\nChris Poole\n[PGP BAD246F9]$0\n" "Cheers" nil
                        ("Signatures")
                        nil nil nil nil)
                       ("sig.cheersNOKEY" "Cheers\n\n\nChris\n\n$0" "CheersNOKEY" nil
                        ("Signatures")
                        nil nil nil nil)
                       ("sig.thanks" "Thanks\n\n\nChris\n\n[PGP BAD246F9]$0\n" "Thanks" nil
                        ("Signatures")
                        nil nil nil nil)
                       ("sig.thanksNOKEY" "Thanks\n\n\nChris\n\n$0" "ThanksNOKEY" nil
                        ("Signatures")
                        nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
                     '(("ali" "\\begin{align}\n  $0\n\\end{align}" "Frac" nil nil nil nil nil nil)
                       ("arr" "\\begin{array}\n$>$0\n\\end{array}" "Array environment" nil nil nil nil nil nil)
                       ("d2" "\\frac{\\Delta}{2}" "Delta/2" nil nil nil nil nil nil)
                       ("dag" "\\dagger" "Dagger" nil nil nil nil nil nil)
                       ("dh" "\\delta{\\cal H}" "delta H" nil nil nil nil nil nil)
                       ("el" "Eqn. \\ref{`(insert (reftex-reference \"e\" t))`}" "RefTeX label insert" nil nil nil nil nil nil)
                       ("fr" "\\frac{$1}{$2} $0" "Frac" nil nil nil nil nil nil)
                       ("h0" "{\\cal H}^0" "H^0" nil nil nil nil nil nil)
                       ("h011" "{\\cal H}^0_{11}" "H^0_11" nil nil nil nil nil nil)
                       ("hatd" "\\hat\\Delta" "delta hat" nil nil nil nil nil nil)
                       ("hb" "\\hbar" "Hbar" nil nil nil nil nil nil)
                       ("heff" "H_\\mathrm{eff}" "Heff" nil nil nil nil nil nil)
                       ("ii" "\\mathbb{I}" "Imaginary i" nil nil nil nil nil nil)
                       ("inf" "\\infinity $0\n" "Infinity" nil nil nil nil nil nil)
                       ("mat" "\\begin{pmatrix}\n$>$0\n\\end{pmatrix}" "Matrix environment" nil nil nil nil nil nil)
                       ("proof" "\\begin{proof}\n$>$0\n\\end{proof}" "Proof environment" nil nil nil nil nil nil)
                       ("quick" "\\documentclass[10pt,a4paper]{article}\n\\usepackage{amsmath}\n\\usepackage{amsthm}\n\\usepackage{amsfonts}\n\\usepackage{nccmath}\n\\usepackage{bm}\n\\usepackage{mathrsfs}\n\\usepackage{varioref}\n\\usepackage{graphicx}\n\\usepackage{dsfont}\n\\usepackage{geometry}\n\\usepackage{float}\n\\usepackage[sort&compress]{natbib}\n\\def \\infint {\\int^\\infty_0}\n\\newcommand{\\ST}[1]{\\mbox{\\tiny{#1}}}\n\\def \\D {\\mathrm{\\,d}}\n\\def \\curl {\\nabla\\times}\n\\def \\P {\\partial}\n\\def \\I {\\mathrm{i}}\n\\def \\E {\\mathrm{e}}\n\\def \\G {\\R{G}}\n\\def \\SI {\\R{sign}\\,}\n\\frenchspacing\n\\begin{document}\n\n$0\n\n\\end{document}" "Quick article" nil
                        ("skeleton")
                        nil nil nil nil)
                       ("sq" "\\sqrt{$1} $0" "Square root" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Fri Apr  5 17:43:54 2013
