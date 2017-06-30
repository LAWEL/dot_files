(defvar auto-insert-alist)

(setq auto-insert-alist
      (append '(
                (("\\.tex$" . "latex template")
                 nil
                 "%% -*- coding: utf-8-unix -*-" \n
                 "%#!ptex2pdf -u -l -ot \"-kanji=utf8 -no-guess-input-enc -synctex=1\" hogehoge" \n
                 "% #!tasklist /fi \"IMAGENAME eq AcroRd32.exe\" /nh | findstr \"AcroRd32.exe\" > nul && pdfopen --rxi --file hogehoge.pdf && pdfclose --rxi --file hogehoge.pdf & ptex2pdf -u -l hogehoge && pdfopen --rxi --file hogehoge.pdf" \n
                 "%#BIBTEX upbibtex hogehoge" \n
                 "%#MAKEINDEX mendex -U hogehoge" \n
                 "% #LPR acroread.bat /p hogehoge.pdf" \n
                 \n
                 _ \n
                 \n
                 "%%% Local Variables:" \n
                 "%%% TeX-master: \"hogehoge\"" \n
                 "%%% End:" \n
                 )) auto-insert-alist))

(provide 'yatex-skeleton)
