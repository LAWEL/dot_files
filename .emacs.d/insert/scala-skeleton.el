(defvar auto-insert-alist)
(setq auto-insert-alist
      (append '(
                (("\\.scala$" . "scala template")
                 nil
                 "import java.util.Scanner" \n
                 \n
                 "object Main {" \n
                 "def main(args: Array[String]) {" \n
                 "val sc = new Scanner(System.in)" \n
                 - "\n"
                 "  }\n"
                 "}\n"
                 )) auto-insert-alist))

(provide 'scala-skeleton)
