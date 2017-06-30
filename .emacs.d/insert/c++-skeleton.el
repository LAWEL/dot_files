(defvar auto-insert-alist)
(setq auto-insert-alist
      (append '(
                (("\\.cpp$" . "c++ template")
                 nil
                 "#include <algorithm>" \n
                 "#include <array>" \n
                 "#include <complex>" \n
                 "#include <cassert>" \n
                 "#include <cctype>" \n
                 "#include <climits>" \n
                 "#include <cmath>" \n
                 "#include <cstdio>" \n
                 "#include <cstdlib>" \n
                 "#include <cstring>" \n
                 "#include <ctime>" \n
                 "#include <functional>" \n
                 "#include <iomanip>" \n
                 "#include <iostream>" \n
                 "#include <map>" \n
                 "#include <memory>" \n
                 "#include <queue>" \n
                 "#include <set>" \n
                 "#include <string>" \n
                 "#include <tuple>" \n
                 "#include <vector>" \n
                 \n
                 "using namespace std;" \n
                 \n
                 "typedef long long ll;" \n
                 "typedef unsigned long long ull;" \n
                 \n
                 "inline bool valid(const int x, const int r) { return 0 <= x && x < r; }" \n
                 \n
                 "void initIOStream() {" \n
                 "ios::sync_with_stdio(false); // stdinなどと同期しない" \n
                 "cin.tie(0); // cinの前にflushしない" \n
                 "cout.setf(ios::fixed);" \n
                 "cout.precision(10); // 四捨五入して指定桁数表示" \n
                 "}" > \n
                 \n
                 "int main() {" \n
                 "initIOStream();" \n
                 \n
                 _ \n
                 "}" > \n
                 )) auto-insert-alist))

(provide 'c++-skeleton)
