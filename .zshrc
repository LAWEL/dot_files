export TERM=xterm-color

# 未定義変数を使わない
# setopt no_unset

#### 文字コードの設定
# 文字コードの設定
export LANG=ja_JP.UTF-8

#### ファイルの変更,削除
alias rm='rmtrash'
alias mv='mv -i'
alias cp='cp -i'
alias ls='ls -AFGh'
alias g++='g++ -g -std=c++11 -O2 -Wshadow'
alias less='less -giMqR'
# GNU環境と揃える
alias date='gdate'

# colordiff installed by brew
if [[ -x `which colordiff` ]]; then
  alias diff='colordiff'
fi

#### 補完の設定
# 補完機能を有効
autoload -Uz compinit; compinit
# 補完候補を一覧で表示(d)
setopt auto_list
# 補完キー連打で補完候補を順に表示(d)
setopt auto_menu
# 補完候補をできるだけ詰めて表示
setopt list_packed
# 補完候補にファイルの種類も表示
setopt list_types
# Shift-Tabで補完候補を逆順("\e[Z"でも動作する)
bindkey "^[[Z" reverse-menu-complete
# 補完時に大文字小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
# sudoでも補完の対象
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

#### プロンプトの設定
# VCSの情報を取得するzshの便利関数 vcs_infoを使う
autoload -Uz vcs_info
# 表示フォーマットの指定
# %b ブランチ情報
# %a アクション名(mergeなど)
zstyle ':vcs_info:*' formats '[%b]'
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
# バージョン管理されているディレクトリにいれば表示，そうでなければ非表示
RPROMPT="[%d]%1(v|%F{green}%1v%f|)"
PROMPT='[%n@%m]# '

# ヒストリの設定
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

# 履歴ファイルに時刻を記録
setopt extended_history

# cdのタイミングで自動的にpushd
setopt auto_pushd

# 複数の zsh を同時に使う時など history ファイルに上書きせず追加
setopt append_history

# ビープ音を鳴らさないようにする
setopt no_beep

# 直前と同じコマンドラインはヒストリに追加しない
setopt hist_ignore_dups

# ヒストリにhistoryコマンドを記録しない
setopt hist_no_store

# 余分なスペースを削除してヒストリに記録する
setopt hist_reduce_blanks

# ヒストリを呼び出してから実行する間に一旦編集できる状態になる
setopt hist_verify

# コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
setopt magic_equal_subst

# 8 ビット目を通すようになり、日本語のファイル名を表示可能
setopt print_eight_bit

# シェルのプロセスごとに履歴を共有
setopt share_history

# cd をしたときにlsを実行する
function chpwd() { ls }
