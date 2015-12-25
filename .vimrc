" 行番号表示
set number

" 色を付ける
:syntax on

" タブ幅
set shiftwidth=4
set tabstop=4

if has('vim_starting')
   " 初回起動時のみruntimepathにneobundleのパスを指定する
	set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" NeoBundleを初期化
call neobundle#begin(expand('~/.vim/bundle/'))

" インストールするプラグインをここに記述
NeoBundle 'derekwyatt/vim-scala'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimfiler'

call neobundle#end()

" ファイルタイプ別のプラグイン/インデントを有効にする
filetype plugin indent on

" /tmp でバックアップファイルを作成しない(crontab と相性が悪いらしい)
set backupskip="/tmp/*,/private/tmp/*" 
