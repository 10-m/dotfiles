# -*- coding: utf-8-unix -*-
# .zshrc

## ---------------------------------------------------------
## 一般 or 未分類
## ---------------------------------------------------------
# ビープ音を鳴らさないようにする
setopt NO_beep

# 8 ビット目を通すようになり、日本語のファイル名を表示可能
setopt print_eight_bit

# C-s, C-qを無効にする。
setopt NO_flow_control

# 新しいファイルのパーミッションは 644
umask 022

# core ファイルを作らない
ulimit -c 0

# 日本語 less
export JLESSCHARSET=japanese-ujis

# suedit ファイル名 で管理者権限ファイル Open
export EDITOR=~/bin/my_editor.sh

# 色付きgrep
# ページャの中でもハイライトしたいときは、lessならば-Rオプション
export GREP_OPTIONS='--color=auto'

# コマンド異常終了時に終了ステータスを表示
setopt print_exit_value

# less オプション
# --tabs=4          タブを4にする
# --LONG-PROMPT     プロンプト(lessの一番下の行)に詳細を表示させる
# --ignore-case      大文字小文字を区別しないて検索。検索パターンに大文字を含む場合は無視
export LESS='--tabs=4 --LONG-PROMPT --ignore-case'

# 時刻
alias dates='date +%Y%m%d'
alias datel='date +%Y%m%d-%H%M'

# git
alias ggraph='git log --graph --decorate --pretty=oneline --all --abbrev-commit'

# 3秒以上かかった処理は詳細表示
REPORTTIME=3

# LANG設定。rootは常にLANG=C
case "$OS" in
    Windows* )
        export LANG=ja_JP.SJIS
    ;;
    *)
        export LANG=ja_JP.UTF-8
    ;;
esac

case ${UID} in
    0)
        LANG=C
    ;;
esac

# .zshrc の変更を全ターミナルへ一発で適用する
# http://d.hatena.ne.jp/kitokitoki/20121103/p2
alias source_all='pkill -usr1 zsh'

# git で自前の証明書を使えるようにする
alias git='GIT_SSL_NO_VERIFY=1 git'

## ---------------------------------------------------------
## キーバインド、エディット関係
## ---------------------------------------------------------
# キーバンドを Emacs 風にする
bindkey -e

# コマンドが省略されていたら cd とみなす
setopt auto_cd

# Ctrl-D でログアウトするのを抑制する
setopt ignore_eof

# カーソル位置は保持したままファイル名一覧を順次その場で表示
setopt always_last_prompt

# rm * を実行する前に確認
setopt rm_star_wait

# (グローバル)エイリアス
# dircolors -p > .dir_colors + 色変更しておくこと
if [ -r $HOME/.dir_colors ]
then
  eval "`dircolors -b $HOME/.dir_colors`"
fi
alias ls='ls --time-style=long-iso --color=auto -F'

#「dmesg lG CPU」と入力することで「dmesg | grep CPU」と同じ意味になる。
alias -g lG='| grep '
alias -g lL='| less '
alias ..='cd ..'
alias pp=' ps aux '
alias p='ps aux | head -1; ps aux | grep -i '
alias free='free -m'
alias df='df -h'
alias h='history -E 1 | grep -i '
alias rm='my_rm.sh'
alias mv='mv -i'
alias cp='cp -i'
alias ll='ls -lhFa'
alias l='ls -ahl | grep -i '
alias up='cd ..'
alias su='su -'
alias SU='sudo -H -s'
alias rdom='echo `od -vAn -N4 -tu4 < /dev/urandom`'

case "$LANG" in
    *SJIS | *sjis | *PCK | *pck)
        alias c='nkf -s'
        ;;
    *UTF-8 | *utf-8 | *UTF8 | *utf8)
        alias c='nkf -w'
        ;;
    *EUCJP | *eucJP | *EUC | *euc | *EUC-JP | *euc-jp | *UJIS | *ujis)
        alias c='nkf -e'
        ;;
    *)
        case "$OS" in
            Windows* )
                alias c='nkf -s'
                ;;
            *)
                alias c='nkf -w'
                ;;
        esac
        ;;
esac

# sudo を履歴に残したくないとき
# alias sudo=' sudo -H'

# replace-string
autoload -U replace-string
zle -N replace-string
bindkey '^[%' replace-string

# M-h で カーソル 前の単語を削除
bindkey "^[h" backward-kill-word

# 単語区切り文字を指定
autoload -Uz select-word-style
select-word-style default
zstyle ':zle:*' word-chars " /;@:{},|"
zstyle ':zle:*' word-style unspecified

## ---------------------------------------------------------
## 履歴、検索
## ---------------------------------------------------------
export HISTFILE=$HOME/.zsh_history

# メモリ内の履歴の数
export HISTSIZE=50000

# 保存される履歴の数
export SAVEHIST=50000

# 履歴ファイルに時刻を記録
setopt extended_history

# 同一ホスト上での履歴の共有
setopt share_history

# history ファイルに上書きせず追加
setopt append_history

# 直前と同じコマンドはヒストリに追加しない
setopt hist_ignore_dups

# 先頭がスペースならヒストリに追加しない
setopt hist_ignore_space

# インクリメンタルサーチ(正規表現もつかえる)
bindkey '^R' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward

# 検索
function search() {
    encoded=$(perl -MURI::Escape -e 'print uri_escape(shift)' "$1")

    echo "0 : bing.com"
    echo "1 : google.co.jp"
    echo "2 : goo dictionary(all)"
    echo "3 : eijiro"
    echo "4 : google translation"
    echo "5 : excite translation"
    echo "6 : wikipedia(jp)"
    echo "7 : wikipedia(en)"
    echo -n "select number: "
    read number

    case "$number" in
        0)
            firefox -new-tab 'http://www.bing.com/search?q='$encoded &
            ;;
        1)
            firefox -new-tab 'http://www.google.co.jp/search?q='$encoded'&hl=ja&ie=UTF_8&lr=lang_ja' &
            ;;
        2)
            firefox -new-tab 'http://dictionary.goo.ne.jp/srch/all/'$encoded'/m0e/' &
            ;;
        3)
            firefox -new-tab 'http://eow.alc.co.jp/'$encoded'/UTF-8/' &
            ;;
        4)
            firefox -new-tab 'http://translate.google.com/translate_t?hl=ja&langpair=en%7Cja&text='$encoded &
            ;;
        5)
            firefox -new-tab 'http://www.excite.co.jp/world/english/?before='$encoded'&wb_lp=ENJA' &
            ;;
        6)
            firefox -new-tab 'http://ja.wikipedia.org/wiki/'$encoded &
            ;;
        7)
            firefox -new-tab 'http://en.wikipedia.org/wiki/'$encoded &
            ;;
        *)
            echo "Unknown number $number" >&2
            ;;
    esac > /dev/null
}
## ---------------------------------------------------------
## 作業用の一時ディレクトリを作って勝手に移動
## ---------------------------------------------------------
# "http://kimoto.hatenablog.com/entry/2012/12/11/135125
function temp(){ cd `mktemp -d --tmpdir="$HOME/tmp/" \`date +'%Y%m%d'.$1${1:+.}\`XXXXXX` }

## ---------------------------------------------------------
## 履歴に入れない条件
## ---------------------------------------------------------
# http://d.hatena.ne.jp/mollifier/20090728/p1
zshaddhistory() {
    local line=${1%%$'\n'}
    local cmd=${line%% *}

    if [[ ${cmd} == (l|l[sal])
         || ${cmd} == rm
         || ${cmd} == (p|p[ps]) ]]
    then
        return 1
    fi
    return 0
}

## ---------------------------------------------------------
## 修正規則
## ---------------------------------------------------------
# ファイル名が微妙に間違っていたとき、修正しますか？と聞いてきてくれる。
# correctall は使えないので無効に
setopt CORRECT
unsetopt correctall

## ---------------------------------------------------------
## ディレクトリ移動
## ---------------------------------------------------------
# 自動で pushd
setopt AUTO_PUSHD

# 重複した pushd を削除
setopt PUSHD_IGNORE_DUPS

# 選択して popd
# "dirs -v"で、ディレクトリスタックを表示できる。
# read でキーボードから数字を読み取り
# cd コマンドでそのディレクトリに移動する。
alias gd='cd +"$(dirs -v | peco | cut -f 1)"'

function pd {
    local dir="$(\ls -a -d */ | sed 's!/$!!' | peco )"
    if [ ! -z "$dir" ] ; then
        cd "$dir"
    fi
}

## ---------------------------------------------------------
## ディレクトリ ブックマーク
## ---------------------------------------------------------
# bd dir(s) : ディレクトリの追加
# bd        : ディレクトリの選択( 移動 or 削除)
#source ~/bin/bd.sh

## ---------------------------------------------------------
## 補完
## ---------------------------------------------------------
# ファイル名の補完
# 一覧表示された補完候補を C-n C-p C-f C-b のカーソルで選択することができる。
# 補完候補を一覧表示させた時点で <tab> をもう 1 度打つとカーソルが表示される。
zstyle ':completion:*:default' menu select=1

# キャッシュ
zstyle ':completion:*' use-cache true


# コマンド特有のオプションをTABキーで補完してくれる。
# たとえば、gcc -g と入力してTABキーを押すと、
# $ gcc -g
# coff     dwarf    dwarf+   gdb      stabs    stabs+   xcoff    xcoff+
# perldoc IO::[Tabキー]]が便利
autoload -U compinit

case "$OS" in
    Windows* )
        # Ignore insecure directories and continue [ny]? を抑止
        compinit -u -d $HOME/.zcompdump
    ;;
    *)
        compinit -d $HOME/.zcompdump
    ;;
esac

# 小文字に対して大文字も補完する
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# 補完候補が複数ある時に、一覧表示
setopt auto_list

# Tabで補完候補を表示
setopt auto_menu

# カッコの対応などを自動的に補完
setopt auto_param_keys

# ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash

# コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
setopt magic_equal_subst

# Emacs dabbrev のような M-/ で履歴補完
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
bindkey "^[/" history-beginning-search-backward-end

## ---------------------------------------------------------
## emacs
## ---------------------------------------------------------
if [[ $EMACS = t || $TERM = "emacs" || $TERM = "dumb" ]]
then
    # プロンプトが正しく表示されるようにする
    unsetopt zle

    # ターミナルでlessが使えないのでcatで出力
    alias man='man -P cat'

    # LANGをUTF8にする。こうしないとなぜかパスワード非表示設定が効かない
    LANG=en_US.UTF-8
fi

## ---------------------------------------------------------
## プロンプト
## ---------------------------------------------------------
# 改行のない出力をプロンプトで上書きするのを防ぐ
unsetopt promptcr

# プロンプトに色を付ける
local GREEN=$'%{\e[0;32m%}'
local CYAN=$'%{\e[0;36m%}'
local RED=$'%{\e[0;31m%}'
local YELLOW=$'%{\e[0;33m%}'
# local DEFAULT=$'%{\e[1;m%}'
local DEFAULT=$'%{\e[m%}'

# 改行
local RETURN=$'\n'

# 失敗したとき@の色を変える
# 元々はプロンプト ($ or #) だったが、 tramp のため変更
local RESULT="%(?.$YELLOW.$RED)"

# ドットをアンダーバーにしておく
local MY_HOST=${HOST/\./_}

# バージョン表示
# http://d.hatena.ne.jp/mollifier/20090814/p1
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable svn git
zstyle ':vcs_info:git:*' use-simple true
zstyle ':vcs_info:*' formats '(%s)[%b]'

vcs_precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info 2> /dev/null
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}

# マージでコンフリクトしたとか、何か特別なことが起こった場合の表示
zstyle ':vcs_info:*' actionformats '(%s)[%b|%a]'

local VERSION="%1(v|%F{green}%1v%f|)"

# rootと一般ユーザでプロンプトをわける
local MARK="%(!.# .\$)"
local _USERNAME="%(!.$RED$USERNAME.$USERNAME)"

PROMPT=$CYAN'['$_USERNAME$RESULT'@'$CYAN$MY_HOST':'$YELLOW'%~'$CYAN']'$VERSION$DEFAULT$RETURN$MARK' '
setopt PROMPT_SUBST

# case $TERM in
#     screen|vt100|kterm|xterm)
#         ROOT_PROMPT=$CYAN"["$RED"${USERNAME}"$YELLOW"@"$CYAN"${MY_HOST}:"$YELLOW"%(3~,%-1~/-/%1~,%~)$CYAN]"$DEFAULT$'\n''# '
#         USER_PROMPT=$CYAN"[${USERNAME}"$YELLOW"@"$CYAN"${MY_HOST}:"$YELLOW"%(3~,%-1~/-/%1~,%~)$CYAN]"$DEFAULT$'\n''$ '
#         ;;
#     *) # 色が表示できないTERM用
#         ROOT_PROMPT="[${USERNAME}@${MY_HOST}:%(3~,%-1~/-/%1~,%~)]"$'\n''# '
#         USER_PROMPT="[${USERNAME}@${MY_HOST}:%(3~,%-1~/-/%1~,%~)]"$'\n''$ '
#         export GREP_OPTIONS='--color=none'
#         alias ls='ls --color=none'
#         ;;
# esac

## ---------------------------------------------------------
## precmd
## ---------------------------------------------------------
## プロンプトを表示する前に実行
precmd () {
    #screen_precmd
    vcs_precmd
}

## ---------------------------------------------------------
## preexec
## ---------------------------------------------------------
## コマンドを実行する前に実行
preexec() {
    #screen_preexec $1
}

## ---------------------------------------------------------
## function
## ---------------------------------------------------------
# F5 で YYYYMMDD を挿入
# http://nanabit.net/blog/2009/11/29/insert-date-on-single-key/
function insert_date {
    LBUFFER=$LBUFFER'$(date +%Y%m%d)'
}
zle -N insert_date
bindkey '^[Ot' insert_date
bindkey '^[[15~' insert_date

# http://dharry.hatenablog.com/entry/2012/09/17/031850
# pscan 192.168.0.0/24
function pscan {
    # nmap -nsP --host-timeout 10 "$1" | awk '{if($1=="Host") print $2 }';
    # nmap -nsP "$1" | awk '{if($1=="Host") print $2 }';
    nmap -nsP "$1"
}

function tm {
    if [ $(tmux ls | wc -l) -eq 0 ] ; then
        tmux
    elif [ $(tmux ls | wc -l) -eq 1 ] ; then
        tmux a -t $(tmux ls | cut -d: -f 1)
    else
	session=$(tmux ls | peco | cut -d: -f 1)
	if [ -n "$session" ] ; then
            tmux a -t "$session"
	fi
    fi
}

function pps {
    for `\ps aux | peco | awk '{ print $2 }'`
  do
    kill $pid
    done
}

# ^ でディレクトリ移動
# ^を入れたくなったら C-V^
# もし間違ってそのまま^してしまったら， M-Q して cd -すれば元に戻る
# function cdup () {
#     echo
#     cd ..
#     zle reset-prompt
# }
# zle -N cdup
#bindkey '\^' cdup

## ---------------------------------------------------------
## ローカル設定
## ---------------------------------------------------------
[[ -f $HOME/local/.zsh_local ]] && source $HOME/local/.zsh_local

## ---------------------------------------------------------
## パスの重複を除去
## ---------------------------------------------------------
typeset -U path cdpath fpath manpath

## ---------------------------------------------------------
## post process
## ---------------------------------------------------------
# 失敗したときプロンプトの色を変わるのでダミーコマンドで成功にしておく
echo hello > /dev/null

## ---------------------------------------------------------
## 機能メモ
## ---------------------------------------------------------
# [nohup]
# http://d.hatena.ne.jp/sasakyh/20091123#p1
#  「setopt nohup」を設定しておく
# 内部コマンド "disown" を実行する.

# [アンドゥ]
# C-/ キーでアンドゥ (編集のやり直し) が行える。たとえばコマンドラインの編集中に
# 誤って M-d (kill-word) でカーソル位置の単語を消してしまった場合、# C-/ で
# 単語を元に戻すことができる。C-/を何度も打てば、どんどん過去に遡ることができる。

# [ワイルドカード(否定)]
# ^がパターンの否定。
#    % ls ^*a
#    geho.cpp  gehogeho.txt  hoge.cpp  hoge.h  hogehoge.sh*
#
#    foo:
#    hoge.pdf  hoge.ps

# [ワイルドカード(再帰検索)]
# % ls **/*ps
# baa/01.ps  baa/04.ps  baa/07.ps  baa/10.ps  baa/13.ps  baa/16.ps  baa/19.ps
# baa/02.ps  baa/05.ps  baa/08.ps  baa/11.ps  baa/14.ps  baa/17.ps  baa/20.ps
# baa/03.ps  baa/06.ps  baa/09.ps  baa/12.ps  baa/15.ps  baa/18.ps  foo/hoge.ps

# [ワイルドカード(グルーピング)]
# ()でくくって、|で「または」を意味する。
#    % ls *.(cpp|h)
#    geho.cpp  hoge.cpp  hoge.h

# [ワイルドカード(数値の範囲指定)]
# % ls <10-15>.ps
# 10.ps  11.ps  12.ps  13.ps  14.ps  15.ps
# zshのおぼえがき

# [ワイルドカード(除外)]
#    % ls *.(cpp|h)
#    geho.cpp  hoge.cpp  hoge.h
#    % ls *.(cpp|h)~hoge*
#    geho.cpp
# zshでは~が特殊文字になっている。このため、~を含むファイルを指定するときには
# \~と書いてやらないといけない(ほんとは)。ただ、ファイルの末尾に~がある場合
# (つまり除外したいパターンがない場合)は、それが「~という文字そのもの」を
# 指しているとzshは判断

# [ワイルドカード(ファイルの種類)]
# % ls *(x)
# hogehoge.sh*
#
# baa:
# 01.ps  03.ps  05.ps  07.ps  09.ps  11.ps  13.ps  15.ps  17.ps  19.ps
# 02.ps  04.ps  06.ps  08.ps  10.ps  12.ps  14.ps  16.ps  18.ps  20.ps
#
# foo:
# hoge.pdf  hoge.ps
# % ls *(.)
# geho.cpp  gehogeho.txt  hoge.cpp  hoge.h  hogehoge.sh*
# % ls *(.x)
# hogehoge.sh*
#
# 2つ以上指定する場合は単純に連続する
#
# / ディレクトリ
# . 通常ファイル
# @ シンボリックリンク
# * ディレクトリでない錫行権のあるファイル
# x 錫行権のあるファイル
# r 読み込み権のあるファイル
# w 書き込み権のあるファイル
# U 自分が所有するファイル
# u[usrname] そのユーザのファイル
# s setuidされたファイル
# ^ それ以降の指定の否定
# - 次の指定にシンボリックリンクを含む

# [ワイルドカード(繰り返し)]
# #が0回以上の繰り返し、##が1回以上のくりかえし。
# ちなみにまえの/までしか効果を及ぼさない。実例。
#
#    % ls baa/[0-9]#*
#    baa/01.ps  baa/04.ps  baa/07.ps  baa/10.ps  baa/13.ps  baa/16.ps  baa/19.ps
#    baa/02.ps  baa/05.ps  baa/08.ps  baa/11.ps  baa/14.ps  baa/17.ps  baa/20.ps
#    baa/03.ps  baa/06.ps  baa/09.ps  baa/12.ps  baa/15.ps  baa/18.ps

# [コマンドラインスタック]
# zshにはコマンドラインスタックという便利機能があるので、ESC-qを押せばいい。
# そうするとあら不思議、それまで書いていたものが消えてしまう。
# もちろん、ただ消えるだけじゃない。そこでコマンドを打つ(lsとかやってみよう)。
# コマンドが終了すると、さっき打っていたコマンドがまた表示される

# [rehash]
# zshは毎回PATHを確認しない。
# command not found と表示されたらrehashコマンドを実行してみる

# [数字の生成]
# touch a{000..255} でOK。{from..to}とやるとfromからtoまでの数字が生 成される。
# {0..2}とやると 0 1 2 に {00..02} とやると 00 01 02 が生 成される

# [繰り返し実行]
# repeat 3 ls
# でlsを3回実行

# [拡張子変更]
# mv foo.{jpeg,jpg}
# でfoo.jpegをfoo.jpgに変更

# [自動ヘルプ]
# % ls ~/ | cut
# まで打ったところで cut の man ページがみたいと思ったときに、 Alt+h で
# cut コマンドのマニュアルが表示される。 man ページで q を押して元の状
# 態に戻ると
# % ls ~/ | cut
# から始められる

# [設定ファイル]
# 以下の5つの環境設定ファイルがある。
#     * .zshenv
#     * .zprofile
#     * .zshrc
#     * .zlogin
#     * .zlogout
# ログインするとき、例えばssh経由やコンソールからの場合、
# .zshenv-.zprofile-.zshrc-.zloginの順番に呼ばれる。
# zshを起動したとき、例えば、ktermを新たに開いたり、zshと打ったりした場合、
# .zshenv-.zshrcの順に呼ばれるようだ。
# スクリプトを実行した場合(% zsh test.zsh など)、.zshenvだけ呼ばれるようだ。

# [デフォルト Shell を変更するには]
# chsh -s [使用するシェル] [ユーザ名]
# 利用可能なシェルを見るには
# chsh --list-shells
