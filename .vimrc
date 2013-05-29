set uc=0
set nocompatible
set autoindent
set nowrap
set number
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set softtabstop=4
set mouse=a
syntax on
colorscheme solarized
"hi LineNr ctermfg=black ctermbg=grey

au BufRead,BufNewFile *.hx set filetype=haxe
au Syntax haxe source ~/.vim/haxe/haxe.vim

map <F3> :w<CR>
imap <F3> <ESC>:w<CR>li
map <F5> :1wincmd<space>w<CR>:w<CR>:RUN<CR>
imap <F5> <ESC>:1wincmd<space>w<CR>:w<CR>:RUN<CR>li
map <C-y> "+y
map <C-p> "+p
map , @:
map <C-w><C-w> :s/\v(.{70}[^ ]* *)/\1\r/g<CR>
map <C-w><C-h> :s/\v(.{70}[^ ]* *)/\1\r--/g<CR>

cmap now r! date "+\%Y-\%m-\%d \%a \%H:\%M"<CR>

set hidden

let g:RUNwin = 1
let g:RUNsize = 5
command -nargs=* RUN call RUN(<f-args>)
function RUN(...)
    1wincmd w
    let interpreter = strpart(getline(1),2) 
    let abspath = expand("%:p")
    let arguments = join(a:000, " ")
    let call = interpreter . " " . abspath . " " . arguments
    let perlexp = "print qq(\n) . q(~)x64 . qq(\n) . <>;"
    if winnr("$") == 1
        below new
    endif
    2wincmd w
    execute "resize " . g:RUNsize
    %s/\_.*/-/g
    execute "perldo $_=`(".call." | perl -0777 -e '".perlexp."')2>&1`;"
    perldo s/&/&a!/g; s/!/&e!/g; s/\n/&nl!/g;
    %s/&nl!/\r/ge | %s/&e!/!/ge | %s/&a!/&/ge
    %s/\(\_.*\)\n\(\~\{64}\n\)\(\_.*\)\n/\3\2\1/
    execute g:RUNwin . "wincmd w"
endfunction
command -nargs=* RUNP call RUNP(<f-args>)
function RUNP(win, size, ...)
    let g:RUNwin = a:win
    let g:RUNsize = a:size
    call RUN(a:000)
endfunction

