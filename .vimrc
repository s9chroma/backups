" External software needed: (only for plugins)
" * Ripgrep (https://github.com/BurntSushi/ripgrep)
" * fzf (https://github.com/junegunn/fzf)
" * bat (https://github.com/sharkdp/bat)
" * bash (need bash in path. ex. git bash)
"
""""""""""""""""""""""
""" Platform Setup
""""""""""""""""""""""
""" fzf to use bash
if has("win32")
    let $PATH = "C:/Program Files/Git/usr/bin;" . $PATH
endif

""""""""""""""""""""""
""" Configuration
""""""""""""""""""""""
set nocompatible

" Temp files
set nobackup
set noswapfile
set nowritebackup

if has('persistent_undo')
    let g:undo_directory = expand("~/.vim/undo-dir")
    if !isdirectory(undo_directory)
        call mkdir(undo_directory, "p")
    endif
    set undodir=~/.vim/undo-dir
    set undofile
end

" Behaviour
if has("syntax")
    syntax enable
end
set encoding=utf8
set incsearch "incremental searching
set nohlsearch "no highlighting last searches
set ignorecase "searching is not case sensitive
set smartcase "if a pattern contains uppercase, searching is case sensitive
" set autochdir "Automatically change the current directory
set clipboard^=unnamed,unnamedplus "use system clipboard
set noeb vb t_vb= "remove error bells
set t_vb= "remove visual bells
set backspace=indent,eol,start " more powerful backspacing
set shortmess=aoOtIF " avoid most of the 'Hit Enter ...' messages
set laststatus=2 "always show status line

filetype indent on

" Interface
set background=dark
set termguicolors
set fillchars+=vert:│
set number

" Tabbing
set tabstop=4
set shiftwidth=4
set softtabstop=0
set autoindent
set expandtab
set smarttab

""""""""""""""""""""""
""" Keybindings 
""""""""""""""""""""""
let mapleader=" "

" Rebind escape
imap jk <ESC>

" Delete to Esc from (almost) all the things
nmap <Del> <Esc>
vmap <Del> <Esc>gV
omap <Del> <Esc>
cmap <Del> <C-C><Esc>
imap <Del> <Esc>`^
tmap <Del> <C-\><C-n>

" Create terminal
if has('terminal')
    nmap <C-t> :tab terminal<CR>
    nmap <M-t> :vert terminal<CR>
    tmap <C-p> <C-W>"+
end

" Splits
set splitbelow
set splitright
set fillchars+=vert:\ 
map <Leader>v :vsp<CR>
map <Leader>h :sp<CR>
nmap <C-J> <C-W><C-J>
nmap <C-K> <C-W><C-K>
nmap <C-L> <C-W><C-L>
nmap <C-H> <C-W><C-H>
tmap <C-J> <C-W><C-J>
tmap <C-K> <C-W><C-K>
tmap <C-L> <C-W><C-L>
tmap <C-H> <C-W><C-H>
tmap <C-N> <C-W>N
nmap <C-down> <C-w><
nmap <C-up> <C-w>>
nmap <M-,> <C-w>R
nmap <Leader>0 <C-W>\|
nmap <Leader>- <C-W>_
nmap <Leader>= <C-W>=
nmap <Leader>T <C-W>T
nmap <Leader>cd :cd%:p:h<CR>

" Tabs
" Go to tab by number
nmap <leader>1 1gt
nmap <leader>2 2gt
nmap <leader>3 3gt
nmap <leader>4 4gt
nmap <leader>5 5gt
nmap <leader>6 6gt
nmap <leader>7 7gt
nmap <leader>8 8gt
nmap <leader>9 9gt
nmap <C-right> :tabnext<CR><CR>
nmap <C-left> :tabprevious<CR><CR>
nmap <leader>m :tab 
nmap <leader>n :tabnew<CR>

" General bindings
nmap <Leader>rc :e $MYVIMRC<CR>
nmap <Leader>Rc :tabe $MYVIMRC<CR>
nmap <Leader>rv :source $MYVIMRC<CR>
nmap <Leader>w :up<CR>
nmap <Leader>q :q<CR>
nmap <silent> <Leader>db :BD<CR>
nmap <Leader>c zz
nmap gF <C-W>gf
if has("win32")
    nmap <Leader>e :!start explorer /select,%:p<CR><CR>
endif
if has("unix")
    nmap <Leader>e :!nautilus .<CR><CR>
endif


" Netrw
let ghregex='\(^\|\s\s\)\zs\.\S\+'
let g:netrw_list_hide=ghregex
nmap <Leader>V :Vex!<CR>
nmap <Leader>H :Sex<CR>
nmap <Leader>y :Tex<CR>

" HTML
" Auto-run Tidy on save for HTML files and press <CR> afterwards
function! RunTidy()
    let save_cursor = getpos('.')
    let save_status = v:statusmsg

    " Redirect standard error (2) to /dev/null to suppress warnings
    silent! %!tidy -indent -wrap 0 -quiet 2>/dev/null

    call setpos('.', save_cursor)
    redraw!
    echo save_status
endfunction

autocmd BufWritePost *.html call RunTidy()
autocmd Filetype html setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2


""""""""""""""""""""""
""" Plugins
""""""""""""""""""""""
" only load plugins if Plug detected
if filereadable(expand("~/.vim/autoload/plug.vim"))
    call plug#begin()
        Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
        Plug 'junegunn/fzf.vim', " Fuzzy file search

        Plug 'tpope/vim-repeat', " Repeat plugin commands with .
        Plug 'tpope/vim-vinegar', " Improved NetRw
        Plug 'tpope/vim-fugitive', " Vim Git Plugin
        Plug 'tpope/vim-surround', " To surround with brackets/quotes etc.
        Plug 'tpope/vim-commentary' " Easy comment code

        Plug 'qpkorr/vim-bufkill' " Emacs kill-buffer
        Plug 'jiangmiao/auto-pairs' " Auto close parenthesis/brackets
        Plug 'ap/vim-css-color' " Color hex code highlighting

        " Snippets
        " Plug 'SirVer/ultisnips'
        Plug 'honza/vim-snippets'
        Plug 'natebosch/dartlang-snippets'

        " CoC
        Plug 'neoclide/coc.nvim', {'branch': 'release'}

        " Web development
        Plug 'yuezk/vim-js'
        Plug 'HerringtonDarkholme/yats.vim'
        Plug 'maxmellon/vim-jsx-pretty' 

        " Language Support
        Plug 'fatih/vim-go'
        Plug 'rust-lang/rust.vim'
        Plug 'dart-lang/dart-vim-plugin'
        Plug 'thosakwe/vim-flutter'
    call plug#end()

    """"""""""""""""""""""
    """ CoC 
    """"""""""""""""""""""
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gD :call CocAction('jumpDefinition', 'vsplit')<CR>
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)

    " Symbol renaming.
    nmap <leader>rn <Plug>(coc-rename)

    " Use `[g` and `]g` to navigate diagnostics
    " Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
    nmap <silent> [g <Plug>(coc-diagnostic-prev)
    nmap <silent> ]g <Plug>(coc-diagnostic-next)

    " Applying code actions to the selected code block
    " Example: `<leader>aap` for current paragraph
    xmap <leader>a  <Plug>(coc-codeaction-selected)
    nmap <leader>a  <Plug>(coc-codeaction-selected)

    " Remap keys for applying code actions at the cursor position
    nmap <leader>ac  <Plug>(coc-codeaction-cursor)
    " Remap keys for apply code actions affect whole buffer
    nmap <leader>as  <Plug>(coc-codeaction-source)

    " Use K to show documentation in preview window
    nnoremap <silent> K :call <SID>show_documentation()<CR>
    function! s:show_documentation()
      if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
      elseif (coc#rpc#ready())
        call CocActionAsync('doHover')
      else
        execute '!' . &keywordprg . " " . expand('<cword>')
      endif
    endfunction

    "coc config
    let g:coc_global_extensions = [
      \ 'coc-flutter',
      \ 'coc-snippets',
      \ 'coc-yaml',
      \ 'coc-html',
      \ 'coc-eslint',
      \ 'coc-tsserver',
      \ 'coc-json',
      \ 'coc-prettier',
      \ 'coc-css'
      \ ]

    """"""""""""""""""""""
    """ CoC Snippets
    """"""""""""""""""""""
    imap <tab> <Plug>(coc-snippets-expand)
    let g:UltiSnipsExpandTrigger = '<Nop>'
    let g:coc_snippet_next = '<TAB>'
    let g:coc_snippet_prev = '<S-TAB>'

    " Use <C-l> for trigger snippet expand.
    imap <C-l> <Plug>(coc-snippets-expand)

    " Use <C-j> for select text for visual placeholder of snippet.
    vmap <C-j> <Plug>(coc-snippets-select)

    " Use <C-j> for jump to next placeholder, it's default of coc.nvim
    let g:coc_snippet_next = '<c-j>'

    " Use <C-k> for jump to previous placeholder, it's default of coc.nvim
    let g:coc_snippet_prev = '<c-k>'

    " Use <C-j> for both expand and jump (make expand higher priority.)
    imap <C-j> <Plug>(coc-snippets-expand-jump)

    " Use <leader>x for convert visual selected code to snippet
    xmap <leader>x  <Plug>(coc-convert-snippet)

    inoremap <silent><expr> <TAB>
          \ coc#pum#visible() ? coc#_select_confirm() :
          \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
          \ CheckBackspace() ? "\<TAB>" :
          \ coc#refresh()

    function! CheckBackspace() abort
      let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~# '\s'
    endfunction

    let g:coc_snippet_next = '<tab>'

    """"""""""""""""""""""
    """ EasyAlign 
    """"""""""""""""""""""
    " Start interactive EasyAlign in visual mode (e.g. vipga)
    xmap ga <Plug>(EasyAlign)

    " Start interactive EasyAlign for a motion/text object (e.g. gaip)
    nmap ga <Plug>(EasyAlign)

    """"""""""""""""""""""
    """ FZF 
    """"""""""""""""""""""
    " Keybinds
    nmap <silent> <Leader>f :FZF<CR>
    nmap <silent> <Leader>g :Files %:p:h<CR>
    nmap <silent> <Leader>F :Files ~<CR>
    nmap <Leader>b :Buffers<CR>
    nmap <Leader>a :Rg<CR>

    " Config
    let g:fzf_layout = { 'down':  '50%'}

    """"""""""""""""""""""
    """ Fugitive 
    """"""""""""""""""""""
    " Keybinds
    nmap <Leader>Ga :Git add %:p<CR><CR>
    nmap <Leader>Gs :G<CR>
    nmap <Leader>Gc :Git commit -v -q<CR>
    nmap <Leader>Gt :Git commit -v -q %:p<CR>
    nmap <Leader>Gd :Git diff<CR>
    nmap <Leader>Ge :Git edit<CR>
    nmap <Leader>Gr :Git read<CR>
    nmap <Leader>Gw :Git write<CR><CR>
    nmap <Leader>Gl :silent! Git log<CR>:bot copen<CR>
    nmap <Leader>Gp :Git grep<Space>
    nmap <Leader>Gm :Git move<Space>
    nmap <Leader>Gb :Git branch<Space>
    nmap <Leader>Go :Git checkout<Space>
    nmap <Leader>Gps :Git push<CR>
    nmap <Leader>Gpl :Git pull<CR>

    """"""""""""""""""""""""""""""""""
    """ Language configs
    """"""""""""""""""""""""""""""""""
    " Golang
    autocmd FileType go map <buffer> <M-m> :w<CR>:!clear; go run %<CR>
    autocmd FileType go map <buffer> <M-n> :w<CR>:!clear; go run .<CR>
    " function! GoImports()
    "     cexpr system('goimports -e -w ' . expand('%'))
    "     edit!
    " endfunction
    " au BufWritePost *.go :call GoImports()
    let g:go_fmt_command="goimports"
    let g:go_def_mapping_enabled=0
    " au FileType go nmap gD <Plug>(go-def-vertical)
    au FileType go nmap gd :GoDef<CR>
    au FileType go nmap gD :call GoToDefSplit()<CR>

    function! GoToDefSplit()
        execute "vsp"
        execute "GoDef"
    endfunction

    " Dart
    let g:dart_format_on_save = 1
    let g:dartfmt_options = ['--fix', '--line-length 120']
    autocmd Filetype dart setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2

    " Flutter
    let g:flutter_autoscroll = 1 
    let g:flutter_use_last_run_option = 1
    autocmd FileType dart map <buffer> <Leader>pr :call FlutterSplitRun()<CR>
    autocmd FileType dart map <buffer> <Leader>pp :FlutterHotRestart<CR>

    function! FlutterSplitRun()
        execute "FlutterRun -d Linux"
        execute "FlutterTab"
        execute "tabprevious"
        execute "close"
    endfunction
endif

""""""""""""""""""""""
""" GUI Settings
""""""""""""""""""""""
if has("gui")
    set guifont=FiraMono_NF:h14:W500
    set guioptions-=m  "remove menu bar
    set guioptions-=T  "remove toolbar
    set guioptions-=r  "remove scrollbar
    set guioptions-=L  "remove left scrollbar
    set guioptions-=e  "remove gui tabs
    au GUIEnter * set vb t_vb= "remove gui error bells
    au GUIEnter * simalt ~x "start fullscreen
end

""""""""""""""""""""""
""" Tabline
""""""""""""""""""""""
set showtabline=2  " 0, 1 or 2; when to use a tab pages line
set tabline=%!MyTabLine()
function MyTabLine()
    let s = ''
    for i in range(tabpagenr('$'))
        " select the highlighting
        if i + 1 == tabpagenr()
            let s .= '%#TabLineSel#'
        else
            let s .= '%#TabLine#'
        endif

        " set the tab page number (for mouse clicks)
        let s .= '%' . (i + 1) . 'T' 

        " the label is made by MyTabLabel()
        let s .= ' %{MyTabLabel(' . (i + 1) . ')} '
    endfor

    " after the last tab fill with TabLineFill and reset tab page nr
    let s .= '%#TabLineFill#%T'
    return s
endfunction

function MyTabLabel(n)
    let buflist = tabpagebuflist(a:n)
    let winnr = tabpagewinnr(a:n)
    let label =  bufname(buflist[winnr - 1]) 
    if len(label) == 0
        let label = "[No Name]"
    endif
    return fnamemodify(label, ":t") 
endfunction


""""""""""""""""""""""""""""""""""
""" Cursor block/line config
""""""""""""""""""""""""""""""""""
" Use a line cursor within insert mode and a block cursor everywhere else.
"
" Reference chart of values:
"   Ps = 0  -> blinking block.
"   Ps = 1  -> blinking block (default).
"   Ps = 2  -> steady block.
"   Ps = 3  -> blinking underline.
"   Ps = 4  -> steady underline.
"   Ps = 5  -> blinking bar (xterm).
"   Ps = 6  -> steady bar (xterm).
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

" Linux Gnome Terminal Setup
let c='a'
while c <= 'z'
  exec "set <A-".c.">=\e".c
  exec "imap \e".c." <A-".c.">"
  let c = nr2char(1+char2nr(c))
endw

set timeout ttimeoutlen=50

""""""""""""""""""""""""""""""""""
""" s9 colorscheme(modified ayu)
""""""""""""""""""""""""""""""""""
hi clear
if exists("syntax_on")
    syntax reset
endif

" Palettes:"{{{
" ----------------------------------------------------------------------------

let s:palette = {}

let s:palette.bg        = "#000000"
let s:palette.comment   = "#5C6773"
let s:palette.markup    = "#F07178"
let s:palette.constant  = "#FFEE99"
let s:palette.operator  = "#E7C547"
let s:palette.tag       = "#36A3D9"
let s:palette.regexp    = "#95E6CB"
let s:palette.string    = "#a19959"
let s:palette.function  = "#FFB454"
let s:palette.special   = "#E6B673"
let s:palette.keyword   = "#FF7733"
let s:palette.error     = "#FF3333"
let s:palette.accent    = "#F29718"
let s:palette.panel     = "#14191F"
let s:palette.guide     = "#2D3640"
let s:palette.line      = "#151A1E"
let s:palette.selection = "#253340"
let s:palette.fg        = "#E6E1CF"
let s:palette.fg_idle   = "#3E4B59"

"}}}

" Highlighting Primitives:"{{{
" ----------------------------------------------------------------------------

function! s:build_prim(hi_elem, field)
    let l:vname = "s:" . a:hi_elem . "_" . a:field
    let l:gui_assign = "gui".a:hi_elem."=".s:palette[a:field]
    exe "let " . l:vname . " = ' " . l:gui_assign . "'"
endfunction

let s:bg_none = ' guibg=NONE ctermbg=NONE'
let s:fg_none = ' guifg=NONE ctermfg=NONE'
for [key_name, d_value] in items(s:palette)
    call s:build_prim('bg', key_name)
    call s:build_prim('fg', key_name)
endfor
" }}}

" Formatting Options:"{{{
" ----------------------------------------------------------------------------
let s:none   = "NONE"
let s:t_none = "NONE"
let s:n      = "NONE"
let s:c      = ",undercurl"
let s:r      = ",reverse"
let s:s      = ",standout"
let s:b      = ",bold"
let s:u      = ",underline"
let s:i      = ",italic"

exe "let s:fmt_none = ' gui=NONE".          " cterm=NONE".          " term=NONE"        ."'"
exe "let s:fmt_bold = ' gui=NONE".s:b.      " cterm=NONE".s:b.      " term=NONE".s:b    ."'"
exe "let s:fmt_bldi = ' gui=NONE".s:b.      " cterm=NONE".s:b.      " term=NONE".s:b    ."'"
exe "let s:fmt_undr = ' gui=NONE".s:u.      " cterm=NONE".s:u.      " term=NONE".s:u    ."'"
exe "let s:fmt_undb = ' gui=NONE".s:u.s:b.  " cterm=NONE".s:u.s:b.  " term=NONE".s:u.s:b."'"
exe "let s:fmt_undi = ' gui=NONE".s:u.      " cterm=NONE".s:u.      " term=NONE".s:u    ."'"
exe "let s:fmt_curl = ' gui=NONE".s:c.      " cterm=NONE".s:c.      " term=NONE".s:c    ."'"
exe "let s:fmt_ital = ' gui=NONE".s:i.      " cterm=NONE".s:i.      " term=NONE".s:i    ."'"
exe "let s:fmt_stnd = ' gui=NONE".s:s.      " cterm=NONE".s:s.      " term=NONE".s:s    ."'"
exe "let s:fmt_revr = ' gui=NONE".s:r.      " cterm=NONE".s:r.      " term=NONE".s:r    ."'"
exe "let s:fmt_revb = ' gui=NONE".s:r.s:b.  " cterm=NONE".s:r.s:b.  " term=NONE".s:r.s:b."'"
"}}}

" Vim Highlighting: (see :help highlight-groups)"{{{
" ----------------------------------------------------------------------------
exe "hi! Normal"               .s:fg_fg          .s:bg_bg          .s:fmt_none
exe "hi! ColorColumn"          .s:fg_none        .s:bg_line        .s:fmt_none
exe "hi! CursorColumn"         .s:fg_none        .s:bg_line        .s:fmt_none
exe "hi! CursorLine"           .s:fg_none        .s:bg_line        .s:fmt_none
exe "hi! CursorLineNr"         .s:fg_accent      .s:bg_line        .s:fmt_none
exe "hi! LineNr"               .s:fg_guide       .s:bg_none        .s:fmt_none
exe "hi! Directory"            .s:fg_fg_idle     .s:bg_none        .s:fmt_none
exe "hi! DiffAdd"              .s:fg_string      .s:bg_panel       .s:fmt_none
exe "hi! DiffChange"           .s:fg_tag         .s:bg_panel       .s:fmt_none
exe "hi! DiffText"             .s:fg_fg          .s:bg_panel       .s:fmt_none
exe "hi! ErrorMsg"             .s:fg_fg          .s:bg_error       .s:fmt_stnd
exe "hi! VertSplit"            .s:fg_bg          .s:bg_none        .s:fmt_none
exe "hi! Folded"               .s:fg_fg_idle     .s:bg_panel       .s:fmt_none
exe "hi! FoldColumn"           .s:fg_none        .s:bg_panel       .s:fmt_none
exe "hi! SignColumn"           .s:fg_none        .s:bg_panel       .s:fmt_none
exe "hi! MatchParen"           .s:fg_fg          .s:bg_bg          .s:fmt_undr
exe "hi! ModeMsg"              .s:fg_string      .s:bg_none        .s:fmt_none
exe "hi! MoreMsg"              .s:fg_string      .s:bg_none        .s:fmt_none
exe "hi! NonText"              .s:fg_guide       .s:bg_none        .s:fmt_none
exe "hi! Pmenu"                .s:fg_fg          .s:bg_selection   .s:fmt_none
exe "hi! PmenuSel"             .s:fg_fg          .s:bg_selection   .s:fmt_revr
exe "hi! Question"             .s:fg_string      .s:bg_none        .s:fmt_none
exe "hi! Search"               .s:fg_bg          .s:bg_constant    .s:fmt_none
exe "hi! SpecialKey"           .s:fg_selection   .s:bg_none        .s:fmt_none
exe "hi! SpellCap"             .s:fg_tag         .s:bg_none        .s:fmt_undr
exe "hi! SpellLocal"           .s:fg_keyword     .s:bg_none        .s:fmt_undr
exe "hi! SpellBad"             .s:fg_error       .s:bg_none        .s:fmt_undr
exe "hi! SpellRare"            .s:fg_regexp      .s:bg_none        .s:fmt_undr
exe "hi! StatusLine"           .s:fg_fg          .s:bg_panel       .s:fmt_none
exe "hi! StatusLineNC"         .s:fg_fg_idle     .s:bg_panel       .s:fmt_none
exe "hi! StatusLineTerm"       .s:fg_bg          .s:bg_function    .s:fmt_none
exe "hi! StatusLineTermNC"     .s:fg_fg_idle     .s:bg_panel       .s:fmt_none
exe "hi! WildMenu"             .s:fg_bg          .s:bg_markup      .s:fmt_none
exe "hi! TabLine"              .s:fg_fg          .s:bg_none        .s:fmt_none
exe "hi! TabLineFill"          .s:fg_bg          .s:bg_none        .s:fmt_none
exe "hi! TabLineSel"           .s:fg_bg          .s:bg_function    .s:fmt_none
exe "hi! Title"                .s:fg_keyword     .s:bg_none        .s:fmt_none
exe "hi! Visual"               .s:fg_none        .s:bg_selection   .s:fmt_none
exe "hi! WarningMsg"           .s:fg_error       .s:bg_none        .s:fmt_none

hi LongLineWarning  guifg=NONE        guibg=#371F1C     gui=underline ctermfg=NONE        ctermbg=NONE        cterm=underline

" Generic Syntax Highlighting: {{{
" ----------------------------------------------------------------------------
exe "hi! Comment"         .s:fg_comment   .s:bg_none        .s:fmt_none
exe "hi! Constant"        .s:fg_constant  .s:bg_none        .s:fmt_none
exe "hi! String"          .s:fg_string    .s:bg_none        .s:fmt_none
exe "hi! Identifier"      .s:fg_tag       .s:bg_none        .s:fmt_none
exe "hi! Function"        .s:fg_function  .s:bg_none        .s:fmt_none
exe "hi! Statement"       .s:fg_keyword   .s:bg_none        .s:fmt_none
exe "hi! Operator"        .s:fg_operator  .s:bg_none        .s:fmt_none
exe "hi! PreProc"         .s:fg_special   .s:bg_none        .s:fmt_none
exe "hi! Type"            .s:fg_tag       .s:bg_none        .s:fmt_none
exe "hi! Structure"       .s:fg_special   .s:bg_none        .s:fmt_none
exe "hi! Special"         .s:fg_special   .s:bg_none        .s:fmt_none
exe "hi! Underlined"      .s:fg_tag       .s:bg_none        .s:fmt_undr
exe "hi! Ignore"          .s:fg_none      .s:bg_none        .s:fmt_none
exe "hi! Error"           .s:fg_fg        .s:bg_error       .s:fmt_none
exe "hi! Todo"            .s:fg_markup    .s:bg_none        .s:fmt_none

" Quickfix window highlighting
exe "hi! qfLineNr"        .s:fg_keyword   .s:bg_none        .s:fmt_none
exe "hi! Conceal"         .s:fg_guide     .s:bg_none        .s:fmt_none
exe "hi! CursorLineConceal" .s:fg_guide   .s:bg_line        .s:fmt_none

" Terminal
" ---------
let g:terminal_ansi_colors =  [s:palette.bg,s:palette.markup]
let g:terminal_ansi_colors += [s:palette.string, s:palette.accent]
let g:terminal_ansi_colors += [s:palette.tag, s:palette.constant]
let g:terminal_ansi_colors += [s:palette.regexp, "#FFFFFF"]
let g:terminal_ansi_colors += [s:palette.fg_idle, s:palette.error]
let g:terminal_ansi_colors += [s:palette.string, s:palette.accent]
let g:terminal_ansi_colors += [s:palette.tag, s:palette.constant]
let g:terminal_ansi_colors += [s:palette.regexp, s:palette.comment]

" Removes background in case of using transparency
" hi Normal guibg=NONE ctermbg=NONE
