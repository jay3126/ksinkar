"----[Gvim]----------
"set the font
set guifont=Inconsolata\ Medium\ 11

"control guioptions
set guioptions=aivc

"set linespace
set linespace=1

"set antialising on
set antialias

"set color scheme
"colorscheme torte

"----[General]----------
"make Vim non compatible with Vi
set nocompatible

"use , over \ as leader
let mapleader = ","

"enable mouse use in all modes
set mouse=a

"recognise mouse for xterm type terminals
set ttymouse=xterm2

"do not display :intro at Vim start
set shortmess+=I

"quickly edit/reload vimrc
nmap <silent> <leader>ev :e $HOME/.vimrc<CR>
nmap <silent> <leader>sv :so $HOME/.vimrc<CR>


"----[Display]----------
"break lines longer than screen width
set wrap

"break lines at characters in 'breakat'
set linebreak


"do not update the display when running macros
"set lazyredraw

"display as much of lastline as possible instead of a lot of @
set display=lastline

"alter the look and feel of the drop-down menu
":highlight Pmenu ctermbg=238 gui=bold


"----[Visual Hints]----------
"list mode strings
set listchars=tab:>-,trail:·,eol:$

"toggle list mode on <leader>l
nmap <leader>l :set nolist!<CR>

"show (partial) command in status line
set showcmd

"show current mode down the bottom
set showmode

"show matching brackets.
set showmatch

"highlight the current line
set cursorline

"highlight current line, and column that the cursor is on
":nnoremap <Leader>h :set cursorline! cursorcolumn!<CR>


"----[Moving Around]----------

"keep three lines above and below the cursor all the times
set scrolloff=3

"show line number relative to the current cursor line
set relativenumber


"----[Searching and Substitution]----------
"do incremental and
set incsearch

"case insensitive search
set ignorecase

"unless query starts with a capital letter
set smartcase

"and highlight the search terms
set hlsearch

"assume 'g' flag for :substitute
set gdefault

"use <C-l> to clear the highlight of search hits
"nnoremap <C-l> :nohls<CR>
"inoremap <C-l> <C-O>:nohls<CR>


"----[Editing]----------
"number of spaces that a <Tab> counts for while editing (<Tab>, <BS>)
set softtabstop=2

"number of spaces that a <Tab> counts for
set tabstop=2

"expand tab into spaces
set expandtab

"number of spaces to use when (auto)indenting (=, <<, >>)
set shiftwidth=2

"round indent to a multiple of shiftwidth
set shiftround

"use shiftwidth when inserting <Tab> in front of a line
set smarttab

"smart autoindenting
set smartindent

"use only one space after a '.', '?', and '!' with a join command
set nojoinspaces

"make Y consistent with C and D
nnoremap Y y$

"toggle paste mode
set pastetoggle=<F10>

"----[Buffer]----------
"hide a buffer when abnandoned, rather than unloading it
set hidden

"don't use a swap or a backup file
set nobackup
set nowritebackup
set noswapfile

"save the file when switching buffers or compile
set autowrite

"enable undo persistence
set undofile
set undodir=$HOME/.vim/undo

"automatically read a file that has been changed externally
set autoread

"automatically save buffers when Vim looses focus
autocmd FocusLost * :wa


"----[Statusline]----------
"always display the status line
set laststatus=2

"syntax errors, or warnings if any
set statusline=%#warningmsg#
set statusline+=%{exists('g:loaded_syntastic_plugin')?SyntasticStatuslineFlag():''}
set statusline+=%*

"preview window flag
set statusline+=%w

"buffer name
set statusline+=%f\

"file type, readonly, and modified flags
set statusline+=%y%r%m

"git head
set statusline+=%{exists('g:loaded_fugitive')?fugitive#statusline():''}

"current working directory
set statusline+=\ [%{getcwd()}]

"ruler
set statusline+=%=%l,%c%V\ %LL\ --%p%%--


"----[Filetype]----------
"switch on filetype identification
filetype on

"enable builtin plugins for various filetypes
filetype plugin on

"enable builtin indenting scheme for various filetypes
filetype indent on

"switch on syntax highlighting
syntax on


"set up compilers for the respective file types
autocmd BufNewFile,BufRead *.py compiler python
autocmd BufNewFile,BufRead *.rb compiler ruby
autocmd BufNewFile,BufRead *.awk compiler awk


"----[Completion]----------
"set omnicompletion for Ruby, Eruby and Rails
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1

"set keyword completion options
set complete=.,w,b,u,t,i,d,k,s

"set what to show in the popup menu
set completefunc=menu,longest,preview

"enable enhanced command line completion
set wildmenu

"using bash style
set wildmode=longest:full,full

"ignoring the following file patterns
set wildignore=*.o,*.obj,*~


"----[Abbreviations]----------
"expand %% to pwd on the command line
cabbr <expr> %% expand('%:p:h')


"----[Session]----------
"what to save in a session
set sessionoptions=buffers,folds,tabpages,winsize

"global session - save, or restore session when vim is invoked without any
"arguments
"let g:session_file = $HOME . "/.vim/Session.vim"
"let g:save_session = 0

"function! SaveSession()
"    if g:save_session
"        execute "mksession! " . g:session_file
"    end
"endfunction

"function! RestoreSession()
"    if argc() == 0
"        let g:save_session = 1
"        if filereadable(g:session_file)
"            execute "source " . g:session_file
"        end
"    end
"endfunction

"autocmd VimLeave * call SaveSession()
"autocmd VimEnter * nested call RestoreSession()

"manually save session at times
"map <leader>ss :execute "mksession! " . g:session_file<CR>

"jump to the last position when reopening a file
"autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif


"----[Windows and Tabs]----------
"create a window to the right of the current one on a vertical split
set splitright

"create a window below the current one on a horizontal split
set splitbelow

"jump to the first open window that contains the specified buffer; this works
"only for quickfix commands, and buffer related split commands
set switchbuf=usetab

"browser like keybindings to create, and move through tabs
nnoremap <C-t> :tabnew<CR>
nnoremap <C-Tab> :tabnext<CR>
nnoremap <C-S-Tab> :tabprev<CR>

"moving between buffers
"nnoremap <C-j> :tabnext<CR>
"nnoremap <C-k> :tabprev<CR>

"use Alt-num to switch to a given tab
"nnoremap <A-1> 1gt<CR>
"nnoremap <A-2> 2gt<CR>
"nnoremap <A-3> 3gt<CR>
"nnoremap <A-4> 4gt<CR>
"nnoremap <A-5> 5gt<CR>
"nnoremap <A-6> 6gt<CR>
"nnoremap <A-7> 7gt<CR>
"nnoremap <A-8> 8gt<CR>
"nnoremap <A-9> 9gt<CR>


"----[Supertab]----------
let g:SuperTabSetDefaultCompletionType="context"
let g:SuperTabCompletionContexts=["s:ContextText","<c-r><tab>","s:ContextDiscover"]
let g:SuperTabContextDiscoverDiscovery=["&completefunc:<c-x><c-u>","&omnifunc:<c-x><c-o>"]


"----[Syntastic]----------
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=1
let g:syntastic_disabled_filetypes = ['c', 'cpp']


"----[Rooter]----------
"autocmd BufEnter * :Rooter


"----[BufferExplorer]----------
"nnoremap <Leader>be :BufExplorer<CR>


"----[NerdTree]----------
"nnoremap <Leader>p :NERDTreeToggle<CR>


"----[FuzzyFinder]----------
"enable mru modes (disabled by default)
let g:fuf_modesDisable = []

"nnoremap <Leader>fb :FufBuffer<CR>
"nnoremap <Leader>ff :FufFile<CR>
"nnoremap <Leader>fc :FufRenewCache<CR>
"nnoremap <Leader>ft :FufTag<CR>
"nnoremap <Leader>fr :FufMruFile<CR>


"----[TagList]----------
"nnoremap <Leader>tt :TlistToggle<CR>


"----[snipMate]----------
let g:snips_author = "Koustubh Sinkar"


"----[AutoClose]----------
let g:AutoClosePairs = {'(': ')', '{': '}', '[': ']', '"': '"', "'": "'", '#{': '}', '`': '`'}
let g:AutoCloseProtectedRegions = ["Character"]


"----[Gundo]----------
"nnoremap <leader>g :GundoToggle<CR>


"----[YankRing]----------
"no limits on the amount text yanked
let g:yankring_max_element_length = 0

"first 80 characters should be enough to identify the yanked text
let g:yankring_max_display = 80

"store yankring's history in ~/.vim
let g:yankring_history_dir = '$HOME/.vim'

"toggle display of yankring entries on <F3>
"nnoremap <silent> <F3> :YRShow<cr>
"inoremap <silent> <F3> <Esc>:YRShow<cr>


"----[AutoCorrect]----------
"enable automatic correction of frequent typos
"source /home/yeban/.vim/bundle/vim-autocorrect/autocorrect.vim


"----[Scratch]----------
"nnoremap <leader>s :Sscratch<CR>


"----[Fugitive]----------
"autocmd BufReadPost fugitive://* set bufhidden=delete"----[Visual Hints]----------
"list mode strings
"set listchars=tab:>-,trail:·,eol:$


"----------[Previous config]-------
"toggle list mode on <leader>l
"nmap <leader>l :set nolist!<CR>

"show (partial) command in status line
"set showcmd

"show current mode down the bottom
"set showmode

"show matching brackets.
"set showmatch

"highlight the current line
"set cursorline

"highlight current line, and column that the cursor is on
":nnoremap <Leader>h :set cursorline! cursorcolumn!<CR>

"show line number relative to the current cursor line
"set relativenumber

"enable builtin indenting scheme for various filetypes
"filetype indent on
