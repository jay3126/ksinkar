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
:nnoremap <Leader>h :set cursorline! cursorcolumn!<CR>

"show line number relative to the current cursor line
set relativenumber

"enable builtin indenting scheme for various filetypes
filetype indent on
