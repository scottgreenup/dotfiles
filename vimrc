
" This must be first, because it changes other options as side effect
set nocompatible

"-------------------------------------------------------------------------------
" Leader customisations
"-------------------------------------------------------------------------------
" Quickly reload the vimrc file
nmap <silent> <leader>ev :e  $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" Toggle functionalities
nmap <silent> <leader>n  :set nonu! <bar> set nornu!<CR>
nmap <silent> <leader>p  :set nopaste!<CR>
"-------------------------------------------------------------------------------

let g:snips_author='Scott Greenup'

set autoindent          " always have autoindent on
set copyindent          " copy the previous indentation on autoindenting
set display+=lastline
set expandtab           " <TAB> will insert <softtabstop> spaces
set hidden              " can edit a new file without saving the first
set history=1000        " more history, more commands
set hlsearch            " highlight the searched terms
set incsearch           " show search matches as you type "set spell
set nowrap              " don't wrap lines
set number              " shows the number of the cursor line
set relativenumber      " shows relative distance between lines
set ruler               " bottom-right line,column number
set shiftwidth=4        " affects how much > moves
set showmatch
set smartcase           " ignore case if lowercaes, else, case-sensitive
set smartindent
set softtabstop=4       " affects <TAB> key
set title               " change terminal title
set undolevels=5000     " more levels of undos
set visualbell          " don't beep
set wildmenu            " autocomplete for command menu

set nobackup
set noswapfile

set linebreak
set fdm=indent

highlight ColumnColor ctermbg=red ctermfg=red
highlight ColorColumn ctermbg=red ctermfg=red
highlight columncolor ctermbg=red ctermfg=red
set colorcolumn=81

set wrapscan
set virtualedit=all

set foldenable
set foldlevelstart=10
set foldnestmax=3
set foldmethod=indent
noremap <space> za

syntax enable
let base16colorspace=256
set background=dark
colorscheme base16-tomorrow

au VimResized * exe "normal! \<c-w>="

filetype plugin on
filetype indent on
let g:tex_flavor='latex'

command! WQA :wqa
command! WqA :wqa
command! WQa :wqa
command! Wqa :wqa
command! WA :wa
command! Wa :wa
command! WQ :wq
command! Wq :wq
command! W :w
command! Wn :wn
command! WN :wn
command! Wp :wp
command! WP :wp
command! QA :qa
command! Qa :qa
command! Q :q

nmap <silent> <c-n> :NERDTreeToggle<CR>
map <F7> :w<CR>:!ispell -x -d british %<CR><CR>:e<CR><CR>

let NERDTreeShowBookmarks=1

" Reomve the help key.
noremap  <F1> :set invfullscreen<CR>
inoremap <F1> <ESC>:set invfullscreen<CR>a

" Remove the hash key.
inoremap # X<BS>#

" Makes indenting easier:
vnoremap < <gv
vnoremap > >gv

" Makes moving easier.
nnoremap j gj
nnoremap k gk

" Remove highlights on F12
nnoremap <silent> <F12> :noh<cr>

autocmd FileType gitcommit DiffGitCached | wincmd L | wincmd p

" Pathogen for third-party libraries
call pathogen#infect()
call pathogen#helptags()

hi Normal ctermbg=None
