" -- General -----------------------------------------------------------------
if &compatible
  set nocompatible
endif

" Enable syntax highlighting
if has('syntax')
  syntax on
endif

" Determine the file type name and possibly its contents.
if has('filetype')
  filetype indent plugin on
endif


set autoindent
set expandtab
set backspace=indent,eol,start
set complete-=i
set display=truncate
set hidden " allow switching buffers without writing to disk
set nrformats-=octal
set showcmd
set ruler
set smarttab
set wildmenu

" Esc kicks-off faster
set ttimeout
set ttimeoutlen=100
set timeoutlen=500

if has("autocmd")
  " When editing a file, always jump to the last cursor position
  autocmd BufReadPost *
  \ if line("'\"") > 0 && line ("'\"") <= line("$") |
  \   exe "normal g'\"" |
  \ endif
endif

" Set to auto read when a file is changed from the outside
set autoread
au FocusGained,BufEnter * :silent! !

" -- Backup and swap ---------------------------------------------------------

" Store undofile in to fixed location
if has('persistent_undo')
  set undofile
  
  if !isdirectory(&undodir)
      silent! execute "!mkdir -p " . &undodir
  endif

  set undodir=~/.nvimtmp/undodir
endif 

set directory=~/.nvimtmp/swap
if !isdirectory(&directory)
    silent! execute "!mkdir -p " . &directory
endif

if has("vms")
  set nobackup
else
  set backup
  set backupext=.bak
  
  set backupdir=~/.vimtmp/backup
  if !isdirectory(&backupdir)
      silent! execute "!mkdir -p " . &backupdir
  endif
endif

" -- Plugins -----------------------------------------------------------------
call plug#begin()
Plug 'folke/tokyonight.nvim', { 'branch': 'main' }
Plug 'tpope/vim-fugitive', { 'branch': 'master' }
call plug#end()

" -- Theming -----------------------------------------------------------------
set guicursor=

if (has("termguicolors"))
    set termguicolors
endif

colorscheme tokyonight
set background=dark


set laststatus=2
set number relativenumber " turn line numbering on at startup

" Highligh current line number
set cursorline
set cursorlineopt=number
highlight CursorLineNR cterm=bold

" -- Searching ---------------------------------------------------------------
set hlsearch

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

set ignorecase
set incsearch
set nowrapscan
set smartcase

" -- Keys --------------------------------------------------------------------


