
" Install vim-plug if doesn't exist
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Packages
call plug#begin()

" Theme
Plug 'joshdick/onedark.vim'

" Lightline
Plug 'itchyny/lightline.vim'

" Telescope
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.0' }

" Which key
Plug 'folke/which-key.nvim'

call plug#end()

" Telescope settings
nnoremap <leader>fc <cmd>Telescope commands<cr>
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fm <cmd>Telescope marks<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" Global settings
set termguicolors

syntax on
colorscheme onedark

let mapleader="\\"
let g:lightline = { 'colorscheme': 'one' }

filetype indent plugin on

" Position the cursor, visually select and scroll with the mouse.
if has('mouse')
  if &term =~ 'xterm'
    set mouse=a
  else
    set mouse=nvi
  endif
endif

set autoindent
set expandtab
set backspace=indent,eol,start
set complete-=i
set display=truncate
set guicursor=
set hidden " allow switching buffers without writing to disk
set laststatus=2
set noshowmode
set nrformats-=octal
set number relativenumber
set ruler
set smarttab
set ttimeout
set ttimeoutlen=100
set timeoutlen=500
set wildmenu

" Set to auto read when a file is changed from the outside
set autoread
au FocusGained,BufEnter * :silent! !

" When editing a file, always jump to the last cursor position
if has("autocmd")
  autocmd BufReadPost *
  \ if line("'\"") > 0 && line ("'\"") <= line("$") |
  \   exe "normal g'\"" |
  \ endif
endif

" Searching
set ignorecase
set incsearch
set nowrapscan
set smartcase

nnoremap <leader>hl <cmd>set hlsearch!<cr>

" Backup and swap
if has('persistent_undo')
  set undofile
  
  if !isdirectory(&undodir)
      silent! execute "!mkdir -p " . &undodir
  endif

  set undodir=/tmp/nvimtmp/undodir
endif 

set directory=/tmp/nvimtmp/swap
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

lua << EOF
  require('which-key').setup { }
EOF
