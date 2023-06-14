(module config.init
  {autoload {core aniseed.core
             nvim aniseed.nvim
             util config.util
             str aniseed.string}})

(set nvim.g.mapleader ",")
(set nvim.g.maplocalleader "-")

;sets a nvim global options
(let [options
      {;settings needed for compe autocompletion
       :completeopt "menuone,noselect"
       ;case insensitive search
       :ignorecase true
       ;smart search case
       :smartcase true
       ;shared clipboard with linux
       :clipboard "unnamedplus"}]
  (each [option value (pairs options)]
    (core.assoc nvim.o option value)))

(set nvim.o.number true)
(set nvim.o.ruler true)
(set nvim.g.ff "unix") ; removes ^M dos stuff
(set nvim.g.foldmethod "marker") ; auto fold {{{,}}}
(set nvim.g.nofoldenable true)

(vim.cmd "colorscheme gruvbox")
(set nvim.g.background "dark")

(set nvim.g.ts 2) ; Tab size
(set nvim.g.sw 2) ; Shift Width - Auth indent size
(set nvim.g.sts 2) ; Soft Tab Stops - Backspace over 2 spaces like it was one tab
(set nvim.g.expandtab true) ; Convert tabs to spaces
(set nvim.g.smarttab true)

(set grepprg "rg --vimgrep")

;; always use the system clipboard for operations
(set vim.o.clipboard "unnamedplus")

(set nvim.g.switchbuf "useopen")

(set vim.o.showmode true)

(set vim.o.statusline "%<%f%r%h%w [%L]%=:b%n %y [%04l,%04v] %p%% %m")
;                         | | | |   |     |   |   |   |   |    |
;                         | | | |   |     |   |   |   |   |    +-- modified
;                         | | | |   |     |   |   |   |   +-- percent
;                         | | | |   |     |   |   |   +-- current column
;                         | | | |   |     |   |   +-- current line
;                         | | | |   |     |   +-- current syntax
;                         | | | |   |     +-- current buffer
;                         | | | |   +-- number of lines
;                         | | | +-- preview flag in square brackets
;                         | | +-- help flag in square brackets
;                         | +-- readonly flag in square brackets
;                         +--path to file buffer

(require :config.plugin)
(require :config.mapping)
