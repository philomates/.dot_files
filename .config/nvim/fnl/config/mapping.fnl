(module config.mapping)

;; alias function
(local map vim.keymap.set)

;; ; -> :
(map :n ";" ":")

; disable man lookup
(map :n :<S-k> :l)

; Movement between split windows
(map :n "<C-k>" "<C-w>k")
(map :n "<C-j>" "<C-w>j")
(map :n "<C-l>" "<C-w>l")
(map :n "<C-h>" "<C-w>h")

; Maps Alt-[h,j,k,l] to resizing a window split
(map :n "<C-LEFT>" "<C-w><")
(map :n "<C-UP>" "<C-W>+")
(map :n "<C-DOWN>" "<C-W>-")
(map :n "<C-RIGHT>" "<C-w>>")


; sets viewport scroll x3
(map :n "<C-e>" "3<C-e>")
(map :n "<C-y>" "3<C-y>")

; remap j and k so that they move one screen line even when file lines wrap
(map :n "j" "gj")
(map :n "k" "gk")

; Map jj to <Esc> during insert mode
(map :i "jj" "")


; Use the arrows to something usefull
(map :n "<right>" ":bn<cr>")
(map :n "<left>" ":bp<cr>")

; Let \ clear the search highlighting
(map [:n :v] :\ ":let @/=\"\"<cr>")

;; Fugitive
(map :n :<leader>gs #(vim.cmd {:cmd "Git" :mods {:vertical true}}))
(map :n :<leader>gw #(vim.cmd {:cmd "Gwrite"}))
(map :n :<leader>gc #(vim.cmd {:cmd "Git" :args ["commit"]}))
(map :n :<leader>gp #(vim.cmd {:cmd "Git" :args ["push"]}))
(map :n :<leader>gb #(vim.cmd {:cmd "Git" :args ["blame"]}))

;; open-browser.vim
(map [:n :v] :gx "<plug>(openbrowser-open)" {})

;; move by visual lines instead of real lines, except when a count is provided,
;; which helps when targetting a specific line with `relativenumber`.
(map [:n :v] :j #(if (not= vim.v.count 0) :j :gj) {:expr true})
(map [:n :v] :k #(if (not= vim.v.count 0) :k :gk) {:expr true})

;; Navigate between matching brackets
;; These specifically `remap` because we want to be bound to whatever % is
;; (currently vim-matchup).
(map [:n :v] :<tab> :% {:remap true})

;; edit config files
(map :n :<leader>E #(vim.cmd {:cmd "edit" :args ["$HOME/.config/nvim/config/init.lua"]}))

(map :n :<leader>w  #(vim.cmd {:cmd "write"}))
(map :n :<leader>cl #(vim.cmd {:cmd "close"}))
(map :n :<leader>ss #(vim.cmd {:cmd "split"}))
(map :n :<leader>vs #(vim.cmd {:cmd "vsplit"}))

;; tab mappings
(map :n "]r" ":tabnext<cr>")
(map :n "[r" ":tabprev<cr>")
(map :n :<leader>tn ":tabnew<cr>")

;; Use Q to repeat last macro, rather than going into ex mode
(map :n :Q "@@")

;; Swap the behavior of the ^ and 0 operators
;; ^ Usually goes to the first non-whitespace character, while 0 goes to the
;; first column in the line. ^ is more useful, but harder to hit, so swap it
;; with 0
(map :n :0 "^")
(map :n :^ "0")

;; always center the screen after any movement command
(map :n :<C-d> "<C-d>zz")
(map :n :<C-f> "<C-f>zz")
(map :n :<C-b> "<C-b>zz")
(map :n :<C-u> "<C-u>zz")

;; Redirect changes to the "black hole" register
(map :n :c "\"_c")
(map :n :C "\"_C")

;; Keep the cursor in place while joining lines
(map :n :J "mzJ`z")

;; similar to vmap but only for visual mode - NOT select mode
;; maintains the currently visual selection between invocations of '<' and '>'
(map :x :< "<gv")
(map :x :> ">gv")

;; <c-k> escape sequences.
(map :i :<c-k> :<esc>)
(map :c :<c-k> :<c-c>)
(map :t :<c-k> :<c-\><c-n>)

; (map :n "<C-l>" ":nohlsearch<cr>" {})

; close quickfix window
(map :n "<leader>r" ":ccl <CR>")

; allow deleting selection without updating the clipboard (yank buffer)
(map :n :x "\"_x")
(map :n :X "\"_X")


; open starting from path of current file
(map :n "<leader>ew" ":e <C-R>=expand(\"%:p:h\") . \"/\"  <CR>")
(map :n "<leader>es" ":sp <C-R>=expand(\"%:p:h\") . \"/\" <CR>")
(map :n "<leader>ev" ":vsp <C-R>=expand(\"%:p:h\") . \"/\" <CR>")


(map :n :<C-G> ":cnext<CR>")
(map :n :<C-T> ":cprev<CR>")
