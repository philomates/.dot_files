(module config.plugin
  {autoload {nvim aniseed.nvim
             a aniseed.core
             util config.util
             packer packer}})

(defn- safe-require-plugin-config [name]
  (let [(ok? val-or-err) (pcall require (.. :config.plugin. name))]
    (when (not ok?)
      (print (.. "config error: " val-or-err)))))

(defn- use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup
      (fn [use]
        (for [i 1 (a.count pkgs) 2]
          (let [name (. pkgs i)
                opts (. pkgs (+ i 1))]
            (-?> (. opts :mod) (safe-require-plugin-config))
            (use (a.assoc opts 1 name)))))))
  nil)

;;; plugins managed by packer
;;; :mod specifies namespace under plugin directory

(use
  ;; plugin Manager
  :wbthomason/packer.nvim {}
  ;; nvim config and plugins in Fennel
  :Olical/aniseed {:branch :develop}

  ;; theme
  :morhetz/gruvbox {}
  ; :kyazdani42/nvim-web-devicons {}

  ;; file searching
  :nvim-telescope/telescope.nvim {:requires [:nvim-telescope/telescope-ui-select.nvim
                                             :nvim-lua/popup.nvim
                                             :nvim-lua/plenary.nvim]
                                  :mod :telescope
                                  :config #(vim.api.nvim_create_autocmd
                                             [:WinLeave]
                                             {:callback #(when (and (= vim.bo.ft "TelescopePrompt")
                                                                  (= (vim.fn.mode) "i"))
                                                           (vim.api.nvim_feedkeys (vim.api.nvim_replace_termcodes "<Esc>" true false true) "i" false))})}

  ;; repl tools
  :Olical/conjure {:branch :master :mod :conjure}

  ;; sexp
  :guns/vim-sexp {:mod :sexp}
  :tpope/vim-sexp-mappings-for-regular-people {}
  :tpope/vim-repeat {}
  :tpope/vim-surround {:config #(do (vim.keymap.set :n :s :ysi))}

  ;; parsing system
  :nvim-treesitter/nvim-treesitter {:run ":TSUpdate"
                                    :mod :treesitter}

  ;; lsp
  :neovim/nvim-lspconfig {:mod :lspconfig}

  ;; snippets
  :L3MON4D3/LuaSnip {:requires [:saadparwaiz1/cmp_luasnip]}

  ;; autocomplete
  :hrsh7th/nvim-cmp {:requires [:hrsh7th/cmp-buffer
                                :hrsh7th/cmp-nvim-lsp
                                :hrsh7th/cmp-vsnip
                                :PaterJason/cmp-conjure]
                     :mod :cmp}

  :tpope/vim-fugitive {:requires [:tyru/open-browser.vim]}
  :tpope/vim-commentary {}
  ;; cpp
  :jackguo380/vim-lsp-cxx-highlight {}
  :prabirshrestha/vim-lsp {}

  ;; clojure
  :m00qek/baleia.nvim {:config #(let [baleia (require :baleia)
                                      ba (baleia.setup {:line_starts_at 3})]
                                  (vim.api.nvim_create_autocmd [:BufWinEnter]
                                                               {:pattern "conjure-log-*"
                                                                :callback #(ba.automatically (vim.api.nvim_get_current_buf))}))}
  :junegunn/vim-easy-align {}

  :jghauser/follow-md-links.nvim {:config (fn []
                                            (let [fl (require :follow-md-links)]
                                              (vim.api.nvim_create_autocmd [:FileType]
                                                                           {:pattern :markdown
                                                                            :callback #(vim.keymap.set :n "gd" (fn [] (fl.follow_link)))})))}
  :bakpakin/janet.vim {}
  :jlanzarotta/bufexplorer {}
  :scrooloose/nerdtree {:config #(do
                                   (vim.keymap.set :n "\\f" ":NERDTreeFind<CR>")
                                   (vim.keymap.set :n :<F6> ":NERDTreeToggle<CR>"))}
  :airblade/vim-gitgutter {:branch :main}
  :echasnovski/mini.trailspace {:config #(let [mini-trailspace (require :mini.trailspace)]
                                           (mini-trailspace.setup)
                                           (vim.keymap.set :n :_$ mini-trailspace.trim))}
  )
