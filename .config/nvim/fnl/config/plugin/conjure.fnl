(module config.plugin.conjure
  {autoload {nvim aniseed.nvim
             log conjure.log}})

(set nvim.g.conjure#mapping#doc_word "K")
(set nvim.g.conjure#client#clojure#nrepl#eval#auto_require false)
(set nvim.g.conjure#client#clojure#nrepl#connection#auto_repl#enabled false)

; (set nvim.g.conjure#filetype#fennel "conjure.client.fennel.stdio")
(set nvim.g.conjure#client#fennel#stdio#format "eval_base64(\"%s\")")
(set nvim.g.conjure#client#fennel#stdio#compile true)
(set nvim.g.conjure#client#fennel#stdio#command "fennel")
(set nvim.g.conjure#client#fennel#stdio#encoding "base64")
(set nvim.g.conjure#client#fennel#stdio#command "websocat --protocol bus.sp.nanomsg.org ws://192.168.178.138:5555")
(set nvim.g.conjure#client#fennel#stdio#prompt_pattern "\n")

; let g:conjure#client#fennel#aniseed#aniseed_module_prefix = "aniseed."
; let g:conjure#client#fennel#stdio#command = "love ."
; let g:conjure#client#fennel#stdio#prompt_pattern = ">>"


(set nvim.g.conjure#log#strip_ansi_escape_sequences_line_limit 0)
(set nvim.g.conjure#log#jump_to_latest#enabled false)
(set nvim.g.conjure#log#jump_to_latest#cursor_scroll_position :bottom)
; let g:conjure#log#fold#enabled=0
(set nvim.g.conjure#log#wrap 1)

; let g:conjure#client#clojure#nrepl#completion#with_context = v:true
(set nvim.g.conjure#client#clojure#nrepl#test#current_form_names ["defspec" "deftest" "defflow" "defflow-wrapped" "wrapped-defflow"])
(set nvim.g.conjure#client#clojure#nrepl#connection#auto_repl#enabled false)
; let g:conjure#debug = v:true

; let g:conjure#log#fold#marker#start="{{{"
; let g:conjure#log#fold#marker#end="}}}"
(set nvim.g.conjure#log#hud#enabled false)

(set nvim.g.conjure_log_direction "horizontal")
(set nvim.g.conjure_log_blacklist ["up" "ret" "ret-multiline" "load-file" "eval"])

(vim.keymap.set :n :<F4> ":ConjureLogToggle<CR>")
(vim.keymap.set :n :<C-S-Space> ":ConjureEvalFile<CR>")
(vim.keymap.set :n :<C-Space> ":ConjureEvalRootForm<CR>")
(vim.keymap.set :n :<Space> ":ConjureEvalCurrentForm<CR>")

(vim.keymap.set :n :<C-d> #(log.jump-to-latest))

(vim.keymap.set :n :<localleader>cs #(do
                                       (vim.cmd "w")
                                       (vim.cmd (.. "ConjureEval (nextjournal.clerk/show! \"" (vim.fn.expand "%:p") "\")"))))
