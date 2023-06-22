(module config.notes
  {autoload {str conjure.aniseed.string
             tb telescope.builtin}})

(def dir "~/notes/")

(defn open [title-query]
  (let [rg-command (.. "cd " dir " && rg  --files . | rg " title-query)
        one-match? (= "1" (str.trim (vim.fn.system (.. rg-command " | wc -l"))))]
  (if one-match?
    (vim.cmd (.. "e " (vim.fn.system rg-command)))
    (tb.find_files {:cwd dir
                    :search_file (.. "*" title-query "*")
                    :prompt_title (.. "notes | " title-query)}))))

; (open "note")
