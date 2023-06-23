(module config.notes
  {autoload {str conjure.aniseed.string
             tb telescope.builtin
             ts_utils nvim-treesitter.ts_utils
             query vim.treesitter.query
             treesitter vim.treesitter}})

(defn starts-with? [string prefix]
  (= (string.sub string 1 (length prefix)) prefix))

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

(defn note_files []
  (-> (vim.fn.system (.. "cd " dir " && rg --files *.md | sort"))
      (str.trim)
      (str.split "\n")))

; (note_files)

(defn get_links_for_buffer [bufnr]
  (let [language_tree (vim.treesitter.get_parser bufnr "markdown_inline")
        syntax_tree (. (language_tree:parse) 1)
        root (syntax_tree:root)
        parse_query (vim.treesitter.query.parse
                      "markdown_inline"
                      "((link_text) @link_text
                        (link_destination) @link_uri)")
        result []]
    (each [_ captures (parse_query:iter_matches root bufnr)]
      (let [desc (treesitter.get_node_text (. captures 1) bufnr)
            uri (treesitter.get_node_text (. captures 2) bufnr)
            (row col _ _) (treesitter.get_node_range (. captures 1))]
        (when uri
          (table.insert result [desc uri (+ 1 row) (+ 1 col)]))))
    result))

; (get_links_for_buffer 1)

(defn process_note [note->referencing-notes note]
  (let [current-buf (vim.api.nvim_get_current_buf)]
    (vim.cmd (.. "cd " dir))
    (vim.cmd (.. "e " note))
    (when (= "markdown" vim.bo.filetype)
      (let [forward-links (get_links_for_buffer (vim.api.nvim_get_current_buf))]
        (each [_ [desc uri row col] (pairs forward-links)]
          (when (not (or (starts-with? uri "http:")
                         (starts-with? uri "https:")))
          (let [links (. note->referencing-notes uri)
                updated-links (if links
                                 (do (table.insert links [note row col desc]) links)
                                 [[note row col desc]])]
            (tset note->referencing-notes uri updated-links))))))
    (when (> (length (vim.api.nvim_list_bufs)) 1)
      (vim.cmd (.. "b" current-buf " | bd#"))))
  note->referencing-notes)

; (process_note {} "./2022.12.22.note.md")

(defn process_notes []
  (let [note->referencing-notes {}]
    (each [_ note (ipairs (note_files))]
      (process_note note->referencing-notes note))
    note->referencing-notes))

; (process_notes)

(defn generate_backlinks [note->referencing-notes]
  (let [current-buf (vim.api.nvim_get_current_buf)
        quickfix-lines []]
    (vim.cmd (.. "e " dir ".backlinks"))
    (each [k backlinks (pairs note->referencing-notes)]
      (table.insert quickfix-lines (.. "___start_" k))
      (each [_ [note row col desc] (ipairs backlinks)]
        (table.insert quickfix-lines (.. note ":" row ":" col ":" desc)))
      (table.insert quickfix-lines (.. "___end_" k)))
    (vim.api.nvim_buf_set_lines 0 0 -1 true quickfix-lines)
    (vim.cmd "w")
    (when (> (length (vim.api.nvim_list_bufs)) 1)
      (vim.cmd (.. "b" current-buf " | bd#")))))

; (generate_backlinks (process_notes)))

(defn show_backlinks [note]
  (vim.fn.setqflist [])
  (vim.cmd (.. "caddexpr \""
               (-> (.. "sed -n '/___start_" note"/,/___end_" note "/p' " dir ".backlinks | head -n -1 | tail -n +2")
                   (vim.fn.system)
                   (string.gsub "\n" "\\n"))
               "\""))
  (vim.cmd "copen"))
; (show_backlinks "2023.03.14.roundabout.md")

(defn show_buffer_backlinks []
  (show_backlinks (vim.fn.expand "%:t")))

;; generate note backlinks
(vim.keymap.set :n :<leader>ng #(generate_backlinks (process_notes)))

;; show backlinks for file
(vim.keymap.set :n :<leader>nb #(show_buffer_backlinks))
