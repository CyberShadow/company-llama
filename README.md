<img src="https://dump.cy.md/15a494e504531dc9bb8fe85956440c8c/1709105888.283808512.png.jpg" width="120" height="120" align="right">company-llama
========

Glue between [company-mode](https://github.com/company-mode/company-mode) and [llama.cpp](https://github.com/ggerganov/llama.cpp).

Quick setup
-----------

1. Download a LLM `.llamafile`
   - For example, [this one](https://huggingface.co/jartine/wizardcoder-13b-python/resolve/main/wizardcoder-python-13b.llamafile?download=true)
   - More [here](https://github.com/jart/emacs-copilot?tab=readme-ov-file#llm-download-links)

2. Run the `.llamafile` on your computer

3. Load this package

4. ```elisp
   (add-to-list 'company-backends 'company-llama-backend)
   ```

Recommended configuration
-------------------------

```elisp
;; Keep offering completions after selecting one
(add-to-list 'company-begin-commands 'company-complete)
(add-to-list 'company-begin-commands 'company-complete-selection))

;; To show probabilities:
(setq company-tooltip-align-annotations t)
(setq company-tooltip-annotation-padding 3)
(setq company-llama-show-probabilities t)
```
