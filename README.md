company-llama
=============

Glue between [company-mode](https://github.com/company-mode/company-mode) and [llama.cpp](https://github.com/ggerganov/llama.cpp).

Quick setup
-----------

1. Download a LLM `.llamafile`
   - For example, [this one](https://huggingface.co/jartine/WizardCoder-Python-34B-V1.0-llamafile/resolve/main/wizardcoder-python-34b-v1.0.Q5_K_M.llamafile?download=true)
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
