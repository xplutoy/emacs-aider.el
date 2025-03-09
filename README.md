# Emacs-Aider

This is just a beginner's practice project, intended for personal use only. Here are some more feature-rich and actively maintained alternative:

- [aider.el](https://github.com/tninja/aider.el)
- [Aidermacs](https://github.com/MatthewZMD/aidermacs)

But, they seem a bit complicated for me.


## Get started

```
(use-package emacs-aider
  :vc (:url "https://github.com/xplutoy/emacs-aider.el" :branch "main")
  :hook (after-init . global-emacs-aider-mode)
  :init
  (setopt emacs-aider-prefix-key "C-c p")
  (setopt emacs-aider-command-default-args '("--model" "deepseek")))
```

The functions and key bindings are simple and straightforward:

| **Key** | **Function** | **Description** |
|---|---|---|
| r | `emacs-aider-run-dwim` | toogle aider chat session |
| q | `emacs-aider-quit` | quit aider chat session and clear |
| a | `emacs-aider-add-single-file` | add file to aider chat session |
| A | `emacs-aider-add-file-by-dired-marked` | add dired marked file to aider chat session |
| d | `emacs-aider-drop-single-file` | drop file from aider chat session |
| w | `emacs-aider-add-web-contents` | order aider to fetch web content |
| s | `emacs-aider-switch-chat-mode` | order aider to switch char mode (/ask, /code, /architect) |
| o | `emacs-aider-other-operator` | send other common commands to aider |
| RET | `emacs-aider-query-dwim` | consult aider about functions and selected content |


## License

Licensed under [GPL-3.0](./LICENSE).
