# Setup fzf
# ---------
if [[ ! "$PATH" =~ "/home/rosstimson/.fzf/bin" ]]; then
  export PATH="$PATH:/home/rosstimson/.fzf/bin"
fi

# Man path
# --------
if [[ ! "$MANPATH" =~ "/home/rosstimson/.fzf/man" && -d "/home/rosstimson/.fzf/man" ]]; then
  export MANPATH="$MANPATH:/home/rosstimson/.fzf/man"
fi

# Auto-completion
# ---------------
[[ $- =~ i ]] && source "$HOME/.fzf/shell/completion.zsh"

# Key bindings
# ------------
source "$HOME/.fzf/shell/key-bindings.zsh"
