return {
  "olimorris/codecompanion.nvim",
  opts = {
    strategies = {
      chat = {
        adapter = {
          name = "opencode",
          model = "openai/gpt-5-codex",
        },
      },
    },
  },
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
}
