return {
  "olimorris/codecompanion.nvim",
  opts = {
    adapters = {
      acp = {
        gemini_cli = function()
          return require("codecompanion.adapters").extend("gemini_cli", {
            defaults = {
              auth_method = "gemini-api-key", -- "oauth-personal"|"gemini-api-key"|"vertex-ai"
            },
            env = {
              GEMINI_API_KEY = "cmd:op read op://personal/Gemini_API/credential --no-newline",
            },
          })
        end,
        opencode = function()
          return require("codecompanion.adapters").extend("opencode", {
            commands = {
              default = {
                "opencode",
              },
            },
          })
        end,
      },
    },
  },
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
}
