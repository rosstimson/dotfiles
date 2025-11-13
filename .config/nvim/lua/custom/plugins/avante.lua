return {
  'yetone/avante.nvim',
  event = 'VeryLazy',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'MunifTanjim/nui.nvim',
    'nvim-tree/nvim-web-devicons',
    'nvim-telescope/telescope.nvim',
  },
  opts = {
    instructions_file = "AGENTS.md",
    provider = 'opencode',
    acp_providers = {
      ['opencode'] = {
        command = 'opencode',
        args = { 'acp' },
      },
    },
  },
}


