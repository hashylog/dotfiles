return
{
  {
    "nvim-treesitter/nvim-treesitter", 
    branch = 'master', 
    lazy = false, 
    build = ":TSUpdate",
    opts = {
      ensure_installed = { 'bash', 'c', 'diff', 'html', 'lua', 'luadoc', 'markdown', 'markdown_inline', 'query', 'vim', 'vimdoc' },
      auto_install = true,
    }
  },
  {
    "hashylog/micro-motion.nvim",
    config = function()
      require("micro-motion").setup()
    end
  },
  {
    'windwp/nvim-autopairs',
    event = "InsertEnter",
    config = true
    -- use opts = {} for passing setup options
    -- this is equivalent to setup({}) function
  },
  {
    'nmac427/guess-indent.nvim',
    config = function() 
      require('guess-indent').setup()
    end
  },
  {
    "rebelot/kanagawa.nvim",
  },
  {
    "catppuccin/nvim", 
    name = "catppuccin", 
    priority = 1000 
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' }
  }
}
