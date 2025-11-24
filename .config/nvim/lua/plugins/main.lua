return
{

  {
    'nvim-treesitter/nvim-treesitter',
    lazy = false,
    branch = 'main',
    build = ':TSUpdate'
  },

  {
    "hashylog/micro-motion.nvim",
    config = function()
      local micro_motion = require("micro-motion")
      micro_motion.setup()

      vim.keymap.set('n', '<C-S-Right>', function()
        vim.cmd('normal! v')
        micro_motion.word_right()
      end, { noremap = true, silent = true, desc = 'Select word right (micro)' })

      vim.keymap.set('n', '<C-S-Left>', function()
        vim.cmd('normal! v')
        micro_motion.word_left()
      end, { noremap = true, silent = true, desc = 'Select word left (micro)' })

      vim.keymap.set('v', '<C-S-Right>', micro_motion.word_right, { noremap = true, silent = true, desc = 'Extend selection right (micro)' })
      vim.keymap.set('v', '<C-S-Left>', micro_motion.word_left, { noremap = true, silent = true, desc = 'Extend selection left (micro)' })
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
    config = function()
      vim.cmd('colorscheme kanagawa-dragon')
    end
  },

  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      require('lualine').setup();
    end,
  },

  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "nvim-tree/nvim-web-devicons", -- optional, but recommended
    },
    lazy = false, -- neo-tree will lazily load itself
  },

}
