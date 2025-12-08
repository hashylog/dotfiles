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
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      require('lualine').setup({
          options = {
            globalstatus = true
          }
        }
      )
    end
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

  {
    "dstein64/nvim-scrollview",
    branch = "main"
  },

  {
    "folke/noice.nvim",
    event = "VeryLazy",
    opts = {
      -- add any options here
    },
    dependencies = {
      -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
      "MunifTanjim/nui.nvim",
      -- OPTIONAL:
      --   `nvim-notify` is only needed, if you want to use the notification view.
      --   If not available, we use `mini` as the fallback
      "rcarriga/nvim-notify",
      }
  },

  {
    'akinsho/bufferline.nvim',
    version = "*",
    dependencies = 'nvim-tree/nvim-web-devicons',
    config = function()
      require('bufferline').setup {
        options = {
          mode = "buffers",
          offsets = {
            {
              filetype = "neo-tree",
              highlight = "Directory",
              text = "File Explorer",
            }
          }
        }
      }
    end
  },

  {
    "declancm/cinnamon.nvim",
    version = "*", -- use latest release
    opts = {
      -- change default options here
    }
  },

  {
    "folke/tokyonight.nvim",
    lazy = true,
    opts = { style = "night" },
  },

  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000
  },

  {
    "ellisonleao/gruvbox.nvim",
    priority = 1000,
    config = true,
    opts = {}
  },

  {
    "rebelot/kanagawa.nvim",
    priority = 1000
  },

  {
    "olimorris/onedarkpro.nvim",
    priority = 1000
  },

}
