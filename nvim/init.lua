
--
--     _   _         __     ___
--    | \ | | ___  __\ \   / (_)_ __ ___
--    |  \| |/ _ \/ _ \ \ / /| | '_ ` _ \
--    | |\  |  __/ (_) \ V / | | | | | | |
--    |_| \_|\___|\___/ \_/  |_|_| |_| |_|
--
--

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Plugins
vim.pack.add(
{

  -- Dependencies
  { src = 'https://github.com/nvim-lua/plenary.nvim', name = 'plenary' },
  { src = 'https://github.com/nvim-telescope/telescope.nvim', name = 'telescope' },
  { src = 'https://github.com/MunifTanjim/nui.nvim', name = 'nui' },
  { src = 'https://github.com/DaikyXendo/nvim-material-icon.git', name = 'material-icon' },

  -- Colorschemes
  { src = 'https://github.com/catppuccin/nvim', name = 'catppuccin' },

  -- Plugins
  { src = 'https://github.com/NMAC427/guess-indent.nvim', name = 'guess-indent'},
  { src = 'https://github.com/nvim-neo-tree/neo-tree.nvim', name = 'neotree' },
  { src = 'https://github.com/windwp/nvim-autopairs', name = 'autopairs' },
  { src = 'https://github.com/nvim-lualine/lualine.nvim', name = 'lualine' },
  { src = 'https://github.com/nvim-treesitter/nvim-treesitter', name = 'treesitter' },
  { src = 'https://github.com/nvim-lualine/lualine.nvim', name = 'lualine' },
  { src = 'https://github.com/akinsho/bufferline.nvim', name = 'bufferline' },

  -- Utils
  { src = 'https://github.com/famiu/bufdelete.nvim', name = 'bufdelete' },

})

-- NVide
require('nvide')

-- Nerd Fonts
vim.g.have_nerd_font = true

-- Show line numbers
vim.o.number = true

-- Enable/Disable terminal GUI colors
vim.opt.termguicolors = true

-- Enable/Disable mouse
vim.o.mouse = 'a'

-- Enable/Disable show current mode
vim.o.showmode = false

-- Enable/Disable backup files
vim.o.undofile = true
vim.o.swapfile = true
vim.opt.writebackup = true
vim.opt.backup = false

-- Enable/Disable case sensitive searches
vim.o.ignorecase = true
vim.o.smartcase = true

-- Enable/Disable vertical spacing on gutter
vim.o.signcolumn = 'auto'

-- Enable/Disable preview substitutions
vim.o.inccommand = 'split'

-- Enable/Disable confirmation dialog
vim.o.confirm = true

-- Disable NeoVim startup message
vim.opt.shortmess:append("I")

-- Enable/Disable display whitespace
vim.o.list = true
vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }

vim.cmd.colorscheme('catppuccin-mocha')

require('catppuccin').setup()

require('guess-indent').setup()

require('nvim-treesitter').setup()

require('nvim-autopairs').setup()

require('lualine').setup({
  options = {
    globalstatus = true,
  },
})

require('neo-tree').setup({
  filesystem = {
    filtered_items = {
      visible = true,
      hide_dotfiles = false,
      hide_gitignored = false
    }
  }
})

require('bufferline').setup({
 options = {
   always_show_bufferline = false,
   close_command = "Bdelete! %d",
   offsets = {
     {
       filetype = "neo-tree",
       text = "File Explorer",
       text_align = "center",
       separator = false
     }
   },
 }
})
