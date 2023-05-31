vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  use 'nvim-lua/plenary.nvim'

	use {
		"nvim-neorg/neorg",
		ft = "norg",
		after = "nvim-treesitter", -- You may want to specify Telescope here as well
		config = function()
			require('neorg').setup {
				load = {
					["core.defaults"] = {}, -- Loads default behaviour
					["core.concealer"] = {}, -- Adds pretty icons to your documents
					["core.dirman"] = { -- Manages Neorg workspaces
						config = {
							workspaces = {
								notes = "~/Notes",
							},
						},
					},
				},
			}
		end,
		run = ":Neorg sync-parsers",
		requires = "nvim-lua/plenary.nvim",
  }

end)
