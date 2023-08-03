return require('packer').startup(function()

	-- Packer can manage itself as a plugin
	use 'wbthomason/packer.nvim'

	-- Theme
	use 'morhetz/gruvbox'

	-- File explorer tree
	use {
		'nvim-tree/nvim-tree.lua',
		requires = {
			'nvim-tree/nvim-web-devicons',
		},
	}

	-- File explorer
	use {
		'nvim-telescope/telescope.nvim', tag = '0.1.2',
		requires = { {'nvim-lua/plenary.nvim'} }
	}

	-- Markdown preview
	use({
		"iamcco/markdown-preview.nvim",
		run = function() vim.fn["mkdp#util#install"]() end,
	})

	-- LSP
	use 'neovim/nvim-lspconfig'

	-- Autocompletion
	use "hrsh7th/nvim-cmp"
	use 'hrsh7th/cmp-nvim-lsp'
	use 'hrsh7th/cmp-buffer'
	use 'hrsh7th/cmp-path'
	use "L3MON4D3/LuaSnip"
	use "saadparwaiz1/cmp_luasnip"

	-- Required by :checkhealth
	--	use("nvim-treesitter/nvim-treesitter", {run = ":TSUpdate"})    
	use {
		'nvim-treesitter/nvim-treesitter',
		run = function()
			local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
			ts_update()
		end,
	}

	-- Package manager for LSP and more 
	use {
		"williamboman/mason.nvim"
	}
end)

