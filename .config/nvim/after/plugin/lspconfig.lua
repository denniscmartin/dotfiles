-- This line must be at the top
local capabilities = require('cmp_nvim_lsp').default_capabilities()

-- LSP CONFIG
local lspconfig = require('lspconfig')

lspconfig.clangd.setup {
	capabilities = capabilities,
	cmd = {"clangd", "--header-insertion=never"}
}

lspconfig.cmake.setup {
	capabilities = capabilities,
}

lspconfig.pyright.setup {}

-- Global mappings
vim.keymap.set("n", "<space>dp", vim.diagnostic.goto_prev)
vim.keymap.set("n", "<space>dn", vim.diagnostic.goto_next)

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(ev)
		local opts = { buffer = ev.buf }

		vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
		vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
		vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
		vim.keymap.set("n", "<leader>f", vim.lsp.buf.format, opts)
		vim.keymap.set("n", "<leader>r", function()
    		local str = ""
    		local new_name = vim.fn.input("New variable name: ", str)
			if new_name == "" then
        		return
    		end
    		vim.lsp.buf.rename(new_name)
		end, { silent = true })
	end,
})

-- AUTOCOMPLETION CONFIG
vim.opt.completeopt={"menu", "menuone", "noselect"}
local cmp = require'cmp'

cmp.setup({
	snippet = {
		expand = function(args)
			require('luasnip').lsp_expand(args.body)
		end,
	},
	window = {
		-- completion = cmp.config.window.bordered(),
		-- documentation = cmp.config.window.bordered(),
	},
	mapping = cmp.mapping.preset.insert({
		['<C-b>'] = cmp.mapping.scroll_docs(-4),
		['<C-f>'] = cmp.mapping.scroll_docs(4),
		['<C-Space>'] = cmp.mapping.complete(),
		['<C-e>'] = cmp.mapping.abort(),
		['<CR>'] = cmp.mapping.confirm({ select = true }),
	}),
	sources = cmp.config.sources({
		{ name = 'nvim_lsp' },
		{ name = 'luasnip' },
	}, {
		{ name = 'buffer' },
	})
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
	sources = cmp.config.sources({
		{ name = 'git' },
	}, {
		{ name = 'buffer' },
	})
})

