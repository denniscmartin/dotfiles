require("telescope").setup {
	extensions = {
		file_browser = {
			hidden = true, -- Show hidden files
			follow = true, -- Automatically follow directories
		}
	}
}

-- Mapping
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<space>ff', builtin.find_files, {})
vim.keymap.set("n", "<space>dl", builtin.diagnostics, {})

-- Using an abbreviation
vim.cmd([[cnoreabbrev ff Telescope find_files]])


