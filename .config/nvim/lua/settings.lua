-- Show line numbers
vim.wo.number = true

vim.opt.shiftwidth = 4
vim.opt.tabstop = 4

-- Map ESC to exit insert mode in terminal
vim.api.nvim_exec([[
augroup TerminalMappings
autocmd!
autocmd TermOpen * tnoremap <buffer> <Esc> <C-\><C-n>
augroup END
]], false)
