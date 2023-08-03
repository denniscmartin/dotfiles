function FormatTextCommand()
	vim.cmd("set tw=100")
	vim.cmd("set wrap")
	vim.cmd("set linebreak")
	vim.cmd("set nolist")
	vim.cmd("%normal gqG")
end

-- Register the custom commands
vim.cmd("command! FormatText lua FormatTextCommand()")

