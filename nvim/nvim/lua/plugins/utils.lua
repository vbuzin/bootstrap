return {
	-- Snacks
	{
		"folke/snacks.nvim",
		priority = 1000,
		lazy = false,
		opts = {
			indent = {
				enabled = true,
				only_scope = true,
				only_current = true,
			},
			picker = {
				-- Common excludes for file listings (top level is sufficient for most sources)
				exclude = { ".git", "node_modules", "dist", "build", "target" },
				layout = {
					preset = "ivy",
				},
				sources = {
					select = {
						preview = "none",
						layout = {
							preset = "ivy",
							hidden = { "preview" },
						},
					},
					buffers = {
						-- Use a safe snapshot preview for buffers to avoid side-effects and
						-- exceptions from special buffers (e.g. netrw, terminal) when their
						-- buffer is set directly into the preview window (triggers ftplugins,
						-- autocommands, etc). For normal buffers we snapshot current lines
						-- (captures unsaved changes) without hijacking the user's buffer.
						preview = function(ctx)
							local item = ctx.item
							local b = item.buf
							if b and vim.api.nvim_buf_is_valid(b) and vim.api.nvim_buf_is_loaded(b) then
								ctx.preview:reset()
								local name = vim.api.nvim_buf_get_name(b)
								if name == "" then
									name = "[Scratch]"
								end
								ctx.preview:set_title(name)
								local ok, lines = pcall(vim.api.nvim_buf_get_lines, b, 0, -1, false)
								lines = ok and lines or { "<failed to read buffer>" }
								if #lines > 5000 then
									lines = vim.list_slice(lines, 1, 5000)
									lines[#lines + 1] = "... (preview truncated)"
								end
								ctx.preview:set_lines(lines)
								local bt = item.buftype or vim.bo[b].buftype
								local ft = item.filetype or vim.bo[b].filetype
								if bt == "" and ft ~= "" and ft ~= "netrw" then
									pcall(function()
										ctx.preview:highlight({ ft = ft })
									end)
								end
								pcall(function()
									ctx.preview:loc()
								end)
							else
								Snacks.picker.preview.file(ctx)
							end
						end,
					},
				},
				icons = {
					tree = {
						vertical = "│ ",
						middle = "├╴",
						last = "└╴",
					},
				},
			},
		},
		keys = {
         --stylua: ignore start
         -- Files & buffers
             { "<leader>bl", function() Snacks.picker.buffers() end,                desc = "Buffers" },
             { "<leader>fe", function() Snacks.explorer() end,                      desc = "File Explorer" },
             { "<leader>ff", function() Snacks.picker.files() end,                  desc = "Find Files" },
             { "<leader>fr", function() Snacks.picker.recent() end,                 desc = "Recent" },
             { "<leader>fs", function() Snacks.picker.smart() end,                  desc = "Smart Find Files" },
             -- Grep
             { "<leader>sb", function() Snacks.picker.lines() end,                  desc = "Buffer Lines" },
             { "<leader>sg", function() Snacks.picker.grep() end,                   desc = "Grep" },
             { "<leader>sw", function() Snacks.picker.grep_word() end,              desc = "Visual selection or word", mode = { "n", "x" } },
             -- Other
             { "<leader>sC", function() Snacks.picker.commands() end,               desc = "Commands" },
             { "<leader>sd", function() Snacks.picker.diagnostics({ severity = vim.g.diagnostic_severity }) end, desc = "Diagnostics (current severity filter)" },
             { "<leader>sh", function() Snacks.picker.help() end,                   desc = "Help Pages" },
             { "<leader>sj", function() Snacks.picker.jumps() end,                  desc = "Jumps" },
             { "<leader>sk", function() Snacks.picker.keymaps() end,                desc = "Keymaps" },
             { "<leader>sl", function() Snacks.picker.loclist() end,                desc = "Location List" },
             { "<leader>sm", function() Snacks.picker.marks() end,                  desc = "Marks" },
             { "<leader>sq", function() Snacks.picker.qflist() end,                 desc = "Quickfix List" },
             { "<leader>sr", function() Snacks.picker.registers() end,              desc = "Registers" },
             { "<leader>sR", function() Snacks.picker.resume() end,                 desc = "Resume" },
             { "<leader>su", function() Snacks.picker.undo() end,                   desc = "Undo History" },
             -- LSP
             { "gd",         function() Snacks.picker.lsp_definitions() end,        desc = "Goto Definition" },
             { "gD",         function() Snacks.picker.lsp_declarations() end,       desc = "Goto Declaration" },
             { "gr",         function() Snacks.picker.lsp_references() end,         nowait = true, desc = "References" },
             { "gI",         function() Snacks.picker.lsp_implementations() end,    desc = "Goto Implementation" },
             { "gy",         function() Snacks.picker.lsp_type_definitions() end,   desc = "Goto Type Definition" },
             { "<leader>ss", function() Snacks.picker.lsp_symbols({ tree = true, keep_parents = true, filter = { default = true } }) end, desc = "LSP Symbols" },
             { "<leader>sS", function() Snacks.picker.lsp_workspace_symbols() end,  desc = "LSP Workspace Symbols" },
		},
	},
	{
		"folke/flash.nvim",
		event = "VeryLazy",
		opts = {},
		keys = {
         --stylua: ignore start
			{ "s", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash", },
			{ "S", mode = { "n", "o" }, function() require("flash").treesitter() end, desc = "Flash Treesitter", },
			{ "r", mode = "o", function() require("flash").remote() end, desc = "Remote Flash", },
			{ "R", mode = { "o", "x" }, function() require("flash").treesitter_search() end, desc = "Treesitter Search", },
			{ "<c-s>", mode = { "c" }, function() require("flash").toggle() end, desc = "Toggle Flash Search", },
		},
	},
  --stylua: ignore start
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		opts = {
			delay = 800,
			spec = {
				{ "<leader>l", group = "lsp" },
				{ "<leader>d", group = "debug" },
				{ "<leader>r", group = "run" },
				{ "<leader>b", group = "buffer" },
				{ "<leader>f", group = "files" },
				{ "<leader>s", group = "search" },
				{ "<leader>x", group = "diagnostics" },
				{ "<leader>h", group = "git" },
				{ "<leader>t", group = "tabs" },
				{ "<leader>c", group = "code" },
				-- g-prefix: explicit labels + sub-groups so which-key shows a clean organized list
				{ "g", group = "goto / g" },
				-- Our controlled goto mappings (Snacks + custom)
				{ "gd", desc = "Definition" },
				{ "gD", desc = "Declaration" },
				{ "gI", desc = "Implementation" },
				{ "gy", desc = "Type Definition" },
				{ "gr", desc = "References" },
				{ "g*", desc = "Word (no boundary)" },
				{ "g;", desc = "Older change" },
				{ "g,", desc = "Newer change" },
				-- Common native g commands (short clean labels instead of long defaults)
				{ "gg", desc = "First line" },
				{ "ge", desc = "End of prev word" },
				{ "gf", desc = "File under cursor" },
				{ "gi", desc = "Last insert" },
				{ "gn", desc = "Search fwd select" },
				{ "gN", desc = "Search bwd select" },
				{ "gt", desc = "Next tab" },
				{ "gT", desc = "Prev tab" },
				{ "gu", desc = "Lowercase" },
				{ "gU", desc = "Uppercase" },
				{ "gv", desc = "Reselect visual" },
				{ "gw", desc = "Format" },
				{ "gx", desc = "Open URI / file" },
				{ "g~", desc = "Toggle case" },
				{ "g%", desc = "Cycle results" },
				-- Comment blockwise (gc is grouped below)
				{ "gb", desc = "Comment blockwise" },
				-- sub-groups (appear as headers when prefix typed)
				{ "gc", group = "comment" },
				{ "gs", group = "swap" },
				-- bracket navigation: [ = prev, ] = next
				-- Explicit short labels for (almost) everything that appears under these
				-- prefixes. This replaces raw rhs ("bnext", "call rust#Jump..."), long
				-- defaults, and the giant mixed list. We hide only the rarely-used
				-- punctuation motions to keep the popup focused and readable.
				{ "[", group = "prev" },
				{ "]", group = "next" },

				-- Git hunks (gitsigns) — traditional [c/]c
				{ "[c", desc = "Hunk" },
				{ "]c", desc = "Hunk" },

				-- Treesitter textobjects (note: class uses uppercase to avoid [c clash)
				{ "[f", desc = "Function" },
				{ "]f", desc = "Function" },
				{ "[C", desc = "Class" },
				{ "]C", desc = "Class" },
				{ "[o", desc = "Block" },
				{ "]o", desc = "Block" },
				{ "[a", desc = "Argument" },
				{ "]a", desc = "Argument" },
				{ "[A", desc = "Arg end" },
				{ "]A", desc = "Arg end" },
				{ "[m", desc = "Method" },
				{ "]m", desc = "Method" },
				{ "[M", desc = "Method end" },
				{ "]M", desc = "Method end" },

				-- LSP diagnostics (override any long default for D)
				{ "[d", desc = "Diagnostic" },
				{ "]d", desc = "Diagnostic" },
				{ "[D", desc = "First diagnostic" },
				{ "]D", desc = "Last diagnostic" },

				-- Lists (quickfix / loclist / tags / spell)
				{ "[q", desc = "Quickfix" },
				{ "]q", desc = "Quickfix" },
				{ "[Q", desc = "Quickfix first" },
				{ "]Q", desc = "Quickfix last" },
				{ "[l", desc = "Loclist" },
				{ "]l", desc = "Loclist" },
				{ "[L", desc = "Loclist first" },
				{ "]L", desc = "Loclist last" },
				{ "[t", desc = "Tag" },
				{ "]t", desc = "Tag" },
				{ "[T", desc = "Tag first" },
				{ "]T", desc = "Tag last" },
				{ "[s", desc = "Spell" },
				{ "]s", desc = "Spell" },

				-- Buffers (common even when <leader>b is primary)
				{ "[b", desc = "Buffer" },
				{ "]b", desc = "Buffer" },
				{ "[B", desc = "Buffer first" },
				{ "]B", desc = "Buffer last" },

				-- Marks / changes
				{ "['", desc = "Change/yank start" },
				{ "]'", desc = "Change/yank end" },

				-- Sections / items (built-in + language ftplugins e.g. Rust)
				{ "[[", desc = "Section" },
				{ "]]", desc = "Section" },
				{ "[]", desc = "Section end" },
				{ "][", desc = "Section end" },

				-- Hide noisy/low-value punctuation & blank-line motions
				{ "[(", hidden = true },
				{ "](", hidden = true },
				{ "[{", hidden = true },
				{ "]{", hidden = true },
				{ "[<", hidden = true },
				{ "]<", hidden = true },
				{ "[%", hidden = true },
				{ "]%", hidden = true },
				{ "[_", hidden = true },
				{ "]_", hidden = true },
			},
		},
		keys = {
			{ "<leader>?", function() require("which-key").show({ global = false }) end, desc = "Buffer local keymaps", },
		},
	},
	-- Surround
	{
		"kylechui/nvim-surround",
		event = "VeryLazy",
		config = function()
			require("nvim-surround").setup({})
		end,
	},
	-- Autopair
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		opts = {
			check_ts = true,
			ts_config = {
				rust = { "string_content", "comment" },
			},
		},
	},
	-- Comments
	{
		"numToStr/Comment.nvim",
		event = "VeryLazy",
		config = function()
			require("Comment").setup()
		end,
	},
}
