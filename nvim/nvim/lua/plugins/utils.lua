return {
	-- Snacks
	{
		"folke/snacks.nvim",
		priority = 1000,
		lazy = false,
		opts = {
			indent = {
				enabled = true,

				indent = {
					enabled = false,
				},
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
             { "<leader>sd", function()
                 Snacks.picker.diagnostics({ severity = vim.g.diagnostic_severity })
             end, desc = "Diagnostics (current severity filter)" },
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
			{
				"s",
				mode = { "n", "x", "o" },
				function()
					require("flash").jump()
				end,
				desc = "Flash",
			},
			{
				"S",
				mode = { "n", "o" },
				function()
					require("flash").treesitter()
				end,
				desc = "Flash Treesitter",
			},
			{
				"r",
				mode = "o",
				function()
					require("flash").remote()
				end,
				desc = "Remote Flash",
			},
			{
				"R",
				mode = { "o", "x" },
				function()
					require("flash").treesitter_search()
				end,
				desc = "Treesitter Search",
			},
			{
				"<c-s>",
				mode = { "c" },
				function()
					require("flash").toggle()
				end,
				desc = "Toggle Flash Search",
			},
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
				{ "<leader>t", group = "toggle" },
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
				-- bracket navigation (also under g? no, top level brackets)
				{ "[", group = "prev" },
				{ "]", group = "next" },
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
