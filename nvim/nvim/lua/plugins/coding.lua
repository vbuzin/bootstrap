return {
	--[[  Git Integration ]]
	{
		"lewis6991/gitsigns.nvim",
		event = { "BufReadPre", "BufNewFile" }, -- Load when opening files
		config = function()
			require("gitsigns").setup({
				on_attach = function(bufnr)
					local gitsigns = require("gitsigns")

					local function map(mode, l, r, desc, opts)
						opts = opts or {}
						opts.buffer = bufnr
						if desc then
							opts.desc = desc
						end
						vim.keymap.set(mode, l, r, opts)
					end

					-- Navigation
					-- stylua: ignore start
					map("n", "]c", function() if vim.wo.diff then vim.cmd.normal({ "]c", bang = true }) else gitsigns.nav_hunk("next") end end, "Next hunk")
					map("n", "[c", function() if vim.wo.diff then vim.cmd.normal({ "[c", bang = true }) else gitsigns.nav_hunk("prev") end end, "Prev hunk")

					-- Actions
					map("n", "<leader>hs", gitsigns.stage_hunk, "Git: Stage Hunk")
					map("n", "<leader>hr", gitsigns.reset_hunk, "Git: Reset Hunk")

					map("v", "<leader>hs", function() gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") }) end, "Git: Stage Hunk")
					map("v", "<leader>hr", function() gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") }) end, "Git: Reset Hunk")

					map("n", "<leader>hS", gitsigns.stage_buffer, "Git: Stage Buffer")
					map("n", "<leader>hR", gitsigns.reset_buffer, "Git: Reset Buffer")
					map("n", "<leader>hp", gitsigns.preview_hunk, "Git: Preview Hunk")
					map("n", "<leader>hi", gitsigns.preview_hunk_inline, "Git: Preview Hunk Inline")

					map("n", "<leader>hb", function() gitsigns.blame_line({ full = true }) end, "Git: Blame Line")

					map("n", "<leader>hd", gitsigns.diffthis, "Git: Diff This")

					map("n", "<leader>hD", function() gitsigns.diffthis("~") end, "Git: Diff This ~")

					map("n", "<leader>hQ", function() gitsigns.setqflist("all") end, "Git: Send All Hunks to QF")
					map("n", "<leader>hq", gitsigns.setqflist, "Git: Send Hunks to QF")

					-- Toggles
					map("n", "<leader>hB", gitsigns.toggle_current_line_blame, "Git: Toggle Line Blame")
					map("n", "<leader>hw", gitsigns.toggle_word_diff, "Git: Toggle Word Diff")

					-- Text object
					map({ "o", "x" }, "ih", gitsigns.select_hunk, "Git: Select Hunk")

					-- stylua: ignore end
				end,
			})
		end,
	},

	--[[ Treesitter for Syntax Highlighting and More ]]
	{
		"nvim-treesitter/nvim-treesitter",
		lazy = false,
		build = ":TSUpdate",
		opts = {
			ensure_installed = { "vim", "vimdoc" },
		},
		config = function(_, opts)
			require("nvim-treesitter").setup({}) -- new API: only install_dir is valid

			-- Install parsers collected via lazy.nvim spec merging across lang files
			if opts.ensure_installed and #opts.ensure_installed > 0 then
				require("nvim-treesitter").install(opts.ensure_installed)
			end

			vim.api.nvim_create_autocmd("FileType", {
				callback = function(ev)
					local lang = vim.treesitter.language.get_lang(ev.match)
					if lang and require("nvim-treesitter.parsers")[lang] then
						pcall(require("nvim-treesitter").install, { lang })
						pcall(vim.treesitter.start)
					end
				end,
			})
		end,
	},

	--[[ Treesitter Context: Pin scope header while scrolling ]]
	{
		"nvim-treesitter/nvim-treesitter-context",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = { "nvim-treesitter/nvim-treesitter" },
		opts = {
			max_lines = 3, -- max lines the context window can be
			min_window_height = 20, -- don't show in very short windows
			trim_scope = "outer", -- trim outermost context when over limit
		},
		config = true,
	},

	--[[ Treesitter Text Objects: select, move, swap ]]
	{
		"nvim-treesitter/nvim-treesitter-textobjects",
		dependencies = { "nvim-treesitter/nvim-treesitter" },
		event = "VeryLazy",
		config = function()
			local sel = require("nvim-treesitter-textobjects.select")
			local move = require("nvim-treesitter-textobjects.move")
			local swap = require("nvim-treesitter-textobjects.swap")

			local function map(modes, lhs, fn, desc)
				vim.keymap.set(modes, lhs, fn, { noremap = true, silent = true, desc = desc })
			end

            -- stylua: ignore start
			local function sel_map(lhs, capture, desc)
				map({ "x", "o" }, "a" .. lhs, function() sel.select_textobject("@" .. capture .. ".outer", "textobjects") end, desc .. " outer")
				map({ "x", "o" }, "i" .. lhs, function() sel.select_textobject("@" .. capture .. ".inner", "textobjects") end, desc .. " inner")
			end

			local function move_map(next_lhs, prev_lhs, capture, desc)
				map("n", next_lhs, function() move.goto_next_start("@" .. capture .. ".outer", "textobjects") end, "Next " .. desc)
				map("n", prev_lhs, function() move.goto_previous_start("@" .. capture .. ".outer", "textobjects") end, "Prev " .. desc)
			end

			-- Select
			sel_map("f", "function", "function")
			sel_map("c", "class", "class/struct/enum")
			sel_map("o", "block", "block")
			sel_map("a", "parameter", "argument/parameter")

			-- Move
			-- NOTE: [c / ]c is reserved for gitsigns hunks (traditional).
			-- Use uppercase C for class to avoid conflict.
			move_map("]f", "[f", "function", "function")
			move_map("]C", "[C", "class", "class")
			move_map("]o", "[o", "block", "block")
			move_map("]a", "[a", "parameter", "argument")

			-- Swap
			map("n", "gsp", function() swap.swap_next("@parameter.inner", "textobjects") end, "Next parameter")
			map("n", "gsP", function() swap.swap_previous("@parameter.inner", "textobjects") end, "Prev parameter")
			-- stylua: ignore end
		end,
	},

	--[[ Mason Core ]]
	{
		"williamboman/mason.nvim",
		lazy = false,
		opts = {
			ui = {
				icons = {
					package_installed = "✓",
					package_pending = "➜",
					package_uninstalled = "✗",
				},
			},
		},
		config = true, -- Calls require("mason").setup(opts)
	},
	--[[ Mason Tool Installer (formatters/linters/debuggers) ]]
	{
		"WhoIsSethDaniel/mason-tool-installer.nvim",
		-- no event/lazy: its plugin/ registers VimEnter autocmd for run_on_start + ensure_installed
		dependencies = { "williamboman/mason.nvim" },
		opts = {
			ensure_installed = {},
			run_on_start = true,
			start_delay = 3000,
			debounce_hours = 12,
		},
		config = function(_, opts)
			require("mason-tool-installer").setup(opts)
		end,
	},

	--[[ LSP Configuration ]]
	{
		"neovim/nvim-lspconfig",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = {
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim",
			"saghen/blink.cmp",
		},
		opts = {
			-- Default server configurations. These will be extended by language-specific plugins.
			servers = {},
		},
		config = function(_, opts)
			local mason_lspconfig = require("mason-lspconfig")

			-- Severity filter toggle: All <-> Errors only (on-the-fly, great for Rust/clippy noise)
			-- We keep our own source of truth in vim.g because vim.diagnostic.config() getter
			-- does not reliably return nil after you explicitly set severity = nil.
			-- Start in "All" mode.
			vim.g.diagnostic_severity = nil -- nil = show all severities

			local function set_diag_severity(sev)
				vim.g.diagnostic_severity = sev
				vim.diagnostic.config({ severity = sev })

				-- Force the current buffer to re-apply diagnostic handlers (signs, virtual text, etc.)
				-- with the new severity filter immediately. This makes the gutter update live on toggle.
				local bufnr = vim.api.nvim_get_current_buf()
				pcall(vim.diagnostic.hide, nil, bufnr)
				pcall(vim.diagnostic.show, nil, bufnr)
			end

			local function toggle_diag_severity()
				-- Flip between "filtered to errors" and "show all".
				-- We deliberately use an explicit if because the common Lua "a and X or Y" idiom
				-- breaks when X is nil/false (the 'or' ends up picking Y).
				local new_sev
				if vim.g.diagnostic_severity then
					-- currently showing only errors → switch to all
					new_sev = nil
				else
					-- currently showing all → switch to errors only
					new_sev = { min = vim.diagnostic.severity.ERROR }
				end
				set_diag_severity(new_sev)
				vim.notify(
					"Diagnostics: " .. (new_sev and "Errors only" or "All"),
					vim.log.levels.INFO,
					{ title = "Diagnostics" }
				)
			end

			-- Always read from our controlled vim.g (never from vim.diagnostic.config() getter)
			local function get_diag_severity()
				return vim.g.diagnostic_severity
			end

			-- Wrappers for navigation/float so they always respect the current filter
			local function diag_jump_prev()
				vim.diagnostic.jump({ count = -1, severity = get_diag_severity() })
			end
			local function diag_jump_next()
				vim.diagnostic.jump({ count = 1, severity = get_diag_severity() })
			end
			local function diag_show_line()
				vim.diagnostic.open_float({ severity = get_diag_severity() })
			end

			-- Initial config (start with All)
			local inline_diagnostics_enabled = false
			vim.diagnostic.config({
				signs = true,
				underline = true,
				update_in_insert = false,
				severity_sort = true,
				virtual_text = inline_diagnostics_enabled,
				-- no severity key = All (we also set vim.g above)
			})

			local function toggle_inline_diagnostics()
				inline_diagnostics_enabled = not inline_diagnostics_enabled
				vim.diagnostic.config({ virtual_text = inline_diagnostics_enabled })
				vim.notify(
					"Inline diagnostics: " .. (inline_diagnostics_enabled and "ON" or "OFF"),
					vim.log.levels.INFO,
					{ title = "Diagnostics" }
				)
			end
			vim.keymap.set(
				"n",
				"<leader>xt",
				toggle_inline_diagnostics,
				{ noremap = true, silent = true, desc = "Toggle Inline Diagnostics" }
			)

			-- Toggle All <-> Errors only (on the fly)
			vim.keymap.set(
				"n",
				"<leader>xs",
				toggle_diag_severity,
				{ noremap = true, silent = true, desc = "Toggle Severity (All / Errors only)" }
			)

			-- LSP capabilities (provided by blink.cmp)
			local capabilities = require("blink.cmp").get_lsp_capabilities()
			capabilities.workspace = capabilities.workspace or {}
			capabilities.workspace.didChangeWatchedFiles = { dynamicRegistration = true }

			-- Apply capabilities to all LSP servers via wildcard
			vim.lsp.config("*", {
				capabilities = capabilities,
			})

			vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
				border = "rounded",
				max_width = math.max(60, math.floor(vim.o.columns * 0.55)),
				max_height = math.max(8, math.floor(vim.o.lines * 0.65)),
			})

			vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
				border = "rounded",
				max_width = math.max(50, math.floor(vim.o.columns * 0.4)),
				max_height = 8,
			})

			-- LspAttach autocmd for keymaps (replaces on_attach)
			vim.api.nvim_create_autocmd("LspAttach", {
				group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
				callback = function(event)
					local bufnr = event.buf

					local function map(mode, lhs, rhs, desc)
						vim.keymap.set(mode, lhs, rhs, { noremap = true, silent = true, buffer = bufnr, desc = desc })
					end
					map("n", "<leader>ci", function()
						local enabled = vim.lsp.inlay_hint.is_enabled()
						vim.lsp.inlay_hint.enable(not enabled)
					end, "Toggle Inlay Hints")
					map("n", "<leader>ca", vim.lsp.buf.code_action, "Code Action")
					map("n", "<leader>co", function()
						vim.lsp.buf.code_action({
							context = { only = { "source.organizeImports" } },
							apply = true,
						})
					end, "Organize Imports")
					map("n", "<leader>cr", vim.lsp.buf.rename, "Rename")
					map("n", "K", vim.lsp.buf.hover, "Hover Documentation")
					-- <C-k> for signature help is provided by blink.cmp (when signature.enabled)
					-- We intentionally do not override here so blink's integrated signature wins.
					map("n", "<leader>xd", diag_show_line, "Show Line Diagnostics")
					map("n", "[d", diag_jump_prev, "Prev diagnostic")
					map("n", "]d", diag_jump_next, "Next diagnostic")

					local client = vim.lsp.get_client_by_id(event.data.client_id)
					if client and client.supports_method("textDocument/codeLens") then
						map("n", "<leader>cl", vim.lsp.codelens.run, "Run Code Lens")
						local lens_on = false
						map("n", "<leader>cL", function()
							lens_on = not lens_on
							if lens_on then
								vim.lsp.codelens.refresh()
							else
								vim.lsp.codelens.clear()
							end
							vim.notify("Code lenses: " .. (lens_on and "ON" or "OFF"), vim.log.levels.INFO)
						end, "Toggle Code Lenses")
					end
				end,
			})

			-- Mason auto-installation
			mason_lspconfig.setup({
				ensure_installed = vim.tbl_keys(opts.servers),
				automatic_installation = true,
			})

			-- Configure and enable each server from opts.servers
			for server_name, server_config in pairs(opts.servers) do
				vim.lsp.config(server_name, server_config)
				vim.lsp.enable(server_name)
			end
		end,
	},

	--[[ Debug Adapter Protocol (DAP) Setup ]]
	{
		"mfussenegger/nvim-dap",
		dependencies = {
			"jay-babu/mason-nvim-dap.nvim", -- Integrates DAP with Mason
		},
		keys = {
        --stylua: ignore start
			{ "<F5>", function() require("dap").continue() end, desc = "Start/Continue", },
			{ "<leader>dc", function() require("dap").continue() end, desc = "Start/Continue", },
			{ "<F10>", function() require("dap").step_over() end, desc = "Step Over", },
			{ "<leader>do", function() require("dap").step_over() end, desc = "Step Over", },
			{ "<F11>", function() require("dap").step_into() end, desc = "Step Into", },
			{ "<leader>di", function() require("dap").step_into() end, desc = "Step Into", },
			{ "<F12>", function() require("dap").step_out() end, desc = "Step Out", },
			{ "<leader>du", function() require("dap").step_out() end, desc = "Step Out", },
			{ "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "Toggle Breakpoint", },
			{ "<leader>dB", function() require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: ")) end, desc = "Set Conditional Breakpoint", },
			{ "<leader>dr", function() require("dap").repl.open() end, desc = "Open REPL", },
			{ "<leader>dl", function() require("dap").run_last() end, desc = "Run Last", },
			-- stylua: ignore end
		},
		config = function()
			vim.fn.sign_define("DapBreakpoint", { text = "●", texthl = "Error", linehl = "", numhl = "" })
			vim.fn.sign_define("DapStopped", { text = "▶", texthl = "DiagnosticInfo", linehl = "", numhl = "" })
		end,
	},
	-- Mason helper for DAP
	{
		"jay-babu/mason-nvim-dap.nvim",
		-- removed VeryLazy so ensure_installed (debugpy, codelldb, ...) runs early via this path too
		dependencies = { "williamboman/mason.nvim", "mfussenegger/nvim-dap" },
		config = true, -- Calls require("mason-nvim-dap").setup({})
	},

	--[[ Formatting with conform.nvim ]]
	{
		"stevearc/conform.nvim",
		event = { "BufWritePre" }, -- Format on save (before writing)
		cmd = { "ConformInfo" }, -- Command to inspect conform.nvim setup
		opts = {
			format_on_save = {
				timeout_ms = 1000, -- Max time for formatting on save
				lsp_fallback = false, -- Do not fallback to LSP for formatting
			},
		},
		keys = {
			{
				"<leader>cf",
				function()
					require("conform").format({ async = true })
				end,
				desc = "Format",
				mode = { "n", "v" },
			},
		},
		config = true, -- Calls require("conform").setup(opts)
	},

	--[[ Autocompletion with blink.cmp ]]
	{
		"saghen/blink.cmp",
		event = { "InsertEnter", "CmdlineEnter" },
		version = "1.*",
		dependencies = {
			"rafamadriz/friendly-snippets", -- provides snippets for the default (vim.snippet) engine
		},
		opts = {
			-- Keymap customizations (super-tab + Enter accept)
			keymap = {
				preset = "super-tab",
				["<C-Space>"] = { "show", "show_documentation", "hide_documentation" },
				["<C-e>"] = { "hide", "fallback" },
				-- Keep Enter working to accept too (original behavior)
				["<CR>"] = { "accept", "fallback" },
			},
			appearance = {},
			completion = {
				menu = {
					border = "rounded",
					auto_show = false,
					max_height = 16, -- default is 10
					draw = {
						columns = {
							{ "kind_icon" },
							{ "label", "label_description", gap = 1 },
							{ "kind" },
						},
					},
				},
				documentation = {
					auto_show = true,
					auto_show_delay_ms = 250, -- default is 500
					window = {
						border = "rounded",
					},
				},
				ghost_text = { enabled = true },
			},
			-- Integrated signature help (replaces direct vim.lsp.buf.signature_help in many cases)
			signature = {
				enabled = true,
				window = { border = "rounded" },
			},

			-- Command line completion (very useful for : commands, searches)
			cmdline = {
				keymap = { preset = "inherit" },
				completion = { menu = { auto_show = true } },
			},
		},
		config = function(_, opts)
			require("blink.cmp").setup(opts)
		end,
	},
}
