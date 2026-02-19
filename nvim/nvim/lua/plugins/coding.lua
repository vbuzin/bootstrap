return {
	--[[  Git Integration ]]
	{
		"lewis6991/gitsigns.nvim",
		event = { "BufReadPre", "BufNewFile" }, -- Load when opening files
		config = function()
			require("gitsigns").setup()
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
			require("nvim-treesitter").install(opts.ensure_installed)

			vim.api.nvim_create_autocmd("FileType", {
				callback = function()
					pcall(vim.treesitter.start)
				end,
			})
		end,
	},

	--[[ Mini.ai: The Modern Text Objects (Replaces nvim-treesitter-textobjects) ]]
	{
		"echasnovski/mini.ai",
		event = "VeryLazy",
		dependencies = { "nvim-treesitter/nvim-treesitter" },
		opts = function()
			local ai = require("mini.ai")
			return {
				n_lines = 500,
				custom_textobjects = {
					o = ai.gen_spec.treesitter({ -- code block
						a = { "@block.outer", "@conditional.outer", "@loop.outer" },
						i = { "@block.inner", "@conditional.inner", "@loop.inner" },
					}),
					f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }), -- function
					c = ai.gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }), -- class
					t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" }, -- tags
				},
			}
		end,
		config = function(_, opts)
			require("mini.ai").setup(opts)
		end,
	},

	--[[ Mason Core ]]
	{
		"williamboman/mason.nvim",
		event = "VeryLazy",
		opts = {
			ui = {
				border = "rounded",
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
		event = "VeryLazy",
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
			"hrsh7th/cmp-nvim-lsp",
		},
		opts = {
			-- Default server configurations. These will be extended by language-specific plugins.
			servers = {},
		},
		config = function(_, opts)
			local cmp_nvim_lsp = require("cmp_nvim_lsp")
			local mason_lspconfig = require("mason-lspconfig")

			-- Inline diagnostics toggle setup (unchanged)
			local inline_diagnostics_enabled = false
			vim.diagnostic.config({
				signs = true,
				underline = true,
				update_in_insert = false,
				severity_sort = true,
				virtual_text = inline_diagnostics_enabled,
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
				"<leader>td",
				toggle_inline_diagnostics,
				{ noremap = true, silent = true, desc = "Toggle Inline Diagnostics" }
			)

			-- LSP capabilities (unchanged)
			local capabilities = cmp_nvim_lsp.default_capabilities()
			capabilities.workspace = capabilities.workspace or {}
			capabilities.workspace.didChangeWatchedFiles = { dynamicRegistration = true }

			-- Apply capabilities to all LSP servers via wildcard
			vim.lsp.config("*", {
				capabilities = capabilities,
			})

			-- LspAttach autocmd for keymaps (replaces on_attach)
			vim.api.nvim_create_autocmd("LspAttach", {
				group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
				callback = function(event)
					local bufnr = event.buf
					local function map(mode, lhs, rhs, desc)
						vim.keymap.set(mode, lhs, rhs, { noremap = true, silent = true, buffer = bufnr, desc = desc })
					end
					map("n", "<leader>ca", vim.lsp.buf.code_action, "LSP: Code Action")
					map("n", "<leader>rn", vim.lsp.buf.rename, "LSP: Rename")
					map("n", "gd", vim.lsp.buf.definition, "LSP: Go to Definition")
					map("n", "gi", vim.lsp.buf.implementation, "LSP: Go to Implementation")
					map("n", "K", vim.lsp.buf.hover, "LSP: Hover Documentation")
					map("n", "<leader>D", vim.lsp.buf.type_definition, "LSP: Go to Type Definition")
					map("n", "<leader>ds", vim.diagnostic.open_float, "Diagnostics: Show Line Diagnostics")
					map("n", "[d", function()
						vim.diagnostic.jump({ count = -1 })
					end, "Diagnostics: Go to Previous")
					map("n", "]d", function()
						vim.diagnostic.jump({ count = 1 })
					end, "Diagnostics: Go to Next")
					map("n", "<leader>lr", vim.lsp.buf.references, "LSP: Find References")
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
			{
				"<F5>",
				function()
					require("dap").continue()
				end,
				desc = "Debug: Start/Continue",
			},
			{
				"<F10>",
				function()
					require("dap").step_over()
				end,
				desc = "Debug: Step Over",
			},
			{
				"<F11>",
				function()
					require("dap").step_into()
				end,
				desc = "Debug: Step Into",
			},
			{
				"<F12>",
				function()
					require("dap").step_out()
				end,
				desc = "Debug: Step Out",
			},
			{
				"<leader>db",
				function()
					require("dap").toggle_breakpoint()
				end,
				desc = "Debug: Toggle Breakpoint",
			},
			{
				"<leader>dB",
				function()
					require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: "))
				end,
				desc = "Debug: Set Conditional Breakpoint",
			},
			{
				"<leader>dr",
				function()
					require("dap").repl.open()
				end,
				desc = "Debug: Open REPL",
			},
			{
				"<leader>dl",
				function()
					require("dap").run_last()
				end,
				desc = "Debug: Run Last",
			},
		},
		config = function()
			vim.fn.sign_define("DapBreakpoint", { text = "●", texthl = "Error", linehl = "", numhl = "" })
			vim.fn.sign_define("DapStopped", { text = "▶", texthl = "DiagnosticInfo", linehl = "", numhl = "" })
		end,
	},
	-- Mason helper for DAP
	{
		"jay-babu/mason-nvim-dap.nvim",
		event = "VeryLazy", -- Load with nvim-dap or when its commands are used
		dependencies = { "williamboman/mason.nvim", "mfussenegger/nvim-dap" },
		config = true, -- Calls require("mason-nvim-dap").setup({})
	},
	-- DAP UI
	{
		"rcarriga/nvim-dap-ui",
		event = "VeryLazy", -- Load when DAP starts
		dependencies = {
			"mfussenegger/nvim-dap",
			"nvim-neotest/nvim-nio", -- Required dependency for nvim-dap-ui
		},
		config = function()
			local dap, dapui = require("dap"), require("dapui")
			dapui.setup() -- Use default dapui configuration
			-- Automatically open/close DAP UI when debugging session starts/ends
			dap.listeners.after.event_initialized["dapui_config"] = function()
				dapui.open()
			end
			dap.listeners.before.event_terminated["dapui_config"] = function()
				dapui.close()
			end
			dap.listeners.before.event_exited["dapui_config"] = function()
				dapui.close()
			end
		end,
	},

	--[[ Formatting with conform.nvim ]]
	{
		"stevearc/conform.nvim",
		event = { "BufWritePre" }, -- Format on save (before writing)
		cmd = { "ConformInfo" }, -- Command to inspect conform.nvim setup
		opts = {
			formatters_by_ft = {
				yaml = { "prettierd" },
				markdown = { "prettierd" },
				graphql = { "prettierd" },
			},
			format_on_save = {
				timeout_ms = 1000, -- Max time for formatting on save
				lsp_fallback = false, -- Do not fallback to LSP for formatting
			},
		},
		config = true, -- Calls require("conform").setup(opts)
	},

	--[[ Autocompletion with nvim-cmp ]]
	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter", -- Load when entering insert mode
		dependencies = {
			"hrsh7th/cmp-nvim-lsp", -- LSP completion source
			"L3MON4D3/LuaSnip", -- Snippet engine
			"saadparwaiz1/cmp_luasnip", -- Bridge between nvim-cmp and LuaSnip
		},
		opts = function()
			local cmp = require("cmp")
			local luasnip = require("luasnip")
			return {
				snippet = {
					expand = function(args)
						luasnip.lsp_expand(args.body) -- Expand snippets provided by LSP
					end,
				},
				mapping = cmp.mapping.preset.insert({
					["<C-b>"] = cmp.mapping.scroll_docs(-4), -- Scroll backwards in documentation
					["<C-f>"] = cmp.mapping.scroll_docs(4), -- Scroll forwards in documentation
					["<C-Space>"] = cmp.mapping.complete(), -- Trigger completion
					["<C-e>"] = cmp.mapping.abort(), -- Abort completion
					["<CR>"] = cmp.mapping.confirm({ select = true }), -- Confirm selection (Enter key)
				}),
				sources = cmp.config.sources({
					{ name = "nvim_lsp" }, -- LSP suggestions
					{ name = "luasnip" }, -- Snippet suggestions
				}),
				window = { -- Appearance of completion and documentation windows
					completion = cmp.config.window.bordered(),
					documentation = cmp.config.window.bordered(),
				},
				experimental = {
					ghost_text = true, -- Show inline virtual text for completion preview (Neovim 0.10+)
				},
			}
		end,
		config = function(_, opts)
			require("luasnip.loaders.from_vscode").lazy_load()
			require("cmp").setup(opts)
		end,
	},
}
