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
		branch = "main",
		lazy = false,
		build = ":TSUpdate",
		opts = {
			ensure_installed = { "vim", "vimdoc", "lua", "toml" },
			indent = { enable = true },
		},
		config = function(_, opts)
			require("nvim-treesitter.configs").setup(opts)
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
		cmd = "Mason", -- Load when :Mason command is run
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
			servers = {
				lua_ls = {
					settings = {
						Lua = {
							diagnostics = { globals = { "vim" } },
							workspace = { checkThirdParty = false },
							telemetry = { enable = false },
						},
					},
				},
			},
		},
		config = function(_, opts)
			local lspconfig = require("lspconfig")
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

			-- Unified on_attach function (unchanged)
			local on_attach = function(client, bufnr)
				local common_opts = { noremap = true, silent = true, buffer = bufnr }
				local function keymap_opts(desc)
					return vim.tbl_deep_extend("force", vim.deepcopy(common_opts), { desc = desc })
				end
				vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, keymap_opts("LSP: Code Action"))
				vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, keymap_opts("LSP: Rename"))
				vim.keymap.set("n", "gd", vim.lsp.buf.definition, keymap_opts("LSP: Go to Definition"))
				vim.keymap.set("n", "gi", vim.lsp.buf.implementation, keymap_opts("LSP: Go to Implementation"))
				vim.keymap.set("n", "K", vim.lsp.buf.hover, keymap_opts("LSP: Hover Documentation"))
				vim.keymap.set("n", "<leader>D", vim.lsp.buf.type_definition, keymap_opts("LSP: Go to Type Definition"))
				vim.keymap.set(
					"n",
					"<leader>ds",
					vim.diagnostic.open_float,
					keymap_opts("Diagnostics: Show Line Diagnostics")
				)
				vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, keymap_opts("Diagnostics: Go to Previous"))
				vim.keymap.set("n", "]d", vim.diagnostic.goto_next, keymap_opts("Diagnostics: Go to Next"))
				vim.keymap.set("n", "<leader>lr", vim.lsp.buf.references, keymap_opts("LSP: Find References"))
			end

			-- LSP capabilities (unchanged)
			local capabilities = cmp_nvim_lsp.default_capabilities()
			capabilities.workspace = capabilities.workspace or {}
			capabilities.workspace.didChangeWatchedFiles = { dynamicRegistration = true }

			-- **This is the new, simplified setup**
			-- It enables automatic installation of LSPs and sets up servers
			-- based on the `opts.servers` table, which is extended by `lang` modules.
			mason_lspconfig.setup({
				automatic_installation = true,
			})

			mason_lspconfig.setup_handlers({
				function(server_name)
					local server_opts = vim.tbl_deep_extend("force", {
						on_attach = on_attach,
						capabilities = capabilities,
					}, opts.servers[server_name] or {})

					lspconfig[server_name].setup(server_opts)
				end,
			})
		end,
	},

	--[[ Debug Adapter Protocol (DAP) Setup ]]
	{
		"mfussenegger/nvim-dap",
		event = "VeryLazy", -- Load only when needed for debugging
		dependencies = {
			"jay-babu/mason-nvim-dap.nvim", -- Integrates DAP with Mason
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
				lua = { "stylua" },
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
