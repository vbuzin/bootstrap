if vim.fn.executable("cargo") ~= 1 and vim.fn.executable("rustc") ~= 1 then
	return {}
end

return {
	-- Treesitter: rust + toml parsers
	{
		"nvim-treesitter/nvim-treesitter",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "rust", "toml" })
			return opts
		end,
	},

	-- Formatter: rustfmt (ships with rustup, not mason)
	{
		"stevearc/conform.nvim",
		ft = { "rust" },
		opts = function(_, opts)
			opts.formatters_by_ft = opts.formatters_by_ft or {}
			opts.formatters_by_ft.rust = { "rustfmt" }
			return opts
		end,
	},

	-- Mason: codelldb debugger + taplo TOML LSP
	{
		"WhoIsSethDaniel/mason-tool-installer.nvim",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "codelldb", "taplo" })
			return opts
		end,
	},

	-- DAP adapter: codelldb (rustaceanvim auto-detects from mason bin)
	{
		"jay-babu/mason-nvim-dap.nvim",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "codelldb" })
			return opts
		end,
	},

	-- taplo: TOML LSP (schema validation, hover for Cargo.toml keys)
	{
		"neovim/nvim-lspconfig",
		ft = { "toml" },
		opts = {
			servers = {
				taplo = {},
			},
		},
	},

	-- crates.nvim: live crate version info + completions inside Cargo.toml
	{
		"saecki/crates.nvim",
		tag = "stable",
		event = { "BufRead Cargo.toml" },
		opts = {
			completion = {
				crates = { enabled = true },
			},
			lsp = {
				enabled = true,
				actions = true,
				completion = true,
				hover = true,
			},
		},
	},

	-- rustaceanvim: full Rust IDE experience (manages rust-analyzer itself)
	-- Do NOT also configure rust_analyzer via nvim-lspconfig — conflicts.
	{
		"mrcjkb/rustaceanvim",
		version = "^5",
		ft = { "rust" },
		opts = {
			tools = {
				float_win_config = { border = "rounded" },
			},
			server = {
				on_attach = function(_, bufnr)
					-- Enable inlay hints (same pattern as fsautocomplete in coding.lua)
					vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })

					local map = function(lhs, rhs, desc)
						vim.keymap.set("n", lhs, rhs, { buffer = bufnr, desc = desc })
					end

					-- Runnables / testables / debuggables
					map("<leader>rr", function() vim.cmd.RustLsp("runnables") end, "Rust: runnables")
					map("<leader>rt", function() vim.cmd.RustLsp("testables") end, "Rust: testables")
					map("<leader>rd", function() vim.cmd.RustLsp("debuggables") end, "Rust: debuggables")

					-- Code navigation / editing
					map("<leader>re", function() vim.cmd.RustLsp("expandMacro") end, "Rust: expand macro")
					map("<leader>rc", function() vim.cmd.RustLsp("openCargo") end, "Rust: open Cargo.toml")
					map("<leader>rp", function() vim.cmd.RustLsp("parentModule") end, "Rust: parent module")
					map("<leader>rj", function() vim.cmd.RustLsp("joinLines") end, "Rust: join lines")

					-- Override hover + code action with rustaceanvim's richer versions
					vim.keymap.set({ "n", "v" }, "K", function()
						vim.cmd.RustLsp({ "hover", "actions" })
					end, { buffer = bufnr, desc = "Rust: hover actions" })
					vim.keymap.set({ "n", "v" }, "<leader>la", function()
						vim.cmd.RustLsp("codeAction")
					end, { buffer = bufnr, desc = "Rust: code action" })
				end,
				default_settings = {
					["rust-analyzer"] = {
						cargo = {
							allFeatures = true,
							loadOutDirsFromCheck = true,
							buildScripts = { enable = true },
						},
						checkOnSave = true,
						check = {
							command = "clippy",
							features = "all",
							extraArgs = { "--no-deps" },
						},
						procMacro = {
							enable = true,
							ignored = {
								["async-trait"] = { "async_trait" },
								["napi-derive"] = { "napi" },
								["async-recursion"] = { "async_recursion" },
							},
						},
						inlayHints = {
							chainingHints = { enable = true },
							closingBraceHints = { enable = true, minLines = 25 },
							lifetimeElisionHints = {
								enable = "skip_trivial",
								useParameterNames = false,
							},
							maxLength = 25,
							parameterHints = { enable = true },
							renderColons = true,
							typeHints = {
								enable = true,
								hideClosureInitialization = false,
								hideNamedConstructor = false,
							},
						},
					},
				},
			},
		},
		config = function(_, opts)
			vim.g.rustaceanvim = vim.tbl_deep_extend("keep", vim.g.rustaceanvim or {}, opts)
		end,
	},

}
