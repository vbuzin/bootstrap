if vim.fn.executable("cargo") ~= 1 and vim.fn.executable("rustc") ~= 1 then
	return {}
end

return {
	{
		"nvim-treesitter/nvim-treesitter",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "rust", "toml" })
			return opts
		end,
	},

	{
		"stevearc/conform.nvim",
		ft = { "rust" },
		opts = function(_, opts)
			opts.formatters_by_ft = opts.formatters_by_ft or {}
			opts.formatters_by_ft.rust = { "rustfmt" }
			return opts
		end,
	},

	{
		"neovim/nvim-lspconfig",
		ft = { "toml" },
		opts = {
			servers = {
				taplo = {},
			},
		},
	},

	{
		"saecki/crates.nvim",
		tag = "stable",
		event = { "BufRead Cargo.toml" },
		opts = {
			completion = {
				crates = { enabled = true },
				blink = {
					use_custom_kind = true,
					kind_text = { version = "Version", feature = "Feature" },
					kind_highlight = {
						version = "BlinkCmpKindVersion",
						feature = "BlinkCmpKindFeature",
					},
					kind_icon = { version = " ", feature = " " },
				},
			},
			lsp = {
				enabled = true,
				actions = true,
				completion = true,
				hover = true,
			},
		},
	},

	-- Do NOT also configure rust_analyzer via nvim-lspconfig — conflicts.
	{
		"mrcjkb/rustaceanvim",
		version = "^5",
		ft = { "rust" },
		opts = {
			tools = {
				float_win_config = { border = "rounded" },
			},
			-- No DAP — debug in Zed or CLI if needed.
			dap = {
				adapter = false,
			},
			server = {
				on_attach = function(_, bufnr)
					vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })

					local map = function(lhs, rhs, desc)
						vim.keymap.set("n", lhs, rhs, { buffer = bufnr, desc = desc })
					end

					map("<leader>rr", function()
						vim.ui.input({ prompt = "Run args (empty = none): " }, function(input)
							if input == nil then
								return
							end
							local args = vim.split(input, " ", { trimempty = true })
							require("rustaceanvim.runnables").runnables(args)
						end)
					end, "Runnables")
					map("<leader>rt", function()
						vim.cmd.RustLsp("testables")
					end, "Testables")

					map("<leader>re", function()
						vim.cmd.RustLsp("expandMacro")
					end, "Expand Macro")
					map("<leader>rc", function()
						vim.cmd.RustLsp("openCargo")
					end, "Open Cargo.toml")
					map("<leader>rp", function()
						vim.cmd.RustLsp("parentModule")
					end, "Parent Module")
					map("<leader>rj", function()
						vim.cmd.RustLsp("joinLines")
					end, "Join Lines")

					vim.keymap.set({ "n", "v" }, "K", function()
						vim.cmd.RustLsp({ "hover", "actions" })
					end, { buffer = bufnr, desc = "Hover actions" })
					vim.keymap.set({ "n", "v" }, "<leader>ca", function()
						vim.cmd.RustLsp("codeAction")
					end, { buffer = bufnr, desc = "Code Action" })
				end,

				on_init = function(client, _)
					client.server_capabilities.semanticTokensProvider = nil
				end,

				default_settings = {
					["rust-analyzer"] = {
						cargo = {
							allFeatures = true,
						},
						check = {
							command = "clippy",
							features = "all",
							extraArgs = { "--no-deps" },
						},
						procMacro = {
							ignored = {
								["async-trait"] = { "async_trait" },
								["napi-derive"] = { "napi" },
								["async-recursion"] = { "async_recursion" },
							},
						},
						inlayHints = {
							closureReturnTypeHints = { enable = true },
							lifetimeElisionHints = {
								enable = "skip_trivial",
								useParameterNames = false,
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
