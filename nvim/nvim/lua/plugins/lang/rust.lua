if vim.fn.executable("cargo") ~= 1 and vim.fn.executable("rustc") ~= 1 then
	return {}
end

-- CodeLLDB ships as a VSIX: adapter + liblldb must stay under extension/
-- (a bare symlink into tools/bin breaks relative liblldb lookup → exit 101).
local codelldb_ext = vim.fn.stdpath("data") .. "/tools/codelldb/extension"
local codelldb_path = codelldb_ext .. "/adapter/codelldb"
local liblldb_path = codelldb_ext .. "/lldb/lib/liblldb.dylib"

local rustacean_opts = {
	tools = {
		float_win_config = { border = "rounded" },
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
			map("<leader>rd", function()
				vim.ui.input({ prompt = "Debug args (empty = none): " }, function(input)
					if input == nil then
						return
					end
					local args = vim.split(input, " ", { trimempty = true })
					require("rustaceanvim.commands.debuggables").debuggables(args)
				end)
			end, "Debuggables")

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

		-- Prefer treesitter highlighting over rust-analyzer semantic tokens.
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
}

if vim.fn.executable(codelldb_path) == 1 and vim.uv.fs_stat(liblldb_path) then
	-- Same shape as rustaceanvim.config.get_codelldb_adapter (Mason layout).
	rustacean_opts.dap = {
		adapter = {
			type = "server",
			port = "${port}",
			host = "127.0.0.1",
			executable = {
				command = codelldb_path,
				args = { "--liblldb", liblldb_path, "--port", "${port}" },
			},
		},
	}
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
		opts = rustacean_opts,
		config = function(_, opts)
			vim.g.rustaceanvim = vim.tbl_deep_extend("keep", vim.g.rustaceanvim or {}, opts)
		end,
	},
}
