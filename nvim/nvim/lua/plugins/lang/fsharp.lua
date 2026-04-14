if vim.fn.executable("dotnet") ~= 1 then
	return {}
end

return {
	-- Add F# filetype detection
	{
		"neovim/nvim-lspconfig",
		ft = { "fsharp" },
		init = function()
			vim.filetype.add({
				extension = { fs = "fsharp", fsx = "fsharp", fsi = "fsharp" },
			})
			vim.api.nvim_create_autocmd("FileType", {
				pattern = "fsharp",
				callback = function()
					vim.opt_local.updatetime = 300
				end,
			})
		end,
		opts = {
			servers = {
				fsautocomplete = {
					root_markers = { ".sln", ".fsproj", ".fsx", ".slnx" },
					init_options = {
						AutomaticWorkspaceInit = true,
					},
					on_init = function(client)
						-- fsautocomplete emits a large semantic token set for complex F# files
						-- (generic constraints, large record types with function-typed fields).
						-- Neovim's semantic token handler calls vim.str_utfindex() once per token
						-- in an O(tokens × file_size) loop, spinning nvim at 100% CPU.
						-- Nullify the provider here — before any buffer attaches — so Neovim
						-- never registers the capability. All other LSP features remain active.
						client.server_capabilities.semanticTokensProvider = nil
					end,
					settings = {
						FSharp = {
							EnableReferenceCodeLens = true,
							ExternalAutocomplete = false,
							InterfaceStubGeneration = true,
							InterfaceStubGenerationMethodBody = 'failwith "Not Implemented"',
							InterfaceStubGenerationObjectIdentifier = "this",
							Linter = true,
							RecordStubGeneration = true,
							RecordStubGenerationBody = 'failwith "Not Implemented"',
							ResolveNamespaces = true,
							SimplifyNameAnalyzer = true,
							UnionCaseStubGeneration = true,
							UnionCaseStubGenerationBody = 'failwith "Not Implemented"',
							UnusedDeclarationsAnalyzer = true,
							UnusedOpensAnalyzer = true,
							UseSdkScripts = true,
							keywordsAutocomplete = true,
							TooltipShowDocumentationLink = false,

							inlayHints = {
								enabled = true,
								parameterNames = true,
								typeAnnotations = false,
							},
						},
					},
				},
			},
		},
	},

	-- F# treesitter parser for syntax highlighting (semantic tokens disabled — see on_init)
	{
		"nvim-treesitter/nvim-treesitter",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "fsharp" })
			return opts
		end,
	},

	-- Conform (unchanged — LSP handles formatting)
	{
		"stevearc/conform.nvim",
		ft = { "fsharp" },
		opts = function(_, opts)
			opts.formatters = opts.formatters or {}
			opts.formatters.fantomas = {
				command = "dotnet",
				args = { "fantomas", "$FILENAME" },
				stdin = false,
			}
			opts.formatters_by_ft = opts.formatters_by_ft or {}
			opts.formatters_by_ft.fsharp = { "fantomas" }
		end,
	},

	-- DAP
	{
		"jay-babu/mason-nvim-dap.nvim",
		opts = function(_, opts)
			opts.ensure_installed = opts.ensure_installed or {}
			vim.list_extend(opts.ensure_installed, { "netcoredbg" })
			return opts
		end,
	},
}
