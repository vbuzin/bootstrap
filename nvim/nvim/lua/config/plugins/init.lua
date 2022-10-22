local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
        vim.cmd([[packadd packer.nvim]])
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

return require("packer").startup({
    function()
        local p = function(name)
            return string.format("require'config.plugins.%s'", name)
        end

        use({ "nvim-lua/plenary.nvim" })
        use({ "wbthomason/packer.nvim" })
        use({ "lewis6991/impatient.nvim" })

        use({ "danilamihailov/beacon.nvim", config = p("beacon") })
        use({ "folke/which-key.nvim", config = p("whichkey") })
        use({ "ggandor/leap.nvim", requires = "tpope/vim-repeat", config = p("leap") })
        use({ "hrsh7th/nvim-cmp", config = p("nvimcmp") })
        use({ "hrsh7th/cmp-nvim-lsp" })
        use({ "hrsh7th/cmp-buffer" })
        use({ "hrsh7th/cmp-path" })
        use({ "hrsh7th/cmp-cmdline" })
        use({ "kylechui/nvim-surround", tag = "*", config = p("surround") })
        use({ "lewis6991/gitsigns.nvim", config = p("gitsigns") })
        use({ "navarasu/onedark.nvim", as = "onedark", config = p("onedark") })
        use({ "numToStr/Comment.nvim", config = p("comment") })
        use({ "windwp/nvim-autopairs", config = p("autopairs") })
        use({ "L3MON4D3/LuaSnip" })
        use({
            "nvim-lualine/lualine.nvim",
            config = p("lualine"),
            after = "onedark",
            requires = { "kyazdani42/nvim-web-devicons", opt = true },
        })

        local telescope_req = { "nvim-telescope/telescope-file-browser.nvim" }
        use({ "nvim-telescope/telescope.nvim", branch = "0.1.x", config = p("telescope"), requires = telescope_req })

        local treesitter_mods = { "p00f/nvim-ts-rainbow", "nvim-treesitter/nvim-treesitter-textobjects" }
        use({
            "nvim-treesitter/nvim-treesitter",
            config = p("treesitter"),
            requires = treesitter_mods,
            run = ":TSUpdate",
        })
    end,
})
