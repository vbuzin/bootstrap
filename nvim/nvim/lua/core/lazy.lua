local fn, opt = vim.fn, vim.opt
local lazypath = fn.stdpath("data").."/lazy/lazy.nvim"

if fn.empty(fn.glob(lazypath)) > 0 then
    local out = fn.system({"git", "clone", "--depth", "1", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", lazypath})
    if vim.v.shell_error ~= 0 then
      vim.api.nvim_echo({
        { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
        { out, "WarningMsg" },
        { "\nPress any key to exit..." },
      }, true, {})
      fn.getchar()
      os.exit(1)
    end
end

opt.rtp:prepend(lazypath)

require("lazy").setup {
  spec = {
    -- import plugins
    { import = "plugins" },
    { import = "plugins.lang" },
  },
  lockfile = fn.stdpath("data") .. "/lazy/lazy-lock.json",

  ui = {
    size = { width = 0.94, height = 0.94 },
    border = "single",
    backdrop = 100,
  },
  checker = { enabled = true } -- automatically check for plugin updates
}
