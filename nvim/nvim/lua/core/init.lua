local g, fn, env = vim.g, vim.fn, vim.env

g.cache_dir = env.XDG_CACHE_HOME or fn.expand("~/.cache/nvim")

-- Create cache dir and subs dir
local data_dir = {
  g.cache_dir .. "/backup",
  g.cache_dir .. "/session",
  g.cache_dir .. "/swap",
  g.cache_dir .. "/tags",
  g.cache_dir .. "/undo",
}
-- There only check once that If cache_dir exists
-- Then I don"t want to check subs dir exists
if fn.isdirectory(g.cache_dir) == 0 then
  os.execute("mkdir -p " .. g.cache_dir)
  for _, v in pairs(data_dir) do
    if fn.isdirectory(v) == 0 then
      os.execute("mkdir -p " .. v)
    end
  end
end

require("core.autocmds")
require("core.keymap")
require("core.options")
require("core.lazy")