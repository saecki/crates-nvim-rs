local M = {}

local ns = vim.api.nvim_create_namespace("crates_nvim.diagnostics")

---@class VimDiagnostics
---@field errors VimDiagnostic[]
---@field warnings VimDiagnostic[]
---@field hints VimDiagnostic[]

---@class VimDiagnostic
---@field lnum integer
---@field end_lnum integer
---@field col integer
---@field end_col integer
---@field message string

---@param d VimDiagnostic
---@param severity integer
local function to_vim_diagnostic(d, severity)
    d.namespace = ns
    d.severity = severity
    return d
end

function M.check_toml()
    local crates_nvim = require("crates_nvim_lib")
    ---@type VimDiagnostics
    local diagnostics = crates_nvim.check_toml()
    local bufnr = vim.api.nvim_get_current_buf()
    local vim_diagnostics = {}
    for _, e in ipairs(diagnostics.errors) do
        local d = to_vim_diagnostic(e, vim.diagnostic.severity.ERROR)
        table.insert(vim_diagnostics, d)
    end
    for _, w in ipairs(diagnostics.warnings) do
        local d = to_vim_diagnostic(w, vim.diagnostic.severity.ERROR)
        table.insert(vim_diagnostics, d)
    end
    for _, h in ipairs(diagnostics.hints) do
        local d = to_vim_diagnostic(h, vim.diagnostic.severity.ERROR)
        table.insert(vim_diagnostics, d)
    end

    vim.diagnostic.set(ns, bufnr, vim_diagnostics, {})
end

return M
