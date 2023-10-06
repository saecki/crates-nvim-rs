pub mod toml;

use mlua::prelude::{LuaResult, LuaTable};
use mlua::{Function, Lua};

#[mlua::lua_module]
pub fn libcrates_nvim(lua: &Lua) -> LuaResult<LuaTable> {
    let exports = lua.create_table()?;

    exports.set("parse_toml", lua.create_function(parse_toml)?)?;

    Ok(exports)
}

pub fn parse_toml(lua: &Lua, buf: f64) -> LuaResult<LuaTable> {
    let vim: LuaTable = lua.globals().get("vim")?;
    let vim_api: LuaTable = vim.get("api")?;
    let nvim_buf_get_text: Function = vim_api.get("nvim_buf_get_lines")?;
    let text: String = nvim_buf_get_text.call(buf)?;

    todo!()
}
