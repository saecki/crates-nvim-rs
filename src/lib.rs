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

    let _tokens = toml::parse(&text).unwrap_or_default();

    let lua_tokens = lua.create_table()?;
    //for (i,t) in tokens.into_iter().enumerate() {
    //    lua_tokens.set(i, t);
    //}

    Ok(lua_tokens)
}
