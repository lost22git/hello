local version = _VERSION:match("%d+%.%d+")

package.path = table.concat({
  'lua_modules/share/lua/' .. version .. '/?.lua',
  'lua_modules/share/lua/' .. version .. '/?/init.lua',
  package.path,
}, ';')

package.cpath = table.concat({
  'lua_modules/lib/lua/' .. version .. '/?.so',
  package.cpath,
}, ';')
