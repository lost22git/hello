local fm = require('fullmoon')

----------------
-- Api Result --
----------------

ApiResult = {}
-- api result: OK
function ApiResult.ok(data)
  return { code = 0, msg = 'OK', data = data }
end

-- api result: Validation error
function ApiResult.validation_error(error)
  return { code = 1400, msg = 'Validation error', data = error }
end

-- api result: Invalid username or password
function ApiResult.invalid_username_password()
  return { code = 1401, msg = 'Invalid username or password' }
end

----------
-- Hash --
----------

-- hash encode password with salt
local function password_hashed(pass, salt)
  return argon2.hash_encoded(pass, salt
  -- ,
  -- {
  --   variant = argon2.variants.argon2_i,
  --   m_cost = 65536,
  --   hash_len = 24,
  --   parallelism = 4,
  --   t_cost = 2,
  -- }
  )
end

-- verify password with hash
local function password_verified(pass, hash)
  return argon2.verify(hash, pass)
end


--------------------
-- database setup --
--------------------

local init_sql = [[
    DROP TABLE IF EXISTS USER;
    CREATE TABLE IF NOT EXISTS USER(
      id VARCHAR PRIMARY KEY,
      username VARCHAR NOT NULL,
      password_hashed VARCHAR NOT NULL
    );
    CREATE UNIQUE INDEX IF NOT EXISTS UK_USERNAME ON USER (username);
    INSERT OR IGNORE INTO USER(id, username, password_hashed) VALUES ('0', 'admin', '$argon2id$v=19$m=4096,t=3,p=1$Y2ZkZmFiODMtMmViOC00MmY4LWIxN2MtYzA1NDMxOGVlNmJk$g2MdPYNkIyFEhbjwHMBtCxBNsqmIDDtcg25yznarLwo');
]]
local db = fm.makeStorage("./test.db", init_sql)

-----------------
-- User Domain --
-----------------

local User = {}

function User:new(o)
  o = o or {}
  setmetatable(o, self)
  self.__index = self
  return o
end

-- verify user
function User:verify(password)
  return password_verified(password, self.password_hashed)
end

-- get user by username
function User:get_by_username(username)
  local sql = [[SELECT * FROM USER WHERE username = ?]]
  local row = assert(db:fetchOne(sql, username))
  if not row then return nil end
  return User:new(row)
end

-- get all users
function User:get_all()
  local sql = [[SELECT * FROM USER]]
  local rows = assert(db:fetchAll(sql))
  local result = {}
  fm.logInfo('[User:get_all] rows: ' .. EncodeJson(rows))
  for _, row in ipairs(rows) do
    table.insert(result, User:new(row))
  end
  return result
end

function User:save()
  local sql = [[INSERT INTO USER(id, username, password_hashed) VALUES (?,?,?)]]
  assert(db:execute('BEGIN'))
  assert(db:execute(sql, self.id, self.username, self.password_hashed))
  assert(db:execute('COMMIT'))
end

-----------------
-- Validations --
-----------------

-- define a validator to validate login param
local valiate_login_param = fm.makeValidator({
  { 'username', minlen = 4, maxlen = 20, msg = 'Invalid %s format' },
  { 'password', minlen = 4, maxlen = 20, msg = 'Invalid %s format' },
  all = true,                 -- validate all rules and get errors
  key = true,
  otherwise = function(error) -- handle validation error
    return fm.serveContent('json', ApiResult.validation_error(error))
  end
})

-- define a validator to validate user add param
local valiate_user_addj_param = fm.makeValidator({
  { 'username', minlen = 4, maxlen = 20, msg = 'Invalid %s format' },
  { 'password', minlen = 4, maxlen = 20, msg = 'Invalid %s format' },
  all = true,                 -- validate all rules and get errors
  key = true,
  otherwise = function(error) -- handle validation error
    return fm.serveContent('json', ApiResult.validation_error(error))
  end
})



------------
-- Routes --
------------


-- login route
fm.setRoute({ '/login', method = 'POST', _ = valiate_login_param }, function(r)
  local username = r.params.username
  local password = r.params.password
  fm.logInfo('[login] param: ' .. EncodeJson({ username = username, password = password }))
  local user = User:get_by_username(username)
  if not user then
    return fm.serveContent('json', ApiResult.invalid_username_password())
  end
  fm.logInfo('[login] found user: ' .. EncodeJson(user))
  if not user:verify(password) then
    return fm.serveContent('json', ApiResult.invalid_username_password())
  end
  return fm.serveContent('json', ApiResult.ok({ id = user.id }))
end)

-- user list route
fm.setRoute({ '/user', method = 'GET' }, function(r)
  return fm.serveContent('json', ApiResult.ok(User:get_all()))
end)

-- user add route
fm.setRoute({ '/user', method = 'POST', _ = vaidate_user_add_param }, function(r)
  local username = r.params.username
  local password = r.params.password
  fm.logInfo('[user add] param: ' .. EncodeJson({ username = username, password = password }))
  local user = User:new({ id = UuidV7(), username = username, password_hashed = password_hashed(password, UuidV4()) })
  user:save()
  return fm.serveContent('json', ApiResult.ok({ id = user.id }))
end)

-- add /proxy?target=http(s)://* route to http(s)?://*
fm.setRoute({ '/proxy', method = { 'GET', 'POST' } }, function(r)
  fm.logInfo('[proxy]...')
  fm.logInfo('[proxy] fetching [%s] %s' % { r.method, r.params.target })

  local status, headers, body = Fetch(r.params.target, { method = r.method, body = r.body, headers = r.headers })
  return fm.serveResponse(status, headers, body)
end)

-- start server
fm.run()
