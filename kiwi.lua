--[[
Copyright (c) 2016 Alexander Matz a.matz.1988@gmail.com

This software is provided 'as-is', without any express or implied
warranty.  In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
claim that you wrote the original software. If you use this software
in a product, an acknowledgment in the product documentation would be
appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be
misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.
--]]

--[[
Lua implementation of the Kiwi constraint solver at:
https://github.com/nucleic/kiwi/tree/master/kiwi

Implementation notes:
 - no enums etc. strings are interned, so string comparisons are FAST
 - no context, no clue what it was for anyway
 - no overloaded operators for arithmetics (there's a frontend)
 - no overloaded functions
 - no getters/setters
 - no extra datatype for shared data (everything is shared anyway)
 - no exceptions, error handling via assertions (for grave errors) and
    return values
--]]

local kiwi = {}

--------------------------------------------------------------------------------
-- utils
local function near_zero(val)
  if val < 0.0 then
    return -val < 1e-8
  else
    return val < 1e-8
  end
end

local function istype(obj, _type)
  return type(obj) == 'table' and obj._type == _type
end

--------------------------------------------------------------------------------
-- variable

local variable = {_type = "variable"}
do
  local variable_mt = {}
  setmetatable(variable, variable_mt)
  variable_mt.__index = variable

  function variable_mt.__tostring(self)
    return "["..self.name..":"..self.value.."]"
  end

  function variable_mt.__call(self, name)
    local new = setmetatable({}, variable_mt)
    new.name = name
    new.value = 0
    return new
  end
end

--------------------------------------------------------------------------------
-- term

local term = {_type = "term"}
do
  local term_mt = {}
  setmetatable(term, term_mt)
  term_mt.__index = term
  function term_mt.__tostring(self)
    return tostring(self.coefficient).." "..tostring(self.variable.name)
  end

  function term_mt.__call(self, var, coefficient)
    assert(variable ~= nil and variable._type == 'variable', 'term():1 ~= variable')
    assert(type(coefficient) == 'number', 'term():2 ~= number')
    local new = setmetatable({}, term_mt)
    new.variable = var
    new.coefficient = coefficient
    return new
  end

  function term.value(self)
    return self.coefficient * self.variable.value
  end
end

--------------------------------------------------------------------------------
-- symbol

local symbol = {_type = "symbol"}
do
  local symbol_types = { ['invalid'] = true, ['external'] = true,
    ['slack'] = true, ['error'] = true, ['dummy'] = true }
  local symbol_mt = {}
  setmetatable(symbol, symbol_mt)
  symbol_mt.__index = symbol
  function symbol_mt.__tostring(self)
    return "(symbol, "..self.id..", "..self.stype..")"
  end

  function symbol_mt.__call(self, stype, id)
    assert(symbol_types[stype] ~= nil, 'symbol():1 invalid symbol')
    assert(type(id) == 'number', 'symbol():2 ~= number')
    local new = setmetatable({}, symbol_mt)
    new.id = id
    new.stype = stype
    return new
  end
end

--------------------------------------------------------------------------------
-- strength

local strength = {}
function strength.create(a, b, c, w)
  w = w or 1
  assert(type(a) == 'number' and type(b) == 'number' and type(c) == 'number'
    and type(w) == 'number', "strength.create():? ~= number")
  local result = 0
  result = result + math.max(0, math.min(1e3, a * w)) * 1e6
  result = result + math.max(0, math.min(1e3, b * w)) * 1e3
  result = result + math.max(0, math.min(1e3, b * w))
  return result
end
function strength.clip(val)
  return math.max(0, math.min(strength.required, val))
end
strength.required = strength.create(1000, 1000, 1000)
strength.strong = strength.create(1, 0, 0)
strength.medium = strength.create(0, 1, 0)
strength.weak = strength.create(0, 0, 1)

--------------------------------------------------------------------------------
-- expression

local expression = {_type = "expression"}
do
  local expression_mt = {}
  setmetatable(expression, expression_mt)
  expression_mt.__index = expression
  function expression_mt.__tostring(self)
    local tmp = {tostring(self.constant)}
    for k, term in ipairs(self.terms) do
      tmp[#tmp+1] = ' + '
      tmp[#tmp+1] = tostring(term)
    end
    return table.concat(tmp)
  end

  function expression_mt.__call(self, terms, constant)
    constant = constant or 0
    assert(type(terms) == 'table' and #terms > 0, 'expression():1 ~= {term, ...}')
    for k, v in ipairs(terms) do
      assert(type(v) == 'table' and v._type == 'term', 'expression():1:k ~= term')
    end
    assert(type(constant) == 'number', 'expression():2 ~= number')
    local new = setmetatable({}, expression_mt)
    new.constant = constant
    new.terms = terms
    return new
  end

  function expression.value(self)
    local result = self.constant
    for i, term in ipairs(self.terms) do
      result = result + term:value()
    end
    return result
  end
end

--------------------------------------------------------------------------------
-- constraint

local constraint = {_type = "constraint"}
do
  rel_ops = { ['=='] = true, ['<='] = true, ['>='] = true}
  local constraint_mt = {}
  setmetatable(constraint, constraint_mt)
  constraint_mt.__index = constraint

  function constraint_mt.__tostring(self)
    return tostring(self.expression) .. ' ' .. self.op .. ' 0 (' .. tostring(self.strength) ..')'
  end

  function constraint_mt.__call(self, expr, op, str)
    str = str or strength.required
    assert(type(expr) == 'table' and expr._type == 'expression', 'constraint():1 ~= expression')
    assert(rel_ops[op] ~= nil, 'constraint():2 invalid relational operator: '..op)
    assert(type(str) == 'number', 'constraint():3 ~= number')
    local new = setmetatable({}, constraint_mt)
    new.expression = expr
    new.op = op
    new.strength = strength.clip(str)
    return new
  end

  function constraint.reduce(expr)
    assert(type(expr) == 'table' and expr._type == 'expression', 'constraint:reduce():1 ~= expression')
    -- add up the coefficients of all unique variables
    local coeffs = {}
    for k, term in ipairs(expr.terms) do
      local var = term.variable
      coeffs[var] = (coeffs[var] or 0) + term.coefficient
    end
    -- generate new terms for expression
    local terms = {}
    for var, coeff in pairs(coeffs) do
      terms[#terms+1] = term(var, coeff)
    end
    return expression(terms, expr.constant)
  end
end

--------------------------------------------------------------------------------
-- row

local row = {_type = "row"}
do
  local row_mt = {}
  setmetatable(row, row_mt)
  row_mt.__index = row
  function row_mt.__tostring(self)
    local tmp = {}
    tmp[#tmp+1] = 'row: '
    tmp[#tmp+1] = tostring(self.constant) .. ' + '
    for k, v in pairs(self.cells) do
      tmp[#tmp+1] = tostring(v) .. ' (' .. tostring(k.id) .. ')'
      tmp[#tmp+1] = ' + '
    end
    tmp[#tmp] = nil
    return table.concat(tmp)
  end

  function row_mt.__call(self, other_or_const)
    other_or_const = other_or_const or 0
    assert(type(other_or_const) == 'number' or
      (type(other_or_const) == 'table' and other_or_const._type == 'row'),
      'row():1 ~= number and ~= row')
    local new = setmetatable({}, row_mt)
    -- cells: symbol -> coefficient
    if type(other_or_const) == 'number' then
      new.constant = other_or_const
      new.cells = {}
    else
      new.constant = other_or_const.constant
      new.cells = {}
      -- we need actual copy of other row
      for k, v in pairs(other_or_const.cells) do
        new.cells[k] = v
      end
    end
    return new
  end

  function row.add(self, value)
    assert(type(value) == 'number')
    self.constant = self.constant + value
    return self.constant
  end

  function row.insert_symbol(self, symbol, coefficient)
    coefficient = coefficient or 1
    assert(istype(symbol, 'symbol'), 'row:insert():1 ~= symbol')
    assert(type(coefficient) == 'number', 'row:insert():2 ~= number')
    -- add new symbol or, if it already exists, add coefficient
    self.cells[symbol] = (self.cells[symbol] or 0) + coefficient
    -- if coefficient is near zero, delete symbol
    if near_zero(self.cells[symbol]) then
      self.cells[symbol] = nil
    end
  end

  function row.insert_row(self, other, coefficient)
    coefficient = coefficient or 1
    assert(istype(symbol, 'symbol'), 'row:insert():1 ~= symbol')
    assert(type(coefficient) == 'number', 'row:insert():2 ~= number')
    -- add or insert all cells of thers (multiplied by coefficient)
    self.constant = self.constant + other.constant * coefficient
    for sym, coeff in pairs(other.cells) do
      self.cells[sym] = (self.cells[sym] or 0) + coeff * coefficient
      -- delete if near zero
      if near_zero(self.cells[sym]) then
        self.cells[sym] = nil
      end
    end
  end

  function row.remove(self, symbol)
    assert(istype(symbol, 'symbol'), 'row:remove():1 ~= symbol')
    self.cells[symbol] = nil
  end

  function row.reverse_sign(self)
    self.constant = -self.constant
    for k, v in pairs(self.cells) do
      self.cells[k] = -v
    end
  end

  function row.solve_for(self, symbol)
    assert(istype(symbol, 'symbol'), 'row:solve_for():1 ~= symbol')
    local coeff = -1 / self.cells[symbol]
    self.cells[symbol] = nil
    self.constant = self.constant * coeff
    for k, v in pairs(self.cells) do
      self.cells[k] = self.cells[k] * coeff
    end
  end

  function row.solve_for2(self, lhs, rhs)
    assert(istype(lhs, 'symbol'), 'row:solve_for2():1 ~= symbol')
    assert(istype(rhs, 'symbol'), 'row:solve_for2():2 ~= symbol')
    self:insert_symbol(lhs, -1)
    self:solve_for(rhs)
  end

  -- coefficient_for -> (row.cells[symbol] or 0)

  function row.substitute(self, symbol, other)
    assert(istype(symbol, 'symbol'), 'row:substitute():1 ~= symbol')
    assert(istype(other, 'row'), 'row:stubstitute():2 ~= row')
    if self.cells[symbol] ~= nil then
      local coeff = self.cells[symbol]
      self.cells[symbol] = nil
      self:insert_row(other, coeff)
    end
  end
end

--------------------------------------------------------------------------------
-- solver
-- this is where all the magic happens

local solver = {_type = "solver"}
do
  local solver_mt = {}
  setmetatable(solver, solver_mt)
  solver_mt.__index = solver
  function solver_mt.__tostring(self)
    return 'solver'
  end

  function solver_mt.__call(self)
    other_or_const = other_or_const or 0
    local new = setmetatable({}, solver_mt)
    new.objective = row()
    new.id_tick = 1
    new.cns = {}             -- constraint -> tag
    new.rows = {}            -- symbol -> row
    new.vars = {}            -- variable -> symbol
    new.edits = {}           -- variable -> edit_info
    new.infeasible_rows = {} -- symbol -> row
    new.artificial = false   -- nil would result in gettable loop
    return new
  end

  function solver.add_constraint(self, constr)
    assert(istype(constr, 'constraint'), 'solver:add_constraint():1 ~= constraint')
    assert(self.cns[constr] == nil, 'solver:add_constraint() duplicate constraint')
    -- not entirely sure what this does
    local tag = {}
    tag.marker = symbol('invalid', 0)
    tag.other = symbol('invalid', 0)
    local row = self:create_row(constr, tag)
    local subject = self:choose_subject(row, tag)
    if subject.stype == 'invalid' and self:all_dummies(row) then
      assert(near_zero(row.constant), 'solver:add_constraint() unsatisfiable constraint')
      subject = tag.marker
    end
    if subject.stype == 'invalid' then
      local success = self:add_with_artificial_variable(row)
      assert(success, 'solver:add_constraint() unsatisfiable constraint')
    else
      row:solve_for(subject)
      self:substitute(subject, row)
      self.rows[subject] = row
    end
    self.cns[constr] = tag
    self:optimize(self.objective)
  end

  function solver.remove_constraint(self, constr)
    assert(istype(constr, 'constraint'), 'solver:remove_constraint():1 ~= constraint')
    assert(self.cns[constr] ~= nil, 'solver:remove_constraint() unknown constraint')
    local tag = self.cns[constr]
    self.cns[constr] = nil
    self:remove_constraint_effects(constr, tag)
    if self.rows[tag.marker] ~= nil then
      self.rows[tag.marker] = nil
    else
      -- returns (key, value) of leaving row in self.rows
      local leaving, row = self:get_marker_leaving_row(tag.marker)
      assert(subject ~= nil, 'solver:remove_constraint() [internal] failed to find leaving row')
      self.rows[leaving] = nil
      row:solve_for2(leaving, tag.marker)
      self:substitute(tag.marker, row)
    end
    self:optimize(self.objective)
  end

  function solver.has_constraint(self, constr)
    assert(istype(constr, 'constraint'), 'solver:has_constraint():1 ~= constraint')
    return self.cns[constr] ~= nil
  end

  function solver.add_edit_variable(self, variable, str)
    assert(istype(variable, 'variable'), 'solver:add_edit_variable():1 ~= variable')
    assert(self.edits[variable] == nil, 'solver:add_edit_variable() duplicate edit variable')
    str = strength.clip(str)
    assert(str ~= strength.required, 'solver:add_edit_variable() bad required strength')
    local cn = constraint(expression({term(variable, 1)}, 0), '==', str)
    self:add_constraint(cn)
    local info = {}
    info.tag = self.cns[cn]
    info.constraint = cn
    info.constant = 0
    self.edits[variable] = info
  end

  function solver.remove_edit_variable(self, variable)
    assert(istype(variable, 'variable'), 'solver:remove_edit_variable():1 ~= variable')
    assert(self.edits[variable] ~= nil, 'solver:remove_edit_variable() unknown edit variable')
    self:remove_constraint(self.edits[variable].constraint)
    self.edits[variable] = nil
  end

  function solver.has_edit_variable(self, variable)
    assert(istype(variable, 'variable'), 'solver:has_edit_variable():1 ~= variable')
    return self.edits[variable] ~= nil
  end

  -- we don't have the "dual optimized guard" (deferred call), so we just
  -- cover all exits
  function solver.suggest_value(self, variable, value)
    assert(istype(variable, 'variable'), 'solver:suggest_value():1 ~= variable')
    assert(type(value) == 'number', 'solver:suggest_value():2 ~= number')
    assert(self.edits[variable] ~= nil, 'solver:suggest_value() unknown edit variable')

    -- update edit info
    local info = self.edits[variable]
    local delta = value - info.constant
    info.constant = value
    
    local row = self.rows[info.tag.marker]
    if row ~= nil then
      if row:add(-delta) < 0 then
        self.infeasible_rows[#self.infeasible_rows+1] = info.tag.marker
      end
      self:dual_optimize()
      return
    end
    row = self.rows[info.tag.other]
    if row ~= nil then
      if row:add(delta) < 0 then
        self.infeasible_rows[#self.infeasible_rows+1] = info.tag.other
      end
      self:dual_optimize()
      return
    end

    for sym, row in pairs(self.rows) do
      local coeff = row.cells[info.tag.marker]
      if coeff ~= nil and row:add(delta * coeff) < 0 and sym.stype ~= 'external' then
        self.infeasible_rows[#self.infeasible_rows+1] = sym
      end
    end
    self:dual_optimize()
  end

  function solver.update_variables(self)
    for var, sym in pairs(self.vars) do
      local row = self.rows[sym]
      if row == nil then
        var.value = 0
      else
        -- we set close to zero to zero (for compare to zero)
        if near_zero(row.constant) then
          var.value = 0
        else
          var.value = row.constant
        end
      end
    end
  end

  function solver.reset(self)
    self.objective = row()
    self.id_tick = 1
    self.cns = {}             -- constraint -> tag
    self.rows = {}            -- symbol -> row
    self.vars = {}            -- variable -> symbol
    self.edits = {}           -- variable -> edit_info
    self.infeasible_rows = {} -- symbol -> row
    self.artificial = false
  end

  function solver.get_var_symbol(self, var)
    assert(istype(var, 'variable'), 'solver:get_var_symbol():1 ~= variable')
    if self.vars[var] ~= nil then
      return self.vars[var]
    else
      local sym = symbol('external', self:tick())
      self.vars[var] = sym
      return sym
    end
  end

  function solver.tick(self)
    self.id_tick = self.id_tick + 1
    return self.id_tick
  end

  function solver.create_row(self, constr, tag)
    assert(istype(constr, 'constraint'), 'solver:create_row():1 ~= constraint')
    assert(type(tag) == 'table', 'solver:create_row():2 ~= table')
    local expr = constr.expression
    local _row = row(expr.constant)

    -- substitute basic variables into new row
    for _, term in ipairs(expr.terms) do
      if not near_zero(term.coefficient) then
        local sym = self:get_var_symbol(term.variable)
        local _row2 = self.rows[sym]
        if _row2 ~= nil then
          _row:insert_row(_row2, term.coefficient)
        else
          _row:insert_symbol(sym, term.coefficient)
        end
      end
    end

    -- add necessary auxiliary variables
    if constr.op == '<=' or constr.op == '>=' then
      local coeff = constr.op == '<=' and 1 or -1
      local slack = symbol('slack', self:tick())
      tag.marker = slack
      _row:insert_symbol(slack, coeff)
      if constr.strength < strength.required then
        local err = symbol('error', self:tick())
        tag.other = err
        _row:insert_symbol(err, -coeff)
        self.objective:insert_symbol(err, constr.strength)
      end
    elseif constr.op == '==' then
      if constr.strength < strength.required then
        local errplus = symbol('error', self:tick())
        local errminus = symbol('error', self:tick())
        tag.marker = errplus
        tag.other = errminus
        _row:insert_symbol(errplus, -1)
        _row:insert_symbol(errminus, 1)
        self.objective:insert_symbol(errplus, constr.strength)
        self.objective:insert_symbol(errminus, constr.strength)
      else
        local dummy = symbol('dummy', self:tick())
        tag.marker = dummy
        _row:insert_symbol(dummy)
      end
    end

    -- ensure positive constant
    if _row.constant < 0 then
      _row:reverse_sign()
    end
    return _row
  end

  function solver.choose_subject(self, row, tag)
    assert(istype(row, 'row'), 'solver:choose_subject():1 ~= row')
    assert(type(tag) == 'table', 'solver:choose_subject():2 ~= table')
    for sym, cell in pairs(row.cells) do
      if sym.stype == 'external' then
        return sym
      end
    end
    if tag.marker.stype == 'slack' or tag.marker.stype == 'error' then
      if (row.cells[tag.marker] or 0) < 0 then
        return tag.marker
      end
    end
    if tag.other.stype == 'slack' or tag.other.stype == 'error' then
      if (row.cells[tag.other] or 0) < 0 then
        return tag.other
      end
    end
    return symbol('invalid', 0)
  end

  function solver.add_with_artificial_variable(self, _row)
    assert(istype(row, 'row'), 'solver:add_with_artificial_variable():1 ~= row')
    local art = symbol('slack', self:tick())
    self.rows[art] = row(_row)
    self.artificial = row(_row)

    self:optimize(self.artificial)
    local success = near_zero(self.artificial.constant)
    self.artificial = false

    local _row2 = self.rows[art]
    if _row2 ~= nil then
      self.rows[art] = nil
      if #_row2.cells == 0 then
        return success
      end
      local entering = self:any_pivotable_symbol(_row2)
      if entering.stype == 'invalid' then
        return false
      end
      _row2:solve_for2(art, entering)
      self:substitute(entering, _row2)
      self.rows[entering] = _row2
    end

    for _, _row2 in pairs(self.rows) do
      _row:remove(art)
    end
    self.objective:remove(art)
    return success
  end

  function solver.substitute(self, sym, _row)
    assert(istype(sym, 'symbol'), 'solver:substitute():1 ~= symbol')
    assert(istype(_row, 'row'), 'solver:substitute():2 ~= row')
    for sym2, _row2 in pairs(self.rows) do
      _row2:substitute(sym, _row)
      if sym2.stype == 'external' and _row2.constant < 0 then
        self.infeasible_rows[#self.infeasible_rows] = sym2
      end
    end
    self.objective:substitute(sym, _row)
    if self.artificial ~= false then
      self.artificial:substitute(sym, _row)
    end
  end

  function solver.optimize(self, objective)
    assert(istype(objective, 'row'), 'solver:optimize():1 ~= row')
    while true do
      local entering = self:get_entering_symbol(objective)
      if entering.stype == 'invalid' then
        return
      end
      local leaving, row = self:get_leaving_row(entering)
      assert(leaving ~= nil and row ~= nil, 'solver:optimize() objective is unbounded')
      self.rows[leaving] = nil
      row:solve_for2(leaving, entering)
      self:substitute(entering, row)
      self.rows[entering] = row
    end
  end

  function solver.dual_optimize(self)
    while #self.infeasible_rows > 0 do
      local leaving = self.infeasible_rows[#self.infeasible_rows]
      self.infeasible_rows[#self.infeasible_rows] = nil
      local row = self.rows[leaving]
      if row ~= nil and row.constant < 0 then
        local entering = self:get_dual_entering_symbol(row)
        assert(entering.stype ~= 'invalid', 'solver:dual_optimize() failed')
        self.rows[leaving] = nil
        row:solve_for2(leaving, entering)
        self:substitute(entering, row)
        self.rows[entering] = row
      end
    end
  end

  function solver.get_entering_symbol(self, objective)
    assert(istype(objective, 'row'), 'solver:get_entering_symbol():1 ~= row')
    for sym, coeff in pairs(objective.cells) do
      if sym.stype ~= 'dummy' and coeff < 0 then
        return sym
      end
    end
    return symbol('invalid', 0)
  end

  function solver.get_dual_entering_symbol(self, _row)
    assert(istype(_row, 'row'), 'solver:get_dual_entering_symbol():1 ~= row')
    local entering
    -- start with maximum value -> inf
    local ratio = 1/0
    for sym, coeff in pairs(_row.cells) do
      if coeff > 0 and sym.stype ~= 'dummy' then
        local _coeff2 = self.objective.cells[sym] or 0
        local r = _coeff2 / coeff
        if r < ratio then
          ratio = r
          entering = sym
        end
      end
    end
    return entering
  end

  function solver.any_pivotable_symbol(self, _row)
    assert(istype(_row, 'row'), 'solver:any_pivotable_symbol():1 ~= row')
    for sym, coeff in pairs(_row.cells) do
      if sym.stype == 'slack' or sym.stype == 'error' then
        return sym
      end
    end
    return symbol('invalid', 0)
  end

  -- returns pair: symbol, row
  function solver.get_leaving_row(self, entering)
    assert(istype(entering, 'symbol'), 'solver:get_leaving_row():1 ~= symbol')
    local ratio = 1/0 -- again, no DBL_MAX in lua, so we just use inf
    local rsym, rrow = nil, nil
    for isym, irow in pairs(self.rows) do
      if isym.stype ~= 'external' then
        local tmp = irow.cells[entering] or 0
        if tmp < 0 then
          local tmp_ratio = irow.constant / tmp
          if tmp_ratio < ratio then
            ratio = tmp_ratio
            rsym, rrow = isym, irow
          end
        end
      end
    end
    return rsym, rrow
  end

  -- returns pair: symbol, row
  function solver.get_marker_leaving_row(self, marker)
    assert(istype(entering, 'symbol'), 'solver:get_marker_leaving_row():1 ~= symbol')
    local r1, r2 = 1/0, 1/0
    local sfst, rfst
    local ssec, rsec
    local sthr, rthr
    for sym, row in pairs(self.rows) do
      local c = row.cells[marker] or 0
      if c ~= 0 then
        if sym.stype == 'external' then
          sthr, rthr = sym, row
        elseif c < 0 then
          local r = - row.constant/c
          if r < r1 then
            r1 = r
            sfst, rfst = sym, row
          end
        else
          local r = row.constant/c
          if r < r2 then
            r2 = r
            ssec, rsec = sym, row
          end
        end
      end
    end
    if sfst ~= nil and rsft ~= nil then
      return sfst, rsft
    end
    if ssec ~= nil and rsec ~= nil then
      return ssec, rsec
    end
    return sthr, rthr
  end

  function solver.remove_constraint_effects(self, cn, tag)
    assert(istype(cn, 'constraint'), 'solver:remove_constraint_effects():1 ~= constraint')
    assert(type(tag) == 'table', 'solver:remove_constraint_effects():2 ~= table')
    if tag.marker.stype == 'error' then
      self:remove_marker_effects(tag.marker, cn.strength)
    end
    if tag.other.stype == 'error' then
      self:remove_marker_effects(tag.other, cn.strength)
    end
  end

  function solver.remove_marker_effects(self, marker, str)
    assert(istype(marker, 'symbol'), 'solver:remove_marker_effects():1 ~= symbol')
    assert(type(str) == 'number', 'solver:remove_marker_effects():2 ~= number')
    local row = self.rows[marker]
    if row ~= nil then
      self.objective:insert_row(row, -str)
    else
      self.objective:insert_symbol(marker, -str)
    end
  end

  function solver.all_dummies(self, _row)
    assert(istype(_row, 'row'), 'solver:all_dummies():1 ~= row')
    for _sym, _coeff in pairs(_row.cells) do
      if _sym.stype ~= 'dummy' then
        return false
      end
    end
    return true
  end
end

--------------------------------------------------------------------------------
-- frontend

-- string, {variable, ...}, string -> constraint
-- examples:
--   1 + x1 >= 2 x3 - x5 #strong
--   x >= 0  (strength defaults to required)
local function parse_constraint(vars, str)
  -- split string into tokens by whitespace
  local parts = {}
  str:gsub("([^ \t\f]+)", function (c) parts[#parts+1] = c end)
  parts[#parts+1] = 'eof'
  
  local strngth = strength.required
  -- check for strength component
  if #parts >=2 and parts[#parts-1]:sub(1,1) == '#' then
    local tmp = tonumber(strength[parts[#parts-1]:sub(2,-1)])
    assert(tmp ~= nil, 'invalid value for strength')
    strngth = tmp
    parts[#parts-1] = 'eof'
    parts[#parts] = nil
  end

  local lhs, op, rhs = {}, 'ERROR', {}
  -- find op and divide into lhs and rhs
  for i, part in ipairs(parts) do
    if part == '>=' or part == '<=' or part == '==' then
      op = part
    else
      -- haven't encountered operator yet -> lhs
      if op == 'ERROR' then
        lhs[#lhs+1] = part
      else
        rhs[#rhs+1] = part
      end
    end
  end
  assert(op ~= 'ERROR', 'No operator found')

  -- collect all terms and adjust signs for lhs/rhs
  local tmp = {sign = 1, coeff = 1}
  local terms = {}
  local constant = 0
  local op = false
  local state = 'coeff_or_var'
  -- sign meaning: expr1 == expr2 -> constraint(expr1 - expr2, ==)
  -- -> constraints have form expr == 0
  local lhs_sign = 1
  local i = 1
  while i <= #parts and state ~= 'done' do
    local part = parts[i]

    if state == 'coeff_or_var' then
      if tonumber(part) ~= nil then
        tmp.coeff = tonumber(part)
        i = i + 1
        state = 'var_or_end'
      elseif part == '+' or part == '-' or part == '<=' or part == '>=' or part == '==' then
        error('[coeff_or_var] invalid token #'..tostring(i)..':'..part)
      elseif vars[part] ~= nil then
        terms[#terms+1] = {vars[part], lhs_sign * tmp.sign * tmp.coeff}
        i = i + 1
        state = 'start'
      else
        error('[coeff_or_var] invalid token #'..tostring(i)..':'..part)
      end
    elseif state == 'var_or_end' then
      if part == '+' or part == '-' or part == '<=' or part == '>=' or part == '==' then
        constant = constant + lhs_sign * tmp.sign * tmp.coeff
        state = 'start'
      elseif part == 'eof' then
        constant = constant + lhs_sign * tmp.sign * tmp.coeff
        state = 'done'
      elseif vars[part] ~= nil then
        terms[#terms+1] = {vars[part], lhs_sign * tmp.sign * tmp.coeff}
        i = i + 1
        state = 'start'
      else
        error('[var_or_end] invalid token #'..tostring(i)..':'..part)
      end
    elseif state == 'start' then
      if part == '+' or part == '-' then
        tmp.sign = part == '+' and 1 or -1
        tmp.coeff = 1
        i = i + 1
        state = 'coeff_or_var'
      elseif part == '<=' or part == '>=' or part == '==' then
        op = part
        lhs_sign = -1
        i = i + 1
        state = 'coeff_or_var'
      elseif part == 'eof' then
        state = 'done'
      else
        error('[start] invalid token #'..tostring(i)..':'..part)
      end
    else
      error('invalid state')
    end
  end

  -- reduce multiple instances of variables
  local coeffs = {}
  for i, term in ipairs(terms) do
    coeffs[term[1]] = (coeffs[term[1]] or 0) + term[2]
  end

  -- build kiwi terms
  terms = {}
  for var, coeff in pairs(coeffs) do
    terms[#terms+1] = term(var, coeff)
  end

  return constraint(expression(terms, constant), op, strngth)
end

--------------------------------------------------------------------------------
-- end

return {
  variable = variable,
  term = term,
  strength = strength,
  expression = expression,
  constraint = constraint,
  solver = solver,
  parse_constraint = parse_constraint,
}
