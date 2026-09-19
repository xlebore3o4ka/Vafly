import forms
import std/[tables]

type
  Context* = ref object
    internmentData*: InternmentData
    env*:            Form

proc symExists(ctx: Context, intern: int): bool =
  var env = ctx.env

  while env != nil and env.mapValue.len != 0:
    if newIntForm(intern) in env.mapValue.at(`ENV-CURRENT`).mapValue:
      return true
    env = env.mapValue.at(`ENV-PARENT`)

  return false