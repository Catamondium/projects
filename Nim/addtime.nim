import strformat
import strutils
import os
import parseopt
import options

type
  Minutes* = range[-59..59]
  Time* = object
    hrs*: int # top range, cannot have upper limit
    mins*: Minutes

proc abs*(t: Time): int =
  t.hrs * 60 + t.mins

proc `$`(t: Time): string =
  fmt"{t.hrs:02}:{t.mins:02}"

proc fromInt*(mins: int): Time =
  Time(hrs: mins /% 60, mins: mins mod 60)

proc fromString*(source: string): Time =
  if ':' in source:
    let lst = source.split(':', 1)
    return Time(hrs: lst[0].parseInt, mins: lst[1].parseInt)
  else:
    return source.parseInt.fromInt

proc `+`(t: Time, o: int): Time =
  let tot = t.abs + o
  fromInt(tot)

proc usage(status: int) =
  echo fmt"""Usage: {getAppFilename()} [-hql] <HH:MM> <mins | HH:MM>
  note: if mins_elapse is negative, precede it with '--'\n"
  options:
    -q quietly output end time
    -h print this message and exit
    -l print written language"""
  quit(status)

var
  quiet: bool = false
  start: Option[Time]
  elapse: Option[int]
  p = initOptParser()

while true:
  p.next()
  case p.kind
  of cmdEnd:
    if start.isNone or elapse.isNone:
      echo "Missing argument"
      usage(QuitFailure)
    break
  of cmdShortOption, cmdLongOption:
    case p.key[0]
    of 'h':
      usage(QuitSuccess)
    of 'l':
      echo fmt"Nim: {NimVersion}"
      quit(0)
    of 'q':
      quiet = true
    else:
      continue
  of cmdArgument:
    try:
      if start.isNone:
        start = some(fromString(p.key))
      elif elapse.isNone:
        elapse = some(fromString(p.key).abs)
    except ValueError:
      echo "Invalid argument"
      usage(QuitFailure)

if quiet:
  echo fmt"{start.get + elapse.get}"
else:
  echo fmt"""Start: {start.get} {elapse.get:+}
  End: {start.get + elapse.get}"""
