import strformat

type
  Minutes* = range[-59..59]
  Time* = object
    hrs*: int # top range, cannot have upper limit
    mins*: Minutes

proc abs(t: Time): int =
  t.hrs*60 + t.mins

proc `$`(t: Time): string =
  fmt"{t.hrs}:{t.mins}"

proc `+`(t: Time, o: Time): Time =
  let tot = t.abs + o.abs
  return Time(hrs: tot /% 60, mins: tot mod 60)

let t = Time(hrs: 11, mins: 30)
echo t
