local num = {}
num.abs = math.abs
num.acos = math.acos
num.asin = math.asin
num.atan = math.atan
num.atan2 = math.atan2
num.ceil = math.ceil
num.cos = math.cos
num.cosh = math.cosh
num.exp = math.exp
num.floor = math.floor
num.ln = math.log
num.log10 = math.log10
num.max = math.max
num.min = math.min
num.random = math.random
num.random_seed = math.randomseed
num.sin = math.sin
num.sinh = math.sinh
num.sqrt = math.sqrt
num.tan = math.tan
num.tanh = math.tanh

num.INF = math.huge
num.NAN = 0/0
num.PI = math.pi
num.TWO_PI = 6.283185307179586

math = nil
return num
