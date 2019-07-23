from fractions import Fraction

raw =  "UDDDUdddDDUDDddDdDddDDUDDdUUDd"

seq = list(raw.replace("D", "0").replace("U", "1").replace("d", "2"))
#print(seq)

#
# reversed section
#
"""
tape = list(reversed(raw))

def rD(num):
	return (Fraction(3 * num[0]), Fraction(3 * num[1]))
def rU(num):
	return (Fraction(3 * num[0], 4), Fraction(3 * num[1] - 2, 4))
def rd(num):
	return (Fraction(3 * num[0], 2), Fraction(3 * num[1] + 1, 2))

original = (1, 0)

for func in tape:
	if func == "D":
		original = rD(original)
	elif func == "U": 
		original = rU(original)
	elif func == "d": 
		original = rd(original)
"""

#
# Forward section
#
"""
def D(num):
	return (Fraction(num[0], 3), Fraction(num[1], 3))
def U(num):
	return (Fraction(num[0] * 4 ,3), Fraction(num[1] * 4 + 2, 3))
def d(num):
	return (Fraction(num[0] * 2, 3), Fraction(num[1] * 2 - 1, 3))

#print(seq)

qualities = []

for i in range(0, len(seq) + 1):
	tup = Fraction(1,1), 0
	for b in range(i):
		func = seq[b]
		if func == "0":
			tup = D(tup)
		elif func == "1": 
			tup = U(tup)
		elif func == "2": 
			tup = d(tup)
	var = int(seq[i-1]) if i < len(seq) else 2
	man = (var, (tup[0].numerator), (tup[1].numerator), tup[0].denominator, tup[1].denominator)
	print(man)
	qualities.append(man)

nums = []
	
for q in qualities:
	#print("here")
	for n in range(q[1]):
		var = ((3 * n + q[0]) * q[3] - q[2])
		if var % q[1] == 0:
			#print(n, q[1], var/q[1], (var/q[1] % 3))
			print(var/q[1], 3 * q[3])
			nums.append((var/q[1], q[3]))

"""


"""
#x = 302412864745714
x = 96521732651065
#x = 508303996840363
for q in qualities:
	print ((x * q[1] + q[2])/q[3])

"""


"""
for x in range(len(nums)-1):
	print((nums[x+1][0] - nums[x][0])/nums[x][1])
"""

def next(num):
	leftovers = num % 3
	if leftovers == 0:
		return num / 3, "D"
	elif leftovers == 1:
		return (4 * num + 2)/3, "U"
	elif leftovers == 2:
		return (2 * num - 1)/3, "d"

def genseq(num, iter):
	string = ""
	n = num
	for i in range(iter):
		n = next(n)
		string += (n[1])
		n = n[0]
	print (num, n, string)

genseq(96521732651065, 26)
genseq(302412864745714, 26)
genseq(508303996840363, 26)
genseq(1125977393124310, 26)

answers = [96521732651065, 302412864745714, 508303996840363]

for i in range(len(answers)):
	while answers[i] < 10**15:
		answers[i] += 617673396283947

print(answers)

"""

def checknum(num, sequence):
	number = num
	flag = 0
	for instruction in sequence:	
		info = next(number)
		if info[1] != instruction:
			return
		else:
			number = info[0]

	print(num, number)


for i in range(10000000):
	var = (i * 4194304 + 1966289) * 205891132094649 + 21110037246199
	#print (var % 4194304)
	checknum(var, seq)"""

"""for i in range(100):
	var = i * 4194304 * 404841468235255287561 + 404841489345292533760
	print (var % 4194304 == 0)
	#checknum(var, seq)"""

"""
for i in range(100000000):
	var = i * 1698028189585004193638252544 + 404841489345292533760
	checknum(var, seq)
"""

"""
#print(205891132094649 % 4194304)
#print(21110037246199 % 4194304) 
#print(Fraction(686265, 356599)) 

print(1966289 * 205891132094649 + 21110037246199)

for i in range(5000000):
	var = (686265 * i + 356599) % 4194304
	if var == 0:
		print(i)
"""