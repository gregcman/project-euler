import sys
import random


def construct_pyramid(size):
	return [[None for x in range(y) ] for y in range(1, size + 1)]

row = 1
column = 0

def add_to_pyramid(num, pyramid):
	global row
	global column
	pyramid[ row - 1 ][ column ] = num
	column += 1
	if column == row:
		row += 1
		column = 0

pyr = []
pyr = construct_pyramid(1000)

def init():	
	t = 0
	foo = 2**20 
	bar = 2**19
	for k in range(500500):
		t = (615949*t + 797807) % foo
		add_to_pyramid(t - bar, pyr)

class mutator:
	def __init__(self, x, y, height, pyramid):
		self.x = x
		self.y = y
		self.height = height
		self.pyramid = pyramid

	def outsidevals(self):
		a = False
		b = False
		if (self.y > 0):
			if (self.x > 0):
				a = True
			if (self.y > self.x):
				b = True
		c = True if self.y + self.height < len(pyr) else False
		aosum = None
		bosum = None
		cosum = None

		if a:
			aosum = sum(self.pyramid[y][self.x - 1] for y in range(self.y - 1, self.y + self.height))
		if b:
			bosum = sum(self.pyramid[y][self.x + 1 + y - self.y] for y in range(self.y - 1, self.y + self.height))
		if c:
			cosum = sum(self.pyramid[self.y + self.height][x] for x in range(self.x, self.x + self.height + 1))
		return aosum, bosum, cosum

	def insidevals(self):
		aisum = sum(self.pyramid[y][self.x] for y in range(self.y, self.y + self.height))
		bisum = sum(self.pyramid[y][self.x + y - self.y] for y in range(self.y, self.y + self.height))
		cisum = sum(self.pyramid[self.y + self.height - 1][x] for x in range(self.x, self.x + self.height))
		return aisum, bisum, cisum

	def ai(self):
		self.x += 1
		self.y += 1
		self.height -= 1
	def bi(self):
		self.y += 1
		self.height -=1
	def ci(self):
		self.height -= 1

	def ao(self):
		self.x -= 1
		self.y -= 1
		self.height += 1
	def bo(self):
		self.y -= 1
		self.height += 1
	def co(self):
		self.height += 1

	def mutate(self):
		osums = self.outsidevals()
		isums = self.insidevals()

		cropone = [(self.ao, osums[0]),(self.bo,osums[1]),(self.co,osums[2])]
		croptwo = [(self.ai, isums[0]),(self.bi, isums[1]),(self.ci, isums[2])]

		choices = []
		for choice in cropone:
			if choice[1] != None:
				choices.append(choice)
		for choice in croptwo:
			choices.append((choice[0], -choice[1]))

		step = sorted(choices, key=lambda x: x[1])
		if step[-1][1] > 0:
			#print(step[-1][0])
			step[-1][0]()
		else:
			return (self.x, self.y, self.height)

	def sumtotal(self):
		return sum(self.pyramid[y][x]for y in range(self.y + self.height) for x in range(y - self.y + 1) )

init()

"""for line in pyr:
	print (line)
mutt = mutator(0,0,3,pyr)
print(mutt.sumtotal())"""

def randmut():
	ey = random.randrange(1000)
	if ey == 0:
		ex = 0
	else:
		ex = random.randrange(ey)
	eh = random.randrange(1000 - ey)
	return mutator(ex, ey, eh, pyr)

#142109880
biggest = (0,0,0)
presets = None

for i in range(100000):
	m = randmut()
	lol = None
	iterations = 0
	while lol == None:
		lol = m.mutate()
		iterations += 1
	contender = (lol, iterations, m.sumtotal())
	#print(contender)

	if contender[2] > biggest[2]:
		biggest = contender
	print(biggest)





