import re

names = {}

f =  open("names","r")
for x in f:
	g = re.match(r'^(.+)@(.+)\.(.+)',x)
	if g:
		names[g.group(2)] = names.setdefault (g.group(2),[]) + [g.group(1)]

print (names)