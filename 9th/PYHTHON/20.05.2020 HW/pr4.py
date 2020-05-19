import re

f = open("snames","r")

l = []

for x in f:
	a = re.match("^([A-Z][a-z]+)\n$",x)
	b = re.match("^([A-Z][a-z]+) ([A-Z][a-z]+)$\n",x)
	c = re.match("^([A-Z][a-z]+) ([A-Z][a-z]+) ([A-Z][a-z]+)$\n",x)
	if a:
		l+=[("-",a.group(1),"-")]
	elif b:
		l+=[(b.group(1),b.group(2),"-")]
	elif c:
		l+=[(c.group(1),c.group(2),c.group(3))]

print (l)
	