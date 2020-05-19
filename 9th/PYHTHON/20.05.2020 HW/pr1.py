import re
#from os import listdir

ip = []

f =  open("ip","r")
for x in f:
	g = re.match(r'^(\d+)\.(\d+)\.(\d+)\.(\d+)(.*)\n$',x)
	if g:
		g = re.match(r'(.*)\n',x)
		ip+=[g.group(1)]

print (ip)