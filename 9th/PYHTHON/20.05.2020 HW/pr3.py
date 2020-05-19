import re

f = open("phone",'r')

def g():
	for x in f:
		phone = ""
		a = re.match(r'^(\d\d\d)-?(\d\d)-?(\d\d)\n$',x)
		b = re.match(r'^\((\d\d\d)\)(\d\d\d)-?(\d\d)-?(\d\d)\n$',x)
		c = re.match(r'^8\-?(\d\d\d)-?(\d\d\d)-?(\d\d)-?(\d\d)\n$',x)
		d = re.match(r'^\+(\d)-?(\d\d\d)-?(\d\d\d)-?(\d\d)-?(\d\d)\n$',x)
		if a:
			phone = '+7-812-'+a.group(1)+"-"+a.group(2)+"-"+a.group(3)
		elif b:
			phone = "+7-"+b.group(1)+"-"+b.group(2)+"-"+b.group(3)+"-"+b.group(4)
		elif c:
			phone = "+7-"+c.group(1)+"-"+c.group(2)+"-"+c.group(3)+"-"+c.group(4)
		elif d:
			if (d.group(1) == '7'):
				phone = "+7-"+d.group(2)+"-"+d.group(3)+"-"+d.group(4)+"-"+d.group(5)
			else:
				phone = ("+"+d.group(1)+'-'+d.group(2)+"-"+d.group(3)+"-"+d.group(4)+"-"+d.group(5),"Not russian")
		else:
			phone = "ERROR"
		return phone

print(g())


