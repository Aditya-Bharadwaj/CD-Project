class Node:
	arg1 = None
	arg2 = None
	op   = None
	value = None

	def __init__(self, op, arg1, arg2):
		self.op = op;
		self.arg1 = arg1;
		self.arg2 = arg2;

		
	def __str__(self):
		#return str(self.op) + str(self.arg1) + str(self.arg2)
		ret = "Node: op: " + str(self.op) + "| arg1: " + str(self.arg1) + "| arg2: " + str(self.arg2)
		return ret


a = Node("=", "a", "b")
print a

class Terminal:
	value = None
	def __init__(self, value):
		self.value = value
	def __str__(self):
		return str(self.value)
